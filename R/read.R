glc_resource_dialect_for_path <- function(x, path) {
  resources <- x$descriptor$resources %||% list()
  for (resource in resources) {
    declared <- glc_compact_character(resource$path)
    matches <- vapply(
      declared,
      function(candidate) {
        prefix <- sub("/$", "", candidate)
        identical(path, candidate) ||
          (endsWith(candidate, "/") && startsWith(path, paste0(prefix, "/")))
      },
      logical(1)
    )
    if (any(matches)) {
      return(resource$dialect %||% list())
    }
  }
  list()
}

glc_split_header <- function(line, delimiter) {
  trimws(strsplit(line, delimiter, fixed = TRUE)[[1L]])
}

glc_detect_layout <- function(
  path,
  variable_names,
  encoding,
  header_row = NA_integer_,
  delimiter = NA_character_
) {
  line_count <- if (is.na(header_row)) 60L else max(60L, header_row)
  lines <- readLines(
    path,
    n = line_count,
    warn = FALSE,
    encoding = encoding,
    skipNul = TRUE
  )
  if (length(lines) == 0L) {
    glc_abort("Data file is empty: {.path {path}}.", class = "glcdp_empty_data")
  }
  lines[[1L]] <- sub("^\\ufeff", "", lines[[1L]])
  delimiters <- if (!is.na(delimiter) && nzchar(delimiter)) {
    delimiter
  } else {
    c(",", ";", "\t", "|")
  }

  candidate_rows <- if (!is.na(header_row)) {
    if (header_row < 1L || header_row > length(lines)) {
      glc_abort(
        "Declared header row {header_row} is outside the inspected portion of {.path {path}}."
      )
    }
    header_row
  } else {
    seq_along(lines)
  }
  scores <- list()
  score_index <- 0L
  for (delim in delimiters) {
    for (row in candidate_rows) {
      fields <- glc_split_header(lines[[row]], delim)
      score_index <- score_index + 1L
      scores[[score_index]] <- list(
        delimiter = delim,
        row = row,
        overlap = sum(fields %in% variable_names),
        field_count = length(fields)
      )
    }
  }
  overlaps <- vapply(scores, function(score) score$overlap, integer(1))
  field_counts <- vapply(scores, function(score) score$field_count, integer(1))
  rows <- vapply(scores, function(score) score$row, integer(1))

  if (is.na(header_row) && length(variable_names) > 0L && max(overlaps) == 0L) {
    glc_abort(
      "Could not find a header matching the metadata-declared variables in the first {length(lines)} lines of {.path {path}}.",
      class = "glcdp_header_not_found"
    )
  }
  best <- which(
    overlaps == max(overlaps) &
      field_counts == max(field_counts[overlaps == max(overlaps)])
  )
  if (length(best) > 1L) {
    best <- best[rows[best] == min(rows[best])]
  }
  selected <- scores[[best[[1L]]]]
  list(delimiter = selected$delimiter, header_row = selected$row)
}

glc_problem <- function(message, mode, class = "glcdp_import_problem") {
  if (identical(mode, "error")) {
    glc_abort(message, class = class)
  } else {
    glc_warn(message, class = class)
  }
}

glc_datetime_format <- function(format) {
  if (is.na(format) || !nzchar(format)) return(NA_character_)
  if (grepl("%", format, fixed = TRUE)) return(format)
  format <- gsub("ss.SSS", "%OS3", format, fixed = TRUE)
  replacements <- c(
    "YYYY" = "%Y",
    "SSS" = "%OS3",
    "YY" = "%y",
    "DD" = "%d",
    "MM" = "%m",
    "HH" = "%H",
    "mm" = "%M",
    "ss" = "%S"
  )
  result <- format
  for (token in names(replacements)) {
    result <- gsub(token, replacements[[token]], result, fixed = TRUE)
  }
  result
}

glc_parse_datetime <- function(data, group, mode) {
  specification <- group$datetime
  timezone <- group$timezone
  if (is.na(timezone) || !timezone %in% OlsonNames()) {
    glc_abort(
      "File group {.val {group$id}} must declare a valid IANA time zone; supplied {.val {timezone}}.",
      class = "glcdp_timezone_error"
    )
  }
  date_value <- specification$date
  date_format <- glc_datetime_format(specification$date_format)
  if (is.na(date_value) || is.na(date_format)) {
    glc_abort(
      "File group {.val {group$id}} does not provide complete datetime metadata.",
      class = "glcdp_datetime_metadata"
    )
  }

  if (identical(specification$source, "collection")) {
    values <- rep(date_value, nrow(data))
    format <- date_format
  } else {
    if (!date_value %in% names(data)) {
      glc_abort(
        "Datetime column {.val {date_value}} declared for group {.val {group$id}} is missing."
      )
    }
    values <- as.character(data[[date_value]])
    format <- date_format
    if (!is.na(specification$time) && nzchar(specification$time)) {
      if (!specification$time %in% names(data)) {
        glc_abort(
          "Time column {.val {specification$time}} declared for group {.val {group$id}} is missing."
        )
      }
      values <- paste(values, as.character(data[[specification$time]]))
      format <- paste(format, glc_datetime_format(specification$time_format))
    }
  }
  parsed_clock <- strptime(values, format = format, tz = timezone)
  parsed <- lubridate::as_datetime(as.numeric(parsed_clock), tz = timezone)
  invalid <- !is.na(values) & nzchar(values) & is.na(parsed)
  if (any(invalid)) {
    glc_problem(
      paste0(
        "Could not parse ",
        sum(invalid),
        " datetime value(s) in file group `",
        group$id,
        "` using format `",
        format,
        "` and time zone `",
        timezone,
        "`."
      ),
      mode,
      class = "glcdp_datetime_parse"
    )
  }
  parsed
}

glc_invalid_cast <- function(original, parsed) {
  !is.na(original) & nzchar(trimws(original)) & is.na(parsed)
}

glc_cast_variable <- function(value, variable, locale, mode) {
  original <- as.character(value)
  type <- variable$type
  parsed <- switch(
    type,
    string = original,
    boolean = {
      lower <- tolower(original)
      result <- rep(NA, length(lower))
      result[lower == "true"] <- TRUE
      result[lower == "false"] <- FALSE
      result[is.na(original) | !nzchar(trimws(original))] <- NA
      result
    },
    numeric = suppressWarnings(readr::parse_double(original, locale = locale)),
    integer = suppressWarnings(readr::parse_integer(original, locale = locale)),
    factor = {
      levels <- variable$factor_levels
      values <- vapply(levels, function(level) level$value, character(1))
      labels <- vapply(levels, function(level) level$label, character(1))
      factor(original, levels = values, labels = labels)
    },
    guess = suppressWarnings(readr::parse_guess(original, locale = locale)),
    glc_abort(
      "Unsupported declared variable type {.val {type}} for {.val {variable$name}}."
    )
  )
  invalid <- glc_invalid_cast(original, parsed)
  if (any(invalid)) {
    glc_problem(
      paste0(
        "Variable `",
        variable$name,
        "` contains ",
        sum(invalid),
        " value(s) incompatible with declared type `",
        type,
        "`."
      ),
      mode,
      class = "glcdp_type_parse"
    )
  }
  parsed
}

glc_selected_variable_names <- function(
  group,
  variables,
  terms,
  primary_only
) {
  metadata <- group$variables
  names <- vapply(metadata, function(variable) variable$name, character(1))
  selected <- rep(TRUE, length(metadata))
  any_filter <- FALSE
  if (!is.null(variables)) {
    selected <- selected & names %in% variables
    any_filter <- TRUE
  }
  if (!is.null(terms)) {
    variable_terms <- vapply(
      metadata,
      function(variable) variable$term,
      character(1)
    )
    selected <- selected & variable_terms %in% terms
    any_filter <- TRUE
  }
  if (primary_only) {
    selected <- selected &
      vapply(metadata, function(variable) variable$primary, logical(1))
    any_filter <- TRUE
  }
  chosen <- names[selected]
  if (any_filter && length(chosen) == 0L) {
    glc_abort("Variable selection is empty for file group {.val {group$id}}.")
  }
  list(names = chosen, filtered = any_filter)
}

glc_group_matches_variables <- function(group, variables, terms, primary_only) {
  metadata <- group$variables
  selected <- rep(TRUE, length(metadata))
  if (!is.null(variables)) {
    names <- vapply(metadata, function(variable) variable$name, character(1))
    selected <- selected & names %in% variables
  }
  if (!is.null(terms)) {
    variable_terms <- vapply(
      metadata,
      function(variable) variable$term,
      character(1)
    )
    selected <- selected & variable_terms %in% terms
  }
  if (primary_only) {
    selected <- selected &
      vapply(metadata, function(variable) variable$primary, logical(1))
  }
  any(selected)
}

glc_validate_variable_filters <- function(
  datasets,
  file_group,
  variables,
  terms
) {
  groups <- unlist(
    lapply(datasets, function(dataset) {
      dataset$groups[vapply(
        dataset$groups,
        glc_group_selected,
        logical(1),
        file_group = file_group,
        role = NULL,
        modality = NULL
      )]
    }),
    recursive = FALSE
  )
  available_variables <- glc_unique_chr(lapply(groups, function(group) {
    vapply(group$variables, function(variable) variable$name, character(1))
  }))
  available_terms <- glc_unique_chr(lapply(groups, function(group) {
    vapply(group$variables, function(variable) variable$term, character(1))
  }))
  unknown_variables <- setdiff(variables %||% character(), available_variables)
  unknown_terms <- setdiff(terms %||% character(), available_terms)
  if (length(unknown_variables) > 0L) {
    glc_abort("Unknown selected variable{?s}: {.val {unknown_variables}}.")
  }
  if (length(unknown_terms) > 0L) {
    glc_abort("Unknown selected semantic term{?s}: {.val {unknown_terms}}.")
  }
  invisible(TRUE)
}

glc_read_group_file <- function(
  x,
  dataset,
  group,
  path,
  variables,
  terms,
  primary_only,
  n_max,
  mode
) {
  if (!group$format %in% c("csv", "txt", "tsv")) {
    glc_abort(
      "File {.path {path}} uses unsupported format {.val {group$format}}. Use a specialized LightLogR importer for non-rectangular device formats.",
      class = "glcdp_unsupported_data_format"
    )
  }
  local_path <- glc_materialize_file(x, path)
  declared_names <- vapply(
    group$variables,
    function(variable) variable$name,
    character(1)
  )
  dialect <- glc_resource_dialect_for_path(x, path)
  delimiter <- glc_scalar_character(dialect$delimiter)
  layout <- glc_detect_layout(
    local_path,
    variable_names = declared_names,
    encoding = group$encoding,
    header_row = group$header_row,
    delimiter = delimiter
  )
  decimal_mark <- glc_scalar_character(
    dialect$decimalChar %||% dialect$decimal_mark,
    "."
  )
  locale <- readr::locale(
    decimal_mark = decimal_mark,
    encoding = group$encoding
  )
  data <- readr::read_delim(
    local_path,
    delim = layout$delimiter,
    skip = layout$header_row - 1L,
    col_types = readr::cols(.default = readr::col_character()),
    locale = locale,
    na = "",
    trim_ws = TRUE,
    n_max = n_max,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
  reserved <- names(data)[startsWith(names(data), ".glc_")]
  if (length(reserved) > 0L) {
    glc_abort(
      "Source file {.path {path}} uses reserved provenance column{?s}: {.val {reserved}}."
    )
  }
  missing <- setdiff(declared_names, names(data))
  if (length(missing) > 0L) {
    glc_abort(
      "File {.path {path}} is missing metadata-declared column{?s}: {.val {missing}}.",
      class = "glcdp_missing_column"
    )
  }
  extra <- setdiff(names(data), declared_names)
  if (length(extra) > 0L) {
    glc_problem(
      paste0(
        "File `",
        path,
        "` contains undeclared column(s): ",
        paste(extra, collapse = ", "),
        "."
      ),
      mode,
      class = "glcdp_extra_column"
    )
  }

  datetime <- glc_parse_datetime(data, group, mode)
  for (variable in group$variables) {
    data[[variable$name]] <- glc_cast_variable(
      data[[variable$name]],
      variable,
      locale,
      mode
    )
  }
  selection <- glc_selected_variable_names(
    group,
    variables,
    terms,
    primary_only
  )
  if (selection$filtered) {
    data <- data[, selection$names, drop = FALSE]
  }
  data[[".glc_dataset_id"]] <- dataset$id
  data[[".glc_file_group"]] <- group$id
  data[[".glc_participant_id"]] <- dataset$participant_id
  data[[".glc_source_file"]] <- path
  data[[".glc_datetime"]] <- datetime
  tibble::as_tibble(data)
}

glc_data_signature <- function(data) {
  paste(
    names(data),
    vapply(
      data,
      function(column) paste(class(column), collapse = "/"),
      character(1)
    ),
    sep = ":"
  )
}

glc_bind_group_files <- function(data, group) {
  signatures <- lapply(data, glc_data_signature)
  reference <- signatures[[1L]]
  incompatible <- which(!vapply(signatures, identical, logical(1), reference))
  if (length(incompatible) > 0L) {
    glc_abort(
      "Files in group {.val {group$id}} do not produce the same columns and types.",
      class = "glcdp_incompatible_group_files"
    )
  }
  dplyr::bind_rows(data)
}

glc_resolve_read_path <- function(x, path) {
  tryCatch(
    glc_resolve_path(x, path),
    glcdp_missing_path = function(cnd) {
      if (identical(x$source_type, "local")) return(NA_character_)
      stop(cnd)
    }
  )
}

glc_inform_local_subset <- function(x) {
  if (!identical(x$source_type, "local")) return(invisible(NULL))

  datasets <- glc_model(x)$datasets
  declared_dataset_ids <- vapply(
    datasets,
    function(dataset) dataset$id,
    character(1)
  )
  files <- glc_files(x)
  available_files <- files$available %in% TRUE
  available_dataset_ids <- unique(files$dataset_id[available_files])
  if (
    length(available_dataset_ids) == length(declared_dataset_ids) &&
      sum(available_files) == nrow(files)
  ) {
    return(invisible(NULL))
  }

  unavailable_dataset_ids <- setdiff(
    declared_dataset_ids,
    available_dataset_ids
  )
  dataset_label <- if (length(declared_dataset_ids) == 1L) {
    "dataset"
  } else {
    "datasets"
  }
  file_label <- if (nrow(files) == 1L) "file" else "files"
  message <- c(
    "Local package is a partial data subset.",
    "i" = paste0(
      length(available_dataset_ids),
      " of ",
      length(declared_dataset_ids),
      " declared ",
      dataset_label,
      " and ",
      sum(available_files),
      " of ",
      nrow(files),
      " declared ",
      file_label,
      " are locally available."
    )
  )
  if (length(unavailable_dataset_ids) > 0L) {
    unavailable_label <- if (length(unavailable_dataset_ids) == 1L) {
      "Unavailable dataset: "
    } else {
      "Unavailable datasets: "
    }
    message <- c(
      message,
      "i" = paste0(
        unavailable_label,
        paste(unavailable_dataset_ids, collapse = ", "),
        "."
      )
    )
  }
  message <- c(
    message,
    "i" = "glc_read() will read only locally available files."
  )
  glc_inform(message, class = "glcdp_local_subset")
  invisible(NULL)
}

#' Read metadata-described dataset files
#'
#' @param x A package opened with [glc_open()].
#' @param dataset_id Dataset id or ids. Use `"all"` explicitly to read every
#'   dataset.
#' @param file_group Optional group index or stable id.
#' @param files Optional declared paths, resolved paths, or basenames.
#' @param variables Optional source variable names.
#' @param terms Optional semantic variable terms.
#' @param primary_only Select only declared primary variables.
#' @param n_max Maximum rows read from each file.
#' @param problems Whether metadata mismatches should be errors or warnings.
#' @param progress Show progress while files are imported. Defaults to `TRUE`
#'   in interactive sessions and `FALSE` otherwise.
#'
#' @details
#' When variable filters are used, source columns required to construct
#' datetimes are used internally but omitted unless selected by the filters.
#' Declared files that are absent from a local package subset are skipped. When
#' a local package contains fewer datasets or files than declared,
#' `glc_read()` reports the discrepancy and reads the available files.
#'
#' @return A `glc_data_collection` tibble with one data list-column per file
#'   group.
#' @export
glc_read <- function(
  x,
  dataset_id,
  file_group = NULL,
  files = NULL,
  variables = NULL,
  terms = NULL,
  primary_only = FALSE,
  n_max = Inf,
  problems = c("error", "warn"),
  progress = interactive()
) {
  glc_assert_package(x)
  problems <- match.arg(problems)
  glc_assert_flag(primary_only, "primary_only")
  glc_assert_flag(progress, "progress")
  if (
    !is.numeric(n_max) ||
      length(n_max) != 1L ||
      is.na(n_max) ||
      n_max < 0 ||
      (is.finite(n_max) && n_max != floor(n_max))
  ) {
    glc_abort("{.arg n_max} must be one non-negative number or `Inf`.")
  }
  if (
    !is.null(file_group) &&
      (!(is.character(file_group) || is.numeric(file_group)) ||
        anyNA(file_group))
  ) {
    glc_abort(
      "{.arg file_group} must contain group ids or indices without missing values."
    )
  }
  for (argument in c("variables", "terms", "files")) {
    value <- get(argument)
    if (!is.null(value) && (!is.character(value) || anyNA(value))) {
      glc_abort(
        "{.arg {argument}} must be a character vector without missing values."
      )
    }
  }
  datasets <- glc_selected_datasets(x, dataset_id)
  glc_validate_variable_filters(datasets, file_group, variables, terms)
  selected_groups <- list()
  used_files <- character()
  group_index <- 0L
  for (dataset in datasets) {
    for (group in dataset$groups) {
      if (!glc_group_selected(group, file_group, role = NULL, modality = NULL))
        next
      if (!glc_group_matches_variables(group, variables, terms, primary_only))
        next
      group_files <- group$files
      resolved_all <- vapply(
        group_files,
        function(path) glc_resolve_read_path(x, path),
        character(1)
      )
      if (!is.null(files)) {
        keep <- group_files %in%
          files |
          resolved_all %in% files |
          basename(resolved_all) %in% files
        group_files <- group_files[keep]
        resolved_all <- resolved_all[keep]
      }
      available_files <- !is.na(resolved_all)
      group_files <- group_files[available_files]
      resolved_all <- resolved_all[available_files]
      if (length(group_files) == 0L) next
      resolved <- resolved_all
      used_files <- c(used_files, group_files, resolved, basename(resolved))
      group_index <- group_index + 1L
      selected_groups[[group_index]] <- list(
        dataset = dataset,
        group = group,
        resolved = resolved
      )
    }
  }
  if (!is.null(files)) {
    unmatched <- files[!files %in% used_files]
    if (length(unmatched) > 0L) {
      glc_abort("Selected file{?s} not found: {.path {unmatched}}.")
    }
  }
  glc_inform_local_subset(x)
  if (length(selected_groups) == 0L) {
    glc_abort("The selection contains no readable file groups.")
  }

  progress_id <- NULL
  if (progress) {
    file_count <- sum(vapply(
      selected_groups,
      function(selection) length(selection$resolved),
      integer(1)
    ))
    progress_id <- cli::cli_progress_bar(
      name = "Reading GLC files",
      type = "tasks",
      total = file_count,
      clear = FALSE
    )
  }

  rows <- lapply(selected_groups, function(selection) {
    dataset <- selection$dataset
    group <- selection$group
    resolved <- selection$resolved
    data <- lapply(resolved, function(path) {
      if (progress) {
        cli::cli_progress_update(
          id = progress_id,
          inc = 0L,
          status = path,
          force = TRUE
        )
      }
      value <- glc_read_group_file(
        x,
        dataset,
        group,
        path,
        variables,
        terms,
        primary_only,
        n_max,
        problems
      )
      if (progress) cli::cli_progress_update(id = progress_id)
      value
    })
    combined <- glc_bind_group_files(data, group)
    tibble::tibble(
      dataset_id = dataset$id,
      file_group = group$index,
      file_group_id = group$id,
      study_id = dataset$study_id,
      participant_id = dataset$participant_id,
      device_id = group$device_id,
      modalities = list(group$modality),
      role = group$role,
      data_state = group$data_state,
      timezone = group$timezone,
      datetime_source = group$datetime$source,
      datetime_date = group$datetime$date,
      datetime_format = group$datetime$date_format,
      datetime_time = group$datetime$time,
      datetime_time_format = group$datetime$time_format,
      primary_variables = list(group$primary_variables),
      files = list(resolved),
      data = list(combined)
    )
  })
  result <- dplyr::bind_rows(rows)
  class(result) <- c("glc_data_collection", class(result))
  result
}

#' @export
print.glc_data_collection <- function(x, ...) {
  cat("<GLC data collection>\n")
  cat("File groups: ", nrow(x), "\n", sep = "")
  cat(
    "Rows: ",
    sum(vapply(x$data, nrow, integer(1))),
    "\n",
    sep = ""
  )
  print(tibble::as_tibble(x[, setdiff(names(x), "data"), drop = FALSE]), ...)
  invisible(x)
}
