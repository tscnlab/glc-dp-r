write_fixture_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    value,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
}

fixture_variables_v2 <- function() {
  list(
    list(
      dataset_file_variables_name = "timestamp",
      dataset_file_variables_labels = "Timestamp",
      dataset_file_variables_units = "ISO8601",
      dataset_file_variables_term = list(
        variable_term = "other",
        variable_name = "timestamp"
      )
    ),
    list(
      dataset_file_variables_name = "lux",
      dataset_file_variables_labels = "Illuminance",
      dataset_file_variables_units = "lx",
      dataset_file_variables_term = list(variable_term = "photopic illuminance")
    )
  )
}

fixture_variables_v3 <- function() {
  list(
    list(
      dataset_file_variables_name = "timestamp",
      dataset_file_variables_labels = "Timestamp",
      dataset_file_variables_units = "ISO8601",
      dataset_file_variables_type = "string",
      dataset_file_variables_term = list(
        variable_term = "other",
        variable_name = "timestamp"
      )
    ),
    list(
      dataset_file_variables_name = "lux",
      dataset_file_variables_labels = "Illuminance",
      dataset_file_variables_units = "lx",
      dataset_file_variables_type = "numeric",
      dataset_file_variables_term = list(variable_term = "photopic illuminance")
    ),
    list(
      dataset_file_variables_name = "worn",
      dataset_file_variables_labels = "Worn",
      dataset_file_variables_units = "1",
      dataset_file_variables_type = "boolean",
      dataset_file_variables_term = list(
        variable_term = "other",
        variable_name = "worn"
      )
    ),
    list(
      dataset_file_variables_name = "quality",
      dataset_file_variables_labels = "Quality",
      dataset_file_variables_units = "1",
      dataset_file_variables_type = "factor",
      dataset_file_variables_factor_levels = list(
        list(value = "good", label = "Good"),
        list(value = "bad", label = "Bad")
      ),
      dataset_file_variables_term = list(
        variable_term = "other",
        variable_name = "quality"
      )
    )
  )
}

make_glc_fixture <- function(
  version = "2.0.0",
  preamble = FALSE,
  explicit_header = preamble,
  extra_column = FALSE,
  invalid_boolean = FALSE,
  participant_associated = TRUE
) {
  root <- tempfile("glcdp-fixture-")
  dir.create(file.path(root, "data", "files"), recursive = TRUE)
  dir.create(file.path(root, "data", "datasheets"), recursive = TRUE)

  core_resources <- list(
    list(
      name = "study",
      path = "data/study.json",
      mediatype = "application/json"
    ),
    list(
      name = "participants",
      path = "data/participants.json",
      format = "json"
    ),
    list(
      name = "datasets",
      path = "data/datasets.json",
      mediatype = "application/json"
    ),
    list(
      name = "devices",
      path = "data/devices.json",
      mediatype = "application/json"
    ),
    list(
      name = "device_datasheets",
      path = "data/datasheets/",
      mediatype = "application/json"
    )
  )
  descriptor <- list(
    name = "fixture-package",
    title = "Fixture package",
    resources = c(
      core_resources,
      list(list(
        name = "notes",
        path = "data/notes.csv",
        format = "csv",
        mediatype = "text/csv"
      ))
    )
  )
  if (!identical(version, "1.0.0")) descriptor$schema_version <- version

  write_fixture_json(
    list(study_internal_id = "S1", title = "Light study"),
    file.path(root, "data", "study.json")
  )
  participants <- if (participant_associated) {
    list(list(participant_internal_id = "P1"))
  } else {
    list()
  }
  write_fixture_json(participants, file.path(root, "data", "participants.json"))
  write_fixture_json(
    list(list(device_internal_id = "D1")),
    file.path(root, "data", "devices.json")
  )
  write_fixture_json(
    list(device_datasheet_internal_id = "D1-sheet"),
    file.path(root, "data", "datasheets", "D1.json")
  )
  writeLines("key,value\nnote,example", file.path(root, "data", "notes.csv"))

  data_path <- "data/files/light.csv"
  if (identical(version, "3.0.0")) {
    datetime <- list(
      dataset_file_datetime_source = "column",
      dataset_file_datetime_date = "timestamp",
      dataset_file_datetime_dateformat = "YYYY-MM-DD HH:mm:ss"
    )
    group <- list(
      dataset_file_modality = list("light"),
      dataset_file_crossref_device_id = "D1",
      dataset_file_device_location = "non-dominant wrist",
      dataset_file_device_location_type = "body_worn",
      dataset_file_temporal_resolution = list(
        resolution_type = "fixed_interval",
        value = 60,
        unit = "seconds"
      ),
      dataset_file_instructions = "Wear continuously",
      dataset_file_names = list(data_path),
      dataset_file_format = "csv",
      dataset_file_encoding = list("UTF-8"),
      dataset_file_timezone = "Europe/Berlin",
      dataset_file_datetime = datetime,
      dataset_file_role = "primary",
      dataset_file_data_state = "raw",
      dataset_file_preprocessing = list(
        dataset_file_preprocessing_bol = FALSE,
        dataset_file_preprocessing_desc = NULL
      ),
      dataset_file_variables = fixture_variables_v3(),
      primary_variables = list("lux")
    )
    if (explicit_header) group$dataset_file_header_row <- 3L
    dataset <- list(
      schema_version = "3.0.0",
      dataset_internal_id = "DS1",
      dataset_participant_associated = participant_associated,
      dataset_crossref = list(
        dataset_crossref_study_id = "S1",
        dataset_crossref_participant_id = if (participant_associated) "P1" else
          NULL
      ),
      dataset_timezone = "Europe/Berlin",
      dataset_location = list(48.1, 11.5),
      dataset_variable_terms = list(
        list(term = "photopic illuminance", label = "Illuminance"),
        list(term = "other", label = "Other")
      ),
      dataset_file = list(group)
    )
    worn <- if (invalid_boolean) "yes" else "true"
    header <- "timestamp,lux,worn,quality"
    if (extra_column) header <- paste0(header, ",extra")
    row1 <- paste("2026-01-01 08:00:00", "12.5", worn, "good", sep = ",")
    row2 <- paste("2026-01-01 08:01:00", "15", "false", "bad", sep = ",")
    if (extra_column) {
      row1 <- paste0(row1, ",x")
      row2 <- paste0(row2, ",y")
    }
  } else {
    group <- list(
      dataset_file_names = list(data_path),
      dataset_file_format = "csv",
      dataset_file_encoding = list("UTF-8"),
      dataset_file_timezone = "Europe/Berlin",
      dataset_file_auxiliary = FALSE,
      dataset_file_preprocessing = list(dataset_file_preprocessing_bol = FALSE),
      dataset_file_variables = fixture_variables_v2(),
      primary_variables = list("lux")
    )
    if (explicit_header) group$dataset_file_header_row <- 3L
    dataset <- list(
      dataset_internal_id = "DS1",
      dataset_crossref = list(
        dataset_crossref_study_id = "S1",
        dataset_crossref_participant_id = "P1",
        dataset_crossref_device_id = "D1"
      ),
      dataset_sampling_interval = 60,
      dataset_datetime = list(
        dataset_datetime_date = "timestamp",
        dataset_datetime_dateformat = "YYYY-MM-DD HH:mm:ss"
      ),
      dataset_timezone = "Europe/Berlin",
      dataset_location = list("48.1", "11.5"),
      dataset_file = list(group)
    )
    if (!identical(version, "1.0.0")) dataset$schema_version <- version
    header <- "timestamp,lux"
    if (extra_column) header <- paste0(header, ",extra")
    row1 <- "2026-01-01 08:00:00,12.5"
    row2 <- "2026-01-01 08:01:00,15"
    if (extra_column) {
      row1 <- paste0(row1, ",x")
      row2 <- paste0(row2, ",y")
    }
  }
  lines <- c(header, row1, row2)
  if (preamble) lines <- c("Device export", "Created for test", lines)
  writeLines(lines, file.path(root, data_path), useBytes = TRUE)
  write_fixture_json(list(dataset), file.path(root, "data", "datasets.json"))
  write_fixture_json(descriptor, file.path(root, "datapackage.json"))
  root
}

make_registry_fixture <- function() {
  path <- tempfile(fileext = ".json")
  sha_a <- paste(rep("a", 40), collapse = "")
  sha_b <- paste(rep("b", 40), collapse = "")
  sha_c <- paste(rep("c", 40), collapse = "")
  sha_d <- paste(rep("d", 40), collapse = "")
  registry <- list(
    generated_at_utc = "2026-07-16T10:00:00Z",
    datasets = list(
      list(
        id = "passing",
        repo = "example/passing",
        branch = "main",
        current_status = "pass",
        resolved_commit_sha = sha_a,
        current = list(
          status = "pass",
          commit_sha = sha_a,
          errors = 0,
          warnings = 1
        ),
        latest_pass = list(status = "pass", commit_sha = sha_a),
        attestation_verified = TRUE
      ),
      list(
        id = "older-pass",
        repo = "example/older-pass",
        branch = "main",
        current = list(status = "fail", commit_sha = sha_b, errors = 2),
        latest_pass = list(status = "pass", commit_sha = sha_c)
      ),
      list(
        id = "no-pass",
        repo = "example/no-pass",
        branch = "main",
        current = list(status = "fail", commit_sha = sha_d, errors = 3),
        latest_pass = NULL
      )
    )
  )
  write_fixture_json(registry, path)
  path
}

fixture_read_datasets <- function(root) {
  jsonlite::fromJSON(
    file.path(root, "data", "datasets.json"),
    simplifyVector = FALSE
  )
}

fixture_write_datasets <- function(root, datasets) {
  write_fixture_json(datasets, file.path(root, "data", "datasets.json"))
}

make_collection_datetime_fixture <- function() {
  root <- make_glc_fixture("3.0.0")
  datasets <- fixture_read_datasets(root)
  group <- datasets[[1]]$dataset_file[[1]]
  group$dataset_file_datetime <- list(
    dataset_file_datetime_source = "collection",
    dataset_file_datetime_date = "2026-01-01 09:30:00",
    dataset_file_datetime_dateformat = "YYYY-MM-DD HH:mm:ss",
    dataset_file_datetime_time = NULL,
    dataset_file_datetime_timeformat = NULL
  )
  datasets[[1]]$dataset_file[[1]] <- group
  fixture_write_datasets(root, datasets)
  root
}

make_separate_datetime_fixture <- function() {
  root <- make_glc_fixture("3.0.0")
  datasets <- fixture_read_datasets(root)
  group <- datasets[[1]]$dataset_file[[1]]
  group$dataset_file_datetime <- list(
    dataset_file_datetime_source = "column",
    dataset_file_datetime_date = "date",
    dataset_file_datetime_dateformat = "YYYY-MM-DD",
    dataset_file_datetime_time = "time",
    dataset_file_datetime_timeformat = "HH:mm:ss"
  )
  date_variable <- fixture_variables_v3()[[1]]
  date_variable$dataset_file_variables_name <- "date"
  date_variable$dataset_file_variables_labels <- "Date"
  date_variable$dataset_file_variables_term$variable_name <- "date"
  time_variable <- date_variable
  time_variable$dataset_file_variables_name <- "time"
  time_variable$dataset_file_variables_labels <- "Time"
  time_variable$dataset_file_variables_term$variable_name <- "time"
  group$dataset_file_variables <- c(
    list(date_variable, time_variable),
    fixture_variables_v3()[-1]
  )
  datasets[[1]]$dataset_file[[1]] <- group
  fixture_write_datasets(root, datasets)
  writeLines(
    c(
      "date,time,lux,worn,quality",
      "2026-01-01,08:00:00,12.5,true,good",
      "2026-01-01,08:01:00,15,false,bad"
    ),
    file.path(root, "data", "files", "light.csv")
  )
  root
}

make_multi_dataset_fixture <- function() {
  root <- make_glc_fixture("3.0.0")
  datasets <- fixture_read_datasets(root)
  second <- datasets[[1]]
  second$dataset_internal_id <- "DS2"
  second$dataset_file[[1]]$dataset_file_names <- list(
    "data/files/light-2.csv"
  )
  datasets[[2]] <- second
  fixture_write_datasets(root, datasets)
  file.copy(
    file.path(root, "data", "files", "light.csv"),
    file.path(root, "data", "files", "light-2.csv")
  )
  root
}
