make_read_factor_union_fixture <- function(conflict = FALSE) {
  root <- make_glc_fixture("3.0.2")
  first <- fixture_read_datasets(root)[[1L]]
  second <- first
  second$dataset_internal_id <- "DS2"
  levels <- second$dataset_file[[1L]]$dataset_file_variables[[4L]][[
    "dataset_file_variables_factor_levels"
  ]]
  if (conflict) {
    levels[[1L]]$label <- "Accepted"
  } else {
    levels <- c(
      levels,
      list(list(
        value = "maybe",
        label = "Maybe",
        description = "Uncertain observation"
      ))
    )
  }
  second$dataset_file[[1L]]$dataset_file_variables[[4L]][[
    "dataset_file_variables_factor_levels"
  ]] <- levels
  fixture_write_datasets(root, list(first, second))
  root
}

test_that("schema 3 files are imported from metadata-defined headers and types", {
  package <- glc_open(
    make_glc_fixture("3.0.2", preamble = TRUE, explicit_header = TRUE),
    quiet = TRUE
  )
  collection <- glc_read(package, dataset_id = "DS1")
  data <- collection$data[[1]]

  expect_s3_class(collection, "glc_data_collection")
  expect_equal(nrow(data), 2)
  expect_type(data$lux, "double")
  expect_type(data$worn, "logical")
  expect_s3_class(data$quality, "factor")
  expect_equal(levels(data$quality), c("Good", "Bad"))
  expect_s3_class(data$.glc_datetime, "POSIXct")
  expect_equal(data$.glc_dataset_id, rep("DS1", 2))
  expect_identical(
    collection$factor_contract[[1L]]$schema,
    "glc-factor-contract"
  )
  expect_match(collection$factor_contract[[1L]]$fingerprint, "^[0-9a-f]{64}$")
})

test_that("schema 3.0.0 retains the same typed import compatibility", {
  package <- glc_open(make_v3_contract_fixture("3.0.0"), quiet = TRUE)
  data <- glc_read(package, dataset_id = "DS1")$data[[1L]]

  expect_equal(package$schema_version, "3.0.0")
  expect_type(data$count, "integer")
  expect_type(data$worn, "logical")
  expect_equal(levels(data$quality), c("Good", "Bad"))
})

test_that("schema 3.0.1 retains the same typed import compatibility", {
  package <- glc_open(make_v3_contract_fixture("3.0.1"), quiet = TRUE)
  data <- glc_read(package, dataset_id = "DS1")$data[[1L]]

  expect_equal(package$schema_version, "3.0.1")
  expect_type(data$count, "integer")
  expect_type(data$worn, "logical")
  expect_equal(levels(data$quality), c("Good", "Bad"))
})

test_that("schema 3 type and factor contracts drive the complete import", {
  package <- glc_open(make_v3_contract_fixture(), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "DS1")
  source <- collection$data[[1L]]

  expect_type(source$Id, "character")
  expect_type(source$file.name, "character")
  expect_type(source$Datetime, "character")
  expect_type(source$lux, "double")
  expect_type(source$count, "integer")
  expect_type(source$worn, "logical")
  expect_equal(source$worn, c(TRUE, FALSE))
  expect_s3_class(source$quality, "factor")
  expect_equal(levels(source$quality), c("Good", "Bad"))
  expect_equal(as.character(source$quality), c("Good", "Bad"))

  variables <- glc_variables(package)
  expect_equal(
    variables$type[match(
      c("Id", "Datetime", "lux", "count", "worn", "quality"),
      variables$name
    )],
    c("string", "string", "numeric", "integer", "boolean", "factor")
  )
  quality <- variables[variables$name == "quality", , drop = FALSE]
  expect_equal(quality$description, "Quality description")
  expect_equal(quality$factor_values[[1L]], c("good", "bad"))
  expect_equal(quality$factor_labels[[1L]], c("Good", "Bad"))
  expect_equal(
    quality$factor_descriptions[[1L]],
    c("Accepted observation", "Rejected observation")
  )

  data <- glc_collect(collection)
  expect_s3_class(data, "grouped_df")
  expect_s3_class(data$Id, "factor")
  expect_equal(as.character(data$Id), rep("DS1", 2L))
  expect_equal(data$participant_Id, rep("P1", 2L))
  expect_s3_class(data$Datetime, "POSIXct")
  expect_equal(data$file.name, c("raw-a.csv", NA_character_))
  expect_type(data$count, "integer")
})

test_that("schema 3.0.2 applies file-specific encodings during import", {
  root <- make_v3_contract_fixture()
  datasets <- fixture_read_datasets(root)
  group <- datasets[[1L]]$dataset_file[[1L]]
  group$dataset_file_names <- list(
    "data/files/light.csv",
    "data/files/light-latin1.csv"
  )
  group$dataset_file_encoding <- list("UTF-8", "ISO-8859-1")
  datasets[[1L]]$dataset_file[[1L]] <- group
  fixture_write_datasets(root, datasets)

  latin1_text <- paste0(
    paste(
      "Id",
      "file.name",
      "Datetime",
      "lux",
      "count",
      "worn",
      "quality",
      sep = ","
    ),
    "\n",
    "P1,café.csv,2026-01-01 08:02:00,20,3,true,good\n"
  )
  writeBin(
    charToRaw(iconv(latin1_text, from = "UTF-8", to = "latin1")),
    file.path(root, "data", "files", "light-latin1.csv")
  )

  package <- glc_open(root, quiet = TRUE)
  expect_equal(
    glc_files(package)$encoding,
    c("UTF-8", "ISO-8859-1")
  )
  all_files <- glc_read(package, dataset_id = "DS1")$data[[1L]]
  expect_true("café.csv" %in% all_files$file.name)

  selected <- glc_read(
    package,
    dataset_id = "DS1",
    files = "light-latin1.csv"
  )$data[[1L]]
  expect_equal(selected$file.name, "café.csv")
  expect_equal(as.character(selected$quality), "Good")
})

test_that("header discovery handles device preambles without an explicit row", {
  package <- glc_open(
    make_glc_fixture("2.0.0", preamble = TRUE, explicit_header = FALSE),
    quiet = TRUE
  )
  data <- glc_read(package, dataset_id = "DS1")$data[[1]]

  expect_equal(nrow(data), 2)
  expect_equal(data$lux, c(12.5, 15))
})

test_that("variable selection uses datetime inputs without retaining them", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  data <- glc_read(
    package,
    dataset_id = "DS1",
    variables = "lux"
  )$data[[1]]

  expect_true(all(c("lux", ".glc_datetime") %in% names(data)))
  expect_false("timestamp" %in% names(data))
  expect_false("quality" %in% names(data))
  expect_equal(
    format(data$.glc_datetime, tz = "Europe/Berlin"),
    c("2026-01-01 08:00:00", "2026-01-01 08:01:00")
  )

  data_with_datetime <- glc_read(
    package,
    dataset_id = "DS1",
    variables = c("lux", "timestamp")
  )$data[[1]]
  expect_true("timestamp" %in% names(data_with_datetime))
})

test_that("declared type and extra-column problems follow the selected policy", {
  invalid <- glc_open(
    make_glc_fixture("3.0.2", invalid_boolean = TRUE),
    quiet = TRUE
  )
  expect_error(
    glc_read(invalid, dataset_id = "DS1"),
    "incompatible",
    class = "glcdp_type_parse"
  )

  extra <- glc_open(
    make_glc_fixture("3.0.2", extra_column = TRUE),
    quiet = TRUE
  )
  expect_warning(
    collection <- glc_read(extra, dataset_id = "DS1", problems = "warn"),
    "undeclared",
    class = "glcdp_extra_column"
  )
  expect_true("extra" %in% names(collection$data[[1]]))
})

test_that("schema 3 requires complete variable type metadata", {
  missing_type <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(missing_type)
  datasets[[1L]]$dataset_file[[1L]]$dataset_file_variables[[
    1L
  ]]$dataset_file_variables_type <- NULL
  fixture_write_datasets(missing_type, datasets)
  expect_error(
    glc_variables(glc_open(missing_type, quiet = TRUE)),
    "does not declare",
    class = "glcdp_variable_metadata"
  )

  missing_levels <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(missing_levels)
  datasets[[1L]]$dataset_file[[1L]]$dataset_file_variables[[
    4L
  ]]$dataset_file_variables_factor_levels <- NULL
  fixture_write_datasets(missing_levels, datasets)
  expect_error(
    glc_variables(glc_open(missing_levels, quiet = TRUE)),
    "factor levels",
    class = "glcdp_variable_metadata"
  )

  missing_encoding <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(missing_encoding)
  datasets[[1L]]$dataset_file[[1L]]$dataset_file_encoding <- NULL
  fixture_write_datasets(missing_encoding, datasets)
  expect_error(
    glc_files(glc_open(missing_encoding, quiet = TRUE)),
    "does not declare",
    class = "glcdp_file_metadata"
  )
})

test_that("schema 3 rejects factor values outside declared levels", {
  root <- make_v3_contract_fixture()
  writeLines(
    c(
      "Id,file.name,Datetime,lux,count,worn,quality",
      "P1,,2026-01-01 08:00:00,12.5,1,1,good",
      "P1,,2026-01-01 08:01:00,15,2,0,unknown"
    ),
    file.path(root, "data", "files", "light.csv")
  )

  expect_error(
    glc_read(glc_open(root, quiet = TRUE), dataset_id = "DS1"),
    "incompatible",
    class = "glcdp_type_parse"
  )
})

test_that("collection creates LightLogR-ready identity and datetime columns", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "DS1")
  data <- glc_collect(collection)

  expect_s3_class(data, "grouped_df")
  expect_s3_class(data$Id, "factor")
  expect_s3_class(data$Datetime, "POSIXct")
  expect_equal(as.character(data$Id), c("DS1", "DS1"))
  expect_equal(data$file_group_id, c("DS1:1", "DS1:1"))
  expect_equal(data$participant_Id, c("P1", "P1"))
  expect_equal(data$file.name, rep("light.csv", 2))
  expect_false(any(startsWith(names(data), ".glc_")))
  expect_false("MEDI" %in% names(data))
})

test_that("non-participant datasets use their dataset id", {
  package <- glc_open(
    make_glc_fixture("3.0.2", participant_associated = FALSE),
    quiet = TRUE
  )
  data <- glc_read(package, dataset_id = "DS1") |>
    glc_collect()
  expect_equal(unique(as.character(data$Id)), "DS1")
  expect_true(all(is.na(data$participant_Id)))
})

test_that("unstandardized collection retains internal provenance columns", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  data <- glc_read(package, dataset_id = "DS1") |>
    glc_collect(standardize = "none")

  expect_s3_class(data, "tbl_df")
  expect_false(inherits(data, "grouped_df"))
  expect_true(all(
    c(
      ".glc_dataset_id",
      ".glc_file_group",
      ".glc_participant_id",
      ".glc_source_file",
      ".glc_datetime"
    ) %in%
      names(data)
  ))
})

test_that("incompatible groups and standard-column conflicts are rejected", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "DS1")
  incompatible <- dplyr::bind_rows(collection, collection)
  class(incompatible) <- class(collection)
  incompatible$timezone[[2]] <- "UTC"
  expect_error(
    glc_collect(incompatible),
    "time zones",
    class = "glcdp_incompatible_collection"
  )

  collection$factor_contract <- NULL
  collection$data[[1]]$Id <- "wrong"
  expect_error(
    glc_collect(collection),
    "conflicts",
    class = "glcdp_standard_column_conflict"
  )
})

test_that("collection preserves the declared factor-level contract", {
  package <- glc_open(make_v3_contract_fixture(), quiet = TRUE)
  first <- glc_read(package, dataset_id = "DS1")
  incompatible <- dplyr::bind_rows(first, first)
  class(incompatible) <- class(first)
  quality <- incompatible$data[[2L]]$quality
  incompatible$data[[2L]]$quality <- factor(
    as.character(quality),
    levels = rev(levels(quality))
  )

  expect_error(
    glc_collect(incompatible),
    "declared factor levels",
    class = "glcdp_factor_contract_tampered"
  )
})

test_that("collection harmonizes compatible declared factor levels", {
  package <- glc_open(make_read_factor_union_fixture(), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "all", progress = FALSE)

  result <- glc_collect(collection, standardize = "none")

  expect_equal(nrow(collection), 2L)
  expect_identical(levels(result$quality), c("Good", "Bad", "Maybe"))
  expect_setequal(unique(result$.glc_file_group), c("DS1:1", "DS2:1"))
})

test_that("collection blocks conflicting factor mappings", {
  package <- glc_open(
    make_read_factor_union_fixture(conflict = TRUE),
    quiet = TRUE
  )
  collection <- glc_read(package, dataset_id = "all", progress = FALSE)

  expect_error(
    glc_collect(collection),
    "conflicting labels",
    class = "glcdp_factor_harmonization_conflict"
  )
})

test_that("legacy collections retain strict factor-level behavior", {
  package <- glc_open(make_v3_contract_fixture(), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "DS1")
  collection$factor_contract <- NULL
  incompatible <- dplyr::bind_rows(collection, collection)
  class(incompatible) <- class(collection)
  incompatible$data[[2L]]$quality <- factor(
    as.character(incompatible$data[[2L]]$quality),
    levels = rev(levels(incompatible$data[[2L]]$quality))
  )

  expect_error(
    glc_collect(incompatible),
    "factor levels",
    class = "glcdp_incompatible_collection"
  )
})

test_that("collection detects tampered factor-contract facts", {
  package <- glc_open(make_v3_contract_fixture(), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "DS1")
  collection$factor_contract[[1L]]$variables[[7L]]$factor_labels[[1L]] <-
    "Changed"

  expect_error(
    glc_collect(collection),
    "changed since reading",
    class = "glcdp_factor_contract_tampered"
  )
})

test_that("collection rejects contradictory links and scopes devices by group", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  first <- glc_read(package, dataset_id = "DS1")

  contradictory <- dplyr::bind_rows(first, first)
  class(contradictory) <- class(first)
  contradictory$participant_id[[2]] <- "P2"
  expect_error(
    glc_collect(contradictory),
    "contradictory",
    class = "glcdp_incompatible_collection"
  )

  second_device <- first
  second_device$file_group[[1]] <- 2L
  second_device$file_group_id[[1]] <- "DS1:2"
  second_device$device_id[[1]] <- "D2"
  second_device$data[[1]]$.glc_file_group <- "DS1:2"
  multi_device <- dplyr::bind_rows(first, second_device)
  class(multi_device) <- class(first)
  collected_devices <- expect_no_error(glc_collect(multi_device))
  expect_setequal(
    unique(collected_devices$file_group_id),
    c("DS1:1", "DS1:2")
  )

  second_dataset <- second_device
  second_dataset$dataset_id[[1]] <- "DS2"
  second_dataset$file_group_id[[1]] <- "DS2:1"
  second_dataset$participant_id[[1]] <- "P2"
  second_dataset$data[[1]]$.glc_dataset_id <- "DS2"
  second_dataset$data[[1]]$.glc_file_group <- "DS2:1"
  second_dataset$data[[1]]$.glc_participant_id <- "P2"
  separate_datasets <- dplyr::bind_rows(first, second_dataset)
  class(separate_datasets) <- class(first)

  expect_no_error(glc_collect(separate_datasets))
})

test_that("collection and separate-column datetime specifications are parsed", {
  collection_package <- glc_open(
    make_collection_datetime_fixture(),
    quiet = TRUE
  )
  collection_data <- glc_read(collection_package, dataset_id = "DS1")$data[[1]]
  expect_equal(length(unique(collection_data$.glc_datetime)), 1)
  expect_equal(
    format(collection_data$.glc_datetime[[1]], tz = "Europe/Berlin"),
    "2026-01-01 09:30:00"
  )

  separate_package <- glc_open(make_separate_datetime_fixture(), quiet = TRUE)
  separate_data <- glc_read(separate_package, dataset_id = "DS1")$data[[1]]
  expect_equal(
    format(separate_data$.glc_datetime, tz = "Europe/Berlin"),
    c("2026-01-01 08:00:00", "2026-01-01 08:01:00")
  )
})

test_that("collection timestamps may differ without changing compatibility", {
  package <- glc_open(make_collection_datetime_fixture(), quiet = TRUE)
  first <- glc_read(package, dataset_id = "DS1")
  second <- first
  second$dataset_id <- "DS2"
  second$file_group_id <- "DS2:1"
  second$participant_id <- "P2"
  second$datetime_date <- "2026-01-02 10:45:00"
  second$data[[1L]]$.glc_dataset_id <- "DS2"
  second$data[[1L]]$.glc_file_group <- "DS2:1"
  second$data[[1L]]$.glc_participant_id <- "P2"
  second$data[[1L]]$.glc_datetime <- second$data[[1L]]$.glc_datetime +
    lubridate::days(1)
  collection <- dplyr::bind_rows(first, second)
  class(collection) <- class(first)

  expect_no_error(data <- glc_collect(collection))
  expect_equal(unique(as.character(data$Id)), c("DS1", "DS2"))
})

test_that("row limits and unknown variable selections are checked", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  data <- glc_read(package, dataset_id = "DS1", n_max = 1)$data[[1]]
  expect_equal(nrow(data), 1)
  expect_error(
    glc_read(package, dataset_id = "DS1", variables = "unknown"),
    "Unknown selected variable"
  )
  expect_error(
    glc_read(package, dataset_id = "DS1", n_max = 1.5),
    "n_max"
  )
  expect_error(
    glc_read(package, dataset_id = "DS1", progress = NA),
    "progress"
  )
  expect_no_error(
    glc_read(package, dataset_id = "DS1", progress = TRUE)
  )
})

test_that("datetime specifications participate in collection compatibility", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "DS1")
  incompatible <- dplyr::bind_rows(collection, collection)
  class(incompatible) <- class(collection)
  incompatible$datetime_format[[2]] <- "DD/MM/YYYY HH:mm:ss"

  expect_error(
    glc_collect(incompatible),
    "datetime specifications",
    class = "glcdp_incompatible_collection"
  )
})
