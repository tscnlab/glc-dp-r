test_that("schema 3 files are imported from metadata-defined headers and types", {
  package <- glc_open(
    make_glc_fixture("3.0.0", preamble = TRUE, explicit_header = TRUE),
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
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
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
    make_glc_fixture("3.0.0", invalid_boolean = TRUE),
    quiet = TRUE
  )
  expect_error(
    glc_read(invalid, dataset_id = "DS1"),
    "incompatible",
    class = "glcdp_type_parse"
  )

  extra <- glc_open(
    make_glc_fixture("3.0.0", extra_column = TRUE),
    quiet = TRUE
  )
  expect_warning(
    collection <- glc_read(extra, dataset_id = "DS1", problems = "warn"),
    "undeclared",
    class = "glcdp_extra_column"
  )
  expect_true("extra" %in% names(collection$data[[1]]))
})

test_that("collection creates LightLogR-ready identity and datetime columns", {
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
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
    make_glc_fixture("3.0.0", participant_associated = FALSE),
    quiet = TRUE
  )
  data <- glc_read(package, dataset_id = "DS1") |>
    glc_collect()
  expect_equal(unique(as.character(data$Id)), "DS1")
  expect_true(all(is.na(data$participant_Id)))
})

test_that("unstandardized collection retains internal provenance columns", {
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
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
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
  collection <- glc_read(package, dataset_id = "DS1")
  incompatible <- dplyr::bind_rows(collection, collection)
  class(incompatible) <- class(collection)
  incompatible$timezone[[2]] <- "UTC"
  expect_error(
    glc_collect(incompatible),
    "time zones",
    class = "glcdp_incompatible_collection"
  )

  collection$data[[1]]$Id <- "wrong"
  expect_error(
    glc_collect(collection),
    "conflicts",
    class = "glcdp_standard_column_conflict"
  )
})

test_that("collection rejects contradictory links and multiple devices per dataset", {
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
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
  expect_error(
    glc_collect(multi_device),
    "multiple devices",
    class = "glcdp_incompatible_collection"
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

test_that("row limits and unknown variable selections are checked", {
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
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
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
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
