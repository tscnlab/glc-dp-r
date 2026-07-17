test_that("Guidolin schema 2 package can be opened and sampled", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("GLCDP_RUN_LIVE_TESTS"), "true"),
    "Set GLCDP_RUN_LIVE_TESTS=true to run live GitHub integration tests"
  )
  package <- glc_open("tscnlab/guidolin-glee-datasetv2", quiet = TRUE)
  expect_equal(package$schema_version, "2.0.0")
  metadata <- glc_metadata(
    package,
    resources = c("study", "participants")
  )
  expect_named(metadata, c("study", "participants"))
  expect_s3_class(
    glc_search_metadata(package, "light", resources = "participants"),
    "tbl_df"
  )
  expect_s3_class(
    glc_search_metadata(package, "light", resources = "datasets"),
    "tbl_df"
  )
  city_fields <- glc_search_metadata(
    package,
    "city",
    resources = "study",
    search_in = "fields"
  )
  expect_true(any(endsWith(
    city_fields$field,
    ".contributor_institution_city"
  )))
  city_matches <- glc_search_metadata(
    package,
    "Munich",
    resources = "study",
    fields = "contributor_institution_city"
  )
  expect_gt(nrow(city_matches), 0L)
  expect_true(all(endsWith(
    city_matches$field,
    ".contributor_institution_city"
  )))
  sample <- glc_read(
    package,
    dataset_id = "DS001",
    variables = "LIGHT",
    n_max = 3,
    progress = FALSE
  )
  expect_equal(nrow(sample$data[[1]]), 3)
  expect_true("LIGHT" %in% names(sample$data[[1]]))
  expect_false("DATE/TIME" %in% names(sample$data[[1]]))
  expect_s3_class(sample$data[[1]]$.glc_datetime, "POSIXct")

  package$transport$token <- NULL
  destination <- tempfile("guidolin-data-subset-")
  downloads <- glc_download(
    package,
    destination,
    include = "data",
    dataset_id = "DS001"
  )
  expect_true(nrow(downloads) > 0L)
  expect_true(file.exists(file.path(destination, "datapackage.json")))

  local <- glc_open(destination, quiet = TRUE)
  local_summary <- glc_summary(local)
  expect_message(
    local_collection <- glc_read(local, dataset_id = "all", n_max = 3),
    "declared datasets",
    class = "glcdp_local_subset"
  )
  expect_true(local_summary$available_file_count < local_summary$file_count)
  expect_equal(unique(local_collection$dataset_id), "DS001")
})
