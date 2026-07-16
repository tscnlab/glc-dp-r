test_that("Guidolin schema 2 package can be opened and sampled", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("GLCDP_RUN_LIVE_TESTS"), "true"),
    "Set GLCDP_RUN_LIVE_TESTS=true to run live GitHub integration tests"
  )
  package <- glc_open("tscnlab/guidolin-glee-datasetv2", quiet = TRUE)
  expect_equal(package$schema_version, "2.0.0")
  sample <- glc_read(
    package,
    dataset_id = "DS001",
    variables = "LIGHT",
    n_max = 3
  )
  expect_equal(nrow(sample$data[[1]]), 3)
  expect_s3_class(sample$data[[1]]$.glc_datetime, "POSIXct")
})
