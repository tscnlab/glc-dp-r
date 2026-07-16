test_that("stable and experimental fixtures normalize to the public inventories", {
  for (version in c("2.0.0", "3.0.0")) {
    package <- glc_open(make_glc_fixture(version), quiet = TRUE)
    datasets <- glc_datasets(package)
    files <- glc_files(package)
    variables <- glc_variables(package)

    expect_s3_class(package, "glc_package")
    expect_equal(package$schema_version, version)
    expect_equal(datasets$dataset_id, "DS1")
    expect_equal(files$path, "data/files/light.csv")
    expect_true(all(c("timestamp", "lux") %in% variables$name))
    expect_true(variables$primary[variables$name == "lux"])
  }
})

test_that("recognizable schema 1 packages are inferred with a warning", {
  expect_warning(
    package <- glc_open(make_glc_fixture("1.0.0"), quiet = TRUE),
    "schema 1.0.0",
    class = "glcdp_inferred_schema"
  )
  expect_equal(package$schema_version, "1.0.0")
})

test_that("metadata resources retain a named-list contract", {
  package <- glc_open(make_glc_fixture("2.0.0"), quiet = TRUE)
  metadata <- glc_metadata(package, resources = c("study", "participants"))

  expect_type(metadata, "list")
  expect_named(metadata, c("study", "participants"))
  expect_equal(metadata$study$study_internal_id, "S1")
  expect_s3_class(metadata$participants, "tbl_df")
})

test_that("metadata search reports resource and nested field path", {
  package <- glc_open(make_glc_fixture("2.0.0"), quiet = TRUE)
  matches <- glc_search_metadata(package, "light study", resources = "study")

  expect_equal(nrow(matches), 1)
  expect_equal(matches$resource, "study")
  expect_equal(matches$field, "title")
})

test_that("summary reports core contents", {
  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
  summary <- glc_summary(package)

  expect_s3_class(summary, "glc_summary")
  expect_equal(summary$dataset_count, 1)
  expect_equal(summary$file_group_count, 1)
  expect_equal(summary$file_count, 1)
  expect_equal(summary$variable_count, 4)
  expect_true("light" %in% summary$modalities[[1]])
})

test_that("directory resources and empty metadata searches are type stable", {
  package <- glc_open(make_glc_fixture("2.0.0"), quiet = TRUE)
  datasheets <- glc_metadata(package, resources = "device_datasheets")
  matches <- glc_search_metadata(
    package,
    "not present anywhere",
    resources = "study"
  )

  expect_type(datasheets$device_datasheets, "list")
  expect_named(datasheets$device_datasheets, "data/datasheets/D1.json")
  expect_s3_class(matches, "tbl_df")
  expect_equal(nrow(matches), 0)
  expect_named(matches, c("resource", "record", "field", "value", "context"))
})

test_that("unsupported schema versions are rejected", {
  root <- make_glc_fixture("2.0.0")
  descriptor <- jsonlite::fromJSON(
    file.path(root, "datapackage.json"),
    simplifyVector = FALSE
  )
  descriptor$schema_version <- "9.0.0"
  write_fixture_json(descriptor, file.path(root, "datapackage.json"))

  expect_error(
    glc_open(root, quiet = TRUE),
    "Unsupported",
    class = "glcdp_unsupported_schema"
  )
})

test_that("LFS-backed files are identified in local inventories", {
  root <- make_glc_fixture("2.0.0")
  data <- charToRaw("logical LFS contents")
  source <- tempfile()
  writeBin(data, source)
  oid <- digest::digest(source, algo = "sha256", serialize = FALSE, file = TRUE)
  writeLines(
    c(
      "version https://git-lfs.github.com/spec/v1",
      paste0("oid sha256:", oid),
      paste0("size ", length(data))
    ),
    file.path(root, "data", "files", "light.csv")
  )
  package <- glc_open(root, quiet = TRUE)
  files <- glc_files(package)

  expect_equal(files$storage, "lfs")
  expect_equal(files$lfs_oid, oid)
  expect_equal(files$expected_bytes, length(data))
})
