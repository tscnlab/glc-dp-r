test_that("stable and legacy fixtures normalize to the public inventories", {
  for (version in c("2.0.0", "3.0.0", "3.0.1", "3.0.2")) {
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

test_that("schema support reports 3.0.2 as the current primary contract", {
  versions <- glc_schema_versions()

  expect_equal(
    versions$status[
      match(
        c("1.0.0", "2.0.0", "3.0.0", "3.0.1", "3.0.2"),
        versions$version
      )
    ],
    c("legacy", "legacy", "stable", "stable", "stable")
  )
  expect_match(
    versions$notes[versions$version == "2.0.0"],
    "Barebones"
  )
  expect_match(
    versions$notes[versions$version == "3.0.0"],
    "predecessor"
  )
  expect_match(
    versions$notes[versions$version == "3.0.1"],
    "predecessor"
  )
  expect_match(
    versions$notes[versions$version == "3.0.2"],
    "Current default"
  )
})

test_that("root and profile schema declarations must agree", {
  root <- make_glc_fixture("3.0.2")
  descriptor_path <- file.path(root, "datapackage.json")
  descriptor <- jsonlite::fromJSON(
    descriptor_path,
    simplifyVector = FALSE
  )
  descriptor$profile <- "schemas/3.0.1/glc-dp-profile.json"
  write_fixture_json(descriptor, descriptor_path)

  expect_error(
    glc_open(root, quiet = TRUE),
    "Conflicting GLC schema declarations",
    class = "glcdp_schema_error"
  )
})

test_that("dataset-only schema detection rejects multiple versions", {
  root <- make_multi_dataset_fixture()
  descriptor_path <- file.path(root, "datapackage.json")
  descriptor <- jsonlite::fromJSON(
    descriptor_path,
    simplifyVector = FALSE
  )
  descriptor$schema_version <- NULL
  descriptor$profile <- NULL
  write_fixture_json(descriptor, descriptor_path)
  datasets <- fixture_read_datasets(root)
  datasets[[1L]]$schema_version <- "3.0.1"
  datasets[[2L]]$schema_version <- "3.0.2"
  fixture_write_datasets(root, datasets)

  expect_error(
    glc_open(root, quiet = TRUE),
    "multiple GLC schema versions",
    class = "glcdp_schema_error"
  )
})

test_that("dataset schema declarations must match the package version", {
  root <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(root)
  datasets[[1L]]$schema_version <- "3.0.1"
  fixture_write_datasets(root, datasets)
  package <- glc_open(root, quiet = TRUE)

  expect_equal(package$schema_version, "3.0.2")
  expect_error(
    glc_datasets(package),
    "conflicts with package schema version",
    class = "glcdp_schema_error"
  )
})

test_that("model loading rejects multiple explicit dataset versions", {
  root <- make_multi_dataset_fixture()
  datasets <- fixture_read_datasets(root)
  datasets[[1L]]$schema_version <- "3.0.1"
  datasets[[2L]]$schema_version <- "3.0.2"
  fixture_write_datasets(root, datasets)
  package <- glc_open(root, quiet = TRUE)

  expect_error(
    glc_datasets(package),
    "multiple GLC schema versions",
    class = "glcdp_schema_error"
  )
})

test_that("missing dataset versions inherit stable and legacy package versions", {
  stable_root <- make_multi_dataset_fixture()
  stable_datasets <- fixture_read_datasets(stable_root)
  stable_datasets[[1L]]$schema_version <- NULL
  fixture_write_datasets(stable_root, stable_datasets)
  stable <- glc_open(stable_root, quiet = TRUE)

  expect_equal(
    glc_datasets(stable)$schema_version,
    rep("3.0.2", 2L)
  )

  legacy_root <- make_glc_fixture("2.0.0")
  legacy_datasets <- fixture_read_datasets(legacy_root)
  legacy_datasets[[1L]]$schema_version <- NULL
  fixture_write_datasets(legacy_root, legacy_datasets)
  legacy <- glc_open(legacy_root, quiet = TRUE)

  expect_equal(glc_datasets(legacy)$schema_version, "2.0.0")
  expect_true(all(glc_variables(legacy)$type == "guess"))
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

test_that("metadata search can match values, fields, or both", {
  package <- glc_open(make_glc_fixture("2.0.0"), quiet = TRUE)

  default_matches <- glc_search_metadata(
    package,
    "study_internal",
    resources = "study"
  )
  field_matches <- glc_search_metadata(
    package,
    "STUDY_INTERNAL",
    resources = "study",
    search_in = "fields"
  )
  both_matches <- glc_search_metadata(
    package,
    "study",
    resources = "study",
    search_in = "both"
  )

  expect_equal(nrow(default_matches), 0L)
  expect_equal(field_matches$field, "study_internal_id")
  expect_equal(field_matches$value, "S1")
  expect_setequal(both_matches$field, c("study_internal_id", "title"))
})

test_that("metadata search traverses nested data-frame columns", {
  value <- jsonlite::fromJSON(
    paste0(
      '[{"id":"DS1","crossref":{"study":"Light study"}},',
      '{"id":"DS2","crossref":{"study":"Other study"}}]'
    ),
    simplifyVector = TRUE
  )
  leaves <- glcdp:::glc_metadata_leaf_table(list(datasets = value))

  matches <- leaves[leaves$value == "Light study", , drop = FALSE]
  expect_equal(nrow(matches), 1)
  expect_equal(matches$record, 1L)
  expect_equal(matches$field, "crossref.study")
  expect_equal(
    leaves$record[leaves$value == "Other study"],
    2L
  )
})

test_that("summary reports core contents", {
  package <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  summary <- glc_summary(package)

  expect_s3_class(summary, "glc_summary")
  expect_equal(summary$dataset_count, 1)
  expect_equal(summary$file_group_count, 1)
  expect_equal(summary$file_count, 1)
  expect_equal(summary$variable_count, 4)
  expect_true("light" %in% summary$modalities[[1]])
})

test_that("summary and metadata load schema 3 tabular core resources", {
  package <- glc_open(make_v3_contract_fixture(), quiet = TRUE)
  metadata <- glc_metadata(package, resources = "participants")
  summary <- glc_summary(package)

  expect_s3_class(metadata$participants, "tbl_df")
  expect_equal(metadata$participants$participant_internal_id, "P1")
  expect_equal(summary$participant_count, 1L)
  expect_equal(summary$variable_count, 7L)
})

test_that("remote summaries and full inventories reuse immutable results", {
  local <- glc_open(make_glc_fixture("3.0.2"), quiet = TRUE)
  model <- glcdp:::glc_model(local)
  remote <- glcdp:::new_glc_package(
    source_type = "remote",
    repo = "example/data",
    commit = paste(rep("a", 40), collapse = ""),
    ref_kind = "commit",
    verified = FALSE,
    descriptor = list(resources = list()),
    schema_version = "3.0.2"
  )
  remote$transport$model <- model
  remote$transport$tree <- tibble::tibble(
    path = "data/files/light.csv",
    type = "blob",
    size = 2048,
    sha = "data-blob"
  )

  summary <- glc_summary(remote)

  expect_equal(summary$variable_count, 4L)
  expect_null(remote$transport$variable_inventory)
  expect_identical(glc_summary(remote), summary)
  expect_identical(remote$transport$summary, summary)
  expect_identical(glc_files(remote), remote$transport$file_inventory)

  variables <- glc_variables(remote)
  expect_equal(variables, glc_variables(local))
  expect_identical(glc_variables(remote), variables)
  expect_identical(remote$transport$variable_inventory, variables)
})

test_that("local summaries continue to reflect filesystem changes", {
  root <- make_glc_fixture("3.0.2")
  package <- glc_open(root, quiet = TRUE)
  before <- glc_summary(package)
  writeLines(
    c(
      "timestamp,lux,worn,quality",
      "2026-01-01 08:00:00,12.5,true,good",
      "2026-01-01 08:01:00,15,false,bad",
      "2026-01-01 08:02:00,20,true,good"
    ),
    file.path(root, "data", "files", "light.csv")
  )
  after <- glc_summary(package)

  expect_gt(after$declared_bytes, before$declared_bytes)
  expect_null(package$transport$summary)
  expect_null(package$transport$file_inventory)
})

test_that("schema 3.0.2 profile supports the complete public data workflow", {
  root <- make_v3_contract_fixture()
  descriptor_path <- file.path(root, "datapackage.json")
  descriptor <- jsonlite::fromJSON(
    descriptor_path,
    simplifyVector = FALSE
  )
  descriptor$schema_version <- NULL
  write_fixture_json(descriptor, descriptor_path)
  datasets <- fixture_read_datasets(root)
  datasets[[1L]]$schema_version <- NULL
  fixture_write_datasets(root, datasets)

  package <- glc_open(root, quiet = TRUE)
  expect_equal(package$schema_version, "3.0.2")
  expect_equal(glc_summary(package)$schema_version, "3.0.2")
  expect_true(all(
    c(
      "study",
      "participants",
      "datasets",
      "devices",
      "device_datasheets"
    ) %in%
      glc_resources(package)$resource
  ))
  expect_equal(glc_datasets(package)$dataset_id, "DS1")
  files <- glc_files(package)
  expect_equal(files$file_group_id, "DS1:1")
  expect_equal(files$description, "Typed observations")
  expect_equal(files$device_location_type, "body_worn")
  expect_equal(files$temporal_type, "fixed_interval")
  expect_equal(nrow(glc_variables(package)), 7L)
  expect_gt(
    nrow(glc_search_metadata(
      package,
      "participant_age",
      resources = "participants",
      search_in = "fields"
    )),
    0L
  )

  imported <- glc_read(package, dataset_id = "DS1") |>
    glc_collect()
  extracted <- extract_metadata(
    imported,
    package,
    fields = "participant_age"
  )
  enriched <- add_metadata(
    imported,
    package,
    fields = "participant_age"
  )
  expect_equal(extracted$participant_age, 34)
  expect_equal(enriched$participant_age, rep(34, nrow(imported)))
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
