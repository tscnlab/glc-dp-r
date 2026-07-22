test_that("metadata are extracted once per file group and added to observations", {
  dataset <- tibble::tibble(
    file_group_id = factor(
      c("DS2:1", "DS1:1", "DS2:1"),
      levels = c("DS2:1", "DS1:1")
    ),
    Id = factor(c("DS2", "DS1", "DS2"), levels = c("DS2", "DS1")),
    value = c(3, 1, 4)
  ) |>
    dplyr::group_by(Id)
  metadata <- tibble::tibble(
    file_group_id = c("DS1:1", "DS2:1", "DS9:1"),
    condition = c("control", "intervention", "unused"),
    score = c(10, 20, 99)
  )

  extracted <- extract_metadata(
    dataset,
    metadata,
    fields = c("condition", "score")
  )
  added <- add_metadata(
    dataset,
    metadata,
    fields = c("condition", "score")
  )

  expect_s3_class(extracted, "tbl_df")
  expect_s3_class(extracted, "grouped_df")
  expect_equal(dplyr::group_vars(extracted), "Id")
  expect_named(
    extracted,
    c("Id", "file_group_id", "condition", "score")
  )
  expect_equal(as.character(extracted$Id), c("DS2", "DS1"))
  expect_s3_class(extracted$file_group_id, "factor")
  expect_equal(
    as.character(extracted$file_group_id),
    c("DS2:1", "DS1:1")
  )
  expect_equal(extracted$condition, c("intervention", "control"))
  expect_equal(extracted$score, c(20, 10))

  expect_s3_class(added, "grouped_df")
  expect_equal(dplyr::group_vars(added), "Id")
  expect_equal(nrow(added), nrow(dataset))
  expect_equal(added$value, dataset$value)
  expect_equal(
    added$condition,
    c("intervention", "control", "intervention")
  )
})

test_that("extraction retains all grouping columns without duplicating the key", {
  dataset <- tibble::tibble(
    Id = factor(c("DS2", "DS1", "DS2"), levels = c("DS2", "DS1")),
    participant_Id = c("P2", "P1", "P2"),
    file_group_id = c("DS2:1", "DS1:1", "DS2:1"),
    value = c(3, 1, 4)
  ) |>
    dplyr::group_by(Id, participant_Id, .drop = FALSE)
  metadata <- tibble::tibble(
    file_group_id = c("DS1:1", "DS2:1"),
    condition = c("control", "intervention")
  )

  extracted <- extract_metadata(dataset, metadata, "condition")

  expect_named(
    extracted,
    c("Id", "participant_Id", "file_group_id", "condition")
  )
  expect_equal(
    dplyr::group_vars(extracted),
    c("Id", "participant_Id")
  )
  expect_false(attr(dplyr::group_data(extracted), ".drop"))
  expect_equal(as.character(extracted$Id), c("DS2", "DS1"))
  expect_equal(extracted$participant_Id, c("P2", "P1"))

  grouped_by_key <- dataset |>
    dplyr::group_by(Id, file_group_id)
  key_extracted <- extract_metadata(
    grouped_by_key,
    metadata,
    "condition"
  )
  expect_named(key_extracted, c("Id", "file_group_id", "condition"))
  expect_equal(
    dplyr::group_vars(key_extracted),
    c("Id", "file_group_id")
  )
})

test_that("extraction rejects ambiguous grouping relationships", {
  dataset <- tibble::tibble(
    Id = c("DS1", "DS2"),
    file_group_id = c("DS1:1", "DS1:1")
  ) |>
    dplyr::group_by(Id)
  metadata <- tibble::tibble(
    file_group_id = "DS1:1",
    condition = "control"
  )

  expect_error(
    extract_metadata(dataset, metadata, "condition"),
    "does not map uniquely",
    class = "glcdp_metadata_grouping_error"
  )

  conflicting_name <- tibble::tibble(
    Id = "DS1",
    file_group_id = "DS1:1",
    condition = "existing"
  ) |>
    dplyr::group_by(Id, condition)
  expect_error(
    extract_metadata(conflicting_name, metadata, "condition"),
    "conflict",
    class = "glcdp_metadata_column_conflict"
  )
})

test_that("partial identifier and field matches warn and retain useful data", {
  dataset <- tibble::tibble(
    file_group_id = factor(c("DS1:1", "DS2:1", "DS3:1"))
  )
  partial_ids <- tibble::tibble(
    file_group_id = c("DS1:1", "DS3:1"),
    condition = c("control", "intervention")
  )

  expect_warning(
    extracted <- extract_metadata(dataset, partial_ids, "condition"),
    "2 of 3",
    class = "glcdp_metadata_id_partial_match"
  )
  expect_equal(
    as.character(extracted$file_group_id),
    c("DS1:1", "DS2:1", "DS3:1")
  )
  expect_equal(extracted$condition, c("control", NA, "intervention"))

  complete_ids <- tibble::tibble(
    file_group_id = c("DS1:1", "DS2:1", "DS3:1"),
    condition = c("control", "control", "intervention")
  )
  expect_warning(
    partial_fields <- extract_metadata(
      dataset,
      complete_ids,
      fields = c("condition", "missing_field")
    ),
    "1 of 2",
    class = "glcdp_metadata_field_partial_match"
  )
  expect_named(partial_fields, c("file_group_id", "condition"))
})

test_that("metadata identifiers and requested fields are validated", {
  dataset <- tibble::tibble(file_group_id = c("DS1:1", "DS2:1"))
  metadata <- tibble::tibble(
    file_group_id = c("DS1:1", "DS2:1"),
    condition = c("A", "B")
  )

  expect_error(
    extract_metadata(
      dataset,
      tibble::tibble(file_group_id = "other:1", condition = "A"),
      "condition"
    ),
    "No input identifiers",
    class = "glcdp_metadata_id_no_match"
  )
  expect_error(
    extract_metadata(dataset, metadata, "unknown"),
    "None of the requested",
    class = "glcdp_metadata_field_no_match"
  )
  expect_error(
    extract_metadata(
      dataset,
      tibble::tibble(
        file_group_id = c("DS1:1", "DS1:1"),
        condition = c("A", "B")
      ),
      "condition"
    ),
    "unique",
    class = "glcdp_metadata_id_error"
  )
  expect_error(
    extract_metadata(
      tibble::tibble(file_group_id = c("DS1:1", NA)),
      metadata,
      "condition"
    ),
    "missing or empty",
    class = "glcdp_metadata_id_error"
  )
  expect_error(
    extract_metadata(
      dataset,
      tibble::tibble(
        file_group_id = c("DS1:1", NA),
        condition = c("A", "B")
      ),
      "condition"
    ),
    "missing or empty",
    class = "glcdp_metadata_id_error"
  )
})

test_that("join columns and public inputs are validated", {
  dataset <- tibble::tibble(Id = "DS1")
  metadata <- tibble::tibble(Id = "DS1", condition = "A")

  expect_error(
    extract_metadata(1, metadata, "condition"),
    "data frame",
    class = "glcdp_metadata_dataset_error"
  )
  expect_error(
    extract_metadata(dataset, metadata, character(), by = "Id"),
    "one or more",
    class = "glcdp_metadata_field_error"
  )
  expect_error(
    extract_metadata(dataset, metadata, "condition", by = c("Id", "other")),
    "one non-empty",
    class = "glcdp_metadata_by_error"
  )
  expect_error(
    extract_metadata(tibble::tibble(other = "DS1"), metadata, "condition"),
    "Dataset join column",
    class = "glcdp_metadata_by_error"
  )
  expect_error(
    extract_metadata(
      tibble::tibble(file_group_id = "DS1:1"),
      tibble::tibble(other = "DS1", condition = "A"),
      "condition"
    ),
    "Metadata join column",
    class = "glcdp_metadata_by_error"
  )
  expect_error(add_metadata(dataset, metadata, "condition", overwrite = NA))
})

test_that("named join mappings work with explicit and inferred package resources", {
  root <- make_glc_fixture("3.0.0")
  write_fixture_json(
    list(list(
      participant_internal_id = "P1",
      cohort = "morning",
      age = 31
    )),
    file.path(root, "data", "participants.json")
  )
  package <- glc_open(root, quiet = TRUE)
  dataset <- tibble::tibble(
    participant_Id = factor(c("P1", "P1")),
    value = c(1, 2)
  )
  mapping <- c(participant_Id = "participant_internal_id")

  explicit <- extract_metadata(
    dataset,
    package,
    fields = c("cohort", "age"),
    by = mapping,
    resource = "participants"
  )
  inferred <- extract_metadata(
    dataset,
    package,
    fields = c("cohort", "age"),
    by = mapping
  )

  expect_equal(explicit, inferred)
  expect_equal(explicit$cohort, "morning")
  expect_equal(explicit$age, 31)
  expect_equal(as.character(explicit$participant_Id), "P1")
})

test_that("file groups traverse linked package metadata", {
  root <- make_glc_fixture("3.0.0")
  datasets <- fixture_read_datasets(root)
  datasets[[1]]$dataset_file[[2]] <- datasets[[1]]$dataset_file[[1]]
  fixture_write_datasets(root, datasets)
  write_fixture_json(
    list(list(
      participant_internal_id = "P1",
      participant_age = 31
    )),
    file.path(root, "data", "participants.json")
  )
  write_fixture_json(
    list(
      study_internal_id = "S1",
      study_title = "Morning light study"
    ),
    file.path(root, "data", "study.json")
  )
  write_fixture_json(
    list(list(
      device_internal_id = "D1",
      device_model = "ActLight"
    )),
    file.path(root, "data", "devices.json")
  )
  package <- glc_open(root, quiet = TRUE)
  dataset <- glc_collect(glc_read(package, dataset_id = "DS1"))
  fields <- c(
    "dataset_timezone",
    "participant_age",
    "study_title",
    "device_model"
  )

  extracted <- extract_metadata(dataset, package, fields)
  added <- add_metadata(dataset, package, fields)
  participant <- extract_metadata(
    dataset,
    package,
    "participant_age",
    resource = "participants"
  )
  dataset_level <- extract_metadata(
    dataset,
    package,
    fields,
    by = "Id"
  )

  expect_named(extracted, c("Id", "file_group_id", fields))
  expect_equal(dplyr::group_vars(extracted), "Id")
  expect_equal(extracted$file_group_id, c("DS1:1", "DS1:2"))
  expect_equal(
    extracted$dataset_timezone,
    rep("Europe/Berlin", 2)
  )
  expect_equal(extracted$participant_age, rep(31, 2))
  expect_equal(
    extracted$study_title,
    rep("Morning light study", 2)
  )
  expect_equal(extracted$device_model, rep("ActLight", 2))
  expect_equal(participant$participant_age, rep(31, 2))
  expect_named(dataset_level, c("Id", fields))
  expect_equal(as.character(dataset_level$Id), "DS1")
  expect_equal(nrow(dataset_level), 1L)
  expect_equal(added$lux, dataset$lux)
  expect_equal(added$file_group_id, dataset$file_group_id)
  expect_equal(added$participant_age, rep(31, nrow(dataset)))
  expect_equal(dplyr::group_vars(added), "Id")
})

test_that("linked package metadata retains partial and rejects absent matches", {
  root <- make_multi_dataset_fixture()
  datasets <- fixture_read_datasets(root)
  datasets[[2]]$dataset_crossref$dataset_crossref_participant_id <- "P2"
  fixture_write_datasets(root, datasets)
  write_fixture_json(
    list(list(
      participant_internal_id = "P1",
      participant_age = 31
    )),
    file.path(root, "data", "participants.json")
  )
  package <- glc_open(root, quiet = TRUE)
  dataset <- tibble::tibble(file_group_id = c("DS1:1", "DS2:1"))

  expect_warning(
    extracted <- extract_metadata(dataset, package, "participant_age"),
    "1 of 2",
    class = "glcdp_metadata_id_partial_match"
  )
  expect_equal(extracted$participant_age, c(31, NA))
  expect_error(
    extract_metadata(
      tibble::tibble(file_group_id = "DS9:1"),
      package,
      "participant_age"
    ),
    "No input identifiers",
    class = "glcdp_metadata_id_no_match"
  )
  expect_warning(
    fields <- extract_metadata(
      tibble::tibble(file_group_id = "DS1:1"),
      package,
      c("participant_age", "unknown")
    ),
    "1 of 2",
    class = "glcdp_metadata_field_partial_match"
  )
  expect_named(fields, c("file_group_id", "participant_age"))
})

test_that("missing participant and device links are partial matches", {
  root <- make_glc_fixture(
    "3.0.0",
    participant_associated = FALSE
  )
  datasets <- fixture_read_datasets(root)
  datasets[[1]]$dataset_file[[1]]$dataset_file_crossref_device_id <- NULL
  fixture_write_datasets(root, datasets)
  write_fixture_json(
    list(list(participant_internal_id = "P9", participant_age = 44)),
    file.path(root, "data", "participants.json")
  )
  write_fixture_json(
    list(list(device_internal_id = "D9", device_model = "Unused")),
    file.path(root, "data", "devices.json")
  )
  package <- glc_open(root, quiet = TRUE)
  dataset <- glc_collect(glc_read(package, dataset_id = "DS1"))

  expect_warning(
    extracted <- extract_metadata(
      dataset,
      package,
      c("participant_age", "device_model")
    ),
    "0 of 1",
    class = "glcdp_metadata_id_partial_match"
  )
  expect_named(
    extracted,
    c("Id", "file_group_id", "participant_age", "device_model")
  )
  expect_true(is.na(extracted$participant_age))
  expect_true(is.na(extracted$device_model))
})

test_that("linked metadata identifiers must be unique", {
  root <- make_glc_fixture("3.0.0")
  write_fixture_json(
    list(
      list(participant_internal_id = "P1", participant_age = 31),
      list(participant_internal_id = "P1", participant_age = 32)
    ),
    file.path(root, "data", "participants.json")
  )
  package <- glc_open(root, quiet = TRUE)

  expect_error(
    extract_metadata(
      tibble::tibble(file_group_id = "DS1:1"),
      package,
      "participant_age"
    ),
    "unique",
    class = "glcdp_metadata_id_error"
  )
})

test_that("connected fields must identify one resource", {
  root <- make_glc_fixture("3.0.0")
  write_fixture_json(
    list(list(participant_internal_id = "P1", shared_label = "person")),
    file.path(root, "data", "participants.json")
  )
  write_fixture_json(
    list(study_internal_id = "S1", shared_label = "study"),
    file.path(root, "data", "study.json")
  )
  package <- glc_open(root, quiet = TRUE)
  dataset <- tibble::tibble(file_group_id = "DS1:1")

  expect_error(
    extract_metadata(dataset, package, "shared_label"),
    "ambiguous",
    class = "glcdp_metadata_resource_ambiguous"
  )
  explicit <- extract_metadata(
    dataset,
    package,
    "shared_label",
    resource = "participants"
  )
  expect_equal(explicit$shared_label, "person")
})

test_that("file groups resolve devices but dataset-level lookup rejects several", {
  root <- make_glc_fixture("3.0.0")
  datasets <- fixture_read_datasets(root)
  second_group <- datasets[[1]]$dataset_file[[1]]
  second_group$dataset_file_crossref_device_id <- "D2"
  datasets[[1]]$dataset_file[[2]] <- second_group
  fixture_write_datasets(root, datasets)
  write_fixture_json(
    list(
      list(device_internal_id = "D1", device_model = "Model 1"),
      list(device_internal_id = "D2", device_model = "Model 2")
    ),
    file.path(root, "data", "devices.json")
  )
  package <- glc_open(root, quiet = TRUE)

  file_groups <- extract_metadata(
    tibble::tibble(file_group_id = c("DS1:1", "DS1:2")),
    package,
    "device_model"
  )
  expect_equal(file_groups$device_model, c("Model 1", "Model 2"))

  expect_error(
    extract_metadata(
      tibble::tibble(Id = "DS1"),
      package,
      "device_model",
      by = "Id"
    ),
    "multiple devices",
    class = "glcdp_metadata_relationship_ambiguous"
  )
})

test_that("ambiguous package resources require an explicit resource", {
  root <- make_glc_fixture("3.0.0")
  write_fixture_json(
    list(list(participant_internal_id = "P1", cohort = "primary")),
    file.path(root, "data", "participants.json")
  )
  writeLines(
    c("participant_internal_id,cohort", "P1,secondary"),
    file.path(root, "data", "participant-notes.csv")
  )
  descriptor <- jsonlite::fromJSON(
    file.path(root, "datapackage.json"),
    simplifyVector = FALSE
  )
  descriptor$resources <- c(
    descriptor$resources,
    list(list(
      name = "participant_notes",
      path = "data/participant-notes.csv",
      format = "csv"
    ))
  )
  write_fixture_json(descriptor, file.path(root, "datapackage.json"))

  package <- glc_open(root, quiet = TRUE)
  dataset <- tibble::tibble(participant_Id = "P1")
  mapping <- c(participant_Id = "participant_internal_id")

  expect_error(
    extract_metadata(dataset, package, "cohort", by = mapping),
    "ambiguous",
    class = "glcdp_metadata_resource_ambiguous"
  )
  explicit <- extract_metadata(
    dataset,
    package,
    "cohort",
    by = mapping,
    resource = "participant_notes"
  )
  expect_equal(explicit$cohort, "secondary")
})

test_that("local CSV and TSV metadata files are supported", {
  dataset <- tibble::tibble(file_group_id = c("DS1:1", "DS2:1"))
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  writeLines(
    c("file_group_id,condition", "DS1:1,A", "DS2:1,B"),
    csv
  )
  writeLines(
    c("file_group_id\tcondition", "DS1:1\tA", "DS2:1\tB"),
    tsv
  )

  expect_equal(
    extract_metadata(dataset, csv, "condition")$condition,
    c("A", "B")
  )
  expect_equal(
    extract_metadata(dataset, tsv, "condition")$condition,
    c("A", "B")
  )

  unsupported <- tempfile(fileext = ".json")
  writeLines("{}", unsupported)
  expect_error(
    extract_metadata(dataset, unsupported, "condition"),
    "csv.*tsv",
    class = "glcdp_metadata_source_error"
  )
  expect_error(
    extract_metadata(dataset, tempfile(fileext = ".csv"), "condition"),
    "does not exist",
    class = "glcdp_metadata_source_error"
  )
  expect_error(
    extract_metadata(
      dataset,
      list(file_group_id = "DS1:1"),
      "condition"
    ),
    "data frame",
    class = "glcdp_metadata_source_error"
  )
})

test_that("adding metadata protects or replaces existing columns", {
  dataset <- tibble::tibble(
    file_group_id = c("DS1:1", "DS2:1"),
    Id = factor(c("DS1", "DS2")),
    condition = c("old", "old"),
    value = c(1, 2)
  ) |>
    dplyr::group_by(Id, condition, .drop = FALSE)
  metadata <- tibble::tibble(
    file_group_id = c("DS1:1", "DS2:1"),
    condition = c("new-a", "new-b")
  )

  expect_error(
    add_metadata(dataset, metadata, "condition"),
    "already",
    class = "glcdp_metadata_column_conflict"
  )
  replaced <- add_metadata(
    dataset,
    metadata,
    "condition",
    overwrite = TRUE
  )

  expect_equal(replaced$condition, c("new-a", "new-b"))
  expect_equal(replaced$value, dataset$value)
  expect_equal(dplyr::group_vars(replaced), c("Id", "condition"))
  expect_false(attr(dplyr::group_data(replaced), ".drop"))
})

test_that("resource arguments are validated and object resources are supported", {
  dataset <- tibble::tibble(Id = "DS1")
  metadata <- tibble::tibble(Id = "DS1", condition = "A")

  expect_error(
    extract_metadata(
      dataset,
      metadata,
      "condition",
      by = "Id",
      resource = "participants"
    ),
    "only be used",
    class = "glcdp_metadata_source_error"
  )

  package <- glc_open(make_glc_fixture("3.0.0"), quiet = TRUE)
  study <- extract_metadata(
    tibble::tibble(study = "S1"),
    package,
    "title",
    by = c(study = "study_internal_id"),
    resource = "study"
  )
  expect_equal(study$title, "Light study")
  expect_error(
    extract_metadata(
      tibble::tibble(participant_Id = "P1"),
      package,
      "unknown",
      by = c(participant_Id = "participant_internal_id")
    ),
    "None of the requested",
    class = "glcdp_metadata_field_no_match"
  )
})
