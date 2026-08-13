collection_metadata_validated_package <- function(
  root,
  commit = strrep("a", 40L)
) {
  package <- glc_open(root, quiet = TRUE)
  package$repo <- "owner/fixture"
  package$commit <- commit
  package$verified <- TRUE
  package$manifest <- list(
    manifest_version = "1.0",
    repository = package$repo,
    commit = commit,
    registry_verified = TRUE,
    files = list()
  )
  package
}

collection_metadata_has_live_value <- function(x) {
  if (
    is.environment(x) ||
      is.function(x) ||
      is.language(x) ||
      typeof(x) %in% c("externalptr", "weakref")
  ) {
    return(TRUE)
  }
  if (is.list(x)) {
    return(any(vapply(
      x,
      collection_metadata_has_live_value,
      logical(1)
    )))
  }
  FALSE
}

make_collection_metadata_fixture <- function() {
  root <- make_glc_fixture("3.0.2")
  descriptor_path <- file.path(root, "datapackage.json")
  descriptor <- jsonlite::fromJSON(descriptor_path, simplifyVector = FALSE)
  descriptor$resources <- c(
    descriptor$resources,
    list(
      list(
        name = "participant_characteristics",
        path = "data/participant-characteristics.json",
        format = "json"
      ),
      list(
        name = "contributors",
        path = "data/contributors.json",
        format = "json"
      )
    )
  )
  write_fixture_json(descriptor, descriptor_path)

  study <- jsonlite::fromJSON(
    file.path(root, "data", "study.json"),
    simplifyVector = FALSE
  )
  study$study_geographical_location <- "Munich, Germany"
  study$study_contributors <- list(list(
    contributor_full_name = "Study Contributor",
    contributor_roles = list("Investigation"),
    contributor_institution = list(
      contributor_institution_name = "Example Institute",
      contributor_institution_city = "Munich",
      contributor_institution_country = "Germany"
    )
  ))
  write_fixture_json(study, file.path(root, "data", "study.json"))

  write_fixture_json(
    list(list(
      participant_internal_id = "P1",
      participant_age = 31.5,
      participant_sex = "female",
      participant_gender = "woman",
      future_participant_field = list(code = 7)
    )),
    file.path(root, "data", "participants.json")
  )
  write_fixture_json(
    list(list(
      participant_internal_id = "P1",
      participant_characteristic_name = "Chronotype score",
      participant_characteristic_value = 7.5,
      participant_characteristic_unit = "score",
      participant_characteristic_description = "Typed source value"
    )),
    file.path(root, "data", "participant-characteristics.json")
  )
  write_fixture_json(
    list(list(
      contributor_full_name = "Package Contributor",
      contributor_roles = list("Data curation", "Validation"),
      contributor_email = "contributor@example.org",
      contributor_orcid = "https://orcid.org/0000-0000-0000-0001",
      contributor_institution = list(
        contributor_institution_name = "Example Institute",
        contributor_institution_city = "Munich",
        contributor_institution_country = "Germany"
      )
    )),
    file.path(root, "data", "contributors.json")
  )
  write_fixture_json(
    list(list(
      schema_version = "3.0.2",
      device_internal_id = "D1",
      device_manufacturer = "Acme",
      device_model = "Light One",
      device_serial_number = "123",
      device_datasheet_id = "D1-sheet",
      device_sensors = list(
        list(
          device_sensor_type = "light",
          device_sensor_datasheet_id = "D1-sheet"
        ),
        list(device_sensor_type = "accelerometer")
      )
    )),
    file.path(root, "data", "devices.json")
  )
  write_fixture_json(
    list(
      schema_version = "3.0.2",
      device_datasheet_internal_id = "D1-sheet",
      datasheet_manufacturer = "Acme",
      datasheet_type = "wearable",
      datasheet_sensor_modality = list("light", "accelerometer"),
      datasheet_model = "Light One"
    ),
    file.path(root, "data", "datasheets", "D1.json")
  )

  datasets <- fixture_read_datasets(root)
  group <- datasets[[1L]]$dataset_file[[1L]]
  group$dataset_file_description <- "A provided description"
  group$dataset_file_instrument <- list(
    instrument_type = "sensor",
    instrument_name = "Light One",
    collection_method = "automatic",
    recorded_by = "device",
    software_name = "Firmware"
  )
  missing <- group
  missing$dataset_file_names <- list("data/files/missing-description.csv")
  missing$dataset_file_description <- NULL
  missing$dataset_file_crossref_device_id <- NULL
  missing$dataset_file_instrument <- NULL
  not_applicable <- group
  not_applicable$dataset_file_names <- list("data/files/not-applicable.csv")
  not_applicable$dataset_file_description <- "not applicable"
  not_applicable$dataset_file_crossref_device_id <- "D-unknown"
  not_applicable$dataset_file_instrument <- list(
    instrument_type = "not applicable",
    instrument_name = "not applicable",
    collection_method = "not applicable",
    recorded_by = "not applicable",
    software_name = "not applicable"
  )
  datasets[[1L]]$dataset_file <- list(group, missing, not_applicable)
  fixture_write_datasets(root, datasets)
  root
}

test_that("collection plans expose typed normalized core metadata", {
  root <- make_collection_metadata_fixture()
  plan <- glc_collection_plan(
    collection_metadata_validated_package(root),
    terms = "photopic illuminance",
    variable_scope = "matched"
  )
  metadata <- plan$metadata

  expect_identical(metadata$schema, "glc-package-metadata")
  expect_identical(metadata$version, "1.0.0")
  expected_columns <- list(
    resource_status = c(
      "resource",
      "declared",
      "status",
      "record_count"
    ),
    studies = c(
      "study_id",
      "schema_version",
      "title",
      "short_description",
      "preregistration",
      "registration",
      "ethics",
      "sample",
      "intervention",
      "setting",
      "geographical_location",
      "study_type",
      "funding_sources",
      "keywords",
      "dataset_ids"
    ),
    study_groups = c(
      "study_id",
      "position",
      "name",
      "description",
      "size",
      "inclusion",
      "exclusion",
      "dataset_ids"
    ),
    study_contributors = c(
      "study_id",
      "position",
      "full_name",
      "roles",
      "email",
      "orcid",
      "institution_name",
      "institution_city",
      "institution_country"
    ),
    contributors = c(
      "contributor_id",
      "position",
      "full_name",
      "roles",
      "email",
      "orcid",
      "institution_name",
      "institution_city",
      "institution_country"
    ),
    datasets = c(
      "dataset_id",
      "schema_version",
      "study_id",
      "study_link_status",
      "participant_id",
      "participant_associated",
      "participant_link_status",
      "timezone",
      "latitude",
      "longitude",
      "file_group_count",
      "file_count",
      "modalities",
      "device_ids",
      "primary_variables"
    ),
    dataset_terms = c("dataset_id", "position", "term", "label"),
    participants = c("participant_id", "age", "sex", "gender"),
    participant_characteristics = c(
      "participant_id",
      "participant_link_status",
      "characteristic_position",
      "value_position",
      "name",
      "value",
      "value_type",
      "unit",
      "description"
    ),
    devices = c(
      "device_id",
      "schema_version",
      "manufacturer",
      "model",
      "serial_number",
      "calibration_date",
      "firmware_version",
      "datasheet_id",
      "datasheet_link_status"
    ),
    device_sensors = c(
      "device_id",
      "position",
      "sensor_type",
      "datasheet_id",
      "datasheet_link_status"
    ),
    datasheets = c(
      "datasheet_id",
      "schema_version",
      "datasheet_version",
      "manufacturer",
      "type",
      "modalities",
      "modality_other",
      "model",
      "calibration_interval",
      "calibration_method",
      "calibration_accuracy",
      "calibration_range",
      "calibration_notes",
      "calibration_spectral_sensitivity",
      "calibration_linearity",
      "calibration_directional_response"
    ),
    datasheet_parameters = c(
      "datasheet_id",
      "position",
      "name",
      "value",
      "value_type",
      "unit",
      "description"
    ),
    datasheet_channels = c(
      "datasheet_id",
      "position",
      "channel_number",
      "name",
      "description",
      "unit"
    ),
    instruments = c(
      "dataset_id",
      "file_group_id",
      "instrument_type",
      "instrument_name",
      "collection_method",
      "recorded_by",
      "software_name"
    ),
    file_group_variables = c(
      "dataset_id",
      "file_group_id",
      "declaration_position",
      "selected_by_request",
      "selected_for_output",
      "selection_origin",
      "name",
      "label",
      "description",
      "unit",
      "calibration",
      "type",
      "term",
      "term_name",
      "primary",
      "factor_level_count"
    ),
    file_group_factor_levels = c(
      "dataset_id",
      "file_group_id",
      "variable_position",
      "variable_name",
      "level_position",
      "value",
      "label",
      "description"
    ),
    extensions = c(
      "resource",
      "entity_type",
      "entity_id",
      "parent_id",
      "position",
      "metadata"
    )
  )
  expect_identical(
    names(metadata),
    c("schema", "version", names(expected_columns))
  )
  for (table in names(expected_columns)) {
    expect_identical(names(metadata[[table]]), expected_columns[[table]])
  }
  expect_identical(
    metadata$resource_status$resource,
    glcdp:::glc_core_resource_names()
  )
  expect_true(all(metadata$resource_status$status == "loaded"))
  expect_false("sites" %in% names(metadata))

  expect_type(metadata$participants$age, "double")
  expect_equal(metadata$participants$age, 31.5)
  expect_type(metadata$datasets$latitude, "double")
  expect_type(metadata$datasets$longitude, "double")
  expect_equal(metadata$datasets$latitude, 48.1)
  expect_equal(metadata$datasets$longitude, 11.5)
  expect_identical(metadata$datasets$timezone, "Europe/Berlin")
  expect_identical(
    metadata$studies$geographical_location,
    "Munich, Germany"
  )

  characteristic <- metadata$participant_characteristics[1L, ]
  expect_identical(characteristic$participant_link_status, "linked")
  expect_type(characteristic$value[[1L]], "double")
  expect_equal(characteristic$value[[1L]], 7.5)
  expect_identical(characteristic$value_type, "double")
  expect_identical(characteristic$unit, "score")
  expect_identical(characteristic$description, "Typed source value")

  expect_identical(metadata$devices$manufacturer, "Acme")
  expect_identical(metadata$devices$model, "Light One")
  expect_identical(metadata$devices$datasheet_id, "D1-sheet")
  expect_identical(metadata$devices$datasheet_link_status, "linked")
  expect_setequal(
    metadata$device_sensors$sensor_type,
    c(
      "light",
      "accelerometer"
    )
  )
  expect_identical(
    metadata$device_sensors$datasheet_link_status,
    c("linked", "not_applicable")
  )
  expect_identical(metadata$datasheets$type, "wearable")
  expect_identical(
    metadata$datasheets$modalities[[1L]],
    c("light", "accelerometer")
  )

  expect_true(is.na(metadata$contributors$contributor_id))
  expect_identical(metadata$contributors$position, 1L)
  expect_identical(
    metadata$contributors$full_name,
    "Package Contributor"
  )
  expect_identical(
    metadata$contributors$institution_city,
    "Munich"
  )
  expect_identical(
    metadata$study_contributors$full_name,
    "Study Contributor"
  )
  expect_identical(metadata$study_contributors$study_id, "S1")

  expect_identical(plan$groups$description[[1L]], "A provided description")
  expect_true(is.na(plan$groups$description[[2L]]))
  expect_identical(plan$groups$description[[3L]], "not applicable")
  expect_identical(plan$groups$instructions, rep("Wear continuously", 3L))
  expect_identical(
    plan$groups$device_link_status,
    c("linked", "not_applicable", "unresolved")
  )
  expect_identical(plan$groups$instrument_declared, c(TRUE, FALSE, TRUE))
  expect_identical(
    metadata$instruments$instrument_type,
    c("sensor", "not applicable")
  )
  expect_equal(nrow(metadata$file_group_variables), 12L)
  expect_equal(nrow(plan$variables), 3L)
  expect_identical(
    metadata$file_group_variables$name[
      metadata$file_group_variables$selected_by_request
    ],
    rep("lux", 3L)
  )
  expect_true("quality" %in% metadata$file_group_factor_levels$variable_name)

  participant_extension <- metadata$extensions[
    metadata$extensions$entity_type == "participant",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(participant_extension), 1L)
  expect_identical(
    participant_extension$metadata[[1L]]$future_participant_field[[1L]]$code,
    7L
  )
  expect_false(collection_metadata_has_live_value(metadata))
  expect_identical(
    unserialize(serialize(metadata, NULL, version = 2L)),
    metadata
  )
})

test_that("normalized metadata tables keep stable empty schemas", {
  plan <- glc_collection_plan(
    collection_metadata_validated_package(make_glc_fixture("3.0.2")),
    terms = "photopic illuminance"
  )

  expect_identical(
    names(plan$metadata$contributors),
    c(
      "contributor_id",
      "position",
      "full_name",
      "roles",
      "email",
      "orcid",
      "institution_name",
      "institution_city",
      "institution_country"
    )
  )
  expect_equal(nrow(plan$metadata$contributors), 0L)
  expect_equal(nrow(plan$metadata$participant_characteristics), 0L)
  expect_equal(nrow(plan$metadata$study_groups), 0L)
  expect_equal(nrow(plan$metadata$datasheet_parameters), 0L)
  expect_equal(nrow(plan$metadata$datasheet_channels), 0L)
  expect_equal(nrow(plan$metadata$instruments), 0L)
  expect_equal(nrow(plan$metadata$extensions), 0L)
})

test_that("planning transports only descriptor-allowlisted core metadata", {
  root <- make_collection_metadata_fixture()
  package <- collection_metadata_validated_package(root)
  unlink(file.path(root, "data", "files"), recursive = TRUE)
  original_materialize <- glcdp:::glc_materialize_file
  accessed <- character()
  materialize_core <- function(x, path) {
    accessed <<- c(accessed, path)
    if (startsWith(path, "data/files/")) {
      stop("measurement path accessed")
    }
    original_materialize(x, path)
  }
  forbidden <- function(...) {
    stop("forbidden content or availability operation")
  }
  testthat::local_mocked_bindings(
    glc_materialize_file = materialize_core,
    glc_files = forbidden,
    glc_summary = forbidden,
    glc_read = forbidden,
    glc_collect = forbidden,
    glc_download = forbidden,
    glc_file_info_internal = glcdp:::glc_file_info_internal,
    .package = "glcdp"
  )

  expect_no_error(
    plan <- glc_collection_plan(
      package,
      terms = "photopic illuminance"
    )
  )
  expected <- c(
    "data/study.json",
    "data/participants.json",
    "data/participant-characteristics.json",
    "data/devices.json",
    "data/datasheets/D1.json",
    "data/contributors.json"
  )
  expect_setequal(accessed, expected)
  expect_true(all(!startsWith(accessed, "data/files/")))
  expect_false(plan$assurance$measurement_contents_transferred)
  expect_false(plan$assurance$measurement_contents_inspected)
})
