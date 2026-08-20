test_that("validated schema 3 package supports the complete live workflow", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("GLCDP_RUN_LIVE_TESTS"), "true"),
    "Set GLCDP_RUN_LIVE_TESTS=true to run live GitHub integration tests"
  )
  package <- glc_open(
    "tscnlab/melidos-iztech-glc-dataset",
    quiet = TRUE
  )
  summary <- glc_summary(package)
  datasets <- glc_datasets(package)
  files <- glc_files(package)
  variables <- glc_variables(package)

  expect_equal(package$schema_version, "3.0.2")
  expect_gt(summary$dataset_count, 0L)
  expect_gt(summary$participant_count, 0L)
  expect_gt(summary$file_group_count, 0L)
  expect_equal(summary$file_count, nrow(files))
  expect_equal(summary$variable_count, nrow(variables))
  expect_true(all(
    variables$type %in%
      c("string", "boolean", "numeric", "integer", "factor")
  ))

  contents <- glcdp:::glc_explorer_load_contents(package)
  expect_equal(
    c(
      datasets = nrow(contents$datasets),
      files = nrow(contents$files),
      variables = nrow(contents$variables),
      metadata = nrow(contents$metadata)
    ),
    c(
      datasets = 17L,
      files = 323L,
      variables = 5554L,
      metadata = 63983L
    )
  )
  expected_metadata_counts <- c(
    datasets = 61537L,
    device_datasheets = 164L,
    devices = 144L,
    participant_characteristics = 1955L,
    participants = 68L,
    study = 115L
  )
  metadata_counts <- table(contents$metadata$resource)
  expect_equal(names(metadata_counts), names(expected_metadata_counts))
  expect_equal(
    unname(as.integer(metadata_counts)),
    unname(expected_metadata_counts)
  )

  factors <- variables[variables$type == "factor", , drop = FALSE]
  expect_gt(nrow(factors), 0L)
  expect_true(all(lengths(factors$factor_values) > 0L))
  expect_equal(
    lengths(factors$factor_values),
    lengths(factors$factor_labels)
  )

  demographic_group <- "MELIDOS_IZTECH_S001:4"
  sex_declaration <- variables[
    variables$file_group_id == demographic_group &
      variables$name == "sex",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(sex_declaration), 1L)
  expect_equal(sex_declaration$type, "factor")
  demographic_data <- glc_read(
    package,
    dataset_id = "MELIDOS_IZTECH_S001",
    file_group = demographic_group,
    n_max = 3,
    progress = FALSE
  )$data[[1L]]
  expect_s3_class(demographic_data$sex, "factor")
  expect_identical(
    levels(demographic_data$sex),
    sex_declaration$factor_labels[[1L]]
  )

  awakenings_group <- "MELIDOS_IZTECH_S001:14"
  awakenings_declaration <- variables[
    variables$file_group_id == awakenings_group &
      variables$name == "awakenings",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(awakenings_declaration), 1L)
  expect_equal(awakenings_declaration$type, "integer")
  awakenings_data <- glc_read(
    package,
    dataset_id = "MELIDOS_IZTECH_S001",
    file_group = awakenings_group,
    variables = "awakenings",
    n_max = 3,
    progress = FALSE
  )$data[[1L]]
  expect_type(awakenings_data$awakenings, "integer")

  metadata <- glc_metadata(
    package,
    resources = c("study", "participants", "participant_characteristics")
  )
  expect_named(
    metadata,
    c("study", "participants", "participant_characteristics")
  )
  expect_s3_class(metadata$participants, "tbl_df")
  expect_equal(nrow(metadata$participants), summary$participant_count)
  expect_gt(
    nrow(glc_search_metadata(
      package,
      "participant_age",
      search_in = "fields"
    )),
    0L
  )

  first <- files[1L, , drop = FALSE]
  sample <- glc_read(
    package,
    dataset_id = first$dataset_id[[1L]],
    file_group = first$file_group_id[[1L]],
    n_max = 3,
    progress = FALSE
  )
  expect_lte(nrow(sample$data[[1L]]), 3L)
  expect_s3_class(sample$data[[1L]]$.glc_datetime, "POSIXct")
  expect_no_error(standard <- glc_collect(sample))
  expect_s3_class(standard$Id, "factor")
  expect_s3_class(standard$Datetime, "POSIXct")

  boolean_group <- variables$file_group_id[variables$type == "boolean"][[1L]]
  boolean_dataset <- variables$dataset_id[
    variables$file_group_id == boolean_group
  ][[1L]]
  boolean_names <- variables$name[
    variables$file_group_id == boolean_group &
      variables$type == "boolean"
  ]
  boolean_data <- glc_read(
    package,
    dataset_id = boolean_dataset,
    file_group = boolean_group,
    n_max = 3,
    progress = FALSE
  )$data[[1L]]
  expect_true(all(vapply(
    boolean_data[boolean_names],
    is.logical,
    logical(1)
  )))

  source_name_group <- variables$file_group_id[
    variables$name == "file.name"
  ][[1L]]
  source_name_dataset <- variables$dataset_id[
    variables$file_group_id == source_name_group
  ][[1L]]
  source_name <- glc_read(
    package,
    dataset_id = source_name_dataset,
    file_group = source_name_group,
    n_max = 3,
    progress = FALSE
  )
  expect_no_error(source_name <- glc_collect(source_name))
  expect_type(source_name$file.name, "character")

  selection <- glcdp:::glc_explorer_load_selection(package)
  empty_facets <- list(
    participant = list(
      age = character(),
      sex = character(),
      gender = character(),
      characteristic_name = character(),
      characteristic_values = character()
    ),
    device = list(
      manufacturer = character(),
      model = character(),
      sensor_type = character()
    )
  )
  medi_plan <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    empty_facets,
    dataset_ids = datasets$dataset_id,
    variables = "MEDI"
  )
  expect_true(medi_plan$script_ready)
  expect_equal(medi_plan$group_filter$candidate_count, 323L)
  expect_equal(medi_plan$group_filter$matching_count, 52L)
  expect_equal(medi_plan$group_filter$included_count, 52L)
  expect_equal(medi_plan$group_filter$excluded_count, 271L)
  expect_equal(length(medi_plan$datasets), 17L)
  expect_equal(length(medi_plan$file_groups), 52L)
  expect_equal(medi_plan$variables, "MEDI")

  medi_term_plan <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    empty_facets,
    dataset_ids = datasets$dataset_id,
    terms = "melanopic_edi"
  )
  expect_true(medi_term_plan$script_ready)
  expect_equal(medi_term_plan$term_filter, "melanopic_edi")
  expect_equal(medi_term_plan$variables, "MEDI")
  expect_equal(medi_term_plan$file_groups, medi_plan$file_groups)

  package$transport$token <- NULL
  destination <- tempfile("melidos-schema3-subset-")
  downloads <- glc_download(
    package,
    destination,
    include = "data",
    dataset_id = first$dataset_id[[1L]],
    file_group = first$file_group_id[[1L]]
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
  expect_equal(local$schema_version, package$schema_version)
  expect_equal(local_summary$available_file_count, 1L)
  expect_equal(unique(local_collection$dataset_id), first$dataset_id[[1L]])
})
