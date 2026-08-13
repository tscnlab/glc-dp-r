collection_refine_validated_package <- function(
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

collection_refine_has_live_value <- function(x) {
  if (
    is.environment(x) ||
      is.function(x) ||
      is.language(x) ||
      typeof(x) %in% c("externalptr", "weakref")
  ) {
    return(TRUE)
  }
  if (is.list(x)) {
    return(any(vapply(x, collection_refine_has_live_value, logical(1))))
  }
  FALSE
}

collection_public_rd <- function(topic) {
  file <- paste0(topic, ".Rd")
  source_path <- testthat::test_path("..", "..", "man", file)
  if (file.exists(source_path)) {
    return(tools::parse_Rd(source_path))
  }
  database <- tools::Rd_db("glcdp")
  database[[file]]
}

collection_rd_section <- function(rd, tag) {
  tags <- vapply(
    rd,
    function(item) {
      value <- attr(item, "Rd_tag")
      if (is.null(value)) "" else value
    },
    character(1)
  )
  section <- rd[tags == tag]
  if (length(section) == 0L) {
    return("")
  }
  paste(unlist(section[[1L]], use.names = FALSE), collapse = "")
}

collection_rd_text <- function(rd) {
  paste(capture.output(tools::Rd2txt(rd)), collapse = "\n")
}

make_collection_refine_device_fixture <- function() {
  root <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(root)
  group_one <- datasets[[1L]]$dataset_file[[1L]]
  group_two <- group_one
  group_one$dataset_file_crossref_device_id <- "D1"
  group_one$dataset_file_names <- list("data/files/device-one.csv")
  group_two$dataset_file_crossref_device_id <- "D2"
  group_two$dataset_file_names <- list("data/files/device-two.csv")
  datasets[[1L]]$dataset_file <- list(group_one, group_two)
  fixture_write_datasets(root, datasets)
  write_fixture_json(
    list(
      list(device_internal_id = "D1"),
      list(device_internal_id = "D2")
    ),
    file.path(root, "data", "devices.json")
  )
  root
}

make_collection_refine_factor_fixture <- function() {
  root <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(root)
  first <- datasets[[1L]]
  second <- first
  first$dataset_internal_id <- "DS1"
  second$dataset_internal_id <- "DS2"
  first$dataset_file[[1L]]$dataset_file_names <- list("data/files/one.csv")
  second$dataset_file[[1L]]$dataset_file_names <- list("data/files/two.csv")
  second$dataset_file[[1L]]$dataset_file_variables[[4L]][[
    "dataset_file_variables_factor_levels"
  ]][[1L]]$label <- "Accepted"
  fixture_write_datasets(root, list(first, second))
  root
}

test_that("refinement is lightweight and matches a fresh restricted plan", {
  package <- collection_refine_validated_package(
    make_collection_refine_device_fixture()
  )
  parent <- glc_collection_plan(package, variable_scope = "all")

  expect_true("glc_collection_refine" %in% getNamespaceExports("glcdp"))
  expect_identical(
    getExportedValue("glcdp", "glc_collection_refine"),
    glc_collection_refine
  )
  expect_identical(
    names(formals(glc_collection_refine)),
    c("plan", "file_group", "compatibility_id")
  )
  expect_identical(formals(glc_collection_refine)$plan, quote(expr = ))
  expect_identical(formals(glc_collection_refine)$file_group, quote(expr = ))
  expect_null(formals(glc_collection_refine)$compatibility_id)
  expect_identical(
    getS3method("print", "glc_collection_refinement"),
    get("print.glc_collection_refinement", envir = asNamespace("glcdp"))
  )
  expect_equal(nrow(parent$compatibility_sets), 1L)
  expect_equal(nrow(parent$units), 2L)
  expect_true(parent$compatibility_sets$final_selection_required)
  compatibility_id <- parent$compatibility_sets$compatibility_id[[1L]]

  selected <- "DS1:1"
  refinement <- glc_collection_refine(parent, selected)
  explicit <- glc_collection_refine(
    parent,
    selected,
    compatibility_id = compatibility_id
  )
  fresh <- glc_collection_plan(
    package,
    variable_scope = "all",
    file_group = selected
  )

  expect_s3_class(refinement, "glc_collection_refinement")
  expect_identical(
    names(refinement),
    c(
      "refinement_schema",
      "refinement_version",
      "parent",
      "provenance",
      "request",
      "assurance",
      "compatibility_id",
      "final_selection_required",
      "preferred_unit_id",
      "units",
      "groups",
      "constraints"
    )
  )
  expect_identical(
    names(refinement$parent),
    c("plan_schema", "plan_version", "fingerprint")
  )
  expect_identical(
    names(refinement$provenance),
    c(
      "package_id",
      "repository",
      "source_revision",
      "package_schema_version"
    )
  )
  expect_identical(
    names(refinement$request),
    c("compatibility_id", "file_group", "original")
  )
  expect_identical(
    names(refinement$assurance),
    c(
      "basis",
      "package_reopened",
      "network_access",
      "remote_availability_probed",
      "measurement_contents_transferred",
      "measurement_contents_inspected",
      "final_validation"
    )
  )
  expect_identical(
    names(refinement$units),
    c(
      "unit_id",
      "compatibility_id",
      "preferred",
      "dataset_count",
      "file_group_count",
      "variable_count",
      "file_count",
      "declared_bytes",
      "known_file_count",
      "unknown_file_count",
      "declared_bytes_complete",
      "file_group_ids"
    )
  )
  expect_identical(
    names(refinement$groups),
    c(
      "status",
      "unit_id",
      "compatibility_id",
      "dataset_id",
      "file_group",
      "file_group_id",
      "study_id",
      "participant_id",
      "participant_associated",
      "study_link_status",
      "participant_link_status",
      "device_id",
      "device_link_status",
      "reason_codes",
      "messages"
    )
  )
  expect_identical(
    names(refinement$constraints),
    c("compatibility_id", "code", "message", "resolved")
  )
  expect_identical(
    refinement$refinement_schema,
    "glc-collection-refinement"
  )
  expect_identical(refinement$refinement_version, "1.0.0")
  expect_identical(refinement$compatibility_id, compatibility_id)
  expect_identical(explicit, refinement)
  expect_false(refinement$final_selection_required)
  expect_equal(nrow(refinement$units), 1L)
  expect_equal(nrow(refinement$constraints), 0L)
  expect_identical(refinement$units, fresh$units)
  expect_identical(
    refinement$groups$unit_id,
    fresh$groups$unit_id[fresh$groups$status == "included"]
  )
  expect_identical(
    refinement$groups$reason_codes,
    fresh$groups$reason_codes[fresh$groups$status == "included"]
  )
  expect_identical(
    refinement$compatibility_id,
    fresh$compatibility_sets$compatibility_id
  )
  expect_false(refinement$assurance$package_reopened)
  expect_false(refinement$assurance$network_access)
  expect_false(refinement$assurance$measurement_contents_transferred)
  expect_false(collection_refine_has_live_value(refinement))
  expect_identical(
    unserialize(serialize(refinement, NULL, version = 2L)),
    refinement
  )
  expect_lt(
    as.numeric(object.size(refinement)),
    as.numeric(object.size(parent))
  )

  printed <- capture_output(print(refinement))
  expect_match(printed, "1 final unit(s) from 1 file group(s)", fixed = TRUE)
  expect_match(printed, "no package or network access", fixed = TRUE)
})

test_that("refinement reruns device allocation deterministically", {
  package <- collection_refine_validated_package(
    make_collection_refine_device_fixture()
  )
  parent <- glc_collection_plan(package, variable_scope = "all")
  file_groups <- c("DS1:2", "DS1:1")
  refinement <- glc_collection_refine(parent, file_groups)
  reordered <- glc_collection_refine(parent, rev(file_groups))
  fresh <- glc_collection_plan(
    package,
    variable_scope = "all",
    file_group = file_groups
  )

  expect_identical(reordered, refinement)
  expect_true(refinement$final_selection_required)
  expect_equal(nrow(refinement$units), 2L)
  expect_identical(refinement$units, fresh$units)
  expect_identical(refinement$preferred_unit_id, fresh$preferred_unit_id)
  expect_identical(refinement$constraints$code, "device_slot_allocation")
  expect_false(refinement$constraints$resolved)
  expect_identical(
    unique(refinement$groups$compatibility_id),
    parent$compatibility_sets$compatibility_id
  )

  one_device <- glc_collection_refine(parent, "DS1:2")
  expect_false(one_device$final_selection_required)
  expect_equal(nrow(one_device$units), 1L)
  expect_identical(one_device$compatibility_id, refinement$compatibility_id)
})

test_that("refinement preserves each original variable request", {
  package <- collection_refine_validated_package(make_glc_fixture("3.0.2"))
  calls <- list(
    list(terms = "photopic illuminance", variable_scope = "matched"),
    list(variable_scope = "all"),
    list(variable_scope = "selected", variables = c("quality", "lux"))
  )
  for (arguments in calls) {
    parent <- do.call(glc_collection_plan, c(list(x = package), arguments))
    selected <- parent$groups$file_group_id[parent$groups$status == "included"]
    refinement <- glc_collection_refine(parent, selected)
    fresh <- do.call(
      glc_collection_plan,
      c(list(x = package, file_group = selected), arguments)
    )
    expect_identical(refinement$units, fresh$units)
    expect_identical(
      refinement$compatibility_id,
      fresh$compatibility_sets$compatibility_id
    )
    expect_identical(refinement$request$original$terms, parent$request$terms)
    expect_identical(
      refinement$request$original$requested_variables,
      parent$request$requested_variables
    )
  }
})

test_that("refinement rejects invalid selections with typed conditions", {
  package <- collection_refine_validated_package(
    make_collection_refine_device_fixture()
  )
  parent <- glc_collection_plan(package, variable_scope = "all")
  compatibility_id <- parent$compatibility_sets$compatibility_id[[1L]]

  expect_error(
    glc_collection_refine(list(), "DS1:1"),
    class = "glcdp_collection_refine_plan"
  )
  unsupported <- parent
  unsupported$plan_version <- "9.0.0"
  expect_error(
    glc_collection_refine(unsupported, "DS1:1"),
    class = "glcdp_collection_refine_version"
  )
  incomplete <- parent
  incomplete$refinement_input <- NULL
  expect_error(
    glc_collection_refine(incomplete, "DS1:1"),
    class = "glcdp_collection_refine_incomplete"
  )
  expect_error(
    glc_collection_refine(parent, character()),
    class = "glcdp_collection_refine_empty"
  )
  expect_error(
    glc_collection_refine(parent, c("DS1:1", "DS1:1")),
    class = "glcdp_collection_refine_duplicate"
  )
  expect_error(
    glc_collection_refine(parent, "unknown"),
    class = "glcdp_collection_refine_unknown_group"
  )
  expect_error(
    glc_collection_refine(
      parent,
      "DS1:1",
      compatibility_id = "glcc_unknown"
    ),
    class = "glcdp_collection_refine_unknown_compatibility"
  )
  expect_no_error(glc_collection_refine(
    parent,
    "DS1:1",
    compatibility_id = compatibility_id
  ))

  restricted <- glc_collection_plan(
    package,
    variable_scope = "all",
    file_group = "DS1:1"
  )
  expect_error(
    glc_collection_refine(restricted, "DS1:2"),
    class = "glcdp_collection_refine_excluded_group"
  )

  tampered <- parent
  tampered$refinement_input$groups[[1L]]$device_id <- "changed"
  expect_error(
    glc_collection_refine(tampered, "DS1:1"),
    class = "glcdp_collection_refine_tampered"
  )
})

test_that("cross-structure and unresolved identities cannot be refined", {
  factor_package <- collection_refine_validated_package(
    make_collection_refine_factor_fixture()
  )
  factor_plan <- glc_collection_plan(factor_package, variable_scope = "all")
  expect_equal(nrow(factor_plan$compatibility_sets), 2L)
  expect_error(
    glc_collection_refine(factor_plan, c("DS1:1", "DS2:1")),
    class = "glcdp_collection_refine_cross_structure"
  )
  first_id <- factor_plan$groups$compatibility_id[
    factor_plan$groups$file_group_id == "DS1:1"
  ]
  second_id <- factor_plan$groups$compatibility_id[
    factor_plan$groups$file_group_id == "DS2:1"
  ]
  expect_error(
    glc_collection_refine(
      factor_plan,
      "DS1:1",
      compatibility_id = second_id
    ),
    class = "glcdp_collection_refine_compatibility_mismatch"
  )
  expect_false(identical(first_id, second_id))

  unresolved_root <- make_glc_fixture("3.0.2")
  unresolved_datasets <- fixture_read_datasets(unresolved_root)
  unresolved_datasets[[1L]]$dataset_file[[1L]][[
    "dataset_file_crossref_device_id"
  ]] <- "D-unknown"
  fixture_write_datasets(unresolved_root, unresolved_datasets)
  unresolved_plan <- glc_collection_plan(
    collection_refine_validated_package(unresolved_root),
    variable_scope = "all"
  )
  expect_identical(unresolved_plan$groups$device_link_status, "unresolved")
  expect_error(
    glc_collection_refine(unresolved_plan, "DS1:1"),
    class = "glcdp_collection_refine_unresolved_identity"
  )
})

test_that("refinement never reopens or transports package content", {
  package <- collection_refine_validated_package(
    make_collection_refine_device_fixture()
  )
  parent <- glc_collection_plan(package, variable_scope = "all")
  forbidden <- function(...) {
    stop("forbidden package or transport operation")
  }
  testthat::local_mocked_bindings(
    glc_open = forbidden,
    glc_metadata = forbidden,
    glc_files = forbidden,
    glc_summary = forbidden,
    glc_read = forbidden,
    glc_collect = forbidden,
    glc_download = forbidden,
    glc_materialize_file = forbidden,
    glc_fetch_remote_raw = forbidden,
    .package = "glcdp"
  )

  expect_no_error(
    refinement <- glc_collection_refine(parent, "DS1:1")
  )
  expect_false(refinement$assurance$network_access)
  expect_false(refinement$assurance$package_reopened)
})

test_that("public collection help matches the exported safe workflow", {
  plan_rd <- collection_public_rd("glc_collection_plan")
  refine_rd <- collection_public_rd("glc_collection_refine")
  expect_s3_class(plan_rd, "Rd")
  expect_s3_class(refine_rd, "Rd")

  expect_match(
    collection_rd_section(plan_rd, "\\alias"),
    "glc_collection_plan",
    fixed = TRUE
  )
  expect_match(
    collection_rd_section(refine_rd, "\\alias"),
    "glc_collection_refine",
    fixed = TRUE
  )
  expect_match(
    collection_rd_section(refine_rd, "\\usage"),
    "compatibility_id = NULL",
    fixed = TRUE
  )

  plan_text <- collection_rd_text(plan_rd)
  refine_text <- collection_rd_text(refine_rd)
  expect_match(plan_text, "glc-collection-plan.*1.1.0")
  expect_match(plan_text, "compatibility_sets", fixed = TRUE)
  expect_match(plan_text, "glc-package-metadata", fixed = TRUE)
  expect_match(plan_text, "refinement_input", fixed = TRUE)
  expect_match(refine_text, "glc-collection-refinement", fixed = TRUE)
  expect_match(refine_text, "zero-access", ignore.case = TRUE)
  expect_match(refine_text, "final_selection_required", fixed = TRUE)
  expect_match(refine_text, "parent plan", ignore.case = TRUE)

  examples <- c(
    collection_rd_section(plan_rd, "\\examples"),
    collection_rd_section(refine_rd, "\\examples")
  )
  expect_true(all(grepl("path/to/manifest-backed-package", examples)))
  expect_false(any(grepl("https?://", examples)))
  expect_false(any(grepl(
    "glc_(read|collect|files|summary|download)[[:space:]]*\\(",
    examples
  )))
})
