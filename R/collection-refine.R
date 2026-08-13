glc_collection_refinement_schema <- function() {
  "glc-collection-refinement"
}

glc_collection_refinement_version <- function() {
  "1.0.0"
}

glc_collection_refinement_input_schema <- function() {
  "glc-collection-refinement-input"
}

glc_collection_refinement_input_version <- function() {
  "1.0.0"
}

glc_collection_refine_supported_plan_versions <- function() {
  "1.1.0"
}

glc_plan_refinement_group <- function(record) {
  list(
    dataset_id = record$dataset_id,
    file_group = record$file_group,
    file_group_id = record$file_group_id,
    study_id = record$study_id,
    participant_id = record$participant_id,
    participant_associated = record$participant_associated,
    study_link_status = record$study_link_status,
    participant_link_status = record$participant_link_status,
    device_id = record$device_id,
    device_link_status = record$device_link_status,
    compatibility_id = record$compatibility_id,
    files = lapply(record$files, function(file) {
      list(declared_bytes = file$declared_bytes)
    })
  )
}

glc_plan_refinement_contracts <- function(records) {
  if (length(records) == 0L) {
    return(list())
  }
  compatibility_ids <- vapply(
    records,
    function(record) record$compatibility_id,
    character(1)
  )
  ids <- glc_plan_sort_utf8(unique(compatibility_ids))
  lapply(ids, function(compatibility_id) {
    index <- which(compatibility_ids == compatibility_id)[[1L]]
    list(
      compatibility_id = compatibility_id,
      compatibility = glc_plan_declared_compatibility(records[[index]])
    )
  })
}

glc_plan_refinement_request <- function(request) {
  request[c(
    "terms",
    "term_match",
    "term_identifier",
    "labels_used_for_matching",
    "variable_scope",
    "requested_variables",
    "dataset_id",
    "file_group",
    "standardize"
  )]
}

glc_plan_refinement_provenance <- function(provenance) {
  provenance[c(
    "package_id",
    "repository",
    "source_revision",
    "package_schema_version"
  )]
}

glc_plan_refinement_membership <- function(records) {
  if (length(records) == 0L) {
    return(tibble::tibble(
      file_group_id = character(),
      status = character(),
      compatibility_id = character()
    ))
  }
  tibble::tibble(
    file_group_id = vapply(
      records,
      function(record) record$file_group_id,
      character(1)
    ),
    status = vapply(records, function(record) record$status, character(1)),
    compatibility_id = vapply(
      records,
      function(record) record$compatibility_id,
      character(1)
    )
  )
}

glc_plan_refinement_input <- function(records, request, provenance) {
  included <- records[vapply(
    records,
    function(record) identical(record$status, "included"),
    logical(1)
  )]
  body <- list(
    schema = glc_collection_refinement_input_schema(),
    version = glc_collection_refinement_input_version(),
    canonicalization = "length-prefixed UTF-8 text",
    digest_algorithm = "SHA-256",
    provenance = glc_plan_refinement_provenance(provenance),
    request = glc_plan_refinement_request(request),
    membership = glc_plan_refinement_membership(records),
    contracts = glc_plan_refinement_contracts(included),
    groups = lapply(included, glc_plan_refinement_group)
  )
  c(body, list(fingerprint = glc_plan_digest(body)))
}

glc_collection_refine_abort <- function(
  message,
  class,
  .envir = parent.frame()
) {
  glc_abort(
    message,
    class = c(class, "glcdp_collection_refine_error"),
    .envir = .envir
  )
}

glc_collection_refine_validate_plan <- function(plan) {
  if (!inherits(plan, "glc_collection_plan") || !is.list(plan)) {
    glc_collection_refine_abort(
      "{.arg plan} must be a validated {.cls glc_collection_plan} object.",
      "glcdp_collection_refine_plan"
    )
  }
  supported_version <- is.character(plan$plan_version) &&
    length(plan$plan_version) == 1L &&
    !is.na(plan$plan_version) &&
    plan$plan_version %in% glc_collection_refine_supported_plan_versions()
  if (
    !identical(plan$plan_schema, glc_collection_plan_schema()) ||
      !supported_version
  ) {
    glc_collection_refine_abort(
      paste0(
        "{.arg plan} uses an unsupported collection-plan schema or version. ",
        "Create a new plan with this glcdp version."
      ),
      "glcdp_collection_refine_version"
    )
  }
  input <- plan$refinement_input
  if (
    !is.list(input) ||
      !identical(input$schema, glc_collection_refinement_input_schema()) ||
      !identical(input$version, glc_collection_refinement_input_version()) ||
      !is.character(input$fingerprint) ||
      length(input$fingerprint) != 1L ||
      is.na(input$fingerprint) ||
      !grepl("^[0-9a-f]{64}$", input$fingerprint)
  ) {
    glc_collection_refine_abort(
      "{.arg plan} does not contain a complete supported refinement input.",
      "glcdp_collection_refine_incomplete"
    )
  }
  body <- input[setdiff(names(input), "fingerprint")]
  if (!identical(glc_plan_digest(body), input$fingerprint)) {
    glc_collection_refine_abort(
      "{.arg plan} refinement facts have changed since the plan was created.",
      "glcdp_collection_refine_tampered"
    )
  }
  expected_provenance <- glc_plan_refinement_provenance(plan$provenance)
  expected_request <- glc_plan_refinement_request(plan$request)
  if (
    !identical(input$provenance, expected_provenance) ||
      !identical(input$request, expected_request)
  ) {
    glc_collection_refine_abort(
      "{.arg plan} provenance or request does not match its refinement input.",
      "glcdp_collection_refine_tampered"
    )
  }
  if (
    !is.data.frame(plan$groups) ||
      !all(
        c("file_group_id", "status", "compatibility_id") %in%
          names(plan$groups)
      ) ||
      !identical(
        input$membership,
        plan$groups[, names(input$membership), drop = FALSE]
      )
  ) {
    glc_collection_refine_abort(
      "{.arg plan} group membership does not match its refinement input.",
      "glcdp_collection_refine_tampered"
    )
  }
  input_ids <- tryCatch(
    vapply(
      input$groups,
      function(group) group$file_group_id,
      character(1)
    ),
    error = function(cnd) NULL
  )
  included_ids <- input$membership$file_group_id[
    input$membership$status == "included"
  ]
  contract_ids <- tryCatch(
    vapply(
      input$contracts,
      function(contract) contract$compatibility_id,
      character(1)
    ),
    error = function(cnd) NULL
  )
  included_contract_ids <- unique(input$membership$compatibility_id[
    input$membership$status == "included"
  ])
  if (
    is.null(input_ids) ||
      is.null(contract_ids) ||
      anyDuplicated(input_ids) ||
      anyDuplicated(contract_ids) ||
      !identical(input_ids, included_ids) ||
      !setequal(contract_ids, included_contract_ids)
  ) {
    glc_collection_refine_abort(
      "{.arg plan} contains incomplete refinement groups or contracts.",
      "glcdp_collection_refine_incomplete"
    )
  }
  invisible(input)
}

glc_collection_refine_file_groups <- function(file_group) {
  if (
    !is.character(file_group) ||
      anyNA(file_group) ||
      any(!nzchar(file_group))
  ) {
    glc_collection_refine_abort(
      "{.arg file_group} must contain non-empty stable identifiers without missing values.",
      "glcdp_collection_refine_file_group"
    )
  }
  if (length(file_group) == 0L) {
    glc_collection_refine_abort(
      "{.arg file_group} must select at least one included file group.",
      "glcdp_collection_refine_empty"
    )
  }
  if (anyDuplicated(file_group)) {
    duplicated_ids <- unique(file_group[duplicated(file_group)])
    glc_collection_refine_abort(
      "{.arg file_group} must not contain duplicate identifiers: {.val {duplicated_ids}}.",
      "glcdp_collection_refine_duplicate"
    )
  }
  glc_plan_sort_utf8(file_group)
}

glc_collection_refine_compatibility_id <- function(compatibility_id) {
  if (is.null(compatibility_id)) {
    return(NULL)
  }
  if (
    !is.character(compatibility_id) ||
      length(compatibility_id) != 1L ||
      is.na(compatibility_id) ||
      !nzchar(compatibility_id)
  ) {
    glc_collection_refine_abort(
      "{.arg compatibility_id} must be one non-missing structural identifier.",
      "glcdp_collection_refine_compatibility"
    )
  }
  compatibility_id
}

glc_collection_refine_selection <- function(
  input,
  file_group,
  compatibility_id
) {
  membership <- input$membership
  indexes <- match(file_group, membership$file_group_id)
  if (anyNA(indexes)) {
    unknown <- file_group[is.na(indexes)]
    glc_collection_refine_abort(
      "Unknown file-group identifier{?s}: {.val {unknown}}.",
      "glcdp_collection_refine_unknown_group"
    )
  }
  selected <- membership[indexes, , drop = FALSE]
  excluded <- selected$file_group_id[selected$status != "included"]
  if (length(excluded) > 0L) {
    glc_collection_refine_abort(
      "Excluded file group{?s} cannot be refined: {.val {excluded}}.",
      "glcdp_collection_refine_excluded_group"
    )
  }
  group_ids <- vapply(
    input$groups,
    function(group) group$file_group_id,
    character(1)
  )
  selected_groups <- input$groups[match(file_group, group_ids)]
  unresolved <- vapply(
    selected_groups,
    function(group) {
      any(
        c(
          group$study_link_status,
          group$participant_link_status,
          group$device_link_status
        ) %in%
          c("unresolved", "metadata_unavailable")
      )
    },
    logical(1)
  )
  if (any(unresolved)) {
    unresolved_ids <- file_group[unresolved]
    glc_collection_refine_abort(
      paste0(
        "File group{?s} {.val {unresolved_ids}} have unresolved required ",
        "metadata identities and cannot be refined."
      ),
      "glcdp_collection_refine_unresolved_identity"
    )
  }
  structural_ids <- unique(selected$compatibility_id)
  if (
    length(structural_ids) != 1L ||
      is.na(structural_ids) ||
      !nzchar(structural_ids)
  ) {
    glc_collection_refine_abort(
      "Selected file groups span more than one structural compatibility set.",
      "glcdp_collection_refine_cross_structure"
    )
  }
  inferred <- structural_ids[[1L]]
  if (!is.null(compatibility_id) && !identical(compatibility_id, inferred)) {
    known_ids <- unique(membership$compatibility_id[
      membership$status == "included"
    ])
    if (!compatibility_id %in% known_ids) {
      glc_collection_refine_abort(
        "Unknown structural compatibility identifier {.val {compatibility_id}}.",
        "glcdp_collection_refine_unknown_compatibility"
      )
    }
    glc_collection_refine_abort(
      paste0(
        "Selected file groups do not belong to {.val {compatibility_id}}. ",
        "Their structural identifier is {.val {inferred}}."
      ),
      "glcdp_collection_refine_compatibility_mismatch"
    )
  }
  inferred
}

glc_collection_refine_empty_constraints <- function() {
  tibble::tibble(
    compatibility_id = character(),
    code = character(),
    message = character(),
    resolved = logical()
  )
}

glc_collection_refine_constraints <- function(compatibility_id, units) {
  if (length(units) <= 1L) {
    return(glc_collection_refine_empty_constraints())
  }
  tibble::tibble(
    compatibility_id = compatibility_id,
    code = "device_slot_allocation",
    message = paste0(
      "The narrowed groups still require multiple final units because one ",
      "dataset links groups to more than one non-missing device."
    ),
    resolved = FALSE
  )
}

glc_collection_refine_groups_table <- function(records) {
  if (length(records) == 0L) {
    return(tibble::tibble(
      status = character(),
      unit_id = character(),
      compatibility_id = character(),
      dataset_id = character(),
      file_group = integer(),
      file_group_id = character(),
      study_id = character(),
      participant_id = character(),
      participant_associated = logical(),
      study_link_status = character(),
      participant_link_status = character(),
      device_id = character(),
      device_link_status = character(),
      reason_codes = list(),
      messages = list()
    ))
  }
  tibble::tibble(
    status = vapply(records, function(record) record$status, character(1)),
    unit_id = vapply(records, function(record) record$unit_id, character(1)),
    compatibility_id = vapply(
      records,
      function(record) record$compatibility_id,
      character(1)
    ),
    dataset_id = vapply(
      records,
      function(record) record$dataset_id,
      character(1)
    ),
    file_group = vapply(
      records,
      function(record) record$file_group,
      integer(1)
    ),
    file_group_id = vapply(
      records,
      function(record) record$file_group_id,
      character(1)
    ),
    study_id = vapply(records, function(record) record$study_id, character(1)),
    participant_id = vapply(
      records,
      function(record) record$participant_id,
      character(1)
    ),
    participant_associated = vapply(
      records,
      function(record) record$participant_associated,
      logical(1)
    ),
    study_link_status = vapply(
      records,
      function(record) record$study_link_status,
      character(1)
    ),
    participant_link_status = vapply(
      records,
      function(record) record$participant_link_status,
      character(1)
    ),
    device_id = vapply(
      records,
      function(record) record$device_id,
      character(1)
    ),
    device_link_status = vapply(
      records,
      function(record) record$device_link_status,
      character(1)
    ),
    reason_codes = lapply(records, function(record) {
      vapply(record$reasons, function(reason) reason$code, character(1))
    }),
    messages = lapply(records, function(record) {
      vapply(record$reasons, function(reason) reason$message, character(1))
    })
  )
}

#' Refine a collection plan from stable file-group identifiers
#'
#' Recompute final collection units for an in-memory subset of one structural
#' compatibility set. Refinement validates and reuses the compact facts stored
#' in a [glc_collection_plan()] result. It does not reopen the package, load
#' metadata, access a network, or inspect measurement contents.
#'
#' @param plan A validated `glc_collection_plan` object with supported plan and
#'   refinement-input schema versions. Keep this parent plan after refinement;
#'   the lightweight result refers to it by schema, version, and fingerprint and
#'   does not copy its normalized metadata tables.
#' @param file_group A character vector of unique, non-missing stable
#'   `file_group_id` values. Every value must be an included member of `plan`
#'   and all values must belong to one structural compatibility set. Numeric
#'   declaration indices are not accepted.
#' @param compatibility_id Optional single structural compatibility identifier.
#'   When `NULL`, the identifier is inferred only if all selected groups share
#'   exactly one set. When supplied, it must equal that set's identifier.
#'
#' @details
#' `glc_collection_refine()` is the final, fast step after interactive metadata
#' narrowing. Build [glc_collection_plan()] once, choose one row of
#' `plan$compatibility_sets`, and filter its member groups through the typed
#' tables in `plan$groups` and `plan$metadata`. Pass only the resulting stable
#' file-group ids to this function. Input order does not affect the result.
#'
#' Refinement reapplies the stored relationship and device-slot rules and
#' creates request-sensitive final unit ids. Its `units` table is identical to
#' a fresh [glc_collection_plan()] call with the parent's original term,
#' variable, dataset, and standardization request and with `file_group` set to
#' the refined ids. The restriction-stable `compatibility_id` is retained. No
#' full variable or metadata tables are rebuilt.
#'
#' A result with more than one final unit has `final_selection_required = TRUE`
#' and an unresolved `"device_slot_allocation"` constraint. Narrow the stable
#' group ids again and refine again. A deterministic `preferred_unit_id` is
#' reported for display parity, but it does not override the final-selection
#' gate.
#'
#' @section Validation and conditions:
#' Refinement supports plan schema `"glc-collection-plan"` version `"1.1.0"`
#' and refinement-input schema `"glc-collection-refinement-input"` version
#' `"1.0.0"`. It verifies the compact input fingerprint and checks it against
#' the parent plan's provenance, request, and group membership. The fingerprint
#' covers only facts needed for refinement, not the larger normalized metadata
#' snapshot, so validation does not rehash the complete plan.
#'
#' All refinement errors inherit from `glcdp_collection_refine_error`.
#' More specific subclasses are:
#'
#' * `glcdp_collection_refine_plan` for a value that is not a collection plan;
#' * `glcdp_collection_refine_version` for an unsupported schema or version;
#' * `glcdp_collection_refine_incomplete` and
#'   `glcdp_collection_refine_tampered` for missing or changed parent facts;
#' * `glcdp_collection_refine_file_group`,
#'   `glcdp_collection_refine_empty`, and `glcdp_collection_refine_duplicate`
#'   for invalid file-group selectors;
#' * `glcdp_collection_refine_unknown_group` and
#'   `glcdp_collection_refine_excluded_group` for groups outside the eligible
#'   parent membership;
#' * `glcdp_collection_refine_compatibility`,
#'   `glcdp_collection_refine_unknown_compatibility`, and
#'   `glcdp_collection_refine_compatibility_mismatch` for invalid structural
#'   identifiers;
#' * `glcdp_collection_refine_cross_structure` when selected groups span more
#'   than one structural set; and
#' * `glcdp_collection_refine_unresolved_identity` when required study,
#'   participant, or device identities cannot be resolved from the stored
#'   metadata facts.
#'
#' @section Zero-access assurance:
#' Refinement performs no file or network input/output. It does not call
#' [glc_open()], [glc_files()], [glc_summary()], [glc_read()], [glc_collect()],
#' [glc_download()], or metadata-loading and materialization functions. Its
#' assurance records that the package was not reopened, remote availability was
#' not probed, and measurement contents were neither transferred nor inspected.
#' [glc_read()] and [glc_collect()] remain authoritative after refinement.
#'
#' @section Return structure:
#' The result is a plain serializable list with class
#' `glc_collection_refinement`, schema `"glc-collection-refinement"`, and
#' version `"1.0.0"`. It contains:
#'
#' * `parent`: `plan_schema`, `plan_version`, and the validated refinement-input
#'   `fingerprint` linking this result to the retained parent plan;
#' * `provenance`: `package_id`, `repository`, exact `source_revision`, and
#'   `package_schema_version`;
#' * `request`: the selected `compatibility_id`, sorted `file_group` ids, and
#'   the parent's compact `original` request;
#' * `assurance`: declaration basis plus package, network, availability-probe,
#'   measurement-transfer, measurement-inspection, and final-validation fields;
#' * `compatibility_id`, `final_selection_required`, and `preferred_unit_id`;
#' * `units`: the same stable final-unit columns documented for
#'   [glc_collection_plan()];
#' * `groups`: `status`, `unit_id`, `compatibility_id`, `dataset_id`, integer
#'   `file_group`, stable `file_group_id`, `study_id`, `participant_id`,
#'   `participant_associated`, `study_link_status`, `participant_link_status`,
#'   `device_id`, `device_link_status`, and list-columns `reason_codes` and
#'   `messages`; and
#' * `constraints`: `compatibility_id`, `code`, `message`, and logical
#'   `resolved`. Its schema is stable even when it has no rows.
#'
#' All tables and list-columns contain only plain serializable values. The
#' result contains no package handle, environment, token, cache path, or
#' temporary path. `print()` reports the structural id, final-unit and group
#' counts, final-selection status, and zero-access assurance, then returns the
#' result invisibly.
#'
#' @return A lightweight `glc_collection_refinement` object described in
#'   **Return structure**.
#' @seealso [glc_collection_plan()] for the required parent plan,
#'   [glc_read()] and [glc_collect()] for runtime validation and collection.
#' @export
#'
#' @examples
#' \dontrun{
#' # Use an existing local, manifest-backed package directory.
#' # This pattern performs no network request and does not read measurements.
#' pkg <- glc_open("path/to/manifest-backed-package", quiet = TRUE)
#' plan <- glc_collection_plan(
#'   pkg,
#'   terms = "photopic illuminance",
#'   variable_scope = "matched"
#' )
#'
#' # In an application, filter these ids with plan$groups and plan$metadata.
#' set <- plan$compatibility_sets[1L, ]
#' selected_ids <- set$file_group_ids[[1L]]
#' refined <- glc_collection_refine(
#'   plan,
#'   selected_ids,
#'   compatibility_id = set$compatibility_id[[1L]]
#' )
#' refined
#' }
glc_collection_refine <- function(
  plan,
  file_group,
  compatibility_id = NULL
) {
  input <- glc_collection_refine_validate_plan(plan)
  file_group <- glc_collection_refine_file_groups(file_group)
  compatibility_id <- glc_collection_refine_compatibility_id(
    compatibility_id
  )
  compatibility_id <- glc_collection_refine_selection(
    input,
    file_group,
    compatibility_id
  )
  group_ids <- vapply(
    input$groups,
    function(group) group$file_group_id,
    character(1)
  )
  records <- input$groups[match(file_group, group_ids)]
  contract_ids <- vapply(
    input$contracts,
    function(contract) contract$compatibility_id,
    character(1)
  )
  contract <- input$contracts[[match(
    compatibility_id,
    contract_ids
  )]]$compatibility
  records <- lapply(records, function(record) {
    record$status <- "included"
    record$reasons <- list(glc_plan_reason(
      "included",
      "The group matches the request and has a complete supported declaration."
    ))
    record$unit_id <- NA_character_
    record$declared_compatibility <- contract
    record
  })
  request <- input$request
  request$file_group <- file_group
  engine <- glc_declared_collection_partition(
    records,
    request,
    input$provenance
  )
  result_ids <- unique(vapply(
    engine$records,
    function(record) record$compatibility_id,
    character(1)
  ))
  if (
    length(result_ids) != 1L ||
      !identical(result_ids[[1L]], compatibility_id) ||
      any(vapply(
        engine$records,
        function(record) !identical(record$status, "included"),
        logical(1)
      ))
  ) {
    glc_collection_refine_abort(
      "Stored refinement facts no longer reproduce the parent structural set.",
      "glcdp_collection_refine_tampered"
    )
  }
  constraints <- glc_collection_refine_constraints(
    compatibility_id,
    engine$units
  )
  structure(
    list(
      refinement_schema = glc_collection_refinement_schema(),
      refinement_version = glc_collection_refinement_version(),
      parent = list(
        plan_schema = plan$plan_schema,
        plan_version = plan$plan_version,
        fingerprint = input$fingerprint
      ),
      provenance = input$provenance,
      request = list(
        compatibility_id = compatibility_id,
        file_group = file_group,
        original = input$request
      ),
      assurance = list(
        basis = "stored_validated_declarations",
        package_reopened = FALSE,
        network_access = FALSE,
        remote_availability_probed = FALSE,
        measurement_contents_transferred = FALSE,
        measurement_contents_inspected = FALSE,
        final_validation = c("glc_read", "glc_collect")
      ),
      compatibility_id = compatibility_id,
      final_selection_required = length(engine$units) > 1L,
      preferred_unit_id = engine$preferred_unit_id,
      units = glc_plan_units_table(engine$units),
      groups = glc_collection_refine_groups_table(engine$records),
      constraints = constraints
    ),
    class = c("glc_collection_refinement", "list")
  )
}

#' @export
print.glc_collection_refinement <- function(x, ...) {
  cat("<GLC collection refinement>\n")
  cat("Structure: ", x$compatibility_id, "\n", sep = "")
  cat(
    "Result: ",
    nrow(x$units),
    " final unit(s) from ",
    nrow(x$groups),
    " file group(s)\n",
    sep = ""
  )
  cat(
    "Final selection required: ",
    if (isTRUE(x$final_selection_required)) "yes" else "no",
    "\n",
    sep = ""
  )
  cat("Assurance: stored declarations only; no package or network access\n")
  invisible(x)
}
