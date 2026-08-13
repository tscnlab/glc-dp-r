# Refine a collection plan from stable file-group identifiers

Recompute final collection units for an in-memory subset of one
structural compatibility set. Refinement validates and reuses the
compact facts stored in a
[`glc_collection_plan()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_plan.md)
result. It does not reopen the package, load metadata, access a network,
or inspect measurement contents.

## Usage

``` r
glc_collection_refine(plan, file_group, compatibility_id = NULL)
```

## Arguments

- plan:

  A validated `glc_collection_plan` object with supported plan and
  refinement-input schema versions. Keep this parent plan after
  refinement; the lightweight result refers to it by schema, version,
  and fingerprint and does not copy its normalized metadata tables.

- file_group:

  A character vector of unique, non-missing stable `file_group_id`
  values. Every value must be an included member of `plan` and all
  values must belong to one structural compatibility set. Numeric
  declaration indices are not accepted.

- compatibility_id:

  Optional single structural compatibility identifier. When `NULL`, the
  identifier is inferred only if all selected groups share exactly one
  set. When supplied, it must equal that set's identifier.

## Value

A lightweight `glc_collection_refinement` object described in **Return
structure**.

## Details

`glc_collection_refine()` is the final, fast step after interactive
metadata narrowing. Build
[`glc_collection_plan()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_plan.md)
once, choose one row of `plan$compatibility_sets`, and filter its member
groups through the typed tables in `plan$groups` and `plan$metadata`.
Pass only the resulting stable file-group ids to this function. Input
order does not affect the result.

Refinement reapplies the stored relationship and device-slot rules and
creates request-sensitive final unit ids. Its `units` table is identical
to a fresh
[`glc_collection_plan()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_plan.md)
call with the parent's original term, variable, dataset, and
standardization request and with `file_group` set to the refined ids.
The restriction-stable `compatibility_id` is retained. No full variable
or metadata tables are rebuilt.

A result with more than one final unit has
`final_selection_required = TRUE` and an unresolved
`"device_slot_allocation"` constraint. Narrow the stable group ids again
and refine again. A deterministic `preferred_unit_id` is reported for
display parity, but it does not override the final-selection gate.

## Validation and conditions

Refinement supports plan schema `"glc-collection-plan"` version
`"1.1.0"` and refinement-input schema
`"glc-collection-refinement-input"` version `"1.0.0"`. It verifies the
compact input fingerprint and checks it against the parent plan's
provenance, request, and group membership. The fingerprint covers only
facts needed for refinement, not the larger normalized metadata
snapshot, so validation does not rehash the complete plan.

All refinement errors inherit from `glcdp_collection_refine_error`. More
specific subclasses are:

- `glcdp_collection_refine_plan` for a value that is not a collection
  plan;

- `glcdp_collection_refine_version` for an unsupported schema or
  version;

- `glcdp_collection_refine_incomplete` and
  `glcdp_collection_refine_tampered` for missing or changed parent
  facts;

- `glcdp_collection_refine_file_group`, `glcdp_collection_refine_empty`,
  and `glcdp_collection_refine_duplicate` for invalid file-group
  selectors;

- `glcdp_collection_refine_unknown_group` and
  `glcdp_collection_refine_excluded_group` for groups outside the
  eligible parent membership;

- `glcdp_collection_refine_compatibility`,
  `glcdp_collection_refine_unknown_compatibility`, and
  `glcdp_collection_refine_compatibility_mismatch` for invalid
  structural identifiers;

- `glcdp_collection_refine_cross_structure` when selected groups span
  more than one structural set; and

- `glcdp_collection_refine_unresolved_identity` when required study,
  participant, or device identities cannot be resolved from the stored
  metadata facts.

## Zero-access assurance

Refinement performs no file or network input/output. It does not call
[`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md),
[`glc_files()`](https://tscnlab.github.io/glc-dp-r/reference/glc_files.md),
[`glc_summary()`](https://tscnlab.github.io/glc-dp-r/reference/glc_summary.md),
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md),
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md),
[`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md),
or metadata-loading and materialization functions. Its assurance records
that the package was not reopened, remote availability was not probed,
and measurement contents were neither transferred nor inspected.
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
and
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
remain authoritative after refinement.

## Return structure

The result is a plain serializable list with class
`glc_collection_refinement`, schema `"glc-collection-refinement"`, and
version `"1.0.0"`. It contains:

- `parent`: `plan_schema`, `plan_version`, and the validated
  refinement-input `fingerprint` linking this result to the retained
  parent plan;

- `provenance`: `package_id`, `repository`, exact `source_revision`, and
  `package_schema_version`;

- `request`: the selected `compatibility_id`, sorted `file_group` ids,
  and the parent's compact `original` request;

- `assurance`: declaration basis plus package, network,
  availability-probe, measurement-transfer, measurement-inspection, and
  final-validation fields;

- `compatibility_id`, `final_selection_required`, and
  `preferred_unit_id`;

- `units`: the same stable final-unit columns documented for
  [`glc_collection_plan()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_plan.md);

- `groups`: `status`, `unit_id`, `compatibility_id`, `dataset_id`,
  integer `file_group`, stable `file_group_id`, `study_id`,
  `participant_id`, `participant_associated`, `study_link_status`,
  `participant_link_status`, `device_id`, `device_link_status`, and
  list-columns `reason_codes` and `messages`; and

- `constraints`: `compatibility_id`, `code`, `message`, and logical
  `resolved`. Its schema is stable even when it has no rows.

All tables and list-columns contain only plain serializable values. The
result contains no package handle, environment, token, cache path, or
temporary path. [`print()`](https://rdrr.io/r/base/print.html) reports
the structural id, final-unit and group counts, final-selection status,
and zero-access assurance, then returns the result invisibly.

## See also

[`glc_collection_plan()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_plan.md)
for the required parent plan,
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
and
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
for runtime validation and collection.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use an existing local, manifest-backed package directory.
# This pattern performs no network request and does not read measurements.
pkg <- glc_open("path/to/manifest-backed-package", quiet = TRUE)
plan <- glc_collection_plan(
  pkg,
  terms = "photopic illuminance",
  variable_scope = "matched"
)

# In an application, filter these ids with plan$groups and plan$metadata.
set <- plan$compatibility_sets[1L, ]
selected_ids <- set$file_group_ids[[1L]]
refined <- glc_collection_refine(
  plan,
  selected_ids,
  compatibility_id = set$compatibility_id[[1L]]
)
refined
} # }
```
