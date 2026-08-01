# Plan declaration-compatible collection units

Build a deterministic, metadata-only plan that partitions matching file
groups into units whose validated declarations are compatible for
collection. No measurement file is read or inspected while the plan is
built.

## Usage

``` r
glc_collection_plan(
  x,
  terms = NULL,
  variable_scope = c("matched", "all", "selected"),
  variables = NULL,
  dataset_id = NULL,
  file_group = NULL,
  standardize = c("lightlogr", "none")
)
```

## Arguments

- x:

  A `glc_package` opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md)
  at an exact, verified revision. A remote package must be at the
  registry's latest passing revision. A local package must be
  manifest-backed, with the same exact revision and
  `registry_verified = true`.

- terms:

  Optional exact canonical semantic-term identifiers. Labels and other
  display text are not identifiers. `terms` is required when
  `variable_scope = "matched"`.

- variable_scope:

  Which declared source variables to plan:

  - `"matched"` selects every variable whose canonical term is one of
    `terms`;

  - `"all"` selects all declared variables in each candidate file group;

  - `"selected"` selects the exact source names supplied in `variables`.

- variables:

  Exact declared source-variable names. This argument is required for
  `variable_scope = "selected"` and must otherwise be `NULL`.

- dataset_id:

  Optional exact dataset identifiers restricting the candidate groups.

- file_group:

  Optional stable file-group identifiers, such as `"DS1:1"`, restricting
  the candidate groups. Numeric group indices are deliberately not
  accepted because they are not stable identifiers.

- standardize:

  Expected collection output convention. `"lightlogr"` plans the columns
  produced by `glc_collect(standardize = "lightlogr")`; `"none"` plans
  the unstandardized provenance columns. This choice changes expected
  output columns and unit identifiers, but not the declaration
  compatibility partition.

## Value

A `glc_collection_plan` object described in **Return tables**.

## Details

`terms` always acts as the file-group discovery predicate. A candidate
group must declare every requested term, while one term may be declared
by more than one variable. With `variable_scope = "matched"`, all
variables carrying any requested term are selected. With `"all"`, all
declared variables are selected after the optional term predicate is
applied. With `"selected"`, `terms` remains an optional, independent
discovery predicate and every requested source name must be declared by
a group. Input order does not affect the result; selected variables
retain declaration order.

`dataset_id` and `file_group` are intersecting restrictions. Omitting a
restriction and explicitly supplying every possible identifier select
the same groups, but deliberately remain different requests and
therefore may produce different unit identifiers.

Included groups are partitioned by the exact selected variable names and
order, declared types, factor values and labels, time zone, ordered
modalities, role, data state, datetime contract, and the package's
validated relationship rules. At most one non-missing device per dataset
is permitted within a unit. Collection-based datetime values are
record-specific and are ignored when comparing otherwise identical
collection-based datetime contracts.

A unit identifier is `"glcu_"` followed by a SHA-256 digest of canonical
UTF-8 text. The material includes the planner schema and version,
package id, repository, exact source revision and package schema, the
normalized request (including restrictions and `standardize`), the
compatibility contract, and sorted stable file-group identifiers. It
never uses R serialized-object bytes. Unit identifiers and table
ordering are therefore reproducible across input row ordering and
supported R versions. They are request- and revision-specific and may
change when the planner schema changes.

## Declaration-only assurance

The planner uses validated descriptor and core metadata associated with
`x`. It does not call
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md),
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md),
[`glc_files()`](https://tscnlab.github.io/glc-dp-r/reference/glc_files.md),
or
[`glc_summary()`](https://tscnlab.github.io/glc-dp-r/reference/glc_summary.md),
request measurement contents, or inspect source rows. Known
declaration-level constraints, including reserved source names that
begin with `.glc_`, are applied before units are formed.

File sizes come only from an explicit supported byte declaration or an
entry already present in the local manifest. The planner never downloads
a file or probes a remote object to discover its size; unavailable sizes
remain `NA`. A unit's `declared_bytes` is the sum of known sizes, and
`declared_bytes_complete` records whether every file size is known.

Compatibility is an assurance about validated declarations, not
downloaded values. Actual columns, parsed classes, factor values,
datetime values, and output-column collisions can only be checked after
reading.
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
remains authoritative for source-file validation, and
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
remains authoritative for final collection compatibility and
standardization.

## Return tables

The result is a plain, serializable list with class
`glc_collection_plan` and these components:

- `plan_schema` and `plan_version`: the top-level schema id
  `"glc-collection-plan"` and its semantic version.

- `provenance`: `package_id`, `repository`, `source_type`, exact
  `source_revision`, `package_schema_version`, `verification`,
  `latest_pass_commit`, `registry_generated_at`, `manifest_version`,
  declaration `metadata_fingerprint`, `planner_schema`, and
  `planner_version`. It contains no package handle, token, cache path,
  or temporary path.

- `request`: normalized `terms`, fixed `term_match = "all"`,
  `term_identifier = "canonical"`, `labels_used_for_matching = FALSE`,
  `variable_scope`, `requested_variables`, `dataset_id`, `file_group`,
  `standardize`, and the sorted union `resolved_variables` from included
  groups.

- `assurance`: `basis`, `actual_data_status`, `final_validation`,
  `measurement_contents_transferred`, `measurement_contents_inspected`,
  and `byte_policy`.

- `preferred_unit_id`: the preferred unit, or `NA_character_` when no
  unit is collectable. Preference is deterministic: most datasets, then
  most file groups, then the lexically smallest unit id.

- `units`: one row per collectable unit. Columns are `unit_id`,
  `preferred`, `dataset_count`, `file_group_count`, `variable_count`,
  `file_count`, `declared_bytes`, `known_file_count`,
  `unknown_file_count`, `declared_bytes_complete`, and the list-column
  `file_group_ids`.

- `groups`: one row per declared file group. Columns are `status`,
  `unit_id`, `dataset_id`, integer declaration index `file_group`,
  stable `file_group_id`, `study_id`, `participant_id`,
  `participant_associated`, `device_id`, `device_location`,
  `device_location_type`, `description`, `format`, `timezone`,
  list-column `modalities`, `modality_other`, `modality_other_type`,
  `role`, `data_state`, `temporal_type`, `temporal_value`,
  `temporal_unit`, `header_row`, list-column `preprocessing`,
  `datetime_source`, `datetime_date`, `datetime_format`,
  `datetime_time`, `datetime_time_format`, and list-columns
  `selected_variables`, `reason_codes`, and `messages`. Excluded groups
  have a missing `unit_id`.

- `variables`: one row per selected variable in an included group.
  Columns are `unit_id`, `dataset_id`, `file_group_id`, `position`,
  `name`, `label`, `description`, `unit`, `calibration`, `type`,
  canonical `term`, `term_name`, `primary`, list-columns
  `factor_values`, `factor_labels`, and `factor_descriptions`, and
  `selection_origin`.

- `read_columns`: one row per selected or automatically required source
  column. Columns are `unit_id`, `dataset_id`, `file_group_id`,
  `position`, `name`, `declared_type`, `origin`, `selected_for_output`,
  `automatic`, and `message`. Datetime source columns needed only for
  parsing are automatic read columns, not requested output variables.

- `output_columns`: one row per expected post-collection column and
  unit. Columns are `unit_id`, `position`, `name`,
  `source_declared_type`, `expected_type`, `origin`, `automatic`,
  `runtime_validation_required`, `collision_validation_required`, and
  `message`. These expectations are still subject to
  [`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
  validation.

- `files`: one row per declared file in included or excluded groups.
  Columns are `status`, `unit_id`, `dataset_id`, `file_group_id`,
  `position`, `declared_path`, `format`, `encoding`, `declared_bytes`,
  and `bytes_known`.

- `compatibility`: one row per unit. Columns are `unit_id`, list-columns
  `selected_names`, `declared_types`, `factor_values`, and
  `factor_labels`, `timezone`, list-column `modalities`, `role`,
  `data_state`, `datetime_source`, `datetime_signature`,
  `datetime_date`, `datetime_format`, `datetime_time`,
  `datetime_time_format`, `collection_values_ignored`,
  `relationship_rule`, `device_rule`, `standardize`, and the fixed
  `standardize_affects_partition = FALSE` assurance.

- `extensions`: one row per group. Columns are `dataset_id`,
  `file_group_id`, and preserved, forward-compatible unknown declaration
  fields in the plain list-column `metadata`.

All tables are tibbles with stable columns, including when they have no
rows. List-columns contain only plain serializable vectors and lists.
[`print()`](https://rdrr.io/r/base/print.html) shows a compact package,
request, unit, byte, preferred-unit, and assurance summary and returns
the plan invisibly.

## Exclusions and errors

Per-group declaration outcomes are returned rather than thrown. Stable
reason codes are `included`, `scope_dataset`, `scope_file_group`,
`reserved_provenance_column`, `term_missing`, `variable_missing`,
`no_declared_files`, `unsupported_format`, `invalid_timezone`, and
`incomplete_datetime`; each has a plain-language message.

Invalid argument types, empty values, duplicates, and inconsistent
`variable_scope`/selector combinations error before planning.
Programmatically useful condition subclasses include
`glcdp_unknown_dataset`, `glcdp_unknown_file_group`,
`glcdp_unknown_term`, `glcdp_unknown_variable`,
`glcdp_collection_plan_revision`, `glcdp_collection_plan_provenance`,
`glcdp_collection_plan_ambiguous_variable`,
`glcdp_collection_plan_relationship`,
`glcdp_collection_plan_serialization`, and `glcdp_collection_plan_id`.
Term labels that are not canonical ids are reported as unknown terms
rather than matched ambiguously.

## See also

[`glc_variables()`](https://tscnlab.github.io/glc-dp-r/reference/glc_variables.md)
for declared selectors,
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
for runtime import validation, and
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
for authoritative final collection.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use an existing local, manifest-backed directory created by glc_download().
# This pattern performs no network request and does not read measurements.
pkg <- glc_open("path/to/manifest-backed-package", quiet = TRUE)
plan <- glc_collection_plan(
  pkg,
  terms = "photopic illuminance",
  variable_scope = "matched"
)
plan
plan$units
plan$groups[, c("file_group_id", "status", "reason_codes")]
} # }
```
