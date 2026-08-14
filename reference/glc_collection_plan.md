# Plan declaration-compatible collection units

Build a deterministic plan from validated declarations and normalized
core metadata. The plan separates structural compatibility sets from
final collectable units, and it never reads or inspects measurement
contents.

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

A serializable `glc_collection_plan` object using plan schema
`"glc-collection-plan"` version `"1.2.0"`, as described in **Return
tables**.

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

## Structural compatibility and final units

Included groups are first compared by selected source names and
declaration order, declared types, time zone, ordered modalities, role,
data state, and datetime contract. Collection-based datetime values are
record-specific and are ignored when comparing otherwise identical
collection-based contracts.

Unordered factor declarations can share a structural set when their raw
values have compatible effective labels and descriptions and their
declared order constraints form an acyclic graph. An absent label means
the raw value; no text normalization or semantic inference is performed.
The union order is deterministic and preserves every declared order
constraint. Duplicate raw values, ambiguous labels, conflicting labels
or non-missing descriptions, cyclic order constraints, and unsupported
ordered factors remain blocking. Blocking families retain exact
factor-contract partitions.

Each structural set has a `compatibility_id` beginning with `"glcc_"`.
It is a version 2 SHA-256 digest of length-prefixed canonical UTF-8 text
containing the exact package identity, repository, source revision,
package schema, and the full structural-family contract computed before
`dataset_id` and `file_group` restrictions are applied. It does not
contain current membership, restrictions, metadata facets, or
`standardize`. The identifier therefore stays the same when an unchanged
family is narrowed at the same package revision. Wearing position,
device identity or location, participant characteristics, file
description, site context, and other descriptive metadata do not split
structural sets.

Final units enforce consistent stable file-group relationships and
dataset study and participant relationships. Device identity is
file-group-scoped, so distinct file groups in one dataset may reference
different devices while remaining in one final unit. The active factor
union is recomputed after restrictions.
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
carries the raw declaration contract and
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
validates and applies the same union to actual factors.

A `unit_id` begins with `"glcu_"` and uses version 2 canonical identity.
It hashes the package and revision, normalized request including
restrictions and `standardize`, active structural contract, and sorted
member file-group identifiers. Final unit ids are request- and
membership-sensitive, while structural ids are restriction stable. Both
use canonical UTF-8 text rather than R serialized-object bytes, and
their values and table order are stable under input row reordering.
Version 2 identifiers deliberately differ from earlier identifiers
because factor equivalence and device allocation rules changed.

`compatibility_diagnostics` explains safe unions and blocking
differences. Selected-variable diagnostics describe the current
partition. Non-selected diagnostics disclose what would require
harmonization or block a later expanded variable request without
selecting those variables now. Re-plan an expanded request before
reading or collecting additional variables.

## Declaration-only assurance

The planner uses the validated descriptor and core metadata associated
with `x`. One explicit planning call may load descriptor-declared
resources named `study`, `participants`, `participant_characteristics`,
`datasets`, `devices`, `device_datasheets`, and optional `contributors`
at the exact source revision. For a remote package, loading an uncached
core resource can make an HTTP request. The allowlist is exactly the
value returned internally by `glc_core_resource_names()`.

The planner does not call
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md),
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md),
[`glc_files()`](https://tscnlab.github.io/glc-dp-r/reference/glc_files.md),
[`glc_summary()`](https://tscnlab.github.io/glc-dp-r/reference/glc_summary.md),
or
[`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md).
It never requests a measurement path, probes measurement availability,
or inspects source rows. Known declaration-level constraints, including
reserved source names that begin with `.glc_`, are applied before units
are formed.

File sizes come only from an explicit supported byte declaration or an
entry already present in the local manifest. The planner never downloads
a file or probes a remote object to discover its size; unavailable sizes
remain `NA`. A unit's `declared_bytes` is the sum of known sizes, and
`declared_bytes_complete` records whether every file size is known.

Compatibility is an assurance about validated declarations, not
downloaded values. Actual columns, parsed classes and factor values,
datetime values, and output-column collisions can only be checked after
reading.
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
remains authoritative for source-file validation.
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
validates the preserved raw factor declarations, harmonizes only safe
unions, and remains authoritative for final compatibility and
standardization.

## Missing values and relationship links

Source missingness is retained. Missing scalar metadata stays as a typed
`NA`, and absent repeated metadata stays an empty vector or list.
Literal source values such as `"not applicable"` remain literal values.
The planner does not synthesize a description, instrument, contributor
id, or site id.

Relationship status columns use `"linked"`, `"not_applicable"`,
`"unresolved"`, or `"metadata_unavailable"`. `"not_applicable"` means
that no link applies, such as a dataset not associated with a
participant or a group without a device id. `"unresolved"` means that an
applicable id is missing or does not resolve in loaded metadata.
`"metadata_unavailable"` means that an id is present but its optional
core resource was not declared.

## Recommended interactive workflow

Build one plan as an explicit planning task. Present one reader-oriented
option per row of `compatibility_sets`, then filter `groups` and the
normalized `metadata` tables in memory by stable ids. Do not rebuild the
plan for each participant, device, position, characteristic, or site
filter. Finally pass the narrowed file-group ids to
[`glc_collection_refine()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_refine.md).
A caller should proceed to reading only when the refinement reports one
final unit and `final_selection_required = FALSE`. Keep the parent plan
because the lightweight refinement does not copy its metadata tables.

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
  measurement transfer and inspection flags, `core_metadata_transport`,
  interactive-filter and refinement network flags, and `byte_policy`.

- `preferred_unit_id`: the preferred unit, or `NA_character_` when no
  unit is collectable. Preference is deterministic: most datasets, then
  most file groups, then the lexically smallest unit id.

- `units`: one row per final unit. Columns are `unit_id`,
  `compatibility_id`, `preferred`, `dataset_count`, `file_group_count`,
  `variable_count`, `file_count`, `declared_bytes`, `known_file_count`,
  `unknown_file_count`, `declared_bytes_complete`,
  `harmonization_required`, and list-columns `harmonized_variables`,
  `diagnostic_ids`, and `file_group_ids`.

- `groups`: one row per declared file group. Columns are `status`,
  `unit_id`, `compatibility_id`, `dataset_id`, integer declaration index
  `file_group`, stable `file_group_id`, `study_id`, `participant_id`,
  `participant_associated`, `study_link_status`,
  `participant_link_status`, `device_id`, `device_link_status`,
  `datasheet_id`, `device_location`, `device_location_type`,
  `description`, `instructions`, `instrument_declared`,
  `dataset_timezone`, `dataset_latitude`, `dataset_longitude`, `format`,
  `timezone`, list-column `modalities`, `modality_other`,
  `modality_other_type`, `role`, `data_state`, `temporal_type`,
  `temporal_value`, `temporal_unit`, `header_row`, list-column
  `preprocessing`, `datetime_source`, `datetime_date`,
  `datetime_format`, `datetime_time`, `datetime_time_format`, and
  list-columns `selected_variables`, `reason_codes`, and `messages`.
  Excluded groups have missing `unit_id` and `compatibility_id` values.

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
  `selected_names`, `declared_types`, `factor_values`, `factor_labels`,
  and `factor_descriptions`, `harmonization_required`, list-columns
  `harmonized_variables` and `diagnostic_ids`, `timezone`, list-column
  `modalities`, `role`, `data_state`, `datetime_source`,
  `datetime_signature`, `datetime_date`, `datetime_format`,
  `datetime_time`, `datetime_time_format`, `collection_values_ignored`,
  `relationship_rule`, `device_rule`, `standardize`, and the fixed
  `standardize_affects_partition = FALSE` assurance.

- `extensions`: one row per group. Columns are `dataset_id`,
  `file_group_id`, and preserved, forward-compatible unknown declaration
  fields in the plain list-column `metadata`.

- `compatibility_sets`: one row per structural set. Columns are
  `compatibility_id`, dataset, file-group, variable, and file counts;
  byte summaries; `harmonization_required`; list-columns
  `harmonized_variables` and `diagnostic_ids`; `final_unit_count`;
  `final_selection_required`; list-columns `constraint_codes`,
  `constraint_messages`, `file_group_ids`, and `final_unit_ids`; the
  selected-name, declared-type, factor, timezone, modality, role,
  data-state, and datetime contract columns also present in
  `compatibility`; `relationship_rule`; `device_rule`; and the fixed
  `standardize_affects_structure = FALSE` assurance.

- `compatibility_diagnostics`: one row per stable diagnostic. Columns
  are `diagnostic_id`, `selection_scope`, `classification`, `code`,
  `variable_name`, `applies_to_current_plan`, list-column
  `prospective_scopes`, `message`, affected group, structure, and unit
  counts, and list-columns `file_group_ids`, `compatibility_ids`,
  `unit_ids`, `union_values`, `union_labels`, and `union_descriptions`.

- `compatibility_diagnostic_groups`: one row per affected diagnostic and
  file-group pair. Columns are `diagnostic_id`, `dataset_id`, stable
  `file_group_id`, `current_status`, `compatibility_id`, `unit_id`,
  `variable_present`, `selected_by_request`, `declaration_position`,
  `declared_type`, and factor value, label, and description
  list-columns.

- `metadata`: a normalized typed core-metadata snapshot described below.

- `refinement_input`: a compact, serializable input used and validated
  by
  [`glc_collection_refine()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_refine.md).
  It contains `schema`, `version`, `canonicalization`,
  `digest_algorithm`, compact `provenance` and `request` lists, a
  `membership` table, plain-list structural `contracts`, per-group
  declarations and relationship facts, and a SHA-256 `fingerprint`.
  Consumers should not modify or reconstruct it.

All tables are tibbles with stable columns, including when they have no
rows. List-columns contain only plain serializable vectors and lists.
[`print()`](https://rdrr.io/r/base/print.html) shows a compact package,
request, unit, diagnostic, harmonization, byte, preferred-unit, and
assurance summary and returns the plan invisibly.

## Normalized metadata tables

`metadata` has schema `"glc-package-metadata"`, version `"1.0.0"`, and
the following stable tables. All identifier joins are explicit; there is
no `sites` table because the supported source schema has no stable site
id.

- `resource_status`: `resource`, `declared`, `status`, and
  `record_count`. Status is `"not_declared"`, `"loaded_empty"`, or
  `"loaded"`.

- `studies`: `study_id`, `schema_version`, `title`, `short_description`,
  `preregistration`, `registration`, `ethics`, `sample`, `intervention`,
  `setting`, `geographical_location`, `study_type`, and list-columns
  `funding_sources`, `keywords`, and `dataset_ids`.

- `study_groups`: `study_id`, `position`, `name`, `description`, `size`,
  and list-columns `inclusion`, `exclusion`, and `dataset_ids`.

- `study_contributors`: `study_id`, `position`, `full_name`, list-column
  `roles`, `email`, `orcid`, `institution_name`, `institution_city`, and
  `institution_country`.

- `contributors`: `contributor_id`, deterministic row key `position`,
  `full_name`, list-column `roles`, `email`, `orcid`,
  `institution_name`, `institution_city`, and `institution_country`. A
  missing source id remains typed `NA`; no id is synthesized.

- `datasets`: `dataset_id`, `schema_version`, `study_id`,
  `study_link_status`, `participant_id`, `participant_associated`,
  `participant_link_status`, `timezone`, numeric `latitude` and
  `longitude`, `file_group_count`, `file_count`, and list-columns
  `modalities`, `device_ids`, and `primary_variables`.

- `dataset_terms`: `dataset_id`, `position`, canonical `term`, and
  `label`.

- `participants`: `participant_id`, numeric `age`, `sex`, and `gender`.

- `participant_characteristics`: `participant_id`,
  `participant_link_status`, `characteristic_position`,
  `value_position`, `name`, typed scalar list-column `value`,
  `value_type`, `unit`, and `description`.

- `devices`: `device_id`, `schema_version`, `manufacturer`, `model`,
  `serial_number`, `calibration_date`, `firmware_version`,
  `datasheet_id`, and `datasheet_link_status`.

- `device_sensors`: `device_id`, `position`, `sensor_type`,
  `datasheet_id`, and `datasheet_link_status`.

- `datasheets`: `datasheet_id`, `schema_version`, `datasheet_version`,
  `manufacturer`, `type`, list-column `modalities`, `modality_other`,
  `model`, `calibration_interval`, `calibration_method`,
  `calibration_accuracy`, `calibration_range`, `calibration_notes`,
  typed list-column `calibration_spectral_sensitivity`,
  `calibration_linearity`, and `calibration_directional_response`.

- `datasheet_parameters`: `datasheet_id`, `position`, `name`, typed
  scalar list-column `value`, `value_type`, `unit`, and `description`.

- `datasheet_channels`: `datasheet_id`, `position`, integer
  `channel_number`, `name`, `description`, and `unit`.

- `instruments`: `dataset_id`, `file_group_id`, `instrument_type`,
  `instrument_name`, `collection_method`, `recorded_by`, and
  `software_name`.

- `file_group_variables`: all declared variables for every included
  group, independent of `variable_scope`. Columns are `dataset_id`,
  `file_group_id`, `declaration_position`, `selected_by_request`,
  `selected_for_output`, `selection_origin`, `name`, `label`,
  `description`, `unit`, `calibration`, `type`, canonical `term`,
  `term_name`, `primary`, and `factor_level_count`.

- `file_group_factor_levels`: `dataset_id`, `file_group_id`,
  `variable_position`, `variable_name`, `level_position`, `value`,
  `label`, and `description`.

- `extensions`: `resource`, `entity_type`, `entity_id`, `parent_id`,
  `position`, and plain list-column `metadata` for forward-compatible
  unknown fields. Standard fields never require parsing this column.

## Exclusions and errors

Per-group declaration outcomes are returned rather than thrown. Stable
reason codes are `included`, `scope_dataset`, `scope_file_group`,
`reserved_provenance_column`, `term_missing`, `variable_missing`,
`invalid_factor_contract`, `no_declared_files`, `unsupported_format`,
`invalid_timezone`, and `incomplete_datetime`; each has a plain-language
message.

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

[`glc_collection_refine()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_refine.md)
for fast in-memory narrowing,
[`glc_variables()`](https://tscnlab.github.io/glc-dp-r/reference/glc_variables.md)
for declared selectors,
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
for runtime import validation, and
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
for authoritative final collection.

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
plan$compatibility_sets[, c(
  "compatibility_id", "file_group_count", "final_unit_count",
  "final_selection_required"
)]

# Filter plan$groups and plan$metadata in memory, then refine exact ids.
set_id <- plan$compatibility_sets$compatibility_id[[1L]]
selected_ids <- plan$compatibility_sets$file_group_ids[[1L]]
refined <- glc_collection_refine(plan, selected_ids, set_id)
refined
} # }
```
