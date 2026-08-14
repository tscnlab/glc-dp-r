# Collect compatible file groups

Explicitly combines file-group tibbles after checking their columns,
types, factor contracts, time zones, modalities, roles, data states, and
relationship consistency. Compatible unordered factor declarations are
harmonized with the same deterministic union used by
[`glc_collection_plan()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collection_plan.md).
Conflicting value, label, description, or order mappings remain
blocking. Distinct stable file groups in one dataset may reference
different devices because device identity is resolved by
`file_group_id`.

## Usage

``` r
glc_collect(x, standardize = c("lightlogr", "none"))
```

## Arguments

- x:

  A collection returned by
  [`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md).

- standardize:

  Either `"lightlogr"` to add the conventional `Id`, `file_group_id`,
  `participant_Id`, `Datetime`, and `file.name` columns and remove
  internal `.glc_*` provenance columns, or `"none"` to retain source and
  provenance columns unchanged.

## Value

A combined tibble. In LightLogR-standardized output, `Id` contains the
dataset id, `file_group_id` identifies the source file group,
`participant_Id` contains the participant id, an existing source
`file.name` column is retained, and the result is grouped by `Id`.

## Details

Collections returned by current
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
carry fingerprinted raw factor declarations. `glc_collect()` first
verifies those facts against each parsed factor, computes a safe active
union, and recasts the factors before binding. Invalid, changed, or
conflicting contracts use condition classes
`glcdp_factor_contract_invalid`, `glcdp_factor_contract_tampered`, or
`glcdp_factor_harmonization_conflict`. The latter also inherits from
`glcdp_incompatible_collection`. Legacy `glc_data_collection` objects
without a factor-contract payload retain strict exact factor-level
comparison.

A repeated `file_group_id` must still have one consistent dataset,
study, participant, and device relationship. Each dataset must retain
consistent study and participant relationships. These runtime checks are
not weakened by file-group-scoped device identity.

## Examples

``` r
if (FALSE) { # interactive()
iztech <- glc_open("tscnlab/melidos-iztech-glc-dataset")
collection <- glc_read(
  iztech,
  dataset_id = "MELIDOS_IZTECH_S001",
  file_group = "MELIDOS_IZTECH_S001:17",
  n_max = 10
)
glc_collect(collection)
}
```
