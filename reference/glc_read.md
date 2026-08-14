# Read metadata-described dataset files

Read metadata-described dataset files

## Usage

``` r
glc_read(
  x,
  dataset_id,
  file_group = NULL,
  files = NULL,
  variables = NULL,
  terms = NULL,
  primary_only = FALSE,
  n_max = Inf,
  problems = c("error", "warn"),
  progress = interactive()
)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- dataset_id:

  Dataset id or ids. Use `"all"` explicitly to read every dataset.

- file_group:

  Optional group index or stable id.

- files:

  Optional declared paths, resolved paths, or basenames.

- variables:

  Optional source variable names.

- terms:

  Optional semantic variable terms.

- primary_only:

  Select only declared primary variables.

- n_max:

  Maximum rows read from each file.

- problems:

  Whether metadata mismatches should be errors or warnings.

- progress:

  Show progress while files are imported. Defaults to `TRUE` in
  interactive sessions and `FALSE` otherwise.

## Value

A `glc_data_collection` tibble with one data list-column per file group
and one serializable `factor_contract` list-column. The contract
contains no package handle, token, cache path, or temporary path.

## Details

When variable filters are used, source columns required to construct
datetimes are used internally but omitted unless selected by the
filters. Declared files that are absent from a local package subset are
skipped. When a local package contains fewer datasets or files than
declared, `glc_read()` reports the discrepancy and reads the available
files.

Each returned file-group row includes a plain serializable
`factor_contract` payload for the selected variables. It preserves raw
factor values, effective labels, descriptions, unordered status, schema
version, and a SHA-256 fingerprint.
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
validates this payload against the parsed data before applying any safe
factor-level union. Invalid declarations error with class
`glcdp_factor_contract_invalid`; changed payloads or parsed factors are
rejected later with class `glcdp_factor_contract_tampered`.

## Examples

``` r
if (FALSE) { # interactive()
iztech <- glc_open("tscnlab/melidos-iztech-glc-dataset")
glc_read(
  iztech,
  dataset_id = "MELIDOS_IZTECH_S001",
  file_group = "MELIDOS_IZTECH_S001:17",
  n_max = 10
)
}
```
