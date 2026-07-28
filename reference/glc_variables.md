# Inventory and search declared variables

Inventory and search declared variables

## Usage

``` r
glc_variables(
  x,
  dataset_id = NULL,
  file_group = NULL,
  term = NULL,
  primary = NULL
)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- dataset_id:

  Optional dataset id or ids.

- file_group:

  Optional group index or stable id.

- term:

  Optional semantic term or terms.

- primary:

  Optional logical filter for primary variables.

## Value

A tibble with one row per declared variable, including its declared type
and factor values, labels, and descriptions.

## Examples

``` r
if (FALSE) { # interactive()
iztech <- glc_open("tscnlab/melidos-iztech-glc-dataset")
glc_variables(
  iztech,
  file_group = "MELIDOS_IZTECH_S001:17",
  primary = TRUE
)
}
```
