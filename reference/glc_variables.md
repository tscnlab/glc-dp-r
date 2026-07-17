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

A tibble with one row per declared variable.
