# Inventory declared data files

Inventory declared data files

## Usage

``` r
glc_files(
  x,
  dataset_id = NULL,
  file_group = NULL,
  role = NULL,
  modality = NULL,
  available = NULL
)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- dataset_id:

  Optional dataset id or ids.

- file_group:

  Optional group index or stable `dataset:group` id.

- role:

  Optional file-group role.

- modality:

  Optional modality.

- available:

  Optional logical filter for file availability.

## Value

A tibble with one row per concrete declared file.
