# Inventory datasets

Inventory datasets

## Usage

``` r
glc_datasets(x, dataset_id = NULL)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- dataset_id:

  Optional dataset id or ids.

## Value

A tibble with one row per dataset.

## Examples

``` r
if (FALSE) { # interactive()
iztech <- glc_open("tscnlab/melidos-iztech-glc-dataset")
glc_datasets(iztech)
}
```
