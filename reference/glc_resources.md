# Inventory data-package resources

Inventory data-package resources

## Usage

``` r
glc_resources(x)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

## Value

A tibble with one row per declared resource path.

## Examples

``` r
if (FALSE) { # interactive()
iztech <- glc_open("tscnlab/melidos-iztech-glc-dataset")
glc_resources(iztech)
}
```
