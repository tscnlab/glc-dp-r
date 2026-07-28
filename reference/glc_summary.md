# Summarize a Global Light Commons data package

Summarize a Global Light Commons data package

## Usage

``` r
glc_summary(x)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

## Value

A one-row `glc_summary` tibble. For local packages, declared and locally
available dataset, file-group, and file counts are reported separately.

## Examples

``` r
if (FALSE) { # interactive()
iztech <- glc_open("tscnlab/melidos-iztech-glc-dataset")
glc_summary(iztech)
}
```
