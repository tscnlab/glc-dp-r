# Load metadata resources

Loads core metadata by default. Additional descriptor resources can be
requested explicitly by name.

## Usage

``` r
glc_metadata(x, resources = NULL)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- resources:

  Optional resource names. The default selects declared core metadata
  resources.

## Value

A named list with one element per requested resource. Tabular resources
are returned as tibbles; directory resources contain named sub-lists.
