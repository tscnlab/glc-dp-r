# Search registered data packages

Search registered data packages

## Usage

``` r
glc_search_packages(
  query = NULL,
  packages = glc_packages(),
  status = NULL,
  has_pass = NULL
)
```

## Arguments

- query:

  Optional fixed, case-insensitive text searched in package ids and
  repository names.

- packages:

  A registry returned by
  [`glc_packages()`](https://tscnlab.github.io/glc-dp-r/reference/glc_packages.md).

- status:

  Optional current validation status or statuses.

- has_pass:

  Optional logical value selecting packages with or without a recorded
  passing revision.

## Value

A filtered `glc_registry` tibble.
