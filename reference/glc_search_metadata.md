# Search metadata values or field paths

Search metadata values or field paths

## Usage

``` r
glc_search_metadata(
  x,
  query,
  resources = NULL,
  fields = NULL,
  fixed = TRUE,
  ignore_case = TRUE,
  search_in = c("values", "fields", "both")
)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- query:

  Text or regular expression to search for.

- resources:

  Optional metadata resource names.

- fields:

  Optional exact field names or complete field paths to include.

- fixed:

  Treat `query` as fixed text rather than a regular expression.

- ignore_case:

  Ignore letter case while matching.

- search_in:

  Where to match `query`: scalar metadata `"values"`, complete field
  paths `"fields"`, or `"both"`. The default is `"values"`.

## Value

A tibble of matching scalar metadata values and their field paths.

## Details

Field searches return the same leaf-level rows as value searches. A
field path that contains multiple scalar values therefore produces one
row per value. The `fields` argument can be combined with any
`search_in` mode to restrict which field paths are searched.
