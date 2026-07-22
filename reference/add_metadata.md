# Add metadata to imported data

Extracts requested metadata with
[`extract_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/extract_metadata.md)
and joins it onto every matching observation in an imported dataset.

## Usage

``` r
add_metadata(
  dataset,
  metadata,
  fields,
  by = "file_group_id",
  resource = NULL,
  overwrite = FALSE
)
```

## Arguments

- dataset:

  A data frame containing imported observations.

- metadata:

  A metadata data frame, a local CSV or TSV path, or a package opened
  with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- fields:

  One or more exact, top-level metadata column names to select.

- by:

  One common identifier column, or a named character mapping from the
  dataset column to the metadata column. The default is
  `"file_group_id"`. Use `"Id"` for explicitly dataset-level extraction,
  or `c(Id = "dataset_internal_id")` for a differently named metadata
  key.

- resource:

  An optional declared resource name when `metadata` is a `glc_package`.
  For file-group or dataset identifiers, omitting `resource` searches
  declared resources connected through the package's file-group,
  dataset, participant, study, and device relationships. Each requested
  field must resolve to exactly one connected resource. For other `by`
  mappings, exactly one declared resource must contain the metadata join
  column and a requested field.

- overwrite:

  Replace existing dataset columns that have the same names as extracted
  metadata fields. The default is `FALSE`.

## Value

`add_metadata()` returns the original dataset with the requested
metadata columns added. Row order, row count, and dplyr grouping are
preserved.

## Examples

``` r
dataset <- tibble::tibble(
  file_group_id = c("DS1:1", "DS1:1", "DS2:1"),
  value = c(1, 2, 3)
)
metadata <- tibble::tibble(
  file_group_id = c("DS1:1", "DS2:1"),
  condition = c("control", "intervention")
)

add_metadata(dataset, metadata, fields = "condition")
#> # A tibble: 3 × 3
#>   file_group_id value condition   
#>   <chr>         <dbl> <chr>       
#> 1 DS1:1             1 control     
#> 2 DS1:1             2 control     
#> 3 DS2:1             3 intervention
```
