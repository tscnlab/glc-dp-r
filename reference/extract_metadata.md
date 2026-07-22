# Extract metadata for imported data

Selects requested metadata fields for the identifiers represented in an
imported dataset. By default, the result contains one row per unique
file group and can be joined back with
[`add_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/add_metadata.md).

## Usage

``` r
extract_metadata(
  dataset,
  metadata,
  fields,
  by = "file_group_id",
  resource = NULL
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

## Value

`extract_metadata()` returns a tibble with one row per unique value of
`by` in first-occurrence order. The dataset's dplyr grouping columns and
grouping are retained, with the `by` column added when it is not already
a grouping column. With the default `by`, this is one row per file
group.

## Details

Identifiers are compared as character values, while the identifier
column in the returned table retains its original class. Metadata-only
identifiers are ignored. If `metadata` is a `glc_package`, the default
file-group link may assemble fields from the linked dataset,
participant, study, and device records. Dataset-level fields therefore
repeat across file groups. Missing participant, study, or device links
are retained as missing values with a warning. Use `by = "Id"` for one
row per dataset; device fields then error when a dataset is linked to
multiple devices.

For a grouped dataset, the grouping columns are placed before the
extraction key and the original dplyr grouping (including its `.drop`
setting) is retained. Each extraction-key value must map to exactly one
combination of grouping-column values.

If only some input identifiers match, unmatched identifiers are retained
with missing metadata and a warning is issued. If only some fields
exist, the missing fields are omitted and a warning is issued.

Custom metadata are best stored in a declared data-package resource, for
example `data/metadata.csv`, rather than discovered from the working
directory or neighboring files.

## Examples

``` r
dataset <- tibble::tibble(
  Id = c("DS1", "DS1", "DS2"),
  file_group_id = c("DS1:1", "DS1:1", "DS2:1"),
  value = c(1, 2, 3)
) |>
  dplyr::group_by(Id)
metadata <- tibble::tibble(
  file_group_id = c("DS1:1", "DS2:1"),
  condition = c("control", "intervention")
)

extract_metadata(dataset, metadata, fields = "condition")
#> # A tibble: 2 × 3
#> # Groups:   Id [2]
#>   Id    file_group_id condition   
#>   <chr> <chr>         <chr>       
#> 1 DS1   DS1:1         control     
#> 2 DS2   DS2:1         intervention

if (FALSE) { # \dontrun{
package <- glc_open("owner/repository")
imported <- glc_read(package, dataset_id = "DS1") |>
  glc_collect()
extract_metadata(
  imported,
  package,
  fields = c("participant_age", "study_title", "device_model")
)
extract_metadata(imported, package, "dataset_timezone", by = "Id")
} # }
```
