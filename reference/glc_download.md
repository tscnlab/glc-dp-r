# Download package metadata or data

Downloads only descriptor-declared and dataset-referenced content,
preserves repository-relative paths, and writes a reproducibility
manifest.

## Usage

``` r
glc_download(
  x,
  dest_dir,
  include = c("metadata", "data", "all"),
  dataset_id = NULL,
  file_group = NULL,
  resources = NULL,
  files = NULL,
  overwrite = FALSE
)
```

## Arguments

- x:

  A package opened with
  [`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md).

- dest_dir:

  Destination directory.

- include:

  One of `"metadata"`, `"data"`, or `"all"`. Metadata is the safe
  default.

- dataset_id:

  Optional dataset id selection for data downloads.

- file_group:

  Optional file-group selection.

- resources:

  Optional descriptor resource names.

- files:

  Optional exact paths, declared paths, or basenames.

- overwrite:

  Whether existing files may be replaced.

## Value

A tibble recording downloaded paths, storage, size, and hashes.
