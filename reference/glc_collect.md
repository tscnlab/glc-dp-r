# Collect compatible file groups

Explicitly combines file-group tibbles after checking their columns,
types, time zones, modalities, roles, data states, and relationship
consistency. Multiple non-missing device links within one dataset are
rejected.

## Usage

``` r
glc_collect(x, standardize = c("lightlogr", "none"))
```

## Arguments

- x:

  A collection returned by
  [`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md).

- standardize:

  Either `"lightlogr"` to add the conventional `Id`, `file_group_id`,
  `participant_Id`, `Datetime`, and `file.name` columns and remove
  internal `.glc_*` provenance columns, or `"none"` to retain source and
  provenance columns unchanged.

## Value

A combined tibble. In LightLogR-standardized output, `Id` contains the
dataset id, `file_group_id` identifies the source file group,
`participant_Id` contains the participant id, and the result is grouped
by `Id`.
