# Changelog

## glcdp 0.9.3

- Added
  [`extract_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/extract_metadata.md)
  and
  [`add_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/add_metadata.md)
  to select requested metadata by imported-data identifiers and
  optionally join it onto every observation. Their default
  `file_group_id` link traverses from each file group to its dataset,
  participant, study, and device; dataset-level extraction remains
  available with `by = "Id"`. Extracted summaries retain the input
  dataset’s grouping and grouping columns.
- LightLogR-standardized
  [`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
  output now exposes `file_group_id` and rejects contradictory group
  relationships or multiple device links within a dataset.
- Added
  [`glc_explore()`](https://tscnlab.github.io/glc-dp-r/reference/glc_explore.md),
  a Shiny application for browsing the registry, inspecting package
  contents and hierarchical metadata, filtering datasets, file groups,
  participants, devices, and variables, previewing selections, and
  exporting annotated reproducible R scripts.
- Added a documented Posit Connect deployment entry point and dependency
  manifest workflow.

## glcdp 0.9.1

- Corrected the package logo, removed its exterior background for
  transparent display, and refreshed the pkgdown logo and favicon
  assets.

## glcdp 0.9.0

- Initial package implementation for registry discovery, metadata
  inspection, selective downloads, Git LFS retrieval, and
  metadata-driven data import.
- Added a pkgdown website and workflow vignettes covering discovery,
  metadata inspection, selective import, collection, and reproducible
  downloads.
- [`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
  now reports file-level progress by default in interactive sessions;
  set `progress = FALSE` to suppress it.
- Filtered
  [`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
  calls now use datetime source columns internally without retaining
  them unless the filters explicitly select them.
- LightLogR-standardized
  [`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
  output now maps dataset ids to `Id`, participant ids to
  `participant_Id`, and removes internal `.glc_*` columns.
- Fixed remote loading of small Git blobs and metadata searches through
  nested data-frame fields, resolving the metadata examples in the
  vignettes.
- [`glc_search_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/glc_search_metadata.md)
  can now search metadata values, field paths, or both with the new
  `search_in` argument.
- Unauthenticated ordinary Git file transfers now use immutable
  raw-content URLs, preventing per-file GitHub API rate limits during
  reads and downloads.
- Summaries of local subsets now distinguish available from declared
  datasets, file groups, and files; `glc_read(dataset_id = "all")` skips
  files that are intentionally absent from a local subset and reports
  the discrepancy.
