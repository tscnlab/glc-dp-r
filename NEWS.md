# glcdp 0.90.0

* Initial package implementation for registry discovery, metadata inspection,
  selective downloads, Git LFS retrieval, and metadata-driven data import.
* Added a pkgdown website and workflow vignettes covering discovery, metadata
  inspection, selective import, collection, and reproducible downloads.
* `glc_read()` now reports file-level progress by default in interactive
  sessions; set `progress = FALSE` to suppress it.
* Filtered `glc_read()` calls now use datetime source columns internally
  without retaining them unless the filters explicitly select them.
* LightLogR-standardized `glc_collect()` output now maps dataset ids to `Id`,
  participant ids to `participant_Id`, and removes internal `.glc_*` columns.
* Fixed remote loading of small Git blobs and metadata searches through nested
  data-frame fields, resolving the metadata examples in the vignettes.
* `glc_search_metadata()` can now search metadata values, field paths, or both
  with the new `search_in` argument.
* Unauthenticated ordinary Git file transfers now use immutable raw-content
  URLs, preventing per-file GitHub API rate limits during reads and downloads.
* Summaries of local subsets now distinguish available from declared datasets,
  file groups, and files; `glc_read(dataset_id = "all")` skips files that are
  intentionally absent from a local subset and reports the discrepancy.
