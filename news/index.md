# Changelog

## glcdp 1.0.0

- Promoted schema 3.0.2 to the current default and primary stable import
  contract, while retaining 3.0.0 and 3.0.1 as compatible stable
  predecessors. Schema 3.0.2 is a corrective patch that requires
  `datasheet_channel` to contain at least one entry when present; its
  metadata-driven data import contract is unchanged. Schema 1.0.0 and
  2.0.0 remain available as barebones legacy compatibility paths.
- Declared variable types and factor levels in schema-declared order now
  drive import, including boolean `true`/`false` and `1`/`0`
  representations. Variable and factor-level descriptions are exposed in
  [`glc_variables()`](https://tscnlab.github.io/glc-dp-r/reference/glc_variables.md),
  and per-file encodings declared by a file group are applied to the
  corresponding files.
  [`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
  rejects groups whose factor labels or level order differ.
- Added a scheduled and manually dispatchable live schema 3.0.2
  integration check against the validated IZTECH package. It covers
  exact Explorer inventories, metadata resources, factor and integer
  imports, selection, download, and local reopen behavior.
- Metadata summaries now load tabular core resources such as
  `participants.csv`, and LightLogR standardization safely reconciles
  schema-declared source `Id`, `Datetime`, and `file.name` columns.
- Collection-datetime groups can now be combined across participants
  when their formats agree; the participant-specific timestamp values
  are no longer mistaken for different datetime schemas in either
  [`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
  or the Explorer.
- Remote file metadata, full inventories, and package summaries are now
  cached for immutable revisions. Small Git blobs are inspected
  concurrently for LFS pointers, and
  [`glc_summary()`](https://tscnlab.github.io/glc-dp-r/reference/glc_summary.md)
  counts variables directly from the normalized model instead of
  constructing the complete variable inventory.
- [`glc_explore()`](https://tscnlab.github.io/glc-dp-r/reference/glc_explore.md)
  now accepts IDE-provided Shiny viewer functions through
  `shiny.launch.browser`. A central, accessible status banner describes
  registry, package, summary, contents, selection, and preview loading
  work. Package contents can be loaded directly from the completed
  summary without leaving that view; completion offers a direct handoff
  to the loaded contents, and failures can be retried in place.
- Explorer file groups can now be discovered by device, wearing position
  and type, modality, role, data state, contained source variables, and
  semantic terms in both Package contents and Select & hand off. These
  field filters narrow participant-specific repeated groups before exact
  group selection. The complete filtered result, rather than only its
  current page, can be transferred from Package contents into a
  preseeded handoff.
- Numeric participant characteristics use inclusive range sliders. The
  metadata hierarchy loads complete records incrementally, keeps table
  paging independent, labels participant-characteristic records with
  participant and characteristic names, and presents singular flat child
  objects inline without hiding genuinely repeatable record collections.
  Repeated leaf fields, such as study-group inclusion, exclusion, and
  dataset lists, are folded with record counts; compact label/value
  grids remove the former wide spacing. Dense records show an in-place
  loading message and progress indicator while their values render.
  Metadata results are no longer truncated after 500 values.
- Large Explorer inventories now remain responsive by paging File
  groups, Variables, and included handoff groups at 100 rows,
  abbreviating long selection-summary lists, deferring metadata
  hierarchy indexing until its view is opened, and serving large
  selectize choice sets from the Shiny session. A page-level busy pulse
  and output spinners provide immediate feedback during remaining
  reactive work.
- Package contents now opens in the order Metadata, Datasets, Variables,
  and File groups. Metadata field labels stay on one aligned line,
  record nodes use a domain-neutral icon, and discovery sidebars omit
  the redundant Filters heading. File-group compatibility is summarized
  by a compact green or orange action above the filters; incompatible
  selections open focused refinement guidance instead of occupying the
  result card. The completed package-summary action opens Package
  contents directly on Metadata.
- Explorer handoffs can now filter on schema semantic terms as well as
  source variable names. Variable filters automatically retain a
  collectable subset of file groups and report included and excluded
  counts instead of emitting one incompatibility error for every
  excluded group. The former Recommended action is now **Primary**,
  reflecting schema-declared primary variables and falling back to all
  variables when none are declared.
- Select & hand off now starts with an explicit package-and-metadata or
  measurement-data choice. Metadata-only scripts pin the validated
  revision, download selected core resources, and load no measurement
  files. Data handoffs now use one full-width, seven-step wizard whose
  visible tabs cover package/metadata, file groups/datasets,
  participants/devices, variables/terms/rows, review, preview, and R
  export. Each step separates controls from a stable information column,
  fills the available window height, and uses equal-width Back/Continue
  navigation. Large dataset and exact-group selections scroll inside
  bounded inputs instead of stretching the card. Downstream filters
  temporarily narrow rather than erase the dataset and exact-file-group
  baseline selected in Step 2. Preview reads two files by default, with
  independent file and row limits, while an optional maximum rows per
  file is retained in the exported
  [`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
  call. Review now presents its selection and included-group tables as
  an accordion, with the selection open initially. Preview results sit
  directly below their build action, export scripts sit directly below
  their download action, and step content scrolls independently so
  Back/Continue controls remain visible. Generated data scripts clean up
  their temporary handoff settings, leaving only `local_package` and
  `glc_data`.

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
