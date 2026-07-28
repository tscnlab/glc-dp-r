# glcdp

`glcdp` discovers, inspects, downloads, and imports Global Light Commons
data packages. It is designed as infrastructure for packages such as
[LightLogR](https://tscnlab.github.io/LightLogR/) and LightLogWeb while
remaining independent of their analysis interfaces.

## Installation

Install the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("tscnlab/glc-dp-r")
```

The development version targets GLC schema 3.0.2 as its current default,
including metadata-driven column types, factor levels in schema-declared
order, and per-file encodings. Schemas 3.0.0 and 3.0.1 remain compatible
stable predecessors; schemas 1.0.0 and 2.0.0 have barebones legacy
support. The package also supports immutable registry revisions,
selective downloads, and GitHub-hosted Git LFS objects.

``` r

packages <- glcdp::glc_packages()
melidos <- glcdp::glc_open("tscnlab/melidos-iztech-glc-dataset")

glcdp::glc_summary(melidos)
datasets <- glcdp::glc_datasets(melidos)
files <- glcdp::glc_files(melidos)

first <- files[1, ]
collection <- glcdp::glc_read(
  melidos,
  dataset_id = first$dataset_id,
  file_group = first$file_group_id
)
light_data <- glcdp::glc_collect(collection)
```

Remote reads use temporary session storage unless a cache directory is
explicitly supplied. Persistent downloads are made only through
[`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md)
or an explicit cache directory.

## Interactive data explorer

Install the optional application dependencies and launch the local
explorer:

``` r

install.packages(c("shiny", "bslib"))
glcdp::glc_explore()
```

The app browses passing registry revisions, summarizes package contents,
and filters participants, devices, datasets, file groups, semantic
terms, and source variables. The completed summary can start the larger
contents load in place and reports its progress, completion, or retry
action centrally. Repeated participant-specific file groups can be
narrowed by device, wearing position, modality, role, state, contained
variable, or semantic term. Numeric participant characteristics use
range filters, and the metadata hierarchy loads complete records
incrementally while the table view retains full paging. Repeated
metadata fields are folded with their record counts, and large
file-group, variable, and handoff inventories use paging and server-side
search choices to keep browser interaction responsive. A page-level busy
indicator remains visible while reactive filtering or rendering is in
progress. It builds a small configurable preview before exporting an
annotated R script that downloads and imports the exact selection.
Package data remain on the machine running the app.

## Documentation

The [package website](https://tscnlab.github.io/glc-dp-r/) includes a
complete function reference and workflow articles:

- [Get started with
  glcdp](https://tscnlab.github.io/glc-dp-r/articles/glcdp.html)
- [Explore and hand off data with the Shiny
  app](https://tscnlab.github.io/glc-dp-r/articles/glc-data-explorer.html)
- [Discover and inspect data
  packages](https://tscnlab.github.io/glc-dp-r/articles/discover-and-inspect.html)
- [Import and download
  data](https://tscnlab.github.io/glc-dp-r/articles/import-and-download.html)

LightLogR-standardized data returned by
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
use the dataset id as `Id`, retain the participant id as
`participant_Id`, provide `Datetime` and `file.name`, and omit internal
`.glc_*` provenance columns. The result follows the conventions used by
[LightLogR’s analysis and visualization
functions](https://tscnlab.github.io/LightLogR/reference/index.html).
