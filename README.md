# glcdp <img src="man/figures/logo.png" align="right" height="139" alt="glcdp package logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/tscnlab/glc-dp-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tscnlab/glc-dp-r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`glcdp` discovers, inspects, downloads, and imports Global Light Commons data
packages. It is designed as infrastructure for packages such as
[LightLogR](https://tscnlab.github.io/LightLogR/) and LightLogWeb while
remaining independent of their analysis interfaces.

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("tscnlab/glc-dp-r")
```

The development version supports stable GLC metadata schemas 1.0.0 and 2.0.0,
experimental reading of the 3.0.0 development schema, immutable registry
revisions, selective downloads, and GitHub-hosted Git LFS objects.

```r
packages <- glcdp::glc_packages()
guidolin <- glcdp::glc_open("tscnlab/guidolin-glee-datasetv2")

glcdp::glc_summary(guidolin)
glcdp::glc_variables(guidolin, primary = TRUE)

collection <- glcdp::glc_read(guidolin, dataset_id = "DS001")
light_data <- glcdp::glc_collect(collection)
```

Remote reads use temporary session storage unless a cache directory is
explicitly supplied. Persistent downloads are made only through
`glc_download()` or an explicit cache directory.

## Interactive data explorer

Install the optional application dependencies and launch the local explorer:

```r
install.packages(c("shiny", "bslib"))
glcdp::glc_explore()
```

The app browses passing registry revisions, summarizes package contents, and
filters participants, devices, datasets, file groups, and source variables.
It builds a small configurable preview before exporting an annotated R script
that downloads and imports the exact selection. Package data remain on the
machine running the app.

## Documentation

The [package website](https://tscnlab.github.io/glc-dp-r/) includes a complete
function reference and workflow articles:

- [Get started with glcdp](https://tscnlab.github.io/glc-dp-r/articles/glcdp.html)
- [Explore and hand off data with the Shiny app](https://tscnlab.github.io/glc-dp-r/articles/glc-data-explorer.html)
- [Discover and inspect data packages](https://tscnlab.github.io/glc-dp-r/articles/discover-and-inspect.html)
- [Import and download data](https://tscnlab.github.io/glc-dp-r/articles/import-and-download.html)

LightLogR-standardized data returned by `glc_collect()` use the dataset id as
`Id`, retain the participant id as `participant_Id`, provide `Datetime` and
`file.name`, and omit internal `.glc_*` provenance columns. The result follows
the conventions used by [LightLogR's analysis and visualization functions](https://tscnlab.github.io/LightLogR/reference/index.html).
