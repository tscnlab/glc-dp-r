# glcdp

`glcdp` discovers, inspects, downloads, and imports Global Light Commons data
packages. It is designed as infrastructure for packages such as LightLogR and
LightLogWeb while remaining independent of their analysis interfaces.

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
