# Import and download data

This article covers two related workflows:

- import selected files directly into R with
  [`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md);
  and
- create a persistent, reproducible local subset with
  [`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md).

Remote examples run on the pkgdown website but remain unevaluated in
ordinary package and CRAN builds. Set `GLCDP_SKIP_LIVE=true` to request
an offline website build.

``` r

library(glcdp)
```

``` r

x <- glc_open("tscnlab/guidolin-glee-datasetv2")
```

## Select before importing

Inspect datasets, file groups, and variables first. This avoids
transferring unneeded data and gives you the stable ids and source names
used by the read selectors.

``` r

glc_datasets(x)
#> # A tibble: 2 × 13
#>   dataset_id schema_version study_id participant_id participant_associated
#>   <chr>      <chr>          <chr>    <chr>          <lgl>                 
#> 1 DS001      2.0.0          CG2024   201            TRUE                  
#> 2 DS002      2.0.0          CG2024   P001           TRUE                  
#> # ℹ 8 more variables: timezone <chr>, latitude <dbl>, longitude <dbl>,
#> #   file_group_count <int>, file_count <int>, modalities <list>,
#> #   device_ids <list>, primary_variables <list>
glc_files(x, dataset_id = "DS001")
#> # A tibble: 1 × 19
#>   dataset_id file_group file_group_id participant_id study_id path              
#>   <chr>           <int> <chr>         <chr>          <chr>    <chr>             
#> 1 DS001               1 DS001:1       201            CG2024   data/datasets/201…
#> # ℹ 13 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, role <chr>, data_state <chr>, modalities <list>,
#> #   device_id <chr>, storage <chr>, expected_bytes <dbl>, lfs_oid <chr>,
#> #   blob_sha <chr>, available <lgl>
glc_variables(x, dataset_id = "DS001")
#> # A tibble: 33 × 13
#>    dataset_id file_group file_group_id name    label unit  type  term  term_name
#>    <chr>           <int> <chr>         <chr>   <chr> <chr> <chr> <chr> <chr>    
#>  1 DS001               1 DS001:1       DATE/T… Time… N/A   guess other DATE/TIME
#>  2 DS001               1 DS001:1       MS      Unkn… Unkn… guess other MS       
#>  3 DS001               1 DS001:1       EVENT   Occu… 0 or… guess other EVENT    
#>  4 DS001               1 DS001:1       TEMPER… Skin… °C    guess other TEMPERAT…
#>  5 DS001               1 DS001:1       EXT TE… envi… °C    guess other EXT. TEM…
#>  6 DS001               1 DS001:1       ORIENT… Unkn… Unkn… guess other ORIENTAT…
#>  7 DS001               1 DS001:1       PIM     Acce… Unkn… guess other PIM      
#>  8 DS001               1 DS001:1       TAT     Acce… Unkn… guess other TAT      
#>  9 DS001               1 DS001:1       ZCM     Acce… Unkn… guess other ZCM      
#> 10 DS001               1 DS001:1       PIMn    Norm… Unkn… guess other PIMn     
#> # ℹ 23 more rows
#> # ℹ 4 more variables: calibration <chr>, primary <lgl>, factor_values <list>,
#> #   factor_labels <list>
```

`dataset_id` is required by
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md).
Use `dataset_id = "all"` only when you deliberately want every dataset.

``` r

collection <- glc_read(x, dataset_id = "DS001")
collection
#> <GLC data collection>
#> File groups: 1
#> Rows: 60043
#> # A tibble: 1 × 17
#>   dataset_id file_group file_group_id study_id participant_id device_id
#>   <chr>           <int> <chr>         <chr>    <chr>          <chr>    
#> 1 DS001               1 DS001:1       CG2024   201            D001     
#> # ℹ 11 more variables: modalities <list>, role <chr>, data_state <chr>,
#> #   timezone <chr>, datetime_source <chr>, datetime_date <chr>,
#> #   datetime_format <chr>, datetime_time <chr>, datetime_time_format <chr>,
#> #   primary_variables <list>, files <list>
```

Interactive calls show progress across the selected files by default.
Use `progress = FALSE` to suppress the indicator when a script or
application provides its own progress reporting.

Narrow the import with any combination of file-group ids or indices,
file paths or basenames, source variable names, and semantic terms:

``` r

file_inventory <- glc_files(x, dataset_id = "DS001")
first_file <- file_inventory[1, ]
first_file
#> # A tibble: 1 × 19
#>   dataset_id file_group file_group_id participant_id study_id path              
#>   <chr>           <int> <chr>         <chr>          <chr>    <chr>             
#> 1 DS001               1 DS001:1       201            CG2024   data/datasets/201…
#> # ℹ 13 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, role <chr>, data_state <chr>, modalities <list>,
#> #   device_id <chr>, storage <chr>, expected_bytes <dbl>, lfs_oid <chr>,
#> #   blob_sha <chr>, available <lgl>
file_group <- first_file$file_group_id[[1]]
file_group
#> [1] "DS001:1"
files <- basename(first_file$path[[1]])
files
#> [1] "201_actlumus_Log_1020_20230821094227441.txt"

selected <- glc_read(
  x,
  dataset_id = "DS001",
  file_group = file_group,
  files = files,
  variables = c("LIGHT"),
  n_max = 1000
)
selected$data
#> [[1]]
#> # A tibble: 1,000 × 6
#>    LIGHT .glc_dataset_id .glc_file_group .glc_participant_id .glc_source_file   
#>    <dbl> <chr>           <chr>           <chr>               <chr>              
#>  1  809. DS001           DS001:1         201                 data/datasets/201_…
#>  2  806. DS001           DS001:1         201                 data/datasets/201_…
#>  3  738. DS001           DS001:1         201                 data/datasets/201_…
#>  4  736. DS001           DS001:1         201                 data/datasets/201_…
#>  5  716. DS001           DS001:1         201                 data/datasets/201_…
#>  6  736. DS001           DS001:1         201                 data/datasets/201_…
#>  7  753. DS001           DS001:1         201                 data/datasets/201_…
#>  8  719. DS001           DS001:1         201                 data/datasets/201_…
#>  9  713. DS001           DS001:1         201                 data/datasets/201_…
#> 10  771. DS001           DS001:1         201                 data/datasets/201_…
#> # ℹ 990 more rows
#> # ℹ 1 more variable: .glc_datetime <dttm>
```

When `variables` or `terms` are selected, the imported tables contain
only the matching source variables. `glcdp` still uses any required date
or time columns internally to construct `.glc_datetime`, but does not
retain them unless the filters select them. Set `primary_only = TRUE` to
select all declared primary variables.

``` r

primary <- glc_read(
  x,
  dataset_id = "DS001",
  primary_only = TRUE
)
```

## Understand the imported collection

A `glc_data_collection` has one row per file group. Descriptive columns
record the dataset, participant, device, modality, role, data state,
time zone, datetime specification, and source files. The imported table
is in the `data` list-column.

``` r

collection[, setdiff(names(collection), "data")]
#> <GLC data collection>
#> File groups: 1
#> Warning: Unknown or uninitialised column: `data`.
#> Rows: 0
#> # A tibble: 1 × 17
#>   dataset_id file_group file_group_id study_id participant_id device_id
#>   <chr>           <int> <chr>         <chr>    <chr>          <chr>    
#> 1 DS001               1 DS001:1       CG2024   201            D001     
#> # ℹ 11 more variables: modalities <list>, role <chr>, data_state <chr>,
#> #   timezone <chr>, datetime_source <chr>, datetime_date <chr>,
#> #   datetime_format <chr>, datetime_time <chr>, datetime_time_format <chr>,
#> #   primary_variables <list>, files <list>
collection$data[[1]]
#> # A tibble: 60,043 × 38
#>    `DATE/TIME`    MS EVENT TEMPERATURE `EXT TEMPERATURE` ORIENTATION   PIM  PIMn
#>    <chr>       <dbl> <dbl>       <dbl>             <dbl>       <dbl> <dbl> <dbl>
#>  1 14/08/2023…     0     0        28.9                 0          16     0   0  
#>  2 14/08/2023…     0     0        28.9                 0          16     0   0  
#>  3 14/08/2023…     0     0        28.8                 0          16     6   0.6
#>  4 14/08/2023…     0     0        28.8                 0          16     0   0  
#>  5 14/08/2023…     0     0        28.8                 0          16     0   0  
#>  6 14/08/2023…     0     0        28.7                 0          16     0   0  
#>  7 14/08/2023…     0     0        28.7                 0          16     0   0  
#>  8 14/08/2023…     0     0        28.7                 0          16     0   0  
#>  9 14/08/2023…     0     0        28.6                 0          16     0   0  
#> 10 14/08/2023…     0     0        28.6                 0          16     0   0  
#> # ℹ 60,033 more rows
#> # ℹ 30 more variables: TAT <dbl>, TATn <dbl>, ZCM <dbl>, ZCMn <dbl>,
#> #   LIGHT <dbl>, `AMB LIGHT` <dbl>, `RED LIGHT` <dbl>, `GREEN LIGHT` <dbl>,
#> #   `BLUE LIGHT` <dbl>, `IR LIGHT` <dbl>, `UVA LIGHT` <dbl>, `UVB LIGHT` <dbl>,
#> #   STATE <dbl>, CAP_SENS_1 <dbl>, CAP_SENS_2 <dbl>, F1 <dbl>, F2 <dbl>,
#> #   F3 <dbl>, F4 <dbl>, F5 <dbl>, F6 <dbl>, F7 <dbl>, F8 <dbl>,
#> #   `MELANOPIC EDI` <dbl>, CLEAR <dbl>, .glc_dataset_id <chr>, …
```

Each imported table also contains `.glc_*` provenance columns such as
the dataset id, participant id, source file, and constructed datetime.
These make row origins explicit after tables are combined.

By default, metadata mismatches such as undeclared extra columns or
values that cannot be parsed to the declared type are errors. During
exploratory work, use `problems = "warn"` to retain problematic data
with warnings; inspect the result before analysis.

``` r

exploratory <- glc_read(
  x,
  dataset_id = "DS001",
  problems = "warn"
)
```

## Collect analysis-ready data

Combine file groups only when their structures and meanings are
compatible:

``` r

light_data <- glc_collect(collection)
light_data
#> # A tibble: 60,043 × 37
#> # Groups:   Id [1]
#>    `DATE/TIME`    MS EVENT TEMPERATURE `EXT TEMPERATURE` ORIENTATION   PIM  PIMn
#>    <chr>       <dbl> <dbl>       <dbl>             <dbl>       <dbl> <dbl> <dbl>
#>  1 14/08/2023…     0     0        28.9                 0          16     0   0  
#>  2 14/08/2023…     0     0        28.9                 0          16     0   0  
#>  3 14/08/2023…     0     0        28.8                 0          16     6   0.6
#>  4 14/08/2023…     0     0        28.8                 0          16     0   0  
#>  5 14/08/2023…     0     0        28.8                 0          16     0   0  
#>  6 14/08/2023…     0     0        28.7                 0          16     0   0  
#>  7 14/08/2023…     0     0        28.7                 0          16     0   0  
#>  8 14/08/2023…     0     0        28.7                 0          16     0   0  
#>  9 14/08/2023…     0     0        28.6                 0          16     0   0  
#> 10 14/08/2023…     0     0        28.6                 0          16     0   0  
#> # ℹ 60,033 more rows
#> # ℹ 29 more variables: TAT <dbl>, TATn <dbl>, ZCM <dbl>, ZCMn <dbl>,
#> #   LIGHT <dbl>, `AMB LIGHT` <dbl>, `RED LIGHT` <dbl>, `GREEN LIGHT` <dbl>,
#> #   `BLUE LIGHT` <dbl>, `IR LIGHT` <dbl>, `UVA LIGHT` <dbl>, `UVB LIGHT` <dbl>,
#> #   STATE <dbl>, CAP_SENS_1 <dbl>, CAP_SENS_2 <dbl>, F1 <dbl>, F2 <dbl>,
#> #   F3 <dbl>, F4 <dbl>, F5 <dbl>, F6 <dbl>, F7 <dbl>, F8 <dbl>,
#> #   `MELANOPIC EDI` <dbl>, CLEAR <dbl>, Id <fct>, participant_Id <chr>, …
```

The default `standardize = "lightlogr"` follows the data conventions
used by [LightLogR](https://tscnlab.github.io/LightLogR/) and adds:

- `Id`, derived from the dataset id;
- `participant_Id`, derived from the participant id;
- `Datetime`, constructed from the metadata-defined datetime
  specification; and
- `file.name`, derived from the source file path.

The standardized result contains no internal `.glc_*` provenance
columns. It is ordered by `Id` and `Datetime` and grouped by `Id`. Use
`standardize = "none"` if you want an ungrouped tibble whose source and
provenance columns are left unchanged.

``` r

source_data <- glc_collect(collection, standardize = "none")
```

[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
refuses to combine groups that differ in columns, types, time zones,
modalities, roles, data states, or datetime specifications. Keep those
groups separate or select a compatible subset with
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md).

Collected data can be used directly with LightLogR’s [data-quality and
insight
functions](https://tscnlab.github.io/LightLogR/reference/index.html#insight),
[visualization
guide](https://tscnlab.github.io/LightLogR/articles/Visualizations.html),
and [metrics
guide](https://tscnlab.github.io/LightLogR/articles/Metrics.html).

## Download a reproducible subset

[`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md)
creates a persistent directory while preserving package-relative paths.
Its safe default downloads only the descriptor, core metadata, and
required schemas:

For public packages, ordinary Git files are transferred from immutable
raw URLs at the selected commit, avoiding per-file GitHub API requests.
A token supplied to
[`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md)
continues to use the authenticated API transport, including for private
repositories.

``` r

metadata_dir <- file.path(tempdir(), "guidolin-metadata")
glc_download(x, metadata_dir)
#> # A tibble: 17 × 6
#>    path                                 destination storage bytes sha256 lfs_oid
#>    <chr>                                <chr>       <chr>   <dbl> <chr>  <chr>  
#>  1 datapackage.json                     /tmp/Rtmp1… git      2363 e841f… NA     
#>  2 data/study.json                      /tmp/Rtmp1… git      3644 fb165… NA     
#>  3 data/participants.json               /tmp/Rtmp1… git       379 1a7ae… NA     
#>  4 data/participant_characteristics.csv /tmp/Rtmp1… git       218 554b8… NA     
#>  5 data/datasets.json                   /tmp/Rtmp1… git     17835 4df6d… NA     
#>  6 data/devices.json                    /tmp/Rtmp1… git      2148 53076… NA     
#>  7 data/datasheets/device_datasheet.js… /tmp/Rtmp1… git      1343 50acf… NA     
#>  8 data/datasheets/sensor_datasheet.js… /tmp/Rtmp1… git      1213 0185f… NA     
#>  9 schemas/2.0.0/gleam-dp-profile.json  /tmp/Rtmp1… git     12715 755e1… NA     
#> 10 schemas/json-entity-resource.json    /tmp/Rtmp1… git       743 a6442… NA     
#> 11 schemas/2.0.0/study.schema.json      /tmp/Rtmp1… git      4361 861e8… NA     
#> 12 schemas/2.0.0/participants.schema.j… /tmp/Rtmp1… git       932 135aa… NA     
#> 13 schemas/2.0.0/participant_character… /tmp/Rtmp1… git      1244 b3b53… NA     
#> 14 schemas/2.0.0/dataset.schema.json    /tmp/Rtmp1… git     12431 6d1ef… NA     
#> 15 schemas/2.0.0/device.schema.json     /tmp/Rtmp1… git      3193 c5efc… NA     
#> 16 schemas/2.0.0/device_datasheet.sche… /tmp/Rtmp1… git      5499 358e3… NA     
#> 17 schemas/light_data.schema.json       /tmp/Rtmp1… git      1653 ce2f6… NA
```

Request data explicitly and apply the same kinds of selectors used
during inspection:

``` r

data_dir <- file.path(tempdir(), "guidolin-ds001")
downloads <- glc_download(
  x,
  data_dir,
  include = "data",
  dataset_id = "DS001"
)
downloads
#> # A tibble: 18 × 6
#>    path                                destination storage  bytes sha256 lfs_oid
#>    <chr>                               <chr>       <chr>    <dbl> <chr>  <chr>  
#>  1 datapackage.json                    /tmp/Rtmp1… git     2.36e3 e841f… NA     
#>  2 data/study.json                     /tmp/Rtmp1… git     3.64e3 fb165… NA     
#>  3 data/participants.json              /tmp/Rtmp1… git     3.79e2 1a7ae… NA     
#>  4 data/participant_characteristics.c… /tmp/Rtmp1… git     2.18e2 554b8… NA     
#>  5 data/datasets.json                  /tmp/Rtmp1… git     1.78e4 4df6d… NA     
#>  6 data/devices.json                   /tmp/Rtmp1… git     2.15e3 53076… NA     
#>  7 data/datasheets/device_datasheet.j… /tmp/Rtmp1… git     1.34e3 50acf… NA     
#>  8 data/datasheets/sensor_datasheet.j… /tmp/Rtmp1… git     1.21e3 0185f… NA     
#>  9 schemas/2.0.0/gleam-dp-profile.json /tmp/Rtmp1… git     1.27e4 755e1… NA     
#> 10 schemas/json-entity-resource.json   /tmp/Rtmp1… git     7.43e2 a6442… NA     
#> 11 schemas/2.0.0/study.schema.json     /tmp/Rtmp1… git     4.36e3 861e8… NA     
#> 12 schemas/2.0.0/participants.schema.… /tmp/Rtmp1… git     9.32e2 135aa… NA     
#> 13 schemas/2.0.0/participant_characte… /tmp/Rtmp1… git     1.24e3 b3b53… NA     
#> 14 schemas/2.0.0/dataset.schema.json   /tmp/Rtmp1… git     1.24e4 6d1ef… NA     
#> 15 schemas/2.0.0/device.schema.json    /tmp/Rtmp1… git     3.19e3 c5efc… NA     
#> 16 schemas/2.0.0/device_datasheet.sch… /tmp/Rtmp1… git     5.50e3 358e3… NA     
#> 17 schemas/light_data.schema.json      /tmp/Rtmp1… git     1.65e3 ce2f6… NA     
#> 18 data/datasets/201_actlumus_Log_102… /tmp/Rtmp1… git     1.03e7 6893b… NA
```

Use `include = "all"` only when you intend to mirror every declared
resource. The `resources` and `files` arguments can further narrow a
download. Existing files are protected unless `overwrite = TRUE` is set
explicitly.

Every download writes `glcdp-manifest.json`, recording the source
repository, exact commit, registry verification state, schema version,
selection, hashes, storage types, sizes, and Git LFS object ids. Reopen
the directory to use the same inspection and import API without fetching
the package again:

``` r

local <- glc_open(data_dir)
glc_summary(local)
#> <GLC package summary>
#> Schema: 2.0.0
#> Studies: 1 | Datasets: 1 available / 2 declared | Participants: 3
#> File groups: 1 available / 2 declared | Files: 1 available / 2 declared | Variables: 35
glc_files(local, available = FALSE)
#> # A tibble: 1 × 19
#>   dataset_id file_group file_group_id participant_id study_id path              
#>   <chr>           <int> <chr>         <chr>          <chr>    <chr>             
#> 1 DS002               1 DS002:1       P001           CG2024   data/datasets/p00…
#> # ℹ 13 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, role <chr>, data_state <chr>, modalities <list>,
#> #   device_id <chr>, storage <chr>, expected_bytes <dbl>, lfs_oid <chr>,
#> #   blob_sha <chr>, available <lgl>

local_collection <- glc_read(local, dataset_id = "all")
#> Local package is a partial data subset.
#> ℹ 1 of 2 declared datasets and 1 of 2 declared files are locally available.
#> ℹ Unavailable dataset: DS002.
#> ℹ glc_read() will read only locally available files.
local_data <- glc_collect(local_collection)
```

The descriptor and core metadata retain the records declared by the
source package. For a local subset,
[`glc_summary()`](https://tscnlab.github.io/glc-dp-r/reference/glc_summary.md)
distinguishes locally available datasets, file groups, and files from
those declared records. When the package is incomplete,
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
also reports how many declared datasets and files are locally available,
then skips absent files. Thus, `dataset_id = "all"` reads all data
included in the subset. Use `glc_files(local, available = FALSE)` to
inspect the omitted file records.

## Temporary reads versus persistent storage

Remote
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
calls use session-temporary storage by default. This is a good fit for
one-off analysis and leaves no persistent files behind.

Pass `cache_dir` to
[`glc_open()`](https://tscnlab.github.io/glc-dp-r/reference/glc_open.md)
when you want remote files reused across calls, or use
[`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md)
when you want an explicit, portable package subset with a manifest:

``` r

cached <- glc_open(
  "tscnlab/guidolin-glee-datasetv2",
  cache_dir = file.path(tempdir(), "glcdp-cache")
)
```

Choose the cache for performance; choose a downloaded subset for a
durable, inspectable analysis input.
