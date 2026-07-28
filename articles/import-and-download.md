# Import and download data

This article covers two related workflows with the validated MELIDOS
IZTECH package:

- import selected files directly into R with
  [`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md);
  and
- create a persistent, reproducible local subset with
  [`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md).

The current passing IZTECH revision uses schema 3.0.2. The examples
deliberately use both a tiny demographics file and a light-sensor file.
The first makes schema-defined R types and factor levels easy to
inspect; the second produces analysis-ready light data. Remote examples
run on the pkgdown website but remain unevaluated in ordinary package
and CRAN builds. Set `GLCDP_SKIP_LIVE=true` to request an offline
website build.

``` r

library(glcdp)
```

``` r

iztech_repository <- "tscnlab/melidos-iztech-glc-dataset"
iztech_dataset <- "MELIDOS_IZTECH_S001"
iztech_demographics <- "MELIDOS_IZTECH_S001:4"
iztech_chest_light <- "MELIDOS_IZTECH_S001:17"

iztech <- glc_open(iztech_repository)
iztech
#> <GLC data package>
#> Source: tscnlab/melidos-iztech-glc-dataset@9353a0c4287d
#> Schema: 3.0.2
#> Registry revision: verified
```

## Select before importing

Inspect datasets, file groups, and variables first. This avoids
transferring unneeded data and gives you the stable ids and source names
used by the read selectors.

``` r

glc_datasets(iztech)
#> # A tibble: 17 × 13
#>    dataset_id      schema_version study_id participant_id participant_associated
#>    <chr>           <chr>          <chr>    <chr>          <lgl>                 
#>  1 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S001    TRUE                  
#>  2 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S002    TRUE                  
#>  3 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S003    TRUE                  
#>  4 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S004    TRUE                  
#>  5 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S005    TRUE                  
#>  6 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S006    TRUE                  
#>  7 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S007    TRUE                  
#>  8 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S008    TRUE                  
#>  9 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S009    TRUE                  
#> 10 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S010    TRUE                  
#> 11 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S011    TRUE                  
#> 12 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S012    TRUE                  
#> 13 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S013    TRUE                  
#> 14 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S014    TRUE                  
#> 15 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S015    TRUE                  
#> 16 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S016    TRUE                  
#> 17 MELIDOS_IZTECH… 3.0.2          MELIDOS… IZTECH_S017    TRUE                  
#> # ℹ 8 more variables: timezone <chr>, latitude <dbl>, longitude <dbl>,
#> #   file_group_count <int>, file_count <int>, modalities <list>,
#> #   device_ids <list>, primary_variables <list>
glc_files(iztech, dataset_id = iztech_dataset)
#> # A tibble: 19 × 30
#>    dataset_id          file_group file_group_id    participant_id study_id path 
#>    <chr>                    <int> <chr>            <chr>          <chr>    <chr>
#>  1 MELIDOS_IZTECH_S001          1 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  2 MELIDOS_IZTECH_S001          2 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  3 MELIDOS_IZTECH_S001          3 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  4 MELIDOS_IZTECH_S001          4 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  5 MELIDOS_IZTECH_S001          5 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  6 MELIDOS_IZTECH_S001          6 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  7 MELIDOS_IZTECH_S001          7 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  8 MELIDOS_IZTECH_S001          8 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  9 MELIDOS_IZTECH_S001          9 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 10 MELIDOS_IZTECH_S001         10 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 11 MELIDOS_IZTECH_S001         11 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 12 MELIDOS_IZTECH_S001         12 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 13 MELIDOS_IZTECH_S001         13 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 14 MELIDOS_IZTECH_S001         14 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 15 MELIDOS_IZTECH_S001         15 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 16 MELIDOS_IZTECH_S001         16 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 17 MELIDOS_IZTECH_S001         17 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 18 MELIDOS_IZTECH_S001         18 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> 19 MELIDOS_IZTECH_S001         19 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#> # ℹ 24 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, description <chr>, instructions <chr>, role <chr>,
#> #   data_state <chr>, modalities <list>, modality_other <chr>,
#> #   modality_other_type <chr>, device_id <chr>, device_location <chr>,
#> #   device_location_type <chr>, temporal_type <chr>, temporal_value <dbl>,
#> #   temporal_unit <chr>, header_row <int>, preprocessing <list>, storage <chr>,
#> #   expected_bytes <dbl>, lfs_oid <chr>, blob_sha <chr>, available <lgl>
glc_variables(iztech, dataset_id = iztech_dataset)
#> # A tibble: 326 × 15
#>    dataset_id file_group file_group_id name  label description unit  type  term 
#>    <chr>           <int> <chr>         <chr> <chr> <chr>       <chr> <chr> <chr>
#>  1 MELIDOS_I…          1 MELIDOS_IZTE… Id    Part… NA          NA    stri… part…
#>  2 MELIDOS_I…          1 MELIDOS_IZTE… affe… Comf… How comfor… NA    fact… inte…
#>  3 MELIDOS_I…          1 MELIDOS_IZTE… burd… Effo… How much e… NA    fact… inte…
#>  4 MELIDOS_I…          1 MELIDOS_IZTE… ethi… Perc… There are … NA    fact… inte…
#>  5 MELIDOS_I…          1 MELIDOS_IZTE… perc… Perc… Wearing li… NA    fact… inte…
#>  6 MELIDOS_I…          1 MELIDOS_IZTE… inte… Unde… It is clea… NA    fact… inte…
#>  7 MELIDOS_I…          1 MELIDOS_IZTE… self… Conf… How confid… NA    fact… inte…
#>  8 MELIDOS_I…          1 MELIDOS_IZTE… oppo… Inte… Wearing th… NA    fact… inte…
#>  9 MELIDOS_I…          1 MELIDOS_IZTE… gene… Over… How accept… NA    fact… inte…
#> 10 MELIDOS_I…          2 MELIDOS_IZTE… Id    Part… NA          NA    stri… part…
#> # ℹ 316 more rows
#> # ℹ 6 more variables: term_name <chr>, calibration <chr>, primary <lgl>,
#> #   factor_values <list>, factor_labels <list>, factor_descriptions <list>
```

`dataset_id` is required by
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md).
Use `dataset_id = "all"` only when you deliberately want every locally
available dataset. A dataset can contain questionnaires, diaries, and
several sensor streams that should be processed separately, so selecting
an entire dataset does not imply that all its file groups can be
collected into one table.

Narrow an import with any combination of file-group ids or indices, file
paths or basenames, source variable names, and semantic terms. Here, the
file inventory identifies the S001 chest-sensor file:

``` r

file_inventory <- glc_files(
  iztech,
  dataset_id = iztech_dataset
)
sensor_file <- file_inventory[
  file_inventory$file_group_id == iztech_chest_light,
]
sensor_file[, c(
  "file_group_id", "path", "device_id", "expected_bytes"
)]
#> # A tibble: 1 × 4
#>   file_group_id          path                           device_id expected_bytes
#>   <chr>                  <chr>                          <chr>              <dbl>
#> 1 MELIDOS_IZTECH_S001:17 data/files/sensor/IZTECH_S001… IZTECH_A…       11411390

source_file <- basename(sensor_file$path[[1]])
source_file
#> [1] "IZTECH_S001_light_chest.csv"
```

Read only its declared melanopic EDI variable and parse at most 10,000
rows:

``` r

light_collection <- glc_read(
  iztech,
  dataset_id = iztech_dataset,
  file_group = iztech_chest_light,
  files = source_file,
  variables = "MEDI",
  n_max = 10000
)
light_collection
#> <GLC data collection>
#> File groups: 1
#> Rows: 10000
#> # A tibble: 1 × 17
#>   dataset_id          file_group file_group_id study_id participant_id device_id
#>   <chr>                    <int> <chr>         <chr>    <chr>          <chr>    
#> 1 MELIDOS_IZTECH_S001         17 MELIDOS_IZTE… MELIDOS… IZTECH_S001    IZTECH_A…
#> # ℹ 11 more variables: modalities <list>, role <chr>, data_state <chr>,
#> #   timezone <chr>, datetime_source <chr>, datetime_date <chr>,
#> #   datetime_format <chr>, datetime_time <chr>, datetime_time_format <chr>,
#> #   primary_variables <list>, files <list>
```

When `variables` or `terms` are selected, the imported tables contain
only the matching source variables. `glcdp` still uses required date or
time columns internally to construct `.glc_datetime`, but does not
retain them unless the filters select them. Set `primary_only = TRUE` to
select declared primary variables:

``` r

primary_light <- glc_read(
  iztech,
  dataset_id = iztech_dataset,
  file_group = iztech_chest_light,
  primary_only = TRUE,
  n_max = 10000
)
primary_data <- primary_light$data[[1]]
primary_data[stats::complete.cases(primary_data), ]
#> # A tibble: 4,876 × 6
#>     MEDI .glc_dataset_id    .glc_file_group .glc_participant_id .glc_source_file
#>    <dbl> <chr>              <chr>           <chr>               <chr>           
#>  1  65.9 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  2  65.9 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  3  68.6 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  4  83.6 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  5  76.9 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  6  78.0 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  7  82.3 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  8  82.3 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#>  9  83.5 MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 10 103.  MELIDOS_IZTECH_S0… MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> # ℹ 4,866 more rows
#> # ℹ 1 more variable: .glc_datetime <dttm>
```

Interactive calls show progress across the selected files by default.
Use `progress = FALSE` to suppress the indicator when a script or
application provides its own progress reporting. `n_max` limits rows
parsed after a remote file is available; it does not reduce the
whole-file transfer.

## Import schema-defined R types and factor levels

Schema 3.0.2 declares a type for each source column. Factor declarations
also contain their levels in schema-declared order. These declarations
are exposed by the variable inventory:

``` r

demographic_variables <- glc_variables(
  iztech,
  file_group = iztech_demographics
)
demographic_variables[, c("name", "type", "factor_values")]
#> # A tibble: 8 × 3
#>   name                   type    factor_values
#>   <chr>                  <chr>   <list>       
#> 1 Id                     string  <chr [0]>    
#> 2 age                    numeric <chr [0]>    
#> 3 sex                    factor  <chr [4]>    
#> 4 gender                 factor  <chr [5]>    
#> 5 native_language        boolean <chr [0]>    
#> 6 language_specification factor  <chr [3]>    
#> 7 employment_status      factor  <chr [6]>    
#> 8 comments               boolean <chr [0]>
```

[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
uses those declarations rather than guessing from file contents. The
IZTECH demographics file therefore yields numeric, logical, and factor
columns with the declared levels:

``` r

demographics <- glc_read(
  iztech,
  dataset_id = iztech_dataset,
  file_group = iztech_demographics
)
demographic_data <- demographics$data[[1]]

levels(demographic_data$sex)
#> [1] "Female"            "Male"              "Intersex"         
#> [4] "Prefer not to say"
levels(demographic_data$employment_status)
#> [1] "Full time employed"                      
#> [2] "Part time employed"                      
#> [3] "Marginally employed (Minijob)"           
#> [4] "Not employed but studying or in training"
#> [5] "Studying and employed"                   
#> [6] "Not employed"
```

The same import metadata controls headers, datetime construction,
decimal marks, encodings, and time zones. By default, undeclared extra
columns, values that cannot be parsed to the declared type, and values
outside declared factor levels are errors. During exploratory work, use
`problems = "warn"` to retain problematic data with warnings; inspect
the result before analysis.

``` r

exploratory_demographics <- glc_read(
  iztech,
  dataset_id = iztech_dataset,
  file_group = iztech_demographics,
  problems = "warn"
)
```

## Understand the imported collection

A `glc_data_collection` has one row per file group. Descriptive columns
record the dataset, participant, device, modality, role, data state,
time zone, datetime specification, and source files. The imported table
is in the `data` list-column.

``` r

light_collection[, setdiff(names(light_collection), "data")]
#> <GLC data collection>
#> File groups: 1
#> Warning: Unknown or uninitialised column: `data`.
#> Rows: 0
#> # A tibble: 1 × 17
#>   dataset_id          file_group file_group_id study_id participant_id device_id
#>   <chr>                    <int> <chr>         <chr>    <chr>          <chr>    
#> 1 MELIDOS_IZTECH_S001         17 MELIDOS_IZTE… MELIDOS… IZTECH_S001    IZTECH_A…
#> # ℹ 11 more variables: modalities <list>, role <chr>, data_state <chr>,
#> #   timezone <chr>, datetime_source <chr>, datetime_date <chr>,
#> #   datetime_format <chr>, datetime_time <chr>, datetime_time_format <chr>,
#> #   primary_variables <list>, files <list>
names(light_collection$data[[1]])
#> [1] "MEDI"                ".glc_dataset_id"     ".glc_file_group"    
#> [4] ".glc_participant_id" ".glc_source_file"    ".glc_datetime"
```

Each imported table also contains `.glc_*` provenance columns such as
the dataset id, participant id, source file, and constructed datetime.
These make row origins explicit after tables are combined.

## Collect analysis-ready data

Combine file groups only when their structures and meanings are
compatible:

``` r

light_data <- glc_collect(light_collection)
head(light_data[!is.na(light_data$MEDI), ])
#> # A tibble: 6 × 6
#> # Groups:   Id [1]
#>    MEDI Id            file_group_id participant_Id Datetime            file.name
#>   <dbl> <fct>         <chr>         <chr>          <dttm>              <chr>    
#> 1  65.9 MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:06 IZTECH_S…
#> 2  65.9 MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:16 IZTECH_S…
#> 3  68.6 MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:26 IZTECH_S…
#> 4  83.6 MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:36 IZTECH_S…
#> 5  76.9 MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:46 IZTECH_S…
#> 6  78.0 MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:56 IZTECH_S…
```

The default `standardize = "lightlogr"` follows the data conventions
used by [LightLogR](https://tscnlab.github.io/LightLogR/) and adds:

- `Id`, derived from the dataset id;
- `file_group_id`, the stable dataset file-group id used by the metadata
  helpers;
- `participant_Id`, derived from the participant id;
- `Datetime`, constructed from the metadata-defined datetime
  specification; and
- `file.name`, retained from a declared source column when present or
  derived from the package path otherwise.

The standardized result contains no internal `.glc_*` provenance
columns. It is ordered by `Id` and `Datetime` and grouped by `Id`. Use
`standardize = "none"` if you want an ungrouped tibble whose source and
provenance columns are left unchanged.

``` r

source_data <- glc_collect(
  light_collection,
  standardize = "none"
)
head(source_data)
#> # A tibble: 6 × 6
#>    MEDI .glc_dataset_id     .glc_file_group .glc_participant_id .glc_source_file
#>   <dbl> <chr>               <chr>           <chr>               <chr>           
#> 1    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 2    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 3    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 4    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 5    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 6    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> # ℹ 1 more variable: .glc_datetime <dttm>
```

[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
refuses to combine groups that differ in columns, types, time zones,
modalities, roles, data states, or datetime specifications. It also
rejects contradictory file-group relationships and multiple device links
within one dataset. Keep those groups separate or select a compatible
subset with
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md).

Collected data can be used directly with LightLogR’s [data-quality and
insight
functions](https://tscnlab.github.io/LightLogR/reference/index.html#insight),
[visualization
guide](https://tscnlab.github.io/LightLogR/articles/Visualizations.html),
and [metrics
guide](https://tscnlab.github.io/LightLogR/articles/Metrics.html).

## Extract or add metadata

Use
[`extract_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/extract_metadata.md)
when you want one concise row per imported file group, and
[`add_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/add_metadata.md)
when the same fields should be available on every observation. Both
functions require an explicit metadata source and field selection. The
default key is `file_group_id`, which
[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
adds to standardized data. If the input is grouped, the extracted tibble
retains that grouping and includes its grouping columns before
`file_group_id`.

``` r

analysis_metadata <- tibble::tibble(
  file_group_id = unique(as.character(light_data$file_group_id)),
  analysis_set = "chest sensor"
)

metadata_summary <- extract_metadata(
  light_data,
  analysis_metadata,
  fields = "analysis_set"
)
metadata_summary
#> # A tibble: 1 × 3
#> # Groups:   Id [1]
#>   Id                  file_group_id          analysis_set
#>   <fct>               <chr>                  <chr>       
#> 1 MELIDOS_IZTECH_S001 MELIDOS_IZTECH_S001:17 chest sensor

enriched_data <- add_metadata(
  light_data,
  analysis_metadata,
  fields = "analysis_set"
)

enriched_data |>
  head() |>
  dplyr::select(-file.name)
#> # A tibble: 6 × 6
#> # Groups:   Id [1]
#>    MEDI Id         file_group_id participant_Id Datetime            analysis_set
#>   <dbl> <fct>      <chr>         <chr>          <dttm>              <chr>       
#> 1    NA MELIDOS_I… MELIDOS_IZTE… IZTECH_S001    2024-12-23 00:00:06 chest sensor
#> 2    NA MELIDOS_I… MELIDOS_IZTE… IZTECH_S001    2024-12-23 00:00:16 chest sensor
#> 3    NA MELIDOS_I… MELIDOS_IZTE… IZTECH_S001    2024-12-23 00:00:26 chest sensor
#> 4    NA MELIDOS_I… MELIDOS_IZTE… IZTECH_S001    2024-12-23 00:00:36 chest sensor
#> 5    NA MELIDOS_I… MELIDOS_IZTE… IZTECH_S001    2024-12-23 00:00:46 chest sensor
#> 6    NA MELIDOS_I… MELIDOS_IZTE… IZTECH_S001    2024-12-23 00:00:56 chest sensor
```

The metadata source can also be a local CSV/TSV path or the package
handle. For a package handle, `glcdp` follows each file group to its
dataset and device, and follows the dataset to its participant and
study. It can therefore assemble fields such as `participant_age`
without manually naming the participant resource. Dataset-,
participant-, and study-level values repeat across file groups. Use
`by = "Id"` when you explicitly want one row per dataset instead. Field
and relationship resolution must remain unambiguous; use `resource` to
restrict field discovery when the same field occurs in multiple
connected resources.

``` r

glc_search_metadata(
  iztech,
  "participant_age",
  resources = "participants",
  search_in = "fields"
)
#> # A tibble: 17 × 5
#>    resource     record field           value context  
#>    <chr>         <int> <chr>           <chr> <chr>    
#>  1 participants      1 participant_age 27    record 1 
#>  2 participants      2 participant_age 24    record 2 
#>  3 participants      3 participant_age 23    record 3 
#>  4 participants      4 participant_age 25    record 4 
#>  5 participants      5 participant_age 25    record 5 
#>  6 participants      6 participant_age 29    record 6 
#>  7 participants      7 participant_age 23    record 7 
#>  8 participants      8 participant_age 23    record 8 
#>  9 participants      9 participant_age 32    record 9 
#> 10 participants     10 participant_age 26    record 10
#> 11 participants     11 participant_age 22    record 11
#> 12 participants     12 participant_age 24    record 12
#> 13 participants     13 participant_age 23    record 13
#> 14 participants     14 participant_age 21    record 14
#> 15 participants     15 participant_age 27    record 15
#> 16 participants     16 participant_age 24    record 16
#> 17 participants     17 participant_age 21    record 17

dataset_metadata <- extract_metadata(
  light_data,
  iztech,
  fields = c(
    "dataset_timezone",
    "dataset_location",
    "participant_age",
    "study_title",
    "device_model"
  )
)
dataset_metadata
#> # A tibble: 1 × 7
#> # Groups:   Id [1]
#>   Id             file_group_id dataset_timezone dataset_location participant_age
#>   <fct>          <chr>         <chr>            <list>                     <dbl>
#> 1 MELIDOS_IZTEC… MELIDOS_IZTE… Europe/Istanbul  <dbl [2]>                     27
#> # ℹ 2 more variables: study_title <chr>, device_model <chr>

add_metadata(
  light_data,
  iztech,
  fields = c(
    "dataset_timezone",
    "dataset_location",
    "participant_age",
    "study_title",
    "device_model"
  )
) |>
  head() |>
  dplyr::select(
    Id,
    dataset_timezone,
    dataset_location,
    participant_age,
    study_title,
    device_model
  )
#> # A tibble: 6 × 6
#> # Groups:   Id [1]
#>   Id               dataset_timezone dataset_location participant_age study_title
#>   <fct>            <chr>            <list>                     <dbl> <chr>      
#> 1 MELIDOS_IZTECH_… Europe/Istanbul  <dbl [2]>                     27 Personal l…
#> 2 MELIDOS_IZTECH_… Europe/Istanbul  <dbl [2]>                     27 Personal l…
#> 3 MELIDOS_IZTECH_… Europe/Istanbul  <dbl [2]>                     27 Personal l…
#> 4 MELIDOS_IZTECH_… Europe/Istanbul  <dbl [2]>                     27 Personal l…
#> 5 MELIDOS_IZTECH_… Europe/Istanbul  <dbl [2]>                     27 Personal l…
#> 6 MELIDOS_IZTECH_… Europe/Istanbul  <dbl [2]>                     27 Personal l…
#> # ℹ 1 more variable: device_model <chr>
```

When adding project-specific metadata to a data package, store it under
a stable package-relative path such as `data/metadata.csv` and declare
it as a resource in `datapackage.json`. The functions never guess from
the working directory or neighboring files. They error when no
identifiers or fields match, and warn while retaining useful results for
partial matches.

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

metadata_dir <- tempfile("iztech-metadata-")
glc_download(iztech, metadata_dir)
#> # A tibble: 15 × 6
#>    path                                destination storage  bytes sha256 lfs_oid
#>    <chr>                               <chr>       <chr>    <dbl> <chr>  <chr>  
#>  1 datapackage.json                    /tmp/RtmpA… git     1.58e3 510d0… NA     
#>  2 data/study.json                     /tmp/RtmpA… git     6.03e3 0ce90… NA     
#>  3 data/participants.csv               /tmp/RtmpA… git     5.27e2 70454… NA     
#>  4 data/participant_characteristics.c… /tmp/RtmpA… git     5.80e4 c018f… NA     
#>  5 data/datasets.json                  /tmp/RtmpA… git     3.94e6 03899… NA     
#>  6 data/devices.json                   /tmp/RtmpA… git     8.36e3 8798c… NA     
#>  7 data/device_datasheets.json         /tmp/RtmpA… git     9.13e3 e4a6b… NA     
#>  8 schemas/3.0.2/glc-dp-profile.json   /tmp/RtmpA… git     1.14e4 4b6bf… NA     
#>  9 json-entity-resource.json           /tmp/RtmpA… git     7.65e2 c89c1… NA     
#> 10 schemas/3.0.2/study.schema.json     /tmp/RtmpA… git     6.50e3 f1e71… NA     
#> 11 schemas/3.0.2/participants.schema.… /tmp/RtmpA… git     1.08e3 e7192… NA     
#> 12 schemas/3.0.2/participant_characte… /tmp/RtmpA… git     1.46e3 25351… NA     
#> 13 schemas/3.0.2/dataset.schema.json   /tmp/RtmpA… git     4.66e4 7c17e… NA     
#> 14 schemas/3.0.2/device.schema.json    /tmp/RtmpA… git     3.95e3 ea149… NA     
#> 15 schemas/3.0.2/device_datasheet.sch… /tmp/RtmpA… git     1.31e4 44d54… NA
```

Request data explicitly and apply the same selectors used during
inspection. This compact example downloads the S001 demographics group:

``` r

data_dir <- tempfile("iztech-s001-demographics-")
downloads <- glc_download(
  iztech,
  data_dir,
  include = "data",
  dataset_id = iztech_dataset,
  file_group = iztech_demographics
)
downloads
#> # A tibble: 16 × 6
#>    path                                destination storage  bytes sha256 lfs_oid
#>    <chr>                               <chr>       <chr>    <dbl> <chr>  <chr>  
#>  1 datapackage.json                    /tmp/RtmpA… git     1.58e3 510d0… NA     
#>  2 data/study.json                     /tmp/RtmpA… git     6.03e3 0ce90… NA     
#>  3 data/participants.csv               /tmp/RtmpA… git     5.27e2 70454… NA     
#>  4 data/participant_characteristics.c… /tmp/RtmpA… git     5.80e4 c018f… NA     
#>  5 data/datasets.json                  /tmp/RtmpA… git     3.94e6 03899… NA     
#>  6 data/devices.json                   /tmp/RtmpA… git     8.36e3 8798c… NA     
#>  7 data/device_datasheets.json         /tmp/RtmpA… git     9.13e3 e4a6b… NA     
#>  8 schemas/3.0.2/glc-dp-profile.json   /tmp/RtmpA… git     1.14e4 4b6bf… NA     
#>  9 json-entity-resource.json           /tmp/RtmpA… git     7.65e2 c89c1… NA     
#> 10 schemas/3.0.2/study.schema.json     /tmp/RtmpA… git     6.50e3 f1e71… NA     
#> 11 schemas/3.0.2/participants.schema.… /tmp/RtmpA… git     1.08e3 e7192… NA     
#> 12 schemas/3.0.2/participant_characte… /tmp/RtmpA… git     1.46e3 25351… NA     
#> 13 schemas/3.0.2/dataset.schema.json   /tmp/RtmpA… git     4.66e4 7c17e… NA     
#> 14 schemas/3.0.2/device.schema.json    /tmp/RtmpA… git     3.95e3 ea149… NA     
#> 15 schemas/3.0.2/device_datasheet.sch… /tmp/RtmpA… git     1.31e4 44d54… NA     
#> 16 data/files/questionnaires/IZTECH_S… /tmp/RtmpA… git     1.3 e2 daaad… NA
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
#> Schema: 3.0.2
#> Studies: 1 | Datasets: 1 available / 17 declared | Participants: 17
#> File groups: 1 available / 323 declared | Files: 1 available / 323 declared | Variables: 5554
glc_files(local, dataset_id = iztech_dataset, available = TRUE)
#> # A tibble: 1 × 30
#>   dataset_id          file_group file_group_id     participant_id study_id path 
#>   <chr>                    <int> <chr>             <chr>          <chr>    <chr>
#> 1 MELIDOS_IZTECH_S001          4 MELIDOS_IZTECH_S… IZTECH_S001    MELIDOS… data…
#> # ℹ 24 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, description <chr>, instructions <chr>, role <chr>,
#> #   data_state <chr>, modalities <list>, modality_other <chr>,
#> #   modality_other_type <chr>, device_id <chr>, device_location <chr>,
#> #   device_location_type <chr>, temporal_type <chr>, temporal_value <dbl>,
#> #   temporal_unit <chr>, header_row <int>, preprocessing <list>, storage <chr>,
#> #   expected_bytes <dbl>, lfs_oid <chr>, blob_sha <chr>, available <lgl>

local_collection <- glc_read(
  local,
  dataset_id = iztech_dataset,
  file_group = iztech_demographics
)
#> Local package is a partial data subset.
#> ℹ 1 of 17 declared datasets and 1 of 323 declared files are locally available.
#> ℹ Unavailable datasets: MELIDOS_IZTECH_S002, MELIDOS_IZTECH_S003,
#>   MELIDOS_IZTECH_S004, MELIDOS_IZTECH_S005, MELIDOS_IZTECH_S006,
#>   MELIDOS_IZTECH_S007, MELIDOS_IZTECH_S008, MELIDOS_IZTECH_S009,
#>   MELIDOS_IZTECH_S010, MELIDOS_IZTECH_S011, MELIDOS_IZTECH_S012,
#>   MELIDOS_IZTECH_S013, MELIDOS_IZTECH_S014, MELIDOS_IZTECH_S015,
#>   MELIDOS_IZTECH_S016, MELIDOS_IZTECH_S017.
#> ℹ glc_read() will read only locally available files.
local_data <- glc_collect(local_collection)
local_data
#> # A tibble: 1 × 12
#> # Groups:   Id [1]
#>   Id                    age sex    gender native_language language_specification
#>   <fct>               <dbl> <fct>  <fct>  <lgl>           <fct>                 
#> 1 MELIDOS_IZTECH_S001    27 Female Woman  TRUE            NA                    
#> # ℹ 6 more variables: employment_status <fct>, comments <lgl>,
#> #   file_group_id <chr>, participant_Id <chr>, Datetime <dttm>, file.name <chr>
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
inspect omitted file records.

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
  iztech_repository,
  cache_dir = file.path(tempdir(), "glcdp-iztech-cache")
)
```

Choose the cache for performance; choose a downloaded subset for a
durable, inspectable analysis input.
