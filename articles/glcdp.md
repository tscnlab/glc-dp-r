# Get started with glcdp

`glcdp` provides a focused route from a Global Light Commons (GLC)
package to analysis-ready R data:

1.  discover a registered package;
2.  open an immutable revision;
3.  inspect its datasets, files, variables, and metadata;
4.  read only the data you need; and
5.  collect compatible file groups for analysis.

This article uses the validated MELIDOS IZTECH package throughout. Its
current passing revision uses schema 3.0.2 and exercises the stable 3.0
import contract with real questionnaire, participant, and light-sensor
data. Remote examples run when pkgdown builds the package website. They
are displayed without execution during ordinary package and CRAN builds,
which keeps those checks independent of network availability. Website
maintainers can also set `GLCDP_SKIP_LIVE=true` for an explicitly
offline pkgdown build.

## Install and load

Install the development version from GitHub and attach the package:

``` r

pak::pak("tscnlab/glc-dp-r")
library(glcdp)
```

`glcdp` currently understands the following GLC schemas:

``` r

library(glcdp)
glc_schema_versions()
#> # A tibble: 5 × 3
#>   version status notes                                                          
#>   <chr>   <chr>  <chr>                                                          
#> 1 1.0.0   legacy Barebones support for recognizable packages without a root ver…
#> 2 2.0.0   legacy Barebones compatibility for the unimplemented legacy schema.   
#> 3 3.0.0   stable Compatible stable predecessor using the typed import contract. 
#> 4 3.0.1   stable Compatible stable predecessor using the typed import contract. 
#> 5 3.0.2   stable Current default schema and primary metadata-driven import impl…
```

## Discover a package

The registry includes both passing and non-passing current revisions.
Keeping both visible makes validation status explicit instead of
silently hiding packages with problems.

``` r

packages <- glc_packages()
packages
#> <GLC registry>
#> Generated: 2026-09-08T00:15:26.985020+00:00 
#> # A tibble: 9 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 melidos-izt… tscnlab/m… main   active            pass           abc456bdb418e…
#> 2 melidos-knu… tscnlab/m… main   active            pass           7fd1dd9e1df17…
#> 3 melidos-bau… tscnlab/m… main   active            pass           643a12126b5e3…
#> 4 melidos-ucr… tscnlab/m… main   active            pass           1d1309c969b9c…
#> 5 melidos-fus… tscnlab/m… main   active            pass           47b7c6aa38d77…
#> 6 melidos-ris… tscnlab/m… main   active            pass           4af2cb284a5bd…
#> 7 melidos-tum… tscnlab/m… main   active            pass           1187d19b7f614…
#> 8 melidos-thu… tscnlab/m… main   active            pass           a0f1057932fbe…
#> 9 melidos-mpi… tscnlab/m… main   active            pass           7ac2c3fe700e0…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>

glc_search_packages("iztech", packages)
#> <GLC registry>
#> Generated: 2026-09-08T00:15:26.985020+00:00 
#> <GLC registry>
#> Generated: 2026-09-08T00:15:26.985020+00:00 
#> # A tibble: 1 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 melidos-izt… tscnlab/m… main   active            pass           abc456bdb418e…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
```

You can also filter on validation status or on whether a package has a
recorded passing revision:

``` r

glc_search_packages(packages = packages, status = "pass")
#> <GLC registry>
#> Generated: 2026-09-08T00:15:26.985020+00:00 
#> <GLC registry>
#> Generated: 2026-09-08T00:15:26.985020+00:00 
#> # A tibble: 9 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 melidos-izt… tscnlab/m… main   active            pass           abc456bdb418e…
#> 2 melidos-knu… tscnlab/m… main   active            pass           7fd1dd9e1df17…
#> 3 melidos-bau… tscnlab/m… main   active            pass           643a12126b5e3…
#> 4 melidos-ucr… tscnlab/m… main   active            pass           1d1309c969b9c…
#> 5 melidos-fus… tscnlab/m… main   active            pass           47b7c6aa38d77…
#> 6 melidos-ris… tscnlab/m… main   active            pass           4af2cb284a5bd…
#> 7 melidos-tum… tscnlab/m… main   active            pass           1187d19b7f614…
#> 8 melidos-thu… tscnlab/m… main   active            pass           a0f1057932fbe…
#> 9 melidos-mpi… tscnlab/m… main   active            pass           7ac2c3fe700e0…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
glc_search_packages(packages = packages, has_pass = TRUE)
#> <GLC registry>
#> Generated: 2026-09-08T00:15:26.985020+00:00 
#> <GLC registry>
#> Generated: 2026-09-08T00:15:26.985020+00:00 
#> # A tibble: 9 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 melidos-izt… tscnlab/m… main   active            pass           abc456bdb418e…
#> 2 melidos-knu… tscnlab/m… main   active            pass           7fd1dd9e1df17…
#> 3 melidos-bau… tscnlab/m… main   active            pass           643a12126b5e3…
#> 4 melidos-ucr… tscnlab/m… main   active            pass           1d1309c969b9c…
#> 5 melidos-fus… tscnlab/m… main   active            pass           47b7c6aa38d77…
#> 6 melidos-ris… tscnlab/m… main   active            pass           4af2cb284a5bd…
#> 7 melidos-tum… tscnlab/m… main   active            pass           1187d19b7f614…
#> 8 melidos-thu… tscnlab/m… main   active            pass           a0f1057932fbe…
#> 9 melidos-mpi… tscnlab/m… main   active            pass           7ac2c3fe700e0…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
```

## Open a reproducible revision

Registered packages open at their latest passing commit by default. The
returned handle records the repository, exact commit, schema version,
and whether the revision was verified against the registry.

``` r

iztech_repository <- "tscnlab/melidos-iztech-glc-dataset"
iztech_dataset <- "MELIDOS_IZTECH_S001"
iztech_demographics <- "MELIDOS_IZTECH_S001:4"
iztech_chest_light <- "MELIDOS_IZTECH_S001:17"

iztech <- glc_open(iztech_repository)
iztech
#> <GLC data package>
#> Source: tscnlab/melidos-iztech-glc-dataset@abc456bdb418
#> Schema: 3.0.2
#> Registry revision: verified
```

The same function opens a local package directory or its
`datapackage.json` file:

``` r

local_package <- glc_open("path/to/data-package")
```

## Inspect before reading

A compact summary is a useful first look:

``` r

glc_summary(iztech)
#> <GLC package summary>
#> Schema: 3.0.2
#> Studies: 1 | Datasets: 17 | Participants: 17
#> File groups: 323 | Files: 323 | Variables: 5554
```

The inventories make data selection explicit. List datasets, then narrow
the file and variable inventories to the dataset and file groups you
intend to read.

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
glc_files(iztech, dataset_id = "MELIDOS_IZTECH_S001")
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
glc_variables(
  iztech,
  file_group = "MELIDOS_IZTECH_S001:17",
  primary = TRUE
)
#> # A tibble: 1 × 15
#>   dataset_id  file_group file_group_id name  label description unit  type  term 
#>   <chr>            <int> <chr>         <chr> <chr> <chr>       <chr> <chr> <chr>
#> 1 MELIDOS_IZ…         17 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#> # ℹ 6 more variables: term_name <chr>, calibration <chr>, primary <lgl>,
#> #   factor_values <list>, factor_labels <list>, factor_descriptions <list>
```

Use
[`glc_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/glc_metadata.md)
for structured metadata and
[`glc_search_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/glc_search_metadata.md)
when you need to locate a value without knowing its resource or field in
advance.

``` r

metadata <- glc_metadata(
  iztech,
  resources = c("study", "participants")
)
metadata$study
#> # A tibble: 1 × 15
#>   schema_version study_internal_id   study_title           study_preregistration
#>   <chr>          <chr>               <chr>                 <chr>                
#> 1 3.0.2          MELIDOS_IZTECH_2025 Personal light expos… The study was not pr…
#> # ℹ 11 more variables: study_ethics <chr>, study_short_description <chr>,
#> #   study_sample <chr>, study_groups <list>, study_setting <chr>,
#> #   study_geographical_location <chr>, study_contributors <list>,
#> #   study_datasets <list>, study_type <chr>, study_funding_sources <list>,
#> #   study_keywords <list>
metadata$participants
#> # A tibble: 17 × 4
#>    participant_internal_id participant_age participant_sex participant_gender
#>    <chr>                             <dbl> <chr>           <chr>             
#>  1 IZTECH_S001                          27 Female          Woman             
#>  2 IZTECH_S002                          24 Female          Woman             
#>  3 IZTECH_S003                          23 Female          Woman             
#>  4 IZTECH_S004                          25 Female          Woman             
#>  5 IZTECH_S005                          25 Female          Woman             
#>  6 IZTECH_S006                          29 Male            Man               
#>  7 IZTECH_S007                          23 Male            Man               
#>  8 IZTECH_S008                          23 Female          Woman             
#>  9 IZTECH_S009                          32 Female          Woman             
#> 10 IZTECH_S010                          26 Female          Woman             
#> 11 IZTECH_S011                          22 Male            Man               
#> 12 IZTECH_S012                          24 Male            Man               
#> 13 IZTECH_S013                          23 Female          Woman             
#> 14 IZTECH_S014                          21 Male            Man               
#> 15 IZTECH_S015                          27 Female          Woman             
#> 16 IZTECH_S016                          24 Male            Man               
#> 17 IZTECH_S017                          21 Female          Woman

glc_search_metadata(iztech, "Izmir", resources = "study")
#> # A tibble: 15 × 5
#>    resource record field                                           value context
#>    <chr>     <int> <chr>                                           <chr> <chr>  
#>  1 study         1 study_title                                     Pers… record…
#>  2 study         1 study_ethics                                    Izmi… record…
#>  3 study         1 study_short_description                         MeLi… record…
#>  4 study         1 study_sample                                    Seve… record…
#>  5 study         1 study_groups.study_group_description            Part… record…
#>  6 study         1 study_geographical_location                     Izmi… record…
#>  7 study         1 study_contributors.contributor_institution.con… Izmi… record…
#>  8 study         1 study_contributors.contributor_institution.con… Izmir record…
#>  9 study         1 study_contributors.contributor_institution.con… Izmi… record…
#> 10 study         1 study_contributors.contributor_institution.con… Izmir record…
#> 11 study         1 study_contributors.contributor_institution.con… Izmi… record…
#> 12 study         1 study_contributors.contributor_institution.con… Izmir record…
#> 13 study         1 study_contributors.contributor_institution.con… Izmi… record…
#> 14 study         1 study_contributors.contributor_institution.con… Izmir record…
#> 15 study         1 study_keywords                                  Izmir record…
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
```

## Let the schema define R column types

Schema 3.0.2 declares every source column’s data type and, for factors,
its allowed levels in schema-declared order.
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
applies those declarations instead of guessing from the first rows of a
file. The compact demographics file contains numeric, logical, and
factor columns:

``` r

demographic_variables <- glc_variables(
  iztech,
  file_group = "MELIDOS_IZTECH_S001:4"
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

The imported R classes and factor levels follow that inventory:

``` r

demographics <- glc_read(
  iztech,
  dataset_id = "MELIDOS_IZTECH_S001",
  file_group = "MELIDOS_IZTECH_S001:4"
)
demographic_data <- demographics$data[[1]]
demographic_data
#> # A tibble: 1 × 13
#>   Id            age sex    gender native_language language_specification
#>   <chr>       <dbl> <fct>  <fct>  <lgl>           <fct>                 
#> 1 IZTECH_S001    27 Female Woman  TRUE            NA                    
#> # ℹ 7 more variables: employment_status <fct>, comments <lgl>,
#> #   .glc_dataset_id <chr>, .glc_file_group <chr>, .glc_participant_id <chr>,
#> #   .glc_source_file <chr>, .glc_datetime <dttm>
levels(demographic_data$sex)
#> [1] "Female"            "Male"              "Intersex"         
#> [4] "Prefer not to say"
```

The same metadata-driven import also handles headers, datetime formats,
decimal marks, encodings, and time zones. By default, values that cannot
be parsed to the declared type or factor level are reported as errors
rather than silently changing the column.

## Read selected light data

A dataset selection is required so that a large package is not imported
accidentally. File-group and variable selectors keep the request
precise. This example reads only photopic illuminance from the S001
chest sensor and limits parsing to the first 10,000 records:

``` r

light_collection <- glc_read(
  iztech,
  dataset_id = "MELIDOS_IZTECH_S001",
  file_group = "MELIDOS_IZTECH_S001:17",
  variables = "LIGHT",
  n_max = 10000
)
light_collection
#> <GLC data collection>
#> File groups: 1
#> Rows: 10000
#> # A tibble: 1 × 18
#>   dataset_id          file_group file_group_id study_id participant_id device_id
#>   <chr>                    <int> <chr>         <chr>    <chr>          <chr>    
#> 1 MELIDOS_IZTECH_S001         17 MELIDOS_IZTE… MELIDOS… IZTECH_S001    IZTECH_A…
#> # ℹ 12 more variables: modalities <list>, role <chr>, data_state <chr>,
#> #   timezone <chr>, datetime_source <chr>, datetime_date <chr>,
#> #   datetime_format <chr>, datetime_time <chr>, datetime_time_format <chr>,
#> #   primary_variables <list>, factor_contract <list>, files <list>
```

In interactive sessions,
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
displays progress across the selected files. Set `progress = FALSE` to
suppress the indicator, for example in a script that manages its own
progress reporting.

The result has one row per compatible file group and stores each
imported table in its `data` list-column. Inspect or process groups
separately when their roles, modalities, or schemas differ.

``` r

names(light_collection$data[[1]])
#> [1] "LIGHT"               ".glc_dataset_id"     ".glc_file_group"    
#> [4] ".glc_participant_id" ".glc_source_file"    ".glc_datetime"
```

`n_max` limits rows parsed after the selected remote file is available;
it does not turn a source file into a byte-range download.

## Collect compatible groups

[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
checks that the selected groups have compatible columns, types, time
zones, modalities, roles, data states, and datetime specifications
before combining them. Its default output maps the dataset id to `Id`,
the participant id to `participant_Id`, parses `Datetime`, and retains a
declared source `file.name` column when present (otherwise deriving it
from the package path). Internal `.glc_*` provenance columns are removed
from this analysis-ready result, matching the core conventions described
in [LightLogR’s import
documentation](https://tscnlab.github.io/LightLogR/reference/import_Dataset.html).

``` r

light_data <- glc_collect(light_collection)
head(light_data[!is.na(light_data$LIGHT), ])
#> # A tibble: 6 × 6
#> # Groups:   Id [1]
#>   LIGHT Id            file_group_id participant_Id Datetime            file.name
#>   <dbl> <fct>         <chr>         <chr>          <dttm>              <chr>    
#> 1  114. MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:06 IZTECH_S…
#> 2  115. MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:16 IZTECH_S…
#> 3  121. MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:26 IZTECH_S…
#> 4  146. MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:36 IZTECH_S…
#> 5  133. MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:46 IZTECH_S…
#> 6  136. MELIDOS_IZTE… MELIDOS_IZTE… IZTECH_S001    2024-12-23 14:14:56 IZTECH_S…
```

The result can be passed directly to LightLogR. Continue with its guides
to [visualizing light logger
data](https://tscnlab.github.io/LightLogR/articles/Visualizations.html)
or [calculating light exposure
metrics](https://tscnlab.github.io/LightLogR/articles/Metrics.html).

Use `standardize = "none"` to leave the source columns and `.glc_*`
provenance columns unchanged:

``` r

source_data <- glc_collect(
  light_collection,
  standardize = "none"
)
head(source_data)
#> # A tibble: 6 × 6
#>   LIGHT .glc_dataset_id     .glc_file_group .glc_participant_id .glc_source_file
#>   <dbl> <chr>               <chr>           <chr>               <chr>           
#> 1    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 2    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 3    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 4    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 5    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> 6    NA MELIDOS_IZTECH_S001 MELIDOS_IZTECH… IZTECH_S001         data/files/sens…
#> # ℹ 1 more variable: .glc_datetime <dttm>
```

## Where to go next

- [Explore and hand off data with the Shiny
  app](https://tscnlab.github.io/glc-dp-r/articles/glc-data-explorer.md)
  follows the same IZTECH package through a guided browser workflow.
- [Discover and inspect data
  packages](https://tscnlab.github.io/glc-dp-r/articles/discover-and-inspect.md)
  covers registry, revision, inventory, and metadata workflows.
- [Import and download
  data](https://tscnlab.github.io/glc-dp-r/articles/import-and-download.md)
  covers schema-defined column types, precise imports, persistent
  downloads, and reproducibility manifests.
