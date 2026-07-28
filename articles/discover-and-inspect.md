# Discover and inspect data packages

This article shows how to choose a Global Light Commons package and
understand its contents before downloading or importing measurements.
Every example uses the validated MELIDOS IZTECH package, whose current
passing revision uses schema 3.0.2. Remote examples run on the pkgdown
website but remain unevaluated in ordinary package and CRAN builds. Set
`GLCDP_SKIP_LIVE=true` to request an offline website build.

``` r

library(glcdp)
```

## Understand the registry

[`glc_packages()`](https://tscnlab.github.io/glc-dp-r/reference/glc_packages.md)
returns one row per registered repository. The most useful fields
distinguish the repository’s current revision from its most recent
passing revision:

- `current_status` and `current_commit` describe the configured current
  revision;
- `latest_pass_commit` and `has_latest_pass` identify the last validated
  revision available for reproducible use; and
- `attestation_verified` reports whether the registry attestation was
  verified.

``` r

packages <- glc_packages()
packages[, c(
  "id", "repository", "current_status", "has_latest_pass",
  "attestation_verified"
)]
#> <GLC registry>
#> Generated: 2026-07-28T13:13:47.465808+00:00 
#> # A tibble: 3 × 5
#>   id              repository current_status has_latest_pass attestation_verified
#>   <chr>           <chr>      <chr>          <lgl>           <lgl>               
#> 1 guidolin-glee-… tscnlab/g… pass           TRUE            TRUE                
#> 2 melidos-iztech… tscnlab/m… pass           TRUE            TRUE                
#> 3 melidos-knust-… tscnlab/m… pass           TRUE            TRUE
```

Registry results are cached for the R session. Set `refresh = TRUE` only
when you need to fetch the registry again.

``` r

packages <- glc_packages(refresh = TRUE)
```

Searches are fixed and case-insensitive by default:

``` r

glc_search_packages("iztech", packages)
#> <GLC registry>
#> Generated: 2026-07-28T13:13:47.465808+00:00 
#> <GLC registry>
#> Generated: 2026-07-28T13:13:47.465808+00:00 
#> # A tibble: 1 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 melidos-izt… tscnlab/m… main   active            pass           9353a0c4287d4…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
glc_search_packages(packages = packages, status = c("pass", "fail"))
#> <GLC registry>
#> Generated: 2026-07-28T13:13:47.465808+00:00 
#> <GLC registry>
#> Generated: 2026-07-28T13:13:47.465808+00:00 
#> # A tibble: 3 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 guidolin-gl… tscnlab/g… main   active            pass           8ec9034a3d967…
#> 2 melidos-izt… tscnlab/m… main   active            pass           9353a0c4287d4…
#> 3 melidos-knu… tscnlab/m… main   active            pass           a7e4d17a7ea7f…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
glc_search_packages(packages = packages, has_pass = FALSE)
#> <GLC registry>
#> Generated: 2026-07-28T13:13:47.465808+00:00 
#> <GLC registry>
#> Generated: 2026-07-28T13:13:47.465808+00:00 
#> # A tibble: 0 × 17
#> # ℹ 17 variables: id <chr>, repository <chr>, branch <chr>,
#> #   repository_status <chr>, current_status <chr>, current_commit <chr>,
#> #   current_validator <chr>, current_validated_at <chr>, current_errors <int>,
#> #   current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
```

## Choose a revision deliberately

Opening a registered package with the default `ref = "latest_pass"`
selects an exact passing commit, not a moving branch:

``` r

iztech_repository <- "tscnlab/melidos-iztech-glc-dataset"
iztech <- glc_open(iztech_repository)
iztech
#> <GLC data package>
#> Source: tscnlab/melidos-iztech-glc-dataset@9353a0c4287d
#> Schema: 3.0.2
#> Registry revision: verified
```

Use `ref = "current"` when you explicitly need the registry’s current
revision. If that revision is not passing, `glcdp` warns. You can also
provide an exact 40-character commit SHA; commits that are not selected
through a registry record are marked as unverified.

``` r

current <- glc_open(
  iztech_repository,
  ref = "current"
)
current
#> <GLC data package>
#> Source: tscnlab/melidos-iztech-glc-dataset@9353a0c4287d
#> Schema: 3.0.2
#> Registry revision: verified

registry_row <- glc_search_packages("melidos-iztech", packages)
registry_row$repository[[1]]
#> [1] "tscnlab/melidos-iztech-glc-dataset"
registry_row$latest_pass_commit[[1]]
#> [1] "9353a0c4287d44cb400d30f45d7dbcf9910f9bde"

pinned <- glc_open(
  registry_row$repository[[1]],
  ref = registry_row$latest_pass_commit[[1]]
)
pinned
#> <GLC data package>
#> Source: tscnlab/melidos-iztech-glc-dataset@9353a0c4287d
#> Schema: 3.0.2
#> Registry revision: verified
```

For private repositories, pass `token` directly or define `GITHUB_PAT`
or `GITHUB_TOKEN`. Do not put tokens in scripts, vignettes, or package
options.

## Start with a package summary

[`glc_summary()`](https://tscnlab.github.io/glc-dp-r/reference/glc_summary.md)
reports the schema version and counts of studies, datasets,
participants, devices, file groups, files, and variables. It also
summarizes modalities, time zones, and primary variables.

``` r

glc_summary(iztech)
#> <GLC package summary>
#> Schema: 3.0.2
#> Studies: 1 | Datasets: 17 | Participants: 17
#> File groups: 323 | Files: 323 | Variables: 5554
```

## Explore inventories

The inventories are tibbles, so they can be printed, filtered, or joined
using ordinary data-frame tools.

``` r

glc_resources(iztech)
#> # A tibble: 6 × 10
#>   resource path  core  directory format media_type profile schema_path delimiter
#>   <chr>    <chr> <lgl> <lgl>     <chr>  <chr>      <chr>   <chr>       <chr>    
#> 1 study    data… TRUE  FALSE     NA     applicati… json-e… schemas/3.… NA       
#> 2 partici… data… TRUE  FALSE     csv    text/csv   tabula… schemas/3.… NA       
#> 3 partici… data… TRUE  FALSE     csv    text/csv   tabula… schemas/3.… NA       
#> 4 datasets data… TRUE  FALSE     NA     applicati… json-e… schemas/3.… NA       
#> 5 devices  data… TRUE  FALSE     NA     applicati… json-e… schemas/3.… NA       
#> 6 device_… data… TRUE  FALSE     NA     applicati… json-e… schemas/3.… NA       
#> # ℹ 1 more variable: decimal_mark <chr>
```

[`glc_datasets()`](https://tscnlab.github.io/glc-dp-r/reference/glc_datasets.md)
describes logical datasets and their associations. Once you have a
dataset id, reuse it to narrow the other inventories.

``` r

iztech_dataset <- "MELIDOS_IZTECH_S001"
iztech_demographics <- "MELIDOS_IZTECH_S001:4"
iztech_chest_light <- "MELIDOS_IZTECH_S001:17"

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
glc_files(
  iztech,
  # dataset_id = iztech_dataset,
  modality = "light",
  available = TRUE
) |>
  dplyr::filter(device_location == "eye level")
#> # A tibble: 18 × 30
#>    dataset_id          file_group file_group_id    participant_id study_id path 
#>    <chr>                    <int> <chr>            <chr>          <chr>    <chr>
#>  1 MELIDOS_IZTECH_S001         18 MELIDOS_IZTECH_… IZTECH_S001    MELIDOS… data…
#>  2 MELIDOS_IZTECH_S002         18 MELIDOS_IZTECH_… IZTECH_S002    MELIDOS… data…
#>  3 MELIDOS_IZTECH_S003         18 MELIDOS_IZTECH_… IZTECH_S003    MELIDOS… data…
#>  4 MELIDOS_IZTECH_S004         19 MELIDOS_IZTECH_… IZTECH_S004    MELIDOS… data…
#>  5 MELIDOS_IZTECH_S004         20 MELIDOS_IZTECH_… IZTECH_S004    MELIDOS… data…
#>  6 MELIDOS_IZTECH_S005         18 MELIDOS_IZTECH_… IZTECH_S005    MELIDOS… data…
#>  7 MELIDOS_IZTECH_S006         18 MELIDOS_IZTECH_… IZTECH_S006    MELIDOS… data…
#>  8 MELIDOS_IZTECH_S007         17 MELIDOS_IZTECH_… IZTECH_S007    MELIDOS… data…
#>  9 MELIDOS_IZTECH_S008         18 MELIDOS_IZTECH_… IZTECH_S008    MELIDOS… data…
#> 10 MELIDOS_IZTECH_S009         18 MELIDOS_IZTECH_… IZTECH_S009    MELIDOS… data…
#> 11 MELIDOS_IZTECH_S010         18 MELIDOS_IZTECH_… IZTECH_S010    MELIDOS… data…
#> 12 MELIDOS_IZTECH_S011         18 MELIDOS_IZTECH_… IZTECH_S011    MELIDOS… data…
#> 13 MELIDOS_IZTECH_S012         18 MELIDOS_IZTECH_… IZTECH_S012    MELIDOS… data…
#> 14 MELIDOS_IZTECH_S013         18 MELIDOS_IZTECH_… IZTECH_S013    MELIDOS… data…
#> 15 MELIDOS_IZTECH_S014         18 MELIDOS_IZTECH_… IZTECH_S014    MELIDOS… data…
#> 16 MELIDOS_IZTECH_S015         17 MELIDOS_IZTECH_… IZTECH_S015    MELIDOS… data…
#> 17 MELIDOS_IZTECH_S016         18 MELIDOS_IZTECH_… IZTECH_S016    MELIDOS… data…
#> 18 MELIDOS_IZTECH_S017         18 MELIDOS_IZTECH_… IZTECH_S017    MELIDOS… data…
#> # ℹ 24 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, description <chr>, instructions <chr>, role <chr>,
#> #   data_state <chr>, modalities <list>, modality_other <chr>,
#> #   modality_other_type <chr>, device_id <chr>, device_location <chr>,
#> #   device_location_type <chr>, temporal_type <chr>, temporal_value <dbl>,
#> #   temporal_unit <chr>, header_row <int>, preprocessing <list>, storage <chr>,
#> #   expected_bytes <dbl>, lfs_oid <chr>, blob_sha <chr>, available <lgl>
```

File inventories expose both declared and resolved paths, format,
encoding, time zone, role, data state, device, storage type, expected
size, and availability. Git LFS-backed files are identified without
requiring a local Git LFS installation.

Variable inventories can be narrowed by dataset, file group, semantic
term, or primary status:

``` r

demographic_variables <- glc_variables(
  iztech,
  file_group = iztech_demographics
)
demographic_variables[, c(
  "name", "type", "factor_values", "primary"
)]
#> # A tibble: 8 × 4
#>   name                   type    factor_values primary
#>   <chr>                  <chr>   <list>        <lgl>  
#> 1 Id                     string  <chr [0]>     FALSE  
#> 2 age                    numeric <chr [0]>     FALSE  
#> 3 sex                    factor  <chr [4]>     FALSE  
#> 4 gender                 factor  <chr [5]>     FALSE  
#> 5 native_language        boolean <chr [0]>     FALSE  
#> 6 language_specification factor  <chr [3]>     FALSE  
#> 7 employment_status      factor  <chr [6]>     FALSE  
#> 8 comments               boolean <chr [0]>     FALSE

glc_variables(
  iztech,
  file_group = iztech_chest_light,
  primary = TRUE
)
#> # A tibble: 1 × 15
#>   dataset_id  file_group file_group_id name  label description unit  type  term 
#>   <chr>            <int> <chr>         <chr> <chr> <chr>       <chr> <chr> <chr>
#> 1 MELIDOS_IZ…         17 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#> # ℹ 6 more variables: term_name <chr>, calibration <chr>, primary <lgl>,
#> #   factor_values <list>, factor_labels <list>, factor_descriptions <list>
glc_variables(iztech, term = "melanopic_edi")
#> # A tibble: 52 × 15
#>    dataset_id file_group file_group_id name  label description unit  type  term 
#>    <chr>           <int> <chr>         <chr> <chr> <chr>       <chr> <chr> <chr>
#>  1 MELIDOS_I…         17 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  2 MELIDOS_I…         18 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  3 MELIDOS_I…         19 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  4 MELIDOS_I…         17 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  5 MELIDOS_I…         18 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  6 MELIDOS_I…         19 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  7 MELIDOS_I…         17 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  8 MELIDOS_I…         18 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#>  9 MELIDOS_I…         19 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#> 10 MELIDOS_I…         18 MELIDOS_IZTE… MEDI  Mela… NA          lx    nume… mela…
#> # ℹ 42 more rows
#> # ℹ 6 more variables: term_name <chr>, calibration <chr>, primary <lgl>,
#> #   factor_values <list>, factor_labels <list>, factor_descriptions <list>
```

The `type` and `factor_values` columns are not merely descriptive:
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
uses them to construct the corresponding R columns and factor levels in
schema-declared order. Use source variable names with its `variables`
argument and semantic terms with its `terms` argument.

## Load and search metadata

With no `resources` argument,
[`glc_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/glc_metadata.md)
loads the core resources that the package declares. Requesting resources
explicitly is often faster and makes dependencies clearer.

``` r

metadata <- glc_metadata(
  iztech,
  resources = c("study", "participants")
)
names(metadata)
#> [1] "study"        "participants"
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
```

JSON objects remain lists, tabular resources become tibbles, and
directory resources become named lists keyed by package-relative path.

Search traverses nested metadata and reports the resource, record,
complete field path, value, and context for each match:

``` r

glc_search_metadata(iztech, "light exposure")
#> # A tibble: 153 × 5
#>    resource record field                                           value context
#>    <chr>     <int> <chr>                                           <chr> <chr>  
#>  1 study         1 study_title                                     Pers… record…
#>  2 study         1 study_keywords                                  ligh… record…
#>  3 datasets      1 dataset_variable_terms.label                    Ligh… record…
#>  4 datasets      1 dataset_file.dataset_file_variables.dataset_fi… Wear… record…
#>  5 datasets      1 dataset_file.dataset_file_variables.dataset_fi… It i… record…
#>  6 datasets      1 dataset_file.dataset_file_variables.dataset_fi… At t… record…
#>  7 datasets      1 dataset_file.dataset_file_instructions          Comp… record…
#>  8 datasets      1 dataset_file.dataset_file_instrument.instrumen… Ligh… record…
#>  9 datasets      1 dataset_file.dataset_file_variables.dataset_fi… Addi… record…
#> 10 datasets      1 dataset_file.dataset_file_variables.dataset_fi… How … record…
#> # ℹ 143 more rows

glc_search_metadata(
  iztech,
  "age",
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

glc_search_metadata(
  iztech,
  "meq_type",
  resources = "participant_characteristics"
)
#> # A tibble: 34 × 5
#>    resource                    record field                        value context
#>    <chr>                        <int> <chr>                        <chr> <chr>  
#>  1 participant_characteristics      2 participant_characteristic_… meq_… record…
#>  2 participant_characteristics      2 participant_characteristic_… Deri… record…
#>  3 participant_characteristics     25 participant_characteristic_… meq_… record…
#>  4 participant_characteristics     25 participant_characteristic_… Deri… record…
#>  5 participant_characteristics     48 participant_characteristic_… meq_… record…
#>  6 participant_characteristics     48 participant_characteristic_… Deri… record…
#>  7 participant_characteristics     71 participant_characteristic_… meq_… record…
#>  8 participant_characteristics     71 participant_characteristic_… Deri… record…
#>  9 participant_characteristics     94 participant_characteristic_… meq_… record…
#> 10 participant_characteristics     94 participant_characteristic_… Deri… record…
#> # ℹ 24 more rows

glc_search_metadata(
  iztech,
  "Izmir",
  resources = "study",
  fields = "study_geographical_location"
)
#> # A tibble: 1 × 5
#>   resource record field                       value               context 
#>   <chr>     <int> <chr>                       <chr>               <chr>   
#> 1 study         1 study_geographical_location Izmir-Türkiye, city record 1
```

## Work with local packages

Local packages use the same public interface, which makes them useful
for development, validation follow-up, and offline analysis:

``` r

iztech_local <- glc_open("path/to/iztech-subset", quiet = TRUE)
glc_summary(iztech_local)
glc_files(iztech_local, available = FALSE)
```

The directory must contain a `datapackage.json` descriptor and all paths
needed by the selected operation. A subset created by
[`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md)
can be reopened in exactly the same way.

After selecting and collecting compatible light data, use the [LightLogR
function
reference](https://tscnlab.github.io/LightLogR/reference/index.html) for
downstream quality checks, summaries, metrics, and visualizations.
