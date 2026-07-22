# Discover and inspect data packages

This article shows how to choose a Global Light Commons package and
understand its contents before downloading or importing measurements.
Remote examples run on the pkgdown website but remain unevaluated in
ordinary package and CRAN builds. Set `GLCDP_SKIP_LIVE=true` to request
an offline website build.

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
#> Generated: 2026-07-22T07:19:05.302619+00:00 
#> # A tibble: 2 × 5
#>   id              repository current_status has_latest_pass attestation_verified
#>   <chr>           <chr>      <chr>          <lgl>           <lgl>               
#> 1 guidolin-glee-… tscnlab/g… pass           TRUE            TRUE                
#> 2 demo-glee-data… tscnlab/d… fail           FALSE           TRUE
```

Registry results are cached for the R session. Set `refresh = TRUE` only
when you need to fetch the registry again.

``` r

packages <- glc_packages(refresh = TRUE)
```

Searches are fixed and case-insensitive by default:

``` r

glc_search_packages("guidolin", packages)
#> <GLC registry>
#> Generated: 2026-07-22T07:19:05.302619+00:00 
#> <GLC registry>
#> Generated: 2026-07-22T07:19:05.302619+00:00 
#> # A tibble: 1 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 guidolin-gl… tscnlab/g… main   active            pass           8ec9034a3d967…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
glc_search_packages(packages = packages, status = c("pass", "fail"))
#> <GLC registry>
#> Generated: 2026-07-22T07:19:05.302619+00:00 
#> <GLC registry>
#> Generated: 2026-07-22T07:19:05.302619+00:00 
#> # A tibble: 2 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 guidolin-gl… tscnlab/g… main   active            pass           8ec9034a3d967…
#> 2 demo-glee-d… tscnlab/d… main   active            fail           6f805a86164f2…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
glc_search_packages(packages = packages, has_pass = FALSE)
#> <GLC registry>
#> Generated: 2026-07-22T07:19:05.302619+00:00 
#> <GLC registry>
#> Generated: 2026-07-22T07:19:05.302619+00:00 
#> # A tibble: 1 × 17
#>   id           repository branch repository_status current_status current_commit
#>   <chr>        <chr>      <chr>  <chr>             <chr>          <chr>         
#> 1 demo-glee-d… tscnlab/d… main   active            fail           6f805a86164f2…
#> # ℹ 11 more variables: current_validator <chr>, current_validated_at <chr>,
#> #   current_errors <int>, current_warnings <int>, latest_pass_commit <chr>,
#> #   latest_pass_validator <chr>, latest_pass_validated_at <chr>,
#> #   has_latest_pass <lgl>, is_current_pass <lgl>, attestation_verified <lgl>,
#> #   registry_generated_at <chr>
```

## Choose a revision deliberately

Opening a registered package with the default `ref = "latest_pass"`
selects an exact passing commit, not a moving branch:

``` r

x <- glc_open("tscnlab/guidolin-glee-datasetv2")
x
#> <GLC data package>
#> Source: tscnlab/guidolin-glee-datasetv2@8ec9034a3d96
#> Schema: 2.0.0
#> Registry revision: verified
```

Use `ref = "current"` when you explicitly need the registry’s current
revision. If that revision is not passing, `glcdp` warns. You can also
provide an exact 40-character commit SHA; commits that are not selected
through a registry record are marked as unverified.

``` r

current <- glc_open(
  "tscnlab/guidolin-glee-datasetv2",
  ref = "current"
)
current
#> <GLC data package>
#> Source: tscnlab/guidolin-glee-datasetv2@8ec9034a3d96
#> Schema: 2.0.0
#> Registry revision: verified

registry_row <- glc_search_packages("guidolin", packages)
registry_row$repository[[1]]
#> [1] "tscnlab/guidolin-glee-datasetv2"
registry_row$latest_pass_commit[[1]]
#> [1] "8ec9034a3d967580cb8c1b04e629cbbe7e502e6e"

pinned <- glc_open(
  registry_row$repository[[1]],
  ref = registry_row$latest_pass_commit[[1]]
)
pinned
#> <GLC data package>
#> Source: tscnlab/guidolin-glee-datasetv2@8ec9034a3d96
#> Schema: 2.0.0
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

glc_summary(x)
#> <GLC package summary>
#> Schema: 2.0.0
#> Studies: 1 | Datasets: 2 | Participants: 3
#> File groups: 2 | Files: 2 | Variables: 35
```

## Explore inventories

The inventories are tibbles, so they can be printed, filtered, or joined
using ordinary data-frame tools.

``` r

glc_resources(x)
#> # A tibble: 7 × 10
#>   resource path  core  directory format media_type profile schema_path delimiter
#>   <chr>    <chr> <lgl> <lgl>     <chr>  <chr>      <chr>   <chr>       <chr>    
#> 1 study    data… TRUE  FALSE     NA     applicati… schema… schemas/2.… NA       
#> 2 partici… data… TRUE  FALSE     json   applicati… tabula… schemas/2.… NA       
#> 3 partici… data… TRUE  FALSE     csv    text/csv   tabula… schemas/2.… NA       
#> 4 datasets data… TRUE  FALSE     NA     applicati… schema… schemas/2.… NA       
#> 5 devices  data… TRUE  FALSE     NA     applicati… schema… schemas/2.… NA       
#> 6 device_… data… TRUE  TRUE      NA     applicati… schema… schemas/2.… NA       
#> 7 light_d… data… FALSE FALSE     csv    text/csv   tabula… schemas/li… ;        
#> # ℹ 1 more variable: decimal_mark <chr>
```

[`glc_datasets()`](https://tscnlab.github.io/glc-dp-r/reference/glc_datasets.md)
describes logical datasets and their associations. Once you have a
dataset id, reuse it to narrow the other inventories.

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
glc_files(
  x,
  dataset_id = "DS001",
  modality = "light",
  available = TRUE
)
#> # A tibble: 1 × 19
#>   dataset_id file_group file_group_id participant_id study_id path              
#>   <chr>           <int> <chr>         <chr>          <chr>    <chr>             
#> 1 DS001               1 DS001:1       201            CG2024   data/datasets/201…
#> # ℹ 13 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, role <chr>, data_state <chr>, modalities <list>,
#> #   device_id <chr>, storage <chr>, expected_bytes <dbl>, lfs_oid <chr>,
#> #   blob_sha <chr>, available <lgl>
```

File inventories expose both declared and resolved paths, format,
encoding, time zone, role, data state, device, storage type, expected
size, and availability. Git LFS-backed files are identified without
requiring a local Git LFS installation.

Variable inventories can be narrowed by dataset, file group, semantic
term, or primary status:

``` r

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
glc_variables(x, dataset_id = "DS001", primary = TRUE)
#> # A tibble: 1 × 13
#>   dataset_id file_group file_group_id name  label    unit  type  term  term_name
#>   <chr>           <int> <chr>         <chr> <chr>    <chr> <chr> <chr> <chr>    
#> 1 DS001               1 DS001:1       LIGHT Photopi… lx    guess phot… NA       
#> # ℹ 4 more variables: calibration <chr>, primary <lgl>, factor_values <list>,
#> #   factor_labels <list>
glc_variables(x, term = "photopic illuminance")
#> # A tibble: 2 × 13
#>   dataset_id file_group file_group_id name  label    unit  type  term  term_name
#>   <chr>           <int> <chr>         <chr> <chr>    <chr> <chr> <chr> <chr>    
#> 1 DS001               1 DS001:1       LIGHT Photopi… lx    guess phot… NA       
#> 2 DS002               1 DS002:1       lux   Photopi… lux   guess phot… NA       
#> # ℹ 4 more variables: calibration <chr>, primary <lgl>, factor_values <list>,
#> #   factor_labels <list>
```

Use source variable names with the `variables` argument of
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
and semantic terms with its `terms` argument.

## Load and search metadata

With no `resources` argument,
[`glc_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/glc_metadata.md)
loads the core resources that the package declares. Requesting resources
explicitly is often faster and makes dependencies clearer.

``` r

metadata <- glc_metadata(x, resources = c("study", "participants"))
names(metadata)
#> [1] "study"        "participants"
metadata$study
#> # A tibble: 1 × 14
#>   study_internal_id study_title study_ethics study_short_descript…¹ study_sample
#>   <chr>             <chr>       <chr>        <chr>                  <chr>       
#> 1 CG2024            Near-corne… Ethics Comm… In this study, we mea… Participant…
#> # ℹ abbreviated name: ¹​study_short_description
#> # ℹ 9 more variables: study_groups <list>, study_setting <chr>,
#> #   study_geographical_location <chr>, study_datasets <list>, study_type <chr>,
#> #   study_funding_sources <list>, study_keywords <list>,
#> #   study_contributors <list>, schema_version <chr>
metadata$participants
#> # A tibble: 3 × 4
#>   participant_internal_id participant_age participant_sex participant_gender
#>   <chr>                             <int> <chr>           <chr>             
#> 1 201                                  29 male            ""                
#> 2 P001                                 34 female          ""                
#> 3 P003                                 22 other           NA
```

JSON objects remain lists, tabular resources become tibbles, and
directory resources become named lists keyed by package-relative path.

Search traverses nested metadata and reports the resource, record,
complete field path, value, and context for each match:

``` r

glc_search_metadata(x, "light exposure")
#> # A tibble: 2 × 5
#>   resource record field                   value                          context
#>   <chr>     <int> <chr>                   <chr>                          <chr>  
#> 1 study         1 study_title             Near-corneal plane light expo… record…
#> 2 study         1 study_short_description In this study, we measured li… record…

glc_search_metadata(
  x,
  "age|chronotype",
  resources = "participant_characteristics",
  fixed = FALSE
)
#> # A tibble: 1 × 5
#>   resource                    record field                         value context
#>   <chr>                        <int> <chr>                         <chr> <chr>  
#> 1 participant_characteristics      1 participant_characteristic_n… Chro… record…

city_fields <- glc_search_metadata(
  x,
  "city",
  resources = "study",
  search_in = "fields"
)
unique(city_fields$field)
#> [1] "study_contributors.contributor_institution.contributor_institution_city"

glc_search_metadata(
  x,
  "Munich",
  resources = "study",
  fields = "contributor_institution_city"
)
#> # A tibble: 3 × 5
#>   resource record field                                            value context
#>   <chr>     <int> <chr>                                            <chr> <chr>  
#> 1 study         1 study_contributors.contributor_institution.cont… Muni… record…
#> 2 study         1 study_contributors.contributor_institution.cont… Tueb… record…
#> 3 study         1 study_contributors.contributor_institution.cont… Tueb… record…
```

## Work with local packages

Local packages use the same public interface, which makes them useful
for development, validation follow-up, and offline analysis:

``` r

x_local <- glc_open("path/to/package", quiet = TRUE)
glc_summary(x_local)
glc_files(x_local, available = FALSE)
```

The directory must contain a `datapackage.json` descriptor and all paths
needed by the selected operation. A subset created by
[`glc_download()`](https://tscnlab.github.io/glc-dp-r/reference/glc_download.md)
can be reopened in exactly the same way.

After selecting and collecting compatible light data, use the [LightLogR
function
reference](https://tscnlab.github.io/LightLogR/reference/index.html) for
downstream quality checks, summaries, metrics, and visualizations.
