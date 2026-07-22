# Get started with glcdp

`glcdp` provides a focused route from a Global Light Commons (GLC)
package to analysis-ready R data:

1.  discover a registered package;
2.  open an immutable revision;
3.  inspect its datasets, files, variables, and metadata;
4.  read only the data you need; and
5.  collect compatible file groups for analysis.

Remote examples run when pkgdown builds the package website. They are
displayed without execution during ordinary package and CRAN builds,
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
#> # A tibble: 3 × 3
#>   version status       notes                                              
#>   <chr>   <chr>        <chr>                                              
#> 1 1.0.0   stable       Legacy packages may omit the root schema version.  
#> 2 2.0.0   stable       Current released schema supported by the validator.
#> 3 3.0.0   experimental Follows the schema-3.0.0-development branch.
```

## Discover a package

The registry includes both passing and non-passing current revisions.
Keeping both visible makes validation status explicit instead of
silently hiding packages with problems.

``` r

packages <- glc_packages()
packages
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
```

You can also filter on validation status or on whether a package has a
recorded passing revision:

``` r

glc_search_packages(packages = packages, status = "pass")
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
glc_search_packages(packages = packages, has_pass = TRUE)
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
```

## Open a reproducible revision

Registered packages open at their latest passing commit by default. The
returned handle records the repository, exact commit, schema version,
and whether the revision was verified against the registry.

``` r

guidolin <- glc_open("tscnlab/guidolin-glee-datasetv2")
guidolin
#> <GLC data package>
#> Source: tscnlab/guidolin-glee-datasetv2@8ec9034a3d96
#> Schema: 2.0.0
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

glc_summary(guidolin)
#> <GLC package summary>
#> Schema: 2.0.0
#> Studies: 1 | Datasets: 2 | Participants: 3
#> File groups: 2 | Files: 2 | Variables: 35
```

The inventories make data selection explicit. List datasets, then narrow
the file and variable inventories to the dataset you intend to read.

``` r

glc_datasets(guidolin)
#> # A tibble: 2 × 13
#>   dataset_id schema_version study_id participant_id participant_associated
#>   <chr>      <chr>          <chr>    <chr>          <lgl>                 
#> 1 DS001      2.0.0          CG2024   201            TRUE                  
#> 2 DS002      2.0.0          CG2024   P001           TRUE                  
#> # ℹ 8 more variables: timezone <chr>, latitude <dbl>, longitude <dbl>,
#> #   file_group_count <int>, file_count <int>, modalities <list>,
#> #   device_ids <list>, primary_variables <list>
glc_files(guidolin, dataset_id = "DS001")
#> # A tibble: 1 × 19
#>   dataset_id file_group file_group_id participant_id study_id path              
#>   <chr>           <int> <chr>         <chr>          <chr>    <chr>             
#> 1 DS001               1 DS001:1       201            CG2024   data/datasets/201…
#> # ℹ 13 more variables: declared_path <chr>, format <chr>, encoding <chr>,
#> #   timezone <chr>, role <chr>, data_state <chr>, modalities <list>,
#> #   device_id <chr>, storage <chr>, expected_bytes <dbl>, lfs_oid <chr>,
#> #   blob_sha <chr>, available <lgl>
glc_variables(guidolin, dataset_id = "DS001", primary = TRUE)
#> # A tibble: 1 × 13
#>   dataset_id file_group file_group_id name  label    unit  type  term  term_name
#>   <chr>           <int> <chr>         <chr> <chr>    <chr> <chr> <chr> <chr>    
#> 1 DS001               1 DS001:1       LIGHT Photopi… lx    guess phot… NA       
#> # ℹ 4 more variables: calibration <chr>, primary <lgl>, factor_values <list>,
#> #   factor_labels <list>
```

Use
[`glc_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/glc_metadata.md)
for structured metadata and
[`glc_search_metadata()`](https://tscnlab.github.io/glc-dp-r/reference/glc_search_metadata.md)
when you need to locate a value without knowing its resource or field in
advance.

``` r

metadata <- glc_metadata(guidolin, resources = c("study", "participants"))
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

glc_search_metadata(guidolin, "light", resources = "study")
#> # A tibble: 3 × 5
#>   resource record field                   value                          context
#>   <chr>     <int> <chr>                   <chr>                          <chr>  
#> 1 study         1 study_title             Near-corneal plane light expo… record…
#> 2 study         1 study_short_description In this study, we measured li… record…
#> 3 study         1 study_keywords          longitundinal light logging    record…
glc_search_metadata(guidolin, "light", resources = "datasets")
#> # A tibble: 17 × 5
#>    resource record field                                           value context
#>    <chr>     <int> <chr>                                           <chr> <chr>  
#>  1 datasets      1 dataset_instructions                            Wear… record…
#>  2 datasets      1 dataset_file.dataset_file_variables.dataset_fi… LIGHT record…
#>  3 datasets      1 dataset_file.dataset_file_variables.dataset_fi… AMB … record…
#>  4 datasets      1 dataset_file.dataset_file_variables.dataset_fi… AMB … record…
#>  5 datasets      1 dataset_file.dataset_file_variables.dataset_fi… RED … record…
#>  6 datasets      1 dataset_file.dataset_file_variables.dataset_fi… RED … record…
#>  7 datasets      1 dataset_file.dataset_file_variables.dataset_fi… GREE… record…
#>  8 datasets      1 dataset_file.dataset_file_variables.dataset_fi… GREE… record…
#>  9 datasets      1 dataset_file.dataset_file_variables.dataset_fi… BLUE… record…
#> 10 datasets      1 dataset_file.dataset_file_variables.dataset_fi… BLUE… record…
#> 11 datasets      1 dataset_file.dataset_file_variables.dataset_fi… IR L… record…
#> 12 datasets      1 dataset_file.dataset_file_variables.dataset_fi… IR L… record…
#> 13 datasets      1 dataset_file.dataset_file_variables.dataset_fi… UVA … record…
#> 14 datasets      1 dataset_file.dataset_file_variables.dataset_fi… UVA … record…
#> 15 datasets      1 dataset_file.dataset_file_variables.dataset_fi… UVB … record…
#> 16 datasets      1 dataset_file.dataset_file_variables.dataset_fi… UVB … record…
#> 17 datasets      1 dataset_file.primary_variables                  LIGHT record…
```

## Read selected data

[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
follows the package metadata when it chooses headers, column types,
factor levels, datetime formats, decimal marks, encodings, and time
zones. A dataset selection is required so that a large package is not
imported accidentally.

``` r

collection <- glc_read(
  guidolin,
  dataset_id = "DS001",
  primary_only = TRUE
)
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

In interactive sessions,
[`glc_read()`](https://tscnlab.github.io/glc-dp-r/reference/glc_read.md)
displays progress across the selected files. Set `progress = FALSE` to
suppress the indicator, for example in a script that manages its own
progress reporting.

The result has one row per compatible file group and stores each
imported table in its `data` list-column. Inspect or process groups
separately when their roles, modalities, or schemas differ.

``` r

collection$data[[1]]
#> # A tibble: 60,043 × 6
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
#> # ℹ 60,033 more rows
#> # ℹ 1 more variable: .glc_datetime <dttm>
```

For quick exploration, limit the number of rows read from each file:

``` r

sample <- glc_read(
  guidolin,
  dataset_id = "DS001",
  variables = "LIGHT",
  n_max = 100
)
```

## Collect compatible groups

[`glc_collect()`](https://tscnlab.github.io/glc-dp-r/reference/glc_collect.md)
checks that the selected groups have compatible columns, types, time
zones, modalities, roles, data states, and datetime specifications
before combining them. Its default output maps the dataset id to `Id`,
the participant id to `participant_Id`, and adds `Datetime` and
`file.name`. Internal `.glc_*` provenance columns are removed from this
analysis-ready result, matching the core conventions described in
[LightLogR’s import
documentation](https://tscnlab.github.io/LightLogR/reference/import_Dataset.html).

``` r

light_data <- glc_collect(collection)
light_data
#> # A tibble: 60,043 × 6
#> # Groups:   Id [1]
#>    LIGHT Id    file_group_id participant_Id Datetime            file.name       
#>    <dbl> <fct> <chr>         <chr>          <dttm>              <chr>           
#>  1  809. DS001 DS001:1       201            2023-08-14 10:55:21 201_actlumus_Lo…
#>  2  806. DS001 DS001:1       201            2023-08-14 10:55:31 201_actlumus_Lo…
#>  3  738. DS001 DS001:1       201            2023-08-14 10:55:41 201_actlumus_Lo…
#>  4  736. DS001 DS001:1       201            2023-08-14 10:55:51 201_actlumus_Lo…
#>  5  716. DS001 DS001:1       201            2023-08-14 10:56:01 201_actlumus_Lo…
#>  6  736. DS001 DS001:1       201            2023-08-14 10:56:11 201_actlumus_Lo…
#>  7  753. DS001 DS001:1       201            2023-08-14 10:56:21 201_actlumus_Lo…
#>  8  719. DS001 DS001:1       201            2023-08-14 10:56:31 201_actlumus_Lo…
#>  9  713. DS001 DS001:1       201            2023-08-14 10:56:41 201_actlumus_Lo…
#> 10  771. DS001 DS001:1       201            2023-08-14 10:56:51 201_actlumus_Lo…
#> # ℹ 60,033 more rows
```

The result can be passed directly to LightLogR. Continue with its guides
to [visualizing light logger
data](https://tscnlab.github.io/LightLogR/articles/Visualizations.html)
or [calculating light exposure
metrics](https://tscnlab.github.io/LightLogR/articles/Metrics.html).

Use `standardize = "none"` to leave the source columns and `.glc_*`
provenance columns unchanged:

``` r

source_data <- glc_collect(sample, standardize = "none")
source_data
#> # A tibble: 100 × 6
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
#> # ℹ 90 more rows
#> # ℹ 1 more variable: .glc_datetime <dttm>
```

## Where to go next

- [Discover and inspect data
  packages](https://tscnlab.github.io/glc-dp-r/articles/discover-and-inspect.md)
  covers registry, revision, inventory, and metadata workflows.
- [Import and download
  data](https://tscnlab.github.io/glc-dp-r/articles/import-and-download.md)
  covers precise import selections, persistent downloads, and
  reproducibility manifests.
