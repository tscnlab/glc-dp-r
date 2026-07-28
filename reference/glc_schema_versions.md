# Report supported GLC schema versions

Report supported GLC schema versions

## Usage

``` r
glc_schema_versions()
```

## Value

A tibble describing support status for each schema version.

## Examples

``` r
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
