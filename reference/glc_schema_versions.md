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
#> # A tibble: 3 × 3
#>   version status       notes                                              
#>   <chr>   <chr>        <chr>                                              
#> 1 1.0.0   stable       Legacy packages may omit the root schema version.  
#> 2 2.0.0   stable       Current released schema supported by the validator.
#> 3 3.0.0   experimental Follows the schema-3.0.0-development branch.       
```
