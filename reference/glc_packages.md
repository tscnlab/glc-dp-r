# List registered Global Light Commons data packages

Downloads and flattens the Global Light Commons registry. Both passing
and non-passing current revisions are retained.

## Usage

``` r
glc_packages(registry = glc_default_registry(), refresh = FALSE)
```

## Arguments

- registry:

  Registry JSON URL or path. Defaults to the official registry or the
  value of option `glcdp.registry_url`.

- refresh:

  Whether to bypass the in-session registry cache.

## Value

A `glc_registry` tibble with one row per registered repository.

## Examples

``` r
if (FALSE) { # interactive()
packages <- glc_packages()
glc_search_packages("guidolin", packages)
}
```
