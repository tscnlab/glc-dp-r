# Explore Global Light Commons data packages

Launches a local Shiny application for browsing the GLC registry,
opening the immutable latest passing revision of a package, reviewing
its contents, and filtering participants, devices, datasets, file
groups, and variables. The app can preview the resulting selection and
export an annotated, reproducible R script without uploading package
data to another service.

## Usage

``` r
glc_explore(
  registry = NULL,
  launch.browser = getOption("shiny.launch.browser", interactive()),
  ...
)
```

## Arguments

- registry:

  Optional registry JSON URL or local path. Defaults to the official
  registry or the value of option `glcdp.registry_url`.

- launch.browser:

  Whether to open the application in a browser.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Called for its side effect of running a Shiny application.

## See also

The [Shiny app
workflow](https://tscnlab.github.io/glc-dp-r/articles/glc-data-explorer.md).

## Examples

``` r
if (FALSE) { # interactive()
glc_explore()
}
```
