# Open a Global Light Commons data package

Opens a local package or resolves a GitHub-hosted package at an
immutable commit. Registered packages default to their latest passing
revision.

## Usage

``` r
glc_open(
  source,
  ref = "latest_pass",
  token = NULL,
  cache_dir = NULL,
  registry = NULL,
  quiet = FALSE
)
```

## Arguments

- source:

  Registry id, `owner/repository`, GitHub URL, local package directory,
  or local `datapackage.json` path.

- ref:

  Remote revision: `"latest_pass"`, `"current"`, or an exact
  40-character commit SHA.

- token:

  Optional GitHub token. When omitted, `GITHUB_PAT` and then
  `GITHUB_TOKEN` are consulted.

- cache_dir:

  Optional explicit persistent cache directory. The default uses
  session-temporary storage for remote reads.

- registry:

  Optional registry object, URL, or local JSON path.

- quiet:

  Suppress informational messages. Warnings remain visible.

## Value

A `glc_package` handle.

## Examples

``` r
if (FALSE) { # interactive()
package <- glc_open("tscnlab/guidolin-glee-datasetv2")
package
}
```
