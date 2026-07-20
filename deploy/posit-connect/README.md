# Posit Connect deployment

Use `deploy/posit-connect` as the content directory for a Git-backed Posit
Connect deployment. The checked-in `manifest.json` records the R version and
the exact package dependency sources used by the application.

Regenerate the manifest after changing the application or its dependencies:

```r
rsconnect::writeManifest(
  appDir = "deploy/posit-connect",
  appPrimaryDoc = "app.R",
  appMode = "shiny",
  dependencyResolution = "library"
)
```

Generate the manifest from a library in which `glcdp` was installed from a
GitHub commit containing the application. This ensures that Connect can
reinstall the package from a reachable source rather than a developer's local
filesystem.
