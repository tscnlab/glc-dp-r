## Update

This updates glcdp from 1.0.0 to 1.1.0. It adds collection planning and
refinement, improves factor and device compatibility, and updates the default
registry URL. See NEWS.md for the complete changes.

## Test environments

* Local: macOS 27.0 arm64, R 4.6.1
* GitHub Actions:
  * macOS 26.6.2, R 4.6.1
  * Windows Server 2022, R 4.6.1
  * Ubuntu 24.04.5, R 4.6.1
  * Ubuntu 24.04.5, R 4.5.3
  * Ubuntu 24.04.5, R-devel (2026-09-25 r90590)

## R CMD check results

The built source tarball passed R CMD check --as-cran --run-donttest locally:

0 errors | 0 warnings | 0 notes

GitHub Actions also reported Status: OK on all five environments above:
https://github.com/tscnlab/glc-dp-r/actions/runs/36348400285

The check included examples, tests, rebuilt vignettes, and PDF and HTML
manuals. Documentation URL checks also passed. The optional live integration
test passed separately; it is skipped during CRAN checks.

## Reverse dependencies

CRAN lists no reverse dependencies, including suggested and enhanced
dependencies, as checked on 2026-09-27.
