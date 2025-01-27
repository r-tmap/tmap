## Test environments
* macOS Ventura 13.3.1, R 4.2.2
* win-builder (devel and release)

## Submission note

* Major release
* All reverse dependencies have been carefully checked
* We did our utmost best to keep the package as small as possible (e.g. vignettes are only online)

## Resubmission note

* Running time examples decreased

## Reresubmission note

* Running time examples further decreased

## Reverse dependency checks

* MazamaSpatialPlots: contacted the author about this issue. He promised to submit an update to resolve this once tmap 4.0 is on CRAN.
* spatialrisk: to solve this, we've moved servr from Suggests to Imports.
* mapping: this is a minor note due to the change of argument names ("col" has been renamed to "color"). It does not effect the output.

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

All reverse dependencies run OK
