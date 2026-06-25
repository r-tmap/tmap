## Test environments
* macOS (devel and release)
* windows (devel and release)
* Ubuntu (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## Release notes

This is a documentation-only patch release (4.4-1) that addresses a JSS
pre-check comment: several man-page titles in `help(package = "tmap")` were
uninformative or duplicated (e.g. `tm_check_fix` and `tmap_options` both titled
"tmap options"). No functional code has changed.

## Reverse dependencies

Carried forward from 4.4: tmap.glyphs triggered a warning ('::' or ':::' import
not declared from: 'units'), which will be fixed in a tmap.glyphs update after
acceptance.
