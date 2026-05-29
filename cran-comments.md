## Test environments
* macOS (devel and release)
* windows (devel and release)
* Ubuntu (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

All reverse dependencies have been carefully checked. No broken packages except:
	- Five packages, namely GREENeR, pct, rsat, spatialrisk, and waterquality do not pass the check because they still use the deprecated function tm_scale_bar(). This function has been deprecated as of tmap 4.0 (2025-01-28). Package maintainers have been contacted about this.
	- tmap.networks also does not pass, which is expected due to an internal change within tmap. I'll submit a newer version of tmap.networks once tmap has been accepted.
