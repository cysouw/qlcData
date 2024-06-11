## New submission
  
Package was archived on CRAN

## Comments to review

- literature reference added to DESCRIPTION
- handling of par() improved as suggested
- \dontrun replaced by \donttest
- The use of "<<-" does not change anything in the global environment, no change
  
## This is a minor update to get this package back on CRAN

This package was archived on 2020-04-09 as check problems were not corrected.
This update should finally resolve those issues.

## Test environments

- devtools::check(remote = TRUE, manual = TRUE) on local macOS 14.5 install, R version 4.4.0
- devtools::check_win_devel() for Windows on r-devel
- rhub::rhub_check() for Linux, Windows and old macOS

## R CMD check results

There were 0 errors, 0 warnings, 1 note

winbuilder finds a NOTE: possibly misspelled words in DESCRIPTION:

- recode
- recoding
- transcode

Oxford Dictionary does include these words, so they are left as is:

- https://www.oed.com/dictionary/recode_v
- https://www.oed.com/dictionary/transcode_v

## Downstream dependencies

checked via revdepcheck::revdep_check() on local macOS X 14.5
None found