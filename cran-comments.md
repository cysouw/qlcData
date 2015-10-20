## This is a resubmission

I corrected the following errors

* Changed the Title field to be in title case
* Non-standard file/directory has been removed from the top level
* Added imports for undefined global functions or variables

## Test environments
* local OS X 10.11 install, R 3.2.2
* CRAN winbuilder via devtools

## R CMD check results
There were no ERRORs or WARNINGs. 

There is one remaining NOTE under windows only, which I am unable to solve. I have tested many different possibilities, but the note remains on windows when using the winbuilder, and I have no idea where the problem arises.

    Error in re-building vignettes:
     ...
    Warning in native_encode(text) :
     some characters may not work under the current locale
    Quitting from lines 110-111 (orthography_processing.Rmd)
    Error: processing vignette 'orthography_processing.Rmd' failed with diagnostics:
    undefined columns selected
    Execution halted

I have tried to remove all non-ASCII characters from this vignette, but the problem remained. In the current submission I have reinserted all non-ASCII character, because they are an integral part of the vignette.

## Downstream dependencies
There are no downstream dependencies (yet)