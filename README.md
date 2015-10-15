qlcTokenize
==========

**Functions for orthography processing in Quantitative Language Comparison (QLC)**

To process strings, it is often very useful to tokenize them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations ('transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile. Function to write and read orthography profiles are provided in this package. The main function 'tokenize' can check orthography profiles against data, and tokenize data into (tailored) graphemes according to orthography profiles.

This is an early alpha version, not yet available on CRAN. However, it is pretty easy to install this package directly from github into R by using:

    install.packages("devtools")
    devtools::install_github("cysouw/qlcTokenize")

There is a vignette trying to explain the intended usage of this package. Unfortunately, the vignette will not by build when you install this package. You can try the following, but it will probably throw an error:

    devtools::install_github("cysouw/qlcTokenize", build_vignettes = TRUE)
    vignette("tokenize")

In the meantime, you can read the vignette directly here on github by looking in the folder "vignettes" above, using the files with suffix ".md".

Note that the current vignette relates to the old version of the functions. The new versions do not yet have a vignette.

Michael Cysouw
cysouw@mac.com