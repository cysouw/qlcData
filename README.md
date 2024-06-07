[![version](https://www.r-pkg.org/badges/version/qlcData)](https://CRAN.R-project.org/package=qlcData)
![downloads](https://cranlogs.r-pkg.org/badges/qlcData)
[![DOI](https://zenodo.org/badge/19068/cysouw/qlcData.svg)](https://zenodo.org/badge/latestdoi/19068/cysouw/qlcData)

qlcData
==========

**Functions for data managements in Quantitative Language Comparison**

The package combines various methods to deal with data in language comparison, and it is intended to grow in the future to allow different datasets to be used and compared.

It consists of various read and write functions to import and produce different kinds of data.

When using external data, there are often various tweaks that one would like to perform before using the data for further research. This package offers assistance for some common recoding problems occurring for nominal data with the function `recode`. Please see the vignette for a detailed explanation of the intended usage.

To process strings, it is often very useful to tokenize them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations ('transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile. Function to write and read orthography profiles are provided in this package. The main function `tokenize` can check orthography profiles against data, and tokenize data into (tailored) graphemes according to orthography profiles.

This is an early alpha version, but it should function. You can download the package directly from CRAN. Have a look at the examples in the help files and at the vignettes to get an idea how to use the package:

    install.packages("qlcData")

If you want to have the latest changes, it is pretty easy to install this package directly from github into R by using:

    install.packages("devtools")
    devtools::install_github("cysouw/qlcData")

There are vignettes trying to explain the intended usage of this package. Unfortunately, the vignette will not by build when you install this package. You can try the following, but it might throw an error:

    devtools::install_github("cysouw/qlcData", build_vignettes = TRUE)
    vignette("orthography_processing")
    vignette("recoding_nominal_data")

A few functions are available through a bash terminal. You will have to manually softlink these interfaces to you PATH, for example to link the function `tokenize` to `/usr/local/bin/` use something like:

    ln -is `Rscript -e 'cat(file.path(find.package("qlcData"), "exec", "tokenize"))'` /usr/local/bin

All available executables are `tokenize`, `writeprofile` and `pass_align`

Michael Cysouw
cysouw@mac.com
