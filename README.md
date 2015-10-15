qlcData
==========

**Functions for data managements in Quantitative Language Comparison (QLC)**

The package combines various methods to deal with data in language comparison, and it is intended to grow in the future to allow different datasets to be used and compared.

It consists of various read and write functions to import and produce different kinds of data.

When using external data, there are often various tweaks that one would like to perform before using the data for further research. This package offers assistance for some common recoding problems occurring for nominal data with the function `recode`. Please see the vignette for a detailed explanation of the intended usage.

To process strings, it is often very useful to tokenize them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations ('transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile. Function to write and read orthography profiles are provided in this package. The main function `tokenize` can check orthography profiles against data, and tokenize data into (tailored) graphemes according to orthography profiles.

This is an early alpha version, not yet available on CRAN. However, it is pretty easy to install this package directly from github into R by using:

    install.packages("devtools")
    devtools::install_github("cysouw/qlcData")

There are vignettes trying to explain the intended usage of this package. Unfortunately, the vignette will not by build when you install this package. You can try the following, but it might throw an error:

    devtools::install_github("cysouw/qlcData", build_vignettes = TRUE)
    vignette("orthography_processing")
    vignette("recoding_nominal_data")

Michael Cysouw
cysouw@mac.com