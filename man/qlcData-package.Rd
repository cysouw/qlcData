\name{qlcData-package}
\alias{qlcData-package}
\alias{qlcData}
\docType{package}
\title{
Processing data for quantitative language comparison (QLC)
}
\description{
The package offers various functions to read, transcode and process data. There are many different function to read in data. Also a general framework to recode nominal data is included. Further, there is a general approach to describe orthographic systems through so-called Orthography Profiles. It offers functions to write such profiles based on some actual written text, and to test and correct profiles given concrete data. The main end-use is to produce tokenized texts in so-called tailored grapheme clusters.
}
\details{
\tabular{ll}{
Package: \tab qlcData\cr
Type: \tab Package\cr
Version: \tab 0.2.1\cr
Date: \tab 2018-01-05\cr
License: \tab GPL-3\cr
}
Various functions to read specific data formats of QLC are documented in \code{\link{read_align}}, \code{\link{read.profile}}, \code{\link{read.recoding}}.

The \code{\link{recode}} function allows for an easy and transparent way to specify a recoding of an existing nominal dataset. The specification of the recoding-decisions is preferably saved in an easily accessible YAML-file. There are utility function \code{\link{write.profile}} for writing and reading such files included.

For processing of strings using orthography profiles, the central function is \code{\link{tokenize}}. A basic sceleton for an orthography profile can be produced with \code{\link{write.profile}}
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\keyword{ package }

