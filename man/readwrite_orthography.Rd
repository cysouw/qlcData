\name{write.profile}
\alias{write.profile}
\alias{read.profile}
\title{
Writing (and reading) of an orthography profile skeleton
}
\description{
To process strings, it is often very useful to tokenise them into graphemes (i.e. functional units of the orthography), and possibly replace those graphemes by other symbols to harmonize the orthographic representation of different orthographic representations (`transcription'). As a quick and easy way to specify, save, and document the decisions taken for the tokenization, we propose using an orthography profile. 

Provided here is a function to prepare a skeleton for an orthography profile. This function takes some strings and lists detailed information on the Unicode characters in the strings.
}
\usage{
write.profile(strings, 
    normalize = NULL, info = TRUE, editing = FALSE, sep = NULL, 
    file.out = NULL, collation.locale = NULL)

read.profile(profile)
}
\arguments{
  \item{strings}{
  A vector of strings on which to base the orthography profile. It is also possibly to pass a filename, which will then simply be read as \code{scan(strings, sep = "\n", what = "character")}.
}
  \item{normalize}{
  Should any unicode normalization be applied before making a profile? By default, no normalization is applied, giving direct feedback on the actual encoding as observed in the strings. Other options are \code{NFC} and \code{NFD}. In combination with \code{sep} these options can lead to different insights into the structure of your strings (see examples below).
}
  \item{info}{
  Add columns with Unicode information on the graphemes: Unicode code points, Unicode names, and frequency of occurrence in the input strings.
}
  \item{editing}{
  Add empty columns for further editing of the orthography profile: left context, right context, class, and translitation. See \code{\link{tokenize}} for detailed information on their usage.
}
  \item{sep}{
  separator to separate the strings. When NULL (by default), then unicode character definitions are used to split (as provided by UCI, ported to R by \code{stringi::stri_split_boundaries}. When \code{sep} is specified, strings are split by this separator. Often useful is \code{sep = ""} to split by unicode codepoints (see examples below).
}
  \item{file.out}{
  Filename for writing the profile to disk. When \code{NULL} the profile is returned as an R dataframe consisting of strings. When \code{file.out} is specified (as a path to a file), then the profile is written to disk and the R dataframe is returned invisibly.
}
  \item{collation.locale}{
  Specify to ordering to be used in writing the profile. By default it uses the ordering as specified in the current locale (check \code{Sys.getlocale("LC_COLLATE")}).
  }
  \item{profile}{
  An orthography profile to be read. Has to be a tab-delimited file with a header. There should be at least a column called "Grapheme".
  }
}
\details{
String are devided into default grapheme clusters as defined by the Unicode specification. Underlying code is due to the UCI as ported to R in the \code{stringi} package.
}
\value{
A dataframe with strings representing a skeleton of an orthography profile.
}
\note{
There is a bash-executable distributed with this package (based on the \code{docopt} package) that let you use this function directly in a bash-terminal. The easiest way to use this executable is to softlink the executable to some directory in your bash PATH, for example \code{/usr/local/bin} or simply \code{~/bin}. To softlink the function \code{tokenize} to this directory, use something like the following in your bash terminal:

\code{ln -is `Rscript -e 'cat(system.file("exec/writeprofile", package="qlcData"))'` ~/bin}

From within R your can also use the following (again, optionally changing the linked-to directory from \code{~/bin} to anything more suitable on your system):

\code{file.symlink(system.file("exec/writeprofile", package="qlcData"), "~/bin")}
}
\references{
Moran & Cysouw (2018) "The Unicode cookbook for linguists". Language Science Press. <doi:10.5281/zenodo.1296780>.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
\code{\link{tokenize}}
}
\examples{
# produce statistics, showing two different kinds of "A"s in Unicode.
# look at the output of "example" in the console to get the point!
(example <- "\u0041\u0391\u0410")
write.profile(example)

# note the differences. Again, look at the example in the console!
(example <- "\u00d9\u00da\u00db\u0055\u0300\u0055\u0301\u0055\u0302")
# default settings
write.profile(example)
# split according to unicode codepoints
write.profile(example, sep = "")
# after NFC normalization unicode codepoints have changed
write.profile(example, normalize = "NFC", sep = "")
# NFD normalization gives yet another structure of the codepoints
write.profile(example, normalize = "NFD", sep = "")
# note that NFC and NFD normalization are identical under unicode character definitions!
write.profile(example, normalize = "NFD")
write.profile(example, normalize = "NFC")
}
