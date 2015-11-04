\name{pass_align}
\alias{pass_align}
\title{
Transfer alignment from one string to another
}
\description{
In the alignment of linguistic strings, it is often better to perform the alignment on a simplified string. This function allows to pass back the alignment from the simplified string to the original
}
\usage{
pass_align(originals, alignment, gap = "-", sep = " ")
}
\arguments{
  \item{originals}{
Vector of strings in the original form, with separators
}
  \item{alignment}{
Vector of simplified strings after alignment, with separators and gaps. The number of non-gap parts should match the number of parts of the originals
}
  \item{gap}{
Symbol used as gap indicator
}
  \item{sep}{
Symbol used as separator between parts of the strings
}
}
\details{
Given some strings, a sound (or graphemic) alignment inserts gaps into the strings in such a way as to align the columns between different strings. We assume here an original string that is separated by \code{sep} into parts (segments, sounds, tailored grapheme clusters). After simplification (e.g. through \code{\link{tokenize}}) and alignment (currently using non-R software) a string is retuned with extra \code{gap}s inserted. The number of non-gap parts should match the original string.
}
\value{
Vector of original strings with the gaps inserted from the aligned strings.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
# make some strings with separators
l <- list(letters[1:3], letters[4:7], letters[10:15])
originals <- sapply(l, paste, collapse = " ")
cbind(originals)

# make some alignment
# note that this alignment is non-sensical!
alignment <- c("X - - - X - X", "X X - - - X X", "X X X - X X X")
cbind(alignment)

# match originals to the alignment
transferred <- pass_align(originals, alignment)
cbind(transferred)

# ========

# a slighly more interesting example
# using the bare-bones pairwise alignment from adist()
originals <- c("cute kitten class","utter tentacles")

# adist returns strings of operations, "I" signals insertion
(levenshtein <- attr(adist(originals, counts = TRUE), "trafos"))

# get only the interesting, show the insertions as gaps
alignment <- c(levenshtein[1,2], levenshtein[2,1])
transferred <- pass_align(originals, alignment, gap = "I", sep = "")
cbind(gsub("I", "-", transferred))

}
