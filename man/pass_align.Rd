\name{pass_align}
\alias{pass_align}
\title{
Transfer alignment from one string to another
}
\description{
In the alignment of linguistic strings, it is often better to perform the alignment on a simplified string. This function allows to pass back the alignment from the simplified string to the original
}
\usage{
pass_align(originals, alignment, sep = " ", in.gap = "-", out.gap = "-")
}
\arguments{
  \item{originals}{
Vector of strings in the original form, with separators
}
  \item{alignment}{
Vector of simplified strings after alignment, with separators and gaps. The number of non-gap parts should match the number of parts of the originals
}
  \item{sep}{
Symbol used as separator between parts of the strings
}
  \item{in.gap}{
Symbol used as gap indicator in the alignments
}
  \item{out.gap}{
Symbol used as gap indicator in the output. This is useful when the gap symbol from the alignments occurs as character in the originals .
}
}
\details{
Given some strings, a sound (or graphemic) alignment inserts gaps into the strings in such a way as to align the columns between different strings. We assume here an original string that is separated by \code{sep} into parts (segments, sounds, tailored grapheme clusters). After simplification (e.g. through \code{\link{tokenize}}) and alignment (currently using non-R software) a string is retuned with extra \code{gap}s inserted. The number of non-gap parts should match the original string.
}
\value{
Vector of original strings with the gaps inserted from the aligned strings.
}
\note{
There is a bash-executable distributed with this package (based on the \code{docopt} package) that let you use this function directly in a bash-terminal. The easiest way to use this executable is to softlink the executable to some directory in your bash PATH, for example \code{/usr/local/bin} or simply \code{~/bin}. To softlink the function \code{tokenize} to this directory, use something like the following in your bash terminal:

\code{ln -is `Rscript -e 'cat(system.file("exec/pass_align", package="qlcData"))'` ~/bin}

From within R your can also use the following (again, optionally changing the linked-to directory from \code{~/bin} to anything more suitable on your system):

\code{file.symlink(system.file("exec/pass_align", package="qlcData"), "~/bin")}

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
cbind(originals)

# adist returns strings of pairwise Levenshtein operations
# "I" signals insertion
(levenshtein <- attr(adist(originals, counts = TRUE), "trafos"))

# pass alignments to original strings, show the insertions as "-" gaps
alignment <- c(levenshtein[1,2], levenshtein[2,1])
transferred <- pass_align(originals, alignment, 
    sep = "", in.gap = "I", out.gap = "-")
cbind(transferred)

}
