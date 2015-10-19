\name{read.align}
\alias{read.align}
\title{
Reading different versions of linguistic multialignments.
}
\description{
Multialignments of strings are a central step for historical linguistics (quite similar to multialignments in bioinformatics). There is no consensus (yet) about the file-structure for multialignments in linguistics. Currently, this functions offers to read various flavours of multialignment, trying to harmonize the internal R-structure.
}
\usage{
read.align(file, flavor)
}
\arguments{
  \item{file}{
Multialignment to be read
}
  \item{flavor}{
Currently two flavours are implemented \code{"PAD"} and \code{"BDPA"}
}
}
\details{
The flavor \code{"PAD"} refers to the Phonetische Atlas Deutschlands, which provides multialignments for german dialects. The flavor \code{"BDPA"} refers to the Benchmark Database for Phonetic Alignments.
}
\value{
Multialignment-files often contain various different kinds of information. An attempt is made to turn the data into a list with the following elements:
\item{meta}{: Metadata}
\item{align}{: The actual alignments as a dataframe. When IDs are present in the original file, they are used as rownames. Some attempt is made to add useful column names.}
\item{doculects}{: The rows of the alignment normally are some kind of doculects ("languages", "dialects"). However, because these doculects might occur more than once (when two different, but cognate words from a languages are included) these names are not used as rownames of \code{$align}, but presented separately here.}
\item{annotations}{: The columns of a multialignment can have annotations, e.g. metathesis or orthographic standard. These annotations are saved here as a dataframe with the same number of columns as the \code{$align} dataframe. The name of the annotation is put in the rownames.}
}
\references{
BDPA is available at \url{http://alignments.lingpy.org}. PAD is available at \url{http://github.com/cysouw/PAD/}
}
\author{
Michael Cysouw <cysouw@mac.com>
}
