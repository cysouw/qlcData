\name{join.align}
\alias{join.align}
\title{
Join various multialignments into one combined dataframe
}
\description{
Multialignments are mostly stored in separate files per cognateset. This function can be used to bind together a list of multialignments read by \link{read.align}.
}
\usage{
join.align(alignments)
}
\arguments{
  \item{alignments}{
A list of objects as returned from \link{read.align}.
}
}
\details{
The alignments have to be reordered for this to work properly. Also, duplicate data (i.e. multiple words from the same language) will be removed. Simply the first occurrence is retained. This is not ideal, but it is currently the best and easiest solution.
}
\value{
The result will be a dataframe with \code{doculects} as rows and \code{alignments} as columns.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
