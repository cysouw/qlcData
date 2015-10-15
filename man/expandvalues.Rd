\name{expandValues}
\alias{expandValues}
\title{
Internal helper
}
\description{
produce combinations of nominal variables
}
\usage{
expandValues(attributes, data)
}
\arguments{
  \item{attributes}{
  a list of attributes to be recoded. Vectors (as elements of the list) are possible to specify combinations of attributes to be recoded as a single complex attribute.
  }
  \item{data}{
  a data frame with nominal data, attributes as columns, observations as rows.
  }
}
\details{
Just a helper.
}
\value{
\code{expandValues} is an internal help function to show the various value-combinations when combining attributes.
}
\author{
Michael Cysouw
}
\keyword{"internal"}