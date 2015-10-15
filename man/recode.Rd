\name{recode}
\alias{recode}
\title{
Recoding nominal data
}
\description{
Nominal data (`categorical data') are data that consist of attributes, and each attribute consists of various discrete values (`types'). The different values that are distinguished in comparative linguistics are mostly open to debate, and different scholars like to make different decisions as to the definition of values. The \code{recode} function allows for an easy and transparent way to specify a recoding of an existing dataset.
}
\usage{
recode(data, recoding)
}
\arguments{
  \item{data}{
  a data frame with nominal data, attributes as columns, observations as rows.
  }
  \item{recoding}{
  a \code{recoding} data structure, specifying the decisions of the recoding. It can also be a path to a file containing the specifications in YAML format. See Details.
  }
}
\details{
Recoding nominal data is normally considered too complex to be performed purely within R. It is possible to do it completely within R, but it is proposed here to use an external YAML document to specify the decisions that are taken in the recoding. The typical process of recoding will be to use \link{write.recoding} to prepare a skeleton that allows for quick and easy YAML-specification of a recoding. Or a YAML-recoding is written manually using various shortcuts (see below), and \link{read.recoding} is used to turn it into a full-fledged recoding that can also be used to document the decisions made. The function \code{recode} then combines the original data with the recoding, and produces a recoded dataframe.

The \code{recoding data structure} in the YAML document basically consists of a list of recodings, each of which describes a new attribute, based on one or more attributes from the original data. Each new attribute is described by:
\itemize{
  \item \emph{attribute}: the new attribute name.
  \item \emph{values}: a character vector with the new value names.
  \item \emph{link}: a numeric vector with length of the original number of values. Each entry specifies the number of the new value. Zero can be used for any values that should be ignored in the new attribute.
  \item \emph{recodingOf}: the name(s) of the original attribute that forms the basis of the recoding. If there are multiple attributes listed, then the new attribute will be a combination of the original attributes.
  \item \emph{OriginalValues}: a character vector with the value names from the original attribute. These are only added to the template to make it easier to specify the recoding. In the actual recoding the listing in this file will be ignored. It is important to keep the ordering as specified, otherwise the linking will be wrong. The ordering of the values follows the result of \code{levels}, which is determined by the current locale.
}

There is a vignette available with detailed information about the process of recoding, check \code{recoding nominal data}.
}

\value{
\code{recode} returns a data frame with the recoded attributes
}
\references{
Cysouw, Michael, Jeffrey Craig Good, Mihai Albu and Hans-Jörg Bibiko. 2005. Can GOLD "cope" with WALS? Retrofitting an ontology onto the World Atlas of Language Structures. \emph{Proceedings of E-MELD Workshop 2005}, \url{http://emeld.org/workshop/2005/papers/good-paper.pdf}
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
The World Atlas of Language Structure (WALS) contains typical data that most people would very much like to recode before using for further analysis. See Cysouw et al. 2005 for a discussion of various issues surrounding the WALS data.
}