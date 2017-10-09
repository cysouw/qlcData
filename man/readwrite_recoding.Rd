\name{write.recoding}
\alias{write.recoding}
\alias{read.recoding}
\title{
Reading and writing of recoding files.
}
\description{
Nominal data (`categorical data') are data that consist of attributes, and each attribute consists of various discrete values (`types'). The different values that are distinguished in comparative linguistics are mostly open to debate, and different scholars like to make different decisions as to the definition of values. The \code{\link{recode}} function allows for an easy and transparent way to specify a recoding of an existing dataset. The current functions help with the preparations and usage of recoding specifications.
}
\usage{
write.recoding(data, attributes = NULL, all.options = FALSE, file = NULL, yaml = TRUE)
read.recoding(recoding, file = NULL, data = NULL)
}
\arguments{
  \item{data}{
  a data frame with nominal data, attributes as columns, observations as rows. Optionally a single column can be supplied. In that case the argument \code{attributes} can be left unspecified.
  }
  \item{recoding}{
  a \code{recoding} data structure, specifying the decisions of the recoding. It can also be a path to a file containing the specifications in YAML format. See Details.
  }
  \item{attributes}{
  a list of attributes to be recoded. Vectors (as elements of the list) are possible to specify combinations of attributes to be recoded as a single complex attribute. When \code{NULL}, then all attributes are included individually.
  }
  \item{all.options}{
  For combinations of attributes: should all theoretical interactions of the attributes be considered (TRUE) or should only the actually existing combinations in the data be presented (FALSE, by default) in the recoding?
  }
  \item{file}{
  file in which the recoding should be written.
  }
  \item{yaml}{
  the recoding template is by default written to a file in YAML format. When \code{yaml=FALSE}, the template is not converted to YAML, but returned inside R as a nested list.
  }
}
\details{
Recoding nominal data is normally considered too complex to be performed purely within R. It is possible to do it completely within R, but it is proposed here to use an external YAML document to specify the decisions that are taken in the recoding. The typical process of recoding will be to use \code{write.recoding.template} to prepare a skeleton that allows for quick and easy YAML-specification of a recoding. Or a YAML-recoding is written manually using various shortcuts (see below), and \code{read.recoding} is used to turn it into a full-fledged recoding that can also be used to document the decisions made. The function \code{recode} then combines the original data with the recoding, and produces a recoded dataframe.

The \code{recoding data structure} in the YAML document basically consists of a list of recodings, each of which describes a new attribute, based on one or more attributes from the original data. Each new attribute is described by:
\itemize{
  \item \emph{attribute}: the new attribute name.
  \item \emph{values}: a character vector with the new value names, optionally each value name has a linkage abbreviation (the name of the name)
  \item \emph{link}: a vector with length of the original number of values. Each entry specifies a link to the new value. Zero can be used for any values that should be ignored in the new attribute. Either a pure numeric vector, using the numbering of the new values, or a vector of names of the new value names. It is also possible to use the new values here without specifying the values in the preceding item.
  \item \emph{recodingOf}: the name(s) of the original attribute that forms the basis of the recoding. If there are multiple attributes listed, then the new attribute will be a combination of the original attributes.
  \item \emph{OriginalFrequencies}: a character vector with the value names from the original attribute and their frequency of occurrence. These are only added to the template to make it easier to specify the recoding. In the actual recoding the listing in this file will be ignored. It is important to keep the ordering as specified, otherwise the linking will be wrong. The ordering of the values follows the result of \code{levels}, which is determined by the current locale.
}

For writing recodings by hand, there are various shortcuts allowed:
\itemize{
  \item the names \code{attributes}, \code{values}, etc. can be abbreviated. The first letter should be sufficient.
  \item the \code{recodingOf} can be the full name of the attribute in the original data, or simply a number of the column in the data frame.
  \item the specification of \code{attribute} and \code{values} can be left out, although the result will be uninformative names like `Att1' and `Val1'.
  \item it is also possible to add an item \code{doNotRecode} with a vector of original attribute names (or column numbers). These original attributes will then be included unchanged in the recoded data table.
}

A minimal recoding consist thus of a specification of \code{recodingOf} and \code{link}. Without \code{link} nothing will be recoded. Omitting \code{recodingOf} will lead to an error.

There is a vignette available with detailed information about the process of recoding, check \code{recoding nominal data}.
}
\value{
\code{write.recoding.template} by default (when \code{yaml=TRUE}) writes a YAML structure to the specified file. When \code{yaml=FALSE} the same structure is returned inside R as a nested list.

\code{read.recoding} either reads a recoding from file, or a list structure within R, and cleans up all the shortcuts used. The output is by default a list structure to be used in \code{recode}, though it is also possible to write the result to a YAML-file (when \code{file} is specified). When \code{data} is specified, the output will be embelished with all the original names from the original data, which makes for an even better documentation of the recoding.
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