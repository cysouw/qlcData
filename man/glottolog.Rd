\name{glottolog}
\alias{glottolog}
\docType{data}
\title{
Glottolog data from \url{http://www.glottolog.org}
}
\description{
Data from Glottolog 2016 with added WALS codes and speaker-community size. Various minor corrections and additions were performed in the preparation of the data (see Details). All stocks (i.e. largest reconstructable units) are linked to macroareas, and they are linked to a single root node calles 'World'.
}
\usage{data("glottolog")}
\format{
  A data frame with 22007 observations on the following 10 variables.
  \describe{
    \item{\code{name}}{a character vector with the name of the entity.}
    \item{\code{father}}{a character vector with the name of the direct parent entity.}
    \item{\code{stock}}{a factor with the highest reconstructable unit. This column is added just for convenience, it does not add any new information.}
    \item{\code{glottocode}}{a character vector with the glottocode. The same identifier is added as rownames of the data.}
    \item{\code{iso}}{a character vector with ISO 639-3 language codes}
    \item{\code{wals}}{a character vector with WALS language codes}
    \item{\code{level}}{a factor with levels \code{dialect}, \code{family} and \code{language}}
    \item{\code{longitude}}{a numeric vector with geographic coordinates as available in the Glottolog}
    \item{\code{latitude}}{a numeric vector with geographic coordinates as available in the Glottolog}
    \item{\code{population}}{a numeric vector with speaker community size from an old Ethnologue version (13th Edition), licensed to the MPI-EVA in Leipzig.}
  }
}
\details{
For Glottolog data: the names were uniquified by adding a glottocode when a name occurs more than once (typically in some cases of a language and a family having the same name). Entries classified as 'bookkeeping', 'unattested' 'artificial language', 'sign language', 'speech register' and 'unclassifiable' were removed. Links to WALS codes were added: note that about 20 links are missing, and for the non-unique links one link was chosen by data availability. Some macro codes from ISO 639-3 were added.

A level 'area' was added to the tree, separating all languages in six areas: Eurasia, Africa, Southeast Asia, Sahul, North America and South America. This is reminiscent of the proposal from Dryer (1992), though Austronesian is grouped with Southeast Asia here, because that makes more sense genealogically. Still, these nodes are surely not monophyletic! Mixed languages are not assigned to an area.

Please note that the data provided here is not identical to the online version of Glottolog, as the online version is constantly being updated! This is Glottolog 2016. Updates might be made available when they are provided for download from the website.

The format of the glottolog data might seem a bit convoluted, but by using \code{\link{getTree}} it is actually really easy to extract genealogical parts of the glottolog data and by using \code{\link[data.tree]{FromDataFrameNetwork}} this can be nicely plotted and turned into various tree format as used in R.
}
\source{
Glottolog 2016 data from \url{http://www.glottolog.org}. WALS 2013 data from \url{http://www.glottolog.org}. Information on macrolanguages from \url{http://www-01.sil.org/iso639-3/macrolanguages.asp}. All data downloaded in March 2017. Population numbers are from the 13th edition of the Ethnologue, licenced to the MPI-EVA in Leipzig.
}
\examples{
# use getTree() to select genealogical parts of the data
data(glottolog)

( aalawa <- getTree(up = "aala1237", kind = "glottocode") )
( kandas <- getTree(down = "Kandas-Duke of York") )
( treeFull <- getTree(up = c("deu", "eng", "ind", "cha"), kind = "iso") )
( treeReduced <- getTree(up = c("deu", "eng", "ind", "cha"), kind = "iso", reduce = TRUE) )

# check out areas
( areas <- glottolog[glottolog$level == "area", "name"] )
# stocks in Southeast Asia
glottolog[glottolog$father == areas[1], "name"]

\dontrun{
# use FromDataFrameNetwork() to visualize the tree
# and export it into various other tree formats in R

library(data.tree)
treeF <- FromDataFrameNetwork(treeFull)
treeR <- FromDataFrameNetwork(treeReduced)

plot(treeF)
plot(treeR)

# turn into phylo format from library 'ape'
t <- as.phylo.Node(treeR)
plot(t)

# turn into dendrogram
t <- as.dendrogram(treeF)
plot(t, center = T)
}
}
\keyword{datasets}
