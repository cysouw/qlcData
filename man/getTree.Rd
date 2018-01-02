\name{getTree}
\alias{getTree}
\title{
Extract parts out of the full Glottolog 2016 tree
}
\description{
getTree is a convenience function to extract parts from the Glottolog 2016 data as provided here in \code{\link{glottolog}}.
}
\usage{
getTree(up = NULL, kind = "iso", down = NULL, reduce = FALSE)
}

\arguments{
  \item{up}{
a vector of names from which to extract the tree upwards. Can be names from Glottolog, ISO 639-3 codes, WALS codes or glottocodes. Default settings expect ISO 639-3 codes.
}
  \item{kind}{
what kind of names are specified in \code{up}? Choose one of \code{iso}, \code{wals}, \code{glottocode} or \code{name} for full names from Glottolog.
}
  \item{down}{
a vector of family names from Glottolog from which to extract the tree downwards. For consistency, a node "World" as added on top, linking separate families. Specifying families that are part of other families in \code{down} will lead to a warning, but any overlap will be gracefully removed.
}
  \item{reduce}{
remove all nodes in the tree that do not branch.
}
}
\details{
Specifying both \code{up} and \code{down} will extract only the intersection of the two. So any name in \code{up} that lies outside any family in \code{down} will be discarded.
}
\value{
Returns a data.frame with the relevant lines from the full glottolog data.
}
\author{
Michael Cysouw <cysouw@mac.com
}
\note{
This function is hard-coded to only use the data as available in \code{\link{glottolog}}.
}
\seealso{
\code{\link{asPhylo}} for tweaking branch lengths in phylo conversion.
}
\examples{
# use getTree() to select genealogical parts of the data
data(glottolog)

( aalawa <- getTree(up = "aala1237", kind = "glottocode") )
( kandas <- getTree(down = "Kandas-Duke of York") )
( treeFull <- getTree(up = c("deu", "eng", "ind", "cha"), kind = "iso") )
( treeReduced <- getTree(up = c("deu", "eng", "ind", "cha"), kind = "iso", reduce = TRUE) )

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
