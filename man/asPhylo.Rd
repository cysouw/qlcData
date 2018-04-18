\name{asPhylo}
\alias{asPhylo}
\docType{data}
\title{
Convert Glottolog trees to phylo format
}
\description{
Convenience version of conversion to phylo format of the \code{ape} package, used throughout packages for phylogenetic methods. This conversion offers various options to tweak the branch lenghts of Glottolog trees (which do not have any inherent branch lengths). Currently very slow for large trees!
}
\usage{
asPhylo(tree, height = 100, fixed.branches = NULL, long.root = NULL, 
  multi.allow = FALSE, quick = FALSE)
}
\arguments{
  \item{tree}{
(selection of) Glottolog data, preferably extracted with \code{\link{getTree}}. If the selection is not logically a tree, then it will lead to errors.
  }
  \item{height}{
Height of the tree. By default all nodes in the tree will be simply equally spaced on this height in an ultrametric fashion, i.e. all leaves will be at height zero.
  }
  \item{fixed.branches}{
Alternatively to \code{height}, specify a fixed length for all branches. Note that this happens before all singleton nodes will be removed, so branches with singleton nodes will become as long as the amount of singleton nodes on it.
  }
  \item{long.root}{
The Glottolog version included in this packages includes a 'World' root and six area nodes below this root. These nodes are not strictly speaking genealogical nodes, though they are often practical for phylogenitic inference. Specify the length of these nodes here.
  }
  \item{multi.allow}{
Allow multichotomies in tree 
  }
  \item{quick}{
Conversion when quick approach is used  
  }
}
\details{
The \code{\link[ape]{phylo}} format is a widely used format for phylogenetic inference. However, many methods depend crucially on branch lengths. As linguistic trees mostly do not have branch lenghts, this conversion offers a few options to tweak branch lengths.

Note that phylo trees do not allow singleton nodes, so they are removed internally (by \code{\link[ape]{collapse.singles}}). The tree is also forced to be binary (by \code{\link[ape]{multi2di}}), which is also expected in many phylogenetic analysis.

The order of length assignment is: reduce (in getTree) >> height >> fixed length >> long root >> collapse singles, which can be used to lead to different results. Also the option \code{reduce} in \code{\link{getTree}} can be used to influence the branch lenghts (see Examples below).

The collapsing of singleton internal nodes after branch length leads to an interesting effect that branch lengths are somehow proportional to the number of internal diversity of the tree, which might make sense as a proxy to branch lengths. Something like \code{reduce = F, fixed.branches = 1, long.root = 10} seem promising.
}
\value{
An object of class \code{phylo}.
}
\author{
Michael Cysouw <cysouw@mac.com
}
\note{
Depends internally on \code{\link[data.tree]{ToNewick}} which is currently very slow for large trees.
}
\examples{
# many different effects can be achieved by combining options

isoCodes <- c("deu", "eng", "swe", "swh", "xho", "fin")
treeWithInternal <- getTree(isoCodes, reduce = FALSE)
treeNoInternal <- getTree(isoCodes, reduce = TRUE)

\dontrun{
# to understand the influence of the option 'reduce' also check
library(data.tree)
plot(FromDataFrameNetwork(treeWithInternal))
plot(FromDataFrameNetwork(treeNoInternal))}

library(ape)
par(mfcol = c(2,3))

phylo <- asPhylo(treeWithInternal, height = 20)
plot(phylo, main = "reduce = FALSE\nheight = 20", cex = 1)
edgelabels(round(phylo$edge.length), cex = 1.5)

phylo <- asPhylo(treeNoInternal, height = 20)
plot(phylo, main = "reduce = TRUE\nheight = 20", cex = 1)
edgelabels(round(phylo$edge.length), cex = 1.5)

phylo <- asPhylo(treeWithInternal, fixed.branches = 1)
plot(phylo, main = "reduce = FALSE\nfixed.branches = 1", cex = 1)
edgelabels(round(phylo$edge.length), cex = 1.5)

phylo <- asPhylo(treeNoInternal, fixed.branches = 1)
plot(phylo, main = "reduce = TRUE\nfixed.branches = 1", cex = 1)
edgelabels(round(phylo$edge.length), cex = 1.5)

phylo <- asPhylo(treeWithInternal, fixed.branches = 1, long.root = 10)
plot(phylo, main = "reduce = FALSE\nfixed.branches = 1\nlong.root = 10", cex = 1)
edgelabels(round(phylo$edge.length), cex = 1.5)

phylo <- asPhylo(treeNoInternal, fixed.branches = 1, long.root = 10)
plot(phylo, main = "reduce = TRUE\nfixed.branches = 1\nlong.root = 10", cex = 1)
edgelabels(round(phylo$edge.length), cex = 1.5)

par(mfcol=c(1,1))

}