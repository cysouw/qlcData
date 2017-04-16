\name{asPhylo}
\alias{asPhylo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
asPhylo(tree, height = 100, fixed.branches = NULL, long.root = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
%%     ~~Describe \code{tree} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
}
  \item{fixed.branches}{
%%     ~~Describe \code{fixed.branches} here~~
}
  \item{long.root}{
%%     ~~Describe \code{long.root} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tree, height = 100, fixed.branches = NULL, long.root = NULL) 
{
    tree <- data.tree::FromDataFrameNetwork(tree)
    newick <- data.tree::ToNewick(tree, heightAttribute = height)
    phylo <- phytools::read.newick(text = newick)
    if (!is.null(fixed.branches)) {
        phylo$edge.length <- rep(fixed.branches, length(phylo$edge.length))
    }
    if (!is.null(long.root)) {
        roots <- c("World", "Southeast_Asia", "Sahul", "Africa", 
            "Eurasia", "South_America", "North_America")
        check <- phylo$node.label \%in\% roots
        if (sum(check) > 0) {
            long.edges <- phylo$edge[, 1] \%in\% (which(check) + 
                length(phylo$tip.label))
            phylo$edge.length[long.edges] <- long.root
        }
    }
    phylo <- ape::multi2di(ape::collapse.singles(phylo))
    return(phylo)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
