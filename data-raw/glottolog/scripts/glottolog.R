# load data
g <- read.csv("../data/glottolog.csv", row.names = 1, as.is = T)

# selectTree function
source("getTree.R")

# select family
getTree(down = "Tucanoan")
# select language, note problem with NAs , so use "which"
g[which(g$iso=="deu"),]
# or use %in%, e.g also to select set of iso codes
codes <- c("aab","aiw","aas")
g[g$iso %in% codes,]


f <- list.files("/Users/cysouw/Documents/Github/ParallelTexts/corpus/bibles/corpus")
iso <- substr(f,1,3)
system.time(sto <- sapply(iso[-which(iso=="tlh")], getStock, kind="iso"))
# 126 different stocks/isolates, which is only 30% ...
length(table(sto))/length(stocks())
table(sto)


# ======= PLOTTING ==========

# use data.tree
library(data.tree)

tree <- getTree(c("mhr", "myv", "est", "fin", "hun", "kpv", "sme"), kind="iso")
tree <- FromDataFrameNetwork(tree)
print(tree, "iso")

# reformat nodes
tree <- getTree(c("deu", "afr", "isl", "swe"), kind = "iso")
tree <- FromDataFrameNetwork(tree)
SetNodeStyle(tree, label = function(node){node$iso})
plot(tree)

# use dendrograms
library(dendextend)
d <- as.dendrogram(tree)
plot(d, center=T)
# problem with labels?
get_nodes_attr(d, "label")
labels(d) # doesn't work!?

# networks :-)
library(networkD3)
network <- ToDataFrameNetwork(tree, "name")
simpleNetwork(network)



# ========= OLD CODE

# select subset of tree based on selected iso codes
# not used anymore

selectTree <- function(codes) {
	# assume codes belong to one family
	f <- g[which(g$iso == codes[1]), "family"]
	# select family
	family <- g[g$family == f,]
	# add selection
	family <- cbind(family, selection = family$iso %in% codes )
	# remove languages and dialects without data
	family <- family[which( family$type=="family" | family$selection ),]
	# turn in to datatree
	tree <- FromDataFrameNetwork(family)
	# extend selection
	tree$Do(function(x){
			x$selection <- Aggregate(x,"selection", sum)
					},traversal = "post-order")
	# Prune
	Prune(tree, function(x){!is.na(x$selection) & x$selection > 0})
	# return
	return(tree)
}

