asPhylo <- function(tree, height = 100, fixed.branches = NULL, long.root = NULL) {
	
  # this export to newick takes time with large trees
  # tree <- data.tree::FromDataFrameNetwork(tree)
	# newick <- data.tree::ToNewick(tree, rootHeight = height)
	# phylo <- phytools::read.newick(text = newick)
	
  # this simple function cannot take 'height' and always takes 'fixed = 1'
  phylo <- convertTreePhylo(tree)
  
	if (!is.null(fixed.branches)) {
		phylo$edge.length <- rep(fixed.branches, length = nrow(phylo$edge))
	} 
  
	if (!is.null(long.root)) {
		roots <- c("World"
		          , "Southeast_Asia"
		          , "Sahul"
		          , "Africa"
		          , "Eurasia"
		          , "South_America"
		          , "North_America")
		check <- phylo$node.label %in% roots
		if (sum(check) > 0) {
			long.edges <- phylo$edge[,1] %in% (which(check) + length(phylo$tip.label))
			phylo$edge.length[long.edges] <- long.root
		}
	}
	
	phylo <- ape::multi2di(ape::collapse.singles(phylo))

	if (is.null(fixed.branches)) {
	  phylo <- compute.brlen(phylo)
	  phylo$edge.length <- phylo$edge.length * height
	}
	
	# Spaces were removed by newick conversion. Reintroduced here.
	# phylo$node.label <- gsub("_", " ", phylo$node.label)
	# phylo$tip.label <- gsub("_", " ", phylo$tip.label)
	
	return(phylo)
}
