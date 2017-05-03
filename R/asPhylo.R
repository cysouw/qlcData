asPhylo <- function(tree, height = 100, fixed.branches = NULL, long.root = NULL, multi.allow = FALSE, quick = FALSE) {
	
  if (quick) {
    # this simple function cannot take 'height' and always takes 'fixed = 1'
    phylo <- .convertTreePhylo(tree)
  } else {
    # this export to newick takes time with large trees
    tree <- data.tree::FromDataFrameNetwork(tree)
    newick <- data.tree::ToNewick(tree, rootHeight = height)
    phylo <- phytools::read.newick(text = newick)
  }
  
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
  
  # reorder.phylo apparently has a memory leak, so it won't allow trees 
  # with more internal nodes than tips :(
  # phylo <- ape::reorder.phylo(phylo)
  phylo <- ape::collapse.singles(phylo)

  if (!multi.allow) {
    phylo <- ape::multi2di(phylo)
  }

	if (is.null(fixed.branches) & !quick) {
	  phylo <- ape::compute.brlen(phylo)
	  phylo$edge.length <- phylo$edge.length * height
	}
	
  if (!quick) {
    # Spaces were removed by newick conversion. Reintroduced here.
	  phylo$node.label <- gsub("_", " ", phylo$node.label)
	  phylo$tip.label <- gsub("_", " ", phylo$tip.label)
  }
	
	return(phylo)
}

.convertTreePhylo <- function(tree) {
  
  tips <- setdiff(tree[,1], tree[,2])
  nodes <- unique(c("World", tree[,2]))
  all <- c(tips, nodes)
  father <- as.integer(factor(tree[,2],levels = all))
  child <- as.integer(factor(tree[,1],levels = all))
  edges <- as.matrix(cbind(father, child))
  edges <- edges[order(edges[,1], edges[,2]), ]
  dimnames(edges) <- NULL
  result <- list(  edge = edges
                   , Nnode = as.integer(length(nodes))
                   , tip.label = tips
                   , edge.length = rep(1,nrow(edges))
                   , node.label = nodes
  )
  class(result) <- "phylo"
  return(result)
}
