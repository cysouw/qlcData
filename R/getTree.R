# select part of glottlog data
# many corner cases make this complex

getTree <- function(up = NULL, kind = "iso", down = NULL, reduce = FALSE) {
	
  g <- qlcData::glottolog
  
	if (!is.null(up)) {
		avail <- up %in% unique(g[,kind])
		if (prod(avail) == 1) {
			parents <- .selectUp(up, type = kind)
		} else {
			stop(paste(up[!avail], collapse = ", "), " not available in Glottolog data")
		}
	} else {
		parents <- c()
	}
	
	if (!is.null(down)) {
		avail <- down %in% unique(g$father)
		availAsLeaf <- down %in% unique(g$name)		
		if (prod(avail) == 1) {
			children <- unlist(sapply(down, .selectDown))
		} else if (prod(avail | availAsLeaf) == 1){
			if (sum(avail) > 0) {
				children <- unlist(sapply(down[avail], .selectDown))
			} else {
				children <- c()
			}
			leaves <- which(g$name %in% down[availAsLeaf & !avail])
			children <- c(children, leaves)
		} else {
			stop(paste(down[!avail], collapse = ", "), " not available as a father in Glottolog data")
		}
		if (anyDuplicated(children) > 0) {
		  children <- unique(children)
		  warning("some families are part of other families as specified in `down`. Duplicates are removed")
		}
		# add root
		children <- unique(c(children, .selectUp(down)))
	} else {
		children <- c()
	}
	
  # combine up and down
	if (!is.null(up) & !is.null(down)) {
		all <- intersect(parents, children)
	} else {
		all <- c(parents, children)
	}

  result <- g[all,]
  
  # remove nodes without branching
	if (reduce) {
		f <- table(result$father)
		# always keep World if available
		if ("World" %in% names(f)) {f <- f[-which(names(f) == "World")]}
		# always keep family that was given in input
		if (!is.null(down)) {f <- f[-which(names(f) == down)]}
		# always keep names given in input
		if (!is.null(up)) {
			ignoreNames <- result[which(result[,kind] %in% up), "name"]
			ignore <- which(names(f) %in% ignoreNames)
			if (length(ignore) != 0) {
  			f <- f[-ignore]
			}
  	}
		# for all others that occur just once, remove them from the tree
		for (i in names(f)[f==1]) {
			result[result$father == i, "father"] <- result[result$name == i, "father"]
			result <- result[result$name != i,]
		}
	}
	
	result <- result[order(rownames(result)),]
	return(droplevels(result))
}

# internal help functions

.selectDown <- function(families) {
	
  g <- qlcData::glottolog
  
	getChildren <- function(x) { which(g$father == g[x,"name"]) }
		
	children <- sapply(families, function(x) {which(g$father == x)} )
	newChildren <- unlist(sapply(children, getChildren, simplify = F))

	while (length(newChildren) > 0) {
		children <- c(children, newChildren)
		newChildren <- unlist(sapply(newChildren, getChildren, simplify = F))			
	}
	return(unique(children))	
}

.selectUp <- function(names, type = "name") {
	
  g <- qlcData::glottolog
  
	getParents <- function(x) { which(g$name== g[x,"father"]) }
	
	parents <- unique(sapply(names, function(x) {which(g[,type] == x)} ))
	newParents <- unique(unlist(sapply(parents, getParents, simplify = F)))

	while (length(newParents) > 0) {	
		parents <- unique(c(parents, newParents))
		newParents <- unique(unlist(sapply(newParents, getParents, simplify = F)))
	}
	return(parents)	
}
