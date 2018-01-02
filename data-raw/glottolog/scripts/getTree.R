# stocks and isolates
# just for convenience

isolates <- function(){
	g[g$father == "World" & g$level == "language", "name"]

}

getName <- function(iso) {
	g[which(g$iso==iso),"name"]
}

# select part of glottlog data
# many corner cases make this complex

getTree <- function(up = NULL, kind = "iso", down = NULL, reduce = FALSE) {
	
	if (!is.null(up)) {
		avail <- up %in% unique(g[,kind])
		if (prod(avail) == 1) {
			parents <- .selectUp(up, type = kind)
		} else {
			stop(up[!avail], " not available in Glottolog data")
		}
	} else {
		parents <- c()
	}
	
	if (!is.null(down)) {
		avail <- down %in% unique(g$father)
		availAsLeaf <- down %in% unique(g$name)		
		if (prod(avail) == 1) {
			children <- .selectDown(down)
		} else if (prod(avail | availAsLeaf) == 1){
			if (sum(avail) > 0) {
				children <- .selectDown(down[avail])
			} else {
				children <- c()
			}
			leaves <- which(g$name %in% down[availAsLeaf & !avail])
			children <- c(children, leaves)
		} else {
			stop(down[!avail], " not available as a family in Glottolog data")
		}
	} else {
		children <- c()
	}
	
	if (!is.null(up) & !is.null(down)) {
		all <- intersect(parents, children)
	} else {
		all <- c(parents, children)
	}
	
	result <- g[all,]
	result <- result[order(rownames(result)),]
	
	if (reduce) {
		f <- table(result$father)
		# always keep World if available
		if ("World" %in% names(f)) {f <- f[-which(names(f) == "World")]}
		# always keep family that was given in input
		if (!is.null(down)) {f <- f[-which(names(f) == down)]}
		# always keep names given in input
		if (!is.null(up)) {
			ignoreNames <- result[which(result$iso %in% up), "name"]
			f <- f[-which(names(f) %in% ignoreNames)]
			}
		# for all others that occur just once, remove them from the tree
		for (i in names(f)[f==1]) {
			result[result$father == i, "father"] <- result[result$name == i, "father"]
			result <- result[result$name != i,]
		}
	}
	return(result)
}

.selectDown <- function(families) {
	
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
	
	getParents <- function(x) { which(g$name== g[x,"father"]) }
	
	parents <- unique(sapply(names, function(x) {which(g[,type] == x)} ))
	newParents <- unique(unlist(sapply(parents, getParents, simplify = F)))

	while (length(newParents) > 0) {	
		parents <- unique(c(parents, newParents))
		newParents <- unique(unlist(sapply(newParents, getParents, simplify = F)))
	}
	return(parents)	
}

# e.g. population of isolates compared to whole data
# boxplot(log10(selectTree(isolates())$population+1), log10(g$population+1))
