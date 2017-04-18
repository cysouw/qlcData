convertTreePhylo <- function(tree, height = 1) {
  
  tips <- setdiff(tree[,1],tree[,2])
  nodes <- unique(c("World",tree[,2]))
  all <- c(tips, nodes)
  to <- as.integer(factor(tree[,2],levels = all))
  from <- as.integer(factor(tree[,1],levels = all))
  edges <- as.matrix(cbind(to, from))
  dimnames(edges) <- NULL
  result <- list(  edge = edges
                 , Nnode = as.integer(length(nodes))
                 , tip.label = tips
                 , edge.length = rep(1,nrow(edges))
                 , node.label = nodes
                 )
  class(result) <- "phylo"
  result <- compute.brlen(result)
  result$edge.length <- result$edge.length * height
  return(result)
}