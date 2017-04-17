convertTreePhylo <- function(tree) {
  
  tips <- setdiff(tree[,1],tree[,2])
  nodes <- unique(tree[,2])
  all <- c(tips, nodes)
  to <- as.numeric(factor(tree[,2],levels = all))
  from <- as.numeric(factor(tree[,1],levels = all))
  edges <- as.matrix(cbind(to, from))
  dimnames(edges) <- NULL
  result <- list(  edge = edges
                 , Nnode = length(nodes)
                 , tip.label = tips
                 , edge.lenght = rep(1,nrow(edges))
                 , node.label = nodes
                 )
  class(result) <- "phylo"
  return(result)
}