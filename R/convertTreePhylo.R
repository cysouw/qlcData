convertTreePhylo <- function(tree) {
  
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