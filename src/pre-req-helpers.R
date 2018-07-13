# Helpers for pre-req network

library(igraph)

# custom subset function (edge-based) --------------------------------------------------

# return an edge-based subset of nodes+edges given a department label
edge.sub <- function(net, dept = "CHEM"){      # use a sensible default
  i <- grep(paste0("^", dept, "\\s\\w"), V(net)$name)
  ei <- E(net)[from(i) | to(i)]
  return(subgraph.edges(net, ei))
}

# wrapper for induced_subgraph (Tac/Both will need to be explicit, as would, e.g. 'CHEM E') --------------------------------------------
# but code will pull anything related to the given DEPT
vert.sub <- function(net, dept = "CHEM"){
  i <- grep(paste0("^", dept, "\\s\\w"), V(net)$name)
  return(induced_subgraph(net, V(net)[i]))
}


