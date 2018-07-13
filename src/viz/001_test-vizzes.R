rm(list = ls())
gc()


# setup -------------------------------------------------------------------
library(igraph)
library(visNetwork)
library(networkD3)
source("src/pre-req-helpers.R")

load("data/pre-req-raw.RData")


# sample: simple D3 network -----------------------------------------------------------------

# sample data
i <- grep("^CHEM\\d", V(n)$name)
chem <- induced_subgraph(n, V(n)[i])
plot(chem)

# create a networkD3 compatible object and plot
chem.d3 <- as_data_frame(chem)
simpleNetwork(chem.d3, fontSize = 12, zoom = T)

# sample: sankey plot -----------------------------------------------------

# random values for link weights
test <- igraph_to_networkD3(chem)
test$links$value <- sample(3:100, size = nrow(chem.d3), replace = T)
sankeyNetwork(Links = test$links, Nodes = test$nodes,
              Source = "source", Target = "target", Value = "value", NodeID = "name")

# visNetwork package ------------------------------------------------------

# can use an igraph object rather than a df
visIgraph(chem, type = "full")
visIgraph(chem) %>%
  visOptions(highlightNearest = T, nodesIdSelection = T) %>%
  visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
  visIgraphLayout(type = "full")

visIgraph(chem, type = "full") %>%
  visOptions(highlightNearest = list(enabled = T, degree = 2), nodesIdSelection = T) %>%
  visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
  visEdges(arrows = "to") %>%
  visHierarchicalLayout()

visIgraph(chem, type = "full") %>%
  visOptions(highlightNearest = list(enabled = T, degree = list(from = 2, to = 1), algorithm = "hierarchical", hideColor = rgb(200, 200, 200, 100, max = 255)), nodesIdSelection = T) %>%
  visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
  visEdges(arrows = "to") %>%
  visHierarchicalLayout(direction = "LR", levelSeparation = 150, sortMethod = "directed")