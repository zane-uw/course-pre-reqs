rm(list = ls())
gc()


# setup -------------------------------------------------------------------
library(igraph)
library(visNetwork)
library(networkD3)
source("src/pre-req-helpers.R")
source("src/viz-prefs.R")

load("data/pre-req-clean.RData")


# sample: simple D3 network -----------------------------------------------------------------

# sample data
i <- grep("^CHEM\\s\\d", V(n)$name)
chem <- induced_subgraph(n, V(n)[i])

  # plot(chem)
  #
  # # create a networkD3 compatible object and plot
  # chem.d3 <- igraph::as_data_frame(chem, what = "edges")
  # simpleNetwork(chem.d3, fontSize = 12, zoom = T)
  #
  # # sample: sankey plot -----------------------------------------------------
  #
  # # random values for link weights
  # test <- igraph_to_networkD3(chem)
  # test$links$value <- sample(3:100, size = nrow(chem.d3), replace = T)
  # setwd("vizzes/")
  # sankeyNetwork(Links = test$links, Nodes = test$nodes,
  #               Source = "source", Target = "target", Value = "value", NodeID = "name") %>%
  #   saveNetwork("d3_chem_sankey_example.html", selfcontained = T)
  # setwd("..")

# visNetwork package ------------------------------------------------------

# can use an igraph object directly rather than a df
visIgraph(chem, type = "full")

# or interactively fiddle with options; req's shiny
# ntest <-
visIgraph(chem, type = "full") %>%
  visOptions(highlightNearest = list(enabled = T, degree = list(from = 2, to = 1), hideColor = rgb(200, 200, 200, 100, max = 255)), nodesIdSelection = T) %>%
  visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
  visEdges(arrows = "to") %>%
  visHierarchicalLayout(direction = "DU", levelSeparation = 50, nodeSpacing = 800, blockShifting = T, parentCentralization = T, treeSpacing = 100)
# custom <- visNetworkEditor(object = ntest)

# selection opts
(vchem <- visIgraph(chem, type = "full") %>%
  visOptions(highlightNearest = list(enabled = T,
                                     algorithm = "hierarchical",
                                     degree = list(from = 3, to = 1),
                                     hideColor = rgb(200, 200, 200, 100, max = 255)),
             selectedBy = list(variable = "course.level")) %>%
  visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
  visEdges(arrows = "to") %>%
  visHierarchicalLayout(direction = "DU", levelSeparation = 40, nodeSpacing = 150))

setwd("vizzes")
visSave(vchem, file = "d3_ex_chem.html", selfcontained = T)

# more examples:
anth <- edge.sub(n, "ANTH")
(vanth <- visIgraph(anth, type = "full") %>%
  visOptions(highlightNearest = list(enabled = T,
                                     algorithm = "hierarchical",
                                     degree = list(from = 3, to = 1),
                                     hideColor = rgb(200, 200, 200, 100, max = 255)),
             selectedBy = list(variable = "department_abbrev")) %>%
  visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
  visEdges(arrows = "to") %>%
  visHierarchicalLayout(direction = "DU", levelSeparation = 40, nodeSpacing = 150, treeSpacing = 100))
visSave(vanth, "d3_ex_anth-edge_based.html", selfcontained = T)


# more examples:
hcde <- edge.sub(n, "HCDE")
(vhcde <- visIgraph(hcde, type = "full") %>%
    visOptions(highlightNearest = list(enabled = T,
                                       algorithm = "hierarchical",
                                       degree = list(from = 3, to = 1),
                                       hideColor = rgb(200, 200, 200, 100, max = 255)),
               selectedBy = list(variable = "department_abbrev")) %>%
    visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
    visEdges(arrows = "to") %>%
    visHierarchicalLayout(direction = "DU", levelSeparation = 40, nodeSpacing = 150, treeSpacing = 150))
visSave(vhcde, "d3_ex_hcde-edge_based.html", selfcontained = T)
