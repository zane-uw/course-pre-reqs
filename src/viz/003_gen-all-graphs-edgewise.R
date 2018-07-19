rm(list = ls())
gc()

library(tidyverse)
library(igraph)
library(visNetwork)

source("src/pre-req-helpers.R")
source("src/viz-prefs.R")

load("data/pre-req-clean.RData")



# get depts ---------------------------------------------------------------

depts <- unique(dat$department_abbrev)


# make subgraphs ----------------------------------------------------------

nets <- list()
for(i in 1:length(depts)){
  nets[[i]] <- edge.sub(n, depts[i])
}

# eg
plot.igraph(nets[[6]])



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
