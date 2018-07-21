rm(list = ls())
gc()

library(tidyverse)
library(igraph)
library(visNetwork)

source("src/pre-req-helpers.R")
# source("src/viz-prefs.R")

load("data/pre-req-clean.RData")


# Create more/new visNetwork compatible fields ----------------------------
# title = hover tooltip
n <- n %>% set.vertex.attribute("title", value = get.vertex.attribute(., "long_course_title"))

# edge hover - an edge attribute named "label"

# edge lty for concurrency
n <- n %>%
  set.edge.attribute("dashes",
                     value = ifelse(get.edge.attribute(., "pr_concurrency") == "Y", T, F))

# get depts ---------------------------------------------------------------

depts <- unique(dat$department_abbrev)


# make subgraphs ----------------------------------------------------------
nets <- list()
for(i in 1:length(depts)){
  nets[[i]] <- edge.sub(n, depts[i])
}


# >> static ex's -------------------------------------------------------------
# eg
plot.igraph(nets[[20]], layout = layout_with_dh, mark.border = V(nets[[20]])$course_branch)

# shortest path from [node_i] -> [node_j]
amath <- nets[[6]]
ph <- shortest_paths(amath,
                     from = V(amath)[name == "MATH 125"],
                     to = V(amath)[name == "AMATH 353"],
                     output = "both")
ec <- rep('gray80', ecount(amath))
ec[unlist(ph$epath)] <- "orange"
ew <- rep(2, ecount(amath))
ew[unlist(ph$epath)] <- 4
vc <- rep("white", vcount(amath))
vc[unlist(ph$vpath)] <- "orange"
plot(amath, edge.color = ec, edge.width = ew, vertex.color = vc)
# save:
png("vizzes/static/ex-amath-shortest-path.png", 900, 800, res = 150)
plot(amath, edge.color = ec, edge.width = ew, vertex.color = vc, asp = 9/16,
     vertex.label.cex = .5, vertex.size = 9, main = "Applied Math",
     sub = "showing: shortest path from MATH 125 -> AMATH 353")
dev.off()




# make D3 plots from nets ----------------------------------------------------

net.plots <- lapply(nets, function(x){
  p <- visIgraph(x, type = "full") %>%
    visOptions(nodesIdSelection = list(enabled = T, main = "Course #"),
               highlightNearest = list(enabled = T,
                                       algorithm = "hierarchical",
                                       degree = list(from = 1, to = 1),
                                       hideColor = rgb(200, 200, 200, 100, max = 255)),
               selectedBy = list(variable = "department_abbrev", main = "Department")) %>%
    visNodes(shape = "circle", size = 22, font = list(size = 17), color = list(background = "white")) %>%
    visEdges(arrows = "to", width = 3, arrowStrikethrough = F, color = list(opacity = .5)) %>%
    visHierarchicalLayout(direction = "DU", levelSeparation = 40, nodeSpacing = 150, treeSpacing = 150) %>%
    visInteraction(navigationButtons = TRUE)
})

# ex
net.plots[[50]]
net.plots[[200]]
net.plots[[6]]

# visSave(vhcde, "d3_ex_hcde-edge_based.html", selfcontained = T)
