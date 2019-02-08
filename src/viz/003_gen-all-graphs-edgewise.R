rm(list = ls())
gc()

library(tidyverse)
library(igraph)
library(visNetwork)

setwd(rstudioapi::getActiveProject())

source("src/pre-req-helpers.R")
# source("src/viz-prefs.R")

load("data/pre-req-clean.RData")


# Create more/new visNetwork compatible fields ----------------------------
# title = hover tooltip

# create the and/or labels and concat with the long_course_title to create a new vertex attribute for title
clean.str.end <- function(string){
  i <- max(str_locate_all(string, "\\d")[[1]][,2])
  return(str_sub(string, 1, i))
}

dat$ao <- ifelse(dat$pr_and_or == "O", " Or", ifelse(dat$pr_and_or == "A", "; and", ""))
dat$ao <- paste(dat$course.from, dat$ao, sep = "")
dat$ao[1:10]
vlab.andor <- dat %>% group_by(course.to) %>% transmute(vlab.prereqs = paste(ao, collapse = " ")) %>% distinct()
vlab.andor$vlab.prereqs <- lapply(vlab.andor$vlab.prereqs, clean.str.end)

attribs <- attribs %>% left_join(vlab.andor, by = c("course" = "course.to"))
attribs$vlab.prereqs[attribs$vlab.prereqs == "NULL"] <- ""
attribs$vlab <- paste(attribs$long_course_title, attribs$vlab.prereqs, sep = "<br>")


n <- n %>% set.vertex.attribute("long_title", value = get.vertex.attribute(., "long_course_title"))
n <- n %>% set.vertex.attribute("title", value = attribs$vlab)

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

# sample
visIgraph(nets[[135]], type = "square") %>%
  visOptions(nodesIdSelection = list(enabled = T, main = "Course #"),
             highlightNearest = list(enabled = T,
                                     algorithm = "hierarchical",
                                     degree = list(from = 1, to = 1),
                                     hideColor = rgb(200, 200, 200, 100, max = 255)),
             selectedBy = list(variable = "department_abbrev", main = "Department")) %>%
  visNodes(shape = "circle", size = 30, font = list(size = 20), color = list(background = "white")) %>%
  visEdges(arrows = "to", width = 3, arrowStrikethrough = F, color = list(opacity = .5)) %>%
  visHierarchicalLayout(direction = "DU", levelSeparation = 40, nodeSpacing = 150, treeSpacing = 150) %>%
  visInteraction(navigationButtons = TRUE)

# # >> static ex's -------------------------------------------------------------
# # eg
# plot.igraph(nets[[20]], layout = layout_with_dh, mark.border = V(nets[[20]])$course_branch)
#
# # shortest path from [node_i] -> [node_j]
# amath <- nets[[6]]
# ph <- shortest_paths(amath,
#                      from = V(amath)[name == "MATH 125"],
#                      to = V(amath)[name == "AMATH 353"],
#                      output = "both")
# ec <- rep('gray80', ecount(amath))
# ec[unlist(ph$epath)] <- "orange"
# ew <- rep(2, ecount(amath))
# ew[unlist(ph$epath)] <- 4
# vc <- rep("white", vcount(amath))
# vc[unlist(ph$vpath)] <- "orange"
# plot(amath, edge.color = ec, edge.width = ew, vertex.color = vc)
# # save:
# png("vizzes/static/ex-amath-shortest-path.png", 900, 800, res = 150)
# plot(amath, edge.color = ec, edge.width = ew, vertex.color = vc, asp = 9/16,
#      vertex.label.cex = .5, vertex.size = 9, main = "Applied Math",
#      sub = "showing: shortest path from MATH 125 -> AMATH 353")
# dev.off()




# make D3 plots from nets ----------------------------------------------------

net.plots <- lapply(nets, function(x){
  p <- visIgraph(x, type = "square") %>%
    visOptions(nodesIdSelection = list(enabled = T, main = "Course #"),
               highlightNearest = list(enabled = T,
                                       algorithm = "hierarchical",
                                       degree = list(from = 1, to = 1),
                                       hideColor = rgb(200, 200, 200, 100, max = 255)),
               selectedBy = list(variable = "department_abbrev", main = "Department")) %>%
    visNodes(shape = "circle", size = 30, font = list(size = 20), color = list(background = "white")) %>%
    visEdges(arrows = "to", width = 3, arrowStrikethrough = F, color = list(opacity = .5)) %>%
    visHierarchicalLayout(direction = "DU", levelSeparation = 40, nodeSpacing = 150, treeSpacing = 150) %>%
    visInteraction(navigationButtons = TRUE)
})

# ex
net.plots[[50]]
net.plots[[200]]
net.plots[[6]]

# visSave(vhcde, "d3_ex_hcde-edge_based.html", selfcontained = T)


# save graphs -------------------------------------------------------------
setwd("vizzes/visnet plots/")
for(i in 1:length(net.plots)){
  visSave(net.plots[[i]], file = paste0("ex-", depts[i], "-.html"), selfcontained = T)
}
