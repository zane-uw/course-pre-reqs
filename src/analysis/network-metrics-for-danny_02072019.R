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

m <- as_adj(n)
detach('package:igraph')

# here it begins anew -----------------------------------------------------


library(statnet)
m <- network::as.network(m)

png(filename = "ex-viz/entire_network.png", 800, 800, type = "quartz")
network::plot.network(m, usearrows = F, vertex.cex = .5, vertex.col = "white", edge.col = "gray")
dev.off()

summary(m)
