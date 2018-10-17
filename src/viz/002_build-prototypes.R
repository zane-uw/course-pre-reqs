# Build pre req net with add. vars  ---------------------------------------
rm(list = ls())
gc()

# setup -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(visNetwork)

source("src/pre-req-helpers.R")
load("data/pre-req-clean.RData")

default.par <- par()
par(mar=c(0,0,0,0)+.1)

igraph_options(vertex.label.cex = .8,
               vertex.label.color = 'black',
               vertex.size = 6,
               vertex.color = "white",
               vertex.frame.color = 'gray',
               edge.arrow.size = .25,
               asp = 0)

# dat$pr.min.grade <- as.numeric(dat$pr_grade_min) / 10
# dat$level <- dat$course_number %/% 100
# dat$pr.level <- str_sub(dat$pr_seq_no, end = 1)
# dat$pr.level[dat$pr.level == 9] <- 0
# dat <- dat %>% mutate(pr.cnc = if_else(pr_cr_s == "Y", 1, 0))
# dat <- dat %>% mutate(pr.concur = if_else(pr_concurrency == "Y", 1, 0))
#
# # for and/or courses:
# dat$dashes <- if_else(dat$pr_and_or == "O", TRUE, FALSE)
# dat$width <- if_else(dat$pr_and_or == "A", 1, 3)
#
# # define shapes based on course.to's level
# dat$shape[dat$level == 0] <- "dot"
# dat$shape[dat$level == 1] <- "circle"
# dat$shape[dat$level == 2] <- "square"
# dat$shape[dat$level == 3] <- "box"
# dat$shape[dat$level == 4] <- "ellipse"
#
# # branch labels
# dat$campus[dat$course_branch == 0] <- "Seattle"
# dat$campus[dat$course_branch == 1] <- "Tacoma"
# dat$campus[dat$course_branch == 2] <- "Bothell"

# sample/test data --------------------------------------------------------

acct <- edge.sub(n, "ACCTG")
plot(acct)

# visNetwork offers a fuller range of options

visIgraph(acct, type = "full")
visIgraph(acct) %>%
  visOptions(highlightNearest = T, nodesIdSelection = T) %>%
  visNodes(shape = "circle", size = 20, font = list(size = 15)) %>%
  visIgraphLayout(type = "full")

# Vertical layout with highlighting backwards
visIgraph(acct, type = "full") %>%
  visOptions(highlightNearest = list(enabled = T, degree = list(from = 3, to = 2), algorithm = "hierarchical", hideColor = rgb(200, 200, 200, 100, max = 255)), nodesIdSelection = T) %>%
  visNodes(shape = "circle", size = 25, font = list(size = 17)) %>%
  visEdges(arrows = "to") %>%
  visHierarchicalLayout()



# refining -------------------------------------------------------

# Instead of an igraph obj use:
# 1) a nodes dataframe
# 2) an edges dataframe

# additional considerations:
# cycles - not worried about these atm

# either/or pre-reqs - if A or B but not both, then assign edge weights evenly between A/B

# co-requisites - bi-directional edge would be nice but is that compatible with visEdges opts? Otherwise maybe color?
#   ...or encode co-reqs as pre-reqs so that A<-->B => B->A

# cross-listings - equivalency between A/B. One option would be to combine the two into a single node, 'AB'.



# —> gen nodes.df -------------------------------------------------------------

# # need to include both sets of from/to
# # the pr_ fields correspond to the 'from' nodes
# n.from <- dat %>% select(id = course.from) %>% unique()
# n.to <- dat %>% select(id = course.to) %>% unique()
#
# nodes.df <- bind_rows(n.from, n.to) %>% distinct() %>% arrange(id)
#
#
# # features
# nodes.df <-
#
#
# #  —> gen edges.df --------------------------------------------------------
#
# edges.df <- dat %>% select(from = course.from, to = course.to, pr.concur,
#                            or_dash, and_weight, pr.cnc, pr.seq = pr_seq_no)

# —> test -----------------------------------------------------------------

# visNetwork(nodes.df, edges.df)



# new igraph obj ----------------------------------------------------------
dat$title <- paste("Min grade:", dat$pr.min.grade)

edges.df <- dat %>% select(from = course.from, to = course.to, dashes, width, title)
edges.df <- edges.df[edges.df$from != edges.df$to,]
net <- graph_from_data_frame(edges.df)

math <- edge.sub(net, "MATH")
# vi <- toVisNetworkData(math)
visIgraph(math)


vi <- toVisNetworkData(math)
nodes <- vi$nodes; edges <- vi$edges
visNetwork(nodes, edges)

visNetwork(nodes, edges) %>%
  visEdges(width = edges$width, arrows = "to")


m425 <- vert.sub(net, "MATH")
visIgraph(m425)
