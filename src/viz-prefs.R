# igraph prefs
library(igraph)

.opar <- par()
par(mar=c(0,0,0,0)+.9)

igraph_options(vertex.label.cex = .5,
               vertex.label.color = 'black',
               vertex.size = 6,
               vertex.color = "white",
               vertex.frame.color = 'gray',
               edge.arrow.size = .25,
               asp = 0)