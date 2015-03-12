rm(list = ls())

require(igraph)

load("./data/sna/stackoverflow_graph.RData")
load("./data/sna/userId_map_reverse.RData")
# 'g' is the network

# Put UserId as name of a vertex
userId <- vector()
for(i in 1:length(V(g))) {
  nodeIdStr <- toString(V(g)[i])
  userIdStr <- userId.map.reverse[[nodeIdStr]]
  if(!is.null(userIdStr)) {
    userId[i] <- userIdStr
  }
  else {
    userId[i] <- "NULL"
  }
}
V(g)$name <- userId

# Subset the data, exclude people who are at the periphery of the network
theta <- 50 # the threshold
bad.nodes <- V(g)[degree(g) < theta] # low-degree nodes
f <- delete.vertices(g, bad.nodes) # f is the new network

# Highlight those people with high degrees with red
V(f)$color <- ifelse(degree(f) >= 20, 'red', 'blue')
# Can also color the edges differently according to weights
# E.g.,
# E(f)$color < ifelse(E(f)$freq >= 50, 'red', 'gray')
# Define edge widths:
# E(f)$width <- Edges$thickness * 5
# Define arrow widths:
# E(f)$arrow.width <- Edges$thickness * 5

# Size the vertices according to their degrees
V(f)$size <- degree(f) / 10

# Finally, plot the network
par(mai = c(0, 0, 1, 0)) # reduce the sizes of the margins
plot(f, # the network to be plotted
     layout = layout.fruchterman.reingold, # the layout method
     main = 'StackOverflow Network Visualization', # specifies the title
     vertex.label.dist = 0.5, # puts the name labels slightly off the dots
#      vertex.frame.color = 'blue', # the color of the border of the dots 
     vertex.label.color = 'blue', # the color of the name labels
     vertex.label.font = 1, # the font of the name labels
     vertex.label=V(f)$name, # specifies the lables of the vertices, in this case, the 'name' attribute is used
     vertex.label.cex = 0.5, # specifies the size of the font of the labels. can also be made to vary
     edge.arrow.size = 0.5
)
