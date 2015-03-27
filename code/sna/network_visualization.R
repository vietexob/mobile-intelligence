## Author: Truc Viet 'Joe' Le at tjle@andrew.cmu.edu
## This script demonstrates network community detection using igraph. It also shows how
## to visualize the network and its discovered communities with different colors (one for
## each community). Nodes in the network are scaled according to their outdegree and colored
## by their community. Edges in the network are colored according to a defined threshold:
## If an edge weight (the number of communications between two nodes) is large than
## the threshold, color it bright orange, otherwise gray. The visualized network is finally
## saved to disk as a PDF file for further use.

rm(list = ls())

library(igraph)

## Load the useful functions
source("./code/sna/plotNetwork.R")

## Load the necessary datasets
load("./data/sna/stackoverflow_graph.RData")
load("./data/sna/userId_map_reverse.RData")
## 'g' is the igraph object representing the network

## Use userId as the vertex name
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

## Exclude those nodes that are at the periphery of the network
theta <- 55 # the threshold
bad.nodes <- V(g)[degree(g) < theta] # low-degree nodes
f <- delete.vertices(g, bad.nodes) # f is the new network

## Run community detection and color those communities
## Fastgreedy community finding algorithm (greedy optimization of modularity)
fc <- fastgreedy.community(as.undirected(f))
print("Fast Greedy community sizes:")
print(sizes(fc))

## Sample random colors for the communities
fc.colors <- sample(colors(), length(fc))
## Color the nodes by the community
for(i in 1:length(fc.colors)) {
  V(f)[membership(fc)==i]$color <- fc.colors[i]
}

## We also can color the edges differently according to weights
## E.g.,
# E(f)$color < ifelse(E(f)$freq >= 50, 'red', 'gray')
## Define edge widths:
# E(f)$width <- Edges$thickness * 5
## Define arrow widths:
# E(f)$arrow.width <- Edges$thickness * 5

## Size the vertices according to their outdegrees
V(f)$size <- degree(f, mode="out") / 8

## Finally, plot and save the network
filename <- "./figures/sna/network_igraph.pdf"
mainStr <- "Stack Overflow Network Visualization"
plotNetwork(f, mainStr, filename, width=15, height=15)
