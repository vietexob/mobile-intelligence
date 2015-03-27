rm(list = ls()) # clear the workspace

library(igraph)

## Load the useful functions
source("./code/sna/plfit.R")
source("./code/sna/plotNetwork.R")

## This dataset is the result of network structure analysis
load("./data/sna/stackoverflow_graph.RData")
## This dataset maps from nodeId to userId
load("./data/sna/userId_map_reverse.RData")

## Load the CSV file representing the network (nodes and edges)
weightedEdges <- read.csv(file = "./data/sna/weighted_edges.csv", header = TRUE)

## Centrality measures for the nodes (users)
top <- 20 # display the top users only

## Find directed betweenness centrality for each node
btw <- betweenness(g, directed = TRUE)
sorted.btw <- sort(btw, decreasing = TRUE)
for(i in 1:top) {
  nodeId <- which(btw == sorted.btw[i])
  userId <- userId.map.reverse[[toString(nodeId)]]
  print(paste("UserId with Betweenness #", i, ":", userId, "@",
              round(sorted.btw[i], digits = 3)))
}

## PageRank centrality
pr <- page.rank(g, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)
for(i in 1:top) {
  nodeId <- which(pr$vector == sorted.pr[i])
  userId <- userId.map.reverse[[toString(nodeId)]]
  print(paste("UserId with PageRank #", i, ":", userId, "@",
              round(sorted.pr[i], digits = 5)))
}

## HITS authority algorithm
authority <- authority.score(g)$vector
sorted.authority <- sort(authority, decreasing = TRUE)
for(i in 1:top) {
  nodeId <- which(authority == sorted.authority[i])
  userId <- userId.map.reverse[[toString(nodeId)]]
  print(paste("UserId with HITS Authority #", i, ":", userId, "@",
              round(sorted.authority[i], digits = 5)))
}

######### START: SKIP THIS PART FOR NOW #########
# # COMMUNITIES
# # Find the maximal k-core any vertex belongs to
# k.core <- graph.coreness(as.undirected(g), mode = "all")

# # InfoMap community finding algorithm (can be slow)
# imc <- infomap.community(g)
# numComm <- length(unique(imc$membership)) # number of communities
# for(i in 1:numComm) {
#   members <- which(imc$membership == i)
#   memUserIdStr <- vector()
#   for(j in 1:length(members)) {
#     nodeIdStr <- toString(members[j])
#     userIdStr <- userId.map.reverse[[nodeIdStr]]
#     if(!is.null(userIdStr)) {
#       memUserIdStr[j] <- userIdStr
#     }
#   }
#   
#   memUserIdStr <- subset(memUserIdStr , !is.na(memUserIdStr))
#   if(length(memUserIdStr) > 0) {
#     print(memUserIdStr)
#   }
# }
# print("InfoMap community sizes:")
# print(sizes(imc))

# # Find the nodes in the largest clique
# print("UserId's in the largest clique:")
# largestClique <- V(g)[largest.cliques(as.undirected(g))[[1]]]
# ## Convert to string
# largestCliqueStr <- toString(largestClique)
# ## Split the string by comma
# largestCliqueStr <- strsplit(largestCliqueStr, ",")
# ## Convert to vector of strings
# largestCliqueStr <- largestCliqueStr[[1]]
# for(i in 1:length(largestCliqueStr)) {
#   userIdStr <- largestCliqueStr[i]
#   print(userIdStr)
# }
######### END: SKIP THIS PART FOR NOW #########

## Subset the data, exclude people who are at the periphery of the network
theta <- 45 # the threshold
bad.nodes <- V(g)[degree(g) < theta] # low-degree nodes
f <- delete.vertices(g, bad.nodes) # f is the new network

## Save as Gephi file
write.graph(f, file = "./data/sna/stackoverflow.graphml", format = "graphml")
