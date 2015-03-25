rm(list = ls())

source("./code/sna/plfit.R")

## This dataset is the result of network structure analysis
load("./data/sna/stackoverflow_graph.RData")
load("./data/sna/userId_map_reverse.RData")

weightedEdges <- read.csv(file = "./data/sna/weighted_edges.csv", header = TRUE)

# Centrality measures for the nodes/users
top <- 50
# (1) No. of questions answered --> weighted outdegree
# (2) Count how many people one helps --> unweighted outdegree
nodeIdAnsCount.map <- new.env() # weighted count
nodeIdUserCount.map <- new.env() # unweighted count
for(i in 1:nrow(weightedEdges)) {
  nodeIdStr <- toString(weightedEdges$fromUserId[i])
  count <- weightedEdges$freq[i]
  
  if(!is.null(nodeIdAnsCount.map[[nodeIdStr]])) {
    totalCount <- nodeIdAnsCount.map[[nodeIdStr]]
    totalCount <- totalCount + count
    nodeIdAnsCount.map[[nodeIdStr]] <- totalCount
    
    userCount <- nodeIdUserCount.map[[nodeIdStr]]
    nodeIdUserCount.map[[nodeIdStr]] <- userCount + 1
  }
  else {
    nodeIdAnsCount.map[[nodeIdStr]] <- count
    nodeIdUserCount.map[[nodeIdStr]] <- 1
  }
}
ansNodeId <- unique(weightedEdges$fromUserId)

ansCount <- vector()
ansUserCount <- vector()
for(i in 1:length(ansNodeId)) {
  nodeIdStr <- toString(ansNodeId[i])
  totalCount <- nodeIdAnsCount.map[[nodeIdStr]]
  ansCount[i] <- totalCount
  
  userCount <- nodeIdUserCount.map[[nodeIdStr]]
  ansUserCount[i] <- userCount
}

sorted.ansCount <- sort(ansCount, decreasing = TRUE)
sorted.ansUserCount <- sort(ansUserCount, decreasing = TRUE)
for(i in 1:top) {
  index <- which(ansCount == sorted.ansCount[i])
  nodeIdStr <- toString(ansNodeId[i])
  userId <- userId.map.reverse[[nodeIdStr]]
  print(paste("UserId with No. Ans #", i, ":", userId, "@", sorted.ansCount[i]))
}
for(i in 1:top) {
  index <- which(ansUserCount == sorted.ansUserCount[i])
  nodeIdStr <- toString(ansNodeId[i])
  userId <- userId.map.reverse[[nodeIdStr]]
  print(paste("UserId with No. Users Helped #", i, ":", userId, "@",
              sorted.ansUserCount[i]))
}

# (3) Find directed betweenness scores for each node
btw <- betweenness(g, directed = TRUE)
sorted.btw <- sort(btw, decreasing = TRUE)
for(i in 1:top) {
  nodeId <- which(btw == sorted.btw[i])
  userId <- userId.map.reverse[[toString(nodeId)]]
  print(paste("UserId with Betweenness #", i, ":", userId, "@",
              round(sorted.btw[i], digits = 3)))
}

# (4) PageRank --> ExpertiseRank algorithm
# Get the top-50 PageRanked nodes
pr <- page.rank(g, directed = TRUE)
sorted.pr <- sort(pr$vector, decreasing = TRUE)
for(i in 1:top) {
  nodeId <- which(pr$vector == sorted.pr[i])
  userId <- userId.map.reverse[[toString(nodeId)]]
  print(paste("UserId with PageRank #", i, ":", userId, "@",
              round(sorted.pr[i], digits = 5)))
}

# (5) HITS authority algorithm
authority <- authority.score(g)$vector
sorted.authority <- sort(authority, decreasing = TRUE)
for(i in 1:top) {
  nodeId <- which(authority == sorted.authority[i])
  userId <- userId.map.reverse[[toString(nodeId)]]
  print(paste("UserId with HITS Authority #", i, ":", userId, "@",
              round(sorted.authority[i], digits = 5)))
}

# COMMUNITIES
# Find the maximal k-core any vertex belongs to
k.core <- graph.coreness(as.undirected(g), mode = "all")

# # Fastgreedy community finding algorithm (greedy optimization of modularity)
# fc <- fastgreedy.community(as.undirected(g)) # This line causes crash!
# # Community sizes
# print("Fast Greedy community sizes:")
# print(sizes(fc))
# # Membership in the 10th community
# members <- V(g)[membership(fc) == 10]
# print("Membership in the 10th community:")
# for(i in 1:length(members)) {
#   memberIdStr <- toString(members[[i]])
#   userIdStr <- userId.map[[memberIdStr]]
#   print(userIdStr)
# }

# InfoMap community finding algorithm (can be slow)
imc <- infomap.community(g)
numComm <- length(unique(imc$membership)) # number of communities
for(i in 1:numComm) {
  members <- which(imc$membership == i)
  memUserIdStr <- vector()
  for(j in 1:length(members)) {
    nodeIdStr <- toString(members[j])
    userIdStr <- userId.map.reverse[[nodeIdStr]]
    if(!is.null(userIdStr)) {
      memUserIdStr[j] <- userIdStr
    }
  }
  
  memUserIdStr <- subset(memUserIdStr , !is.na(memUserIdStr))
  if(length(memUserIdStr) > 0) {
    print(memUserIdStr)
  }
}
print("InfoMap community sizes:")
print(sizes(imc))

# Find the nodes in the largest clique
print("UserId's in the largest clique:")
largestClique <- V(g)[largest.cliques(as.undirected(g))[[1]]]
## Convert to string
largestCliqueStr <- toString(largestClique)
## Split the string by comma
largestCliqueStr <- strsplit(largestCliqueStr, ",")
## Convert to vector of strings
largestCliqueStr <- largestCliqueStr[[1]]
for(i in 1:length(largestCliqueStr)) {
  userIdStr <- largestCliqueStr[i]
  print(userIdStr)
}

# Save as Gephi file
write.graph(g, file = "./data/sna/stackoverflow.graphml", format = "graphml")
