getGraphObj <- function(weightedEdges) {
  # Construct & returns the iGraph object from the weightedEdges data frame
  require(igraph)
  
  freq <- weightedEdges$Freq
  # Remove the less frequent edges
  summ.freq <- summary(freq)
  first.quartile <- as.numeric(summ.freq[2])
  weightedEdges <- subset(weightedEdges, Freq > first.quartile)
  freq <- weightedEdges$Freq
  
  weightedEdges.matrix <- as.matrix(weightedEdges[, -3]) # remove the 'Freq' column
  g <- graph.edgelist(weightedEdges.matrix, directed = TRUE)
  E(g)$weight <- freq # add weights to the edges
  
  return(g)
}
