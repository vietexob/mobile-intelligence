getGraphObj <- function(weightedEdges) {
  # Construct & returns the igraph object from the weightedEdges data frame
  require(igraph)
  
  freq <- weightedEdges$freq
  # Remove the less frequent edges
  summ.freq <- summary(freq)
  first.quartile <- as.numeric(summ.freq[2])
  weightedEdges <- subset(weightedEdges, freq > first.quartile)
  freq <- weightedEdges$freq
  
  weightedEdges.matrix <- as.matrix(weightedEdges[, -3]) # remove the 'freq' column
  g <- graph.edgelist(weightedEdges.matrix, directed = TRUE)
  E(g)$weight <- freq # add weights to the edges
  
  return(g)
}
