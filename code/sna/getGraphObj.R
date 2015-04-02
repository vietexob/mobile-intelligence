## Author: Truc Viet 'Joe' Le at tjle@andrew.cmu.edu

## This function constructs and returns an igraph object from the input data frame.

getGraphObj <- function(weightedEdges, threshold=0) {
  require(igraph)
  
  freq <- weightedEdges$freq
  # Remove the less frequent edges
  if(threshold == 0) {
    summ.freq <- summary(freq)
    first.quartile <- as.numeric(summ.freq[2])
    threshold <- first.quartile
  }
  weightedEdges <- subset(weightedEdges, freq > threshold)
  freq <- weightedEdges$freq
  
  weightedEdges.matrix <- as.matrix(weightedEdges[, -3]) # remove the 'freq' column
  g <- graph.edgelist(weightedEdges.matrix, directed = TRUE)
  E(g)$weight <- freq # add weights back to the edges
  
  return(g)
}
