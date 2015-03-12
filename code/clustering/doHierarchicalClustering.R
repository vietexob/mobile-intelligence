doHierarchicalClustering <- function(activityMatrix, dist.matrix, tree.filename,
                                     is.trate=FALSE) {
  library(TraMineR)
  library(cluster)
  
  # Define the sequences
  n.states <- length(table(activityMatrix))
  cl <- colors()[seq(from = 1, by = 5, length.out = n.states)]
  trajec.seq <- seqdef(activityMatrix, xtstep = 2, cpal = cl)
  
  # Compute the pairwise optimal matching (OM) distances between sequences
  # with insertion/deletion cost of 1 and substitution cost matrix based on dist.matrix
  if(is.trate) {
    trajec.om <- seqdist(trajec.seq, method = "OM", indel = 1, sm = "TRATE")
  }
  else {
    trajec.om <- seqdist(trajec.seq, method = "OM", indel = 1, sm = dist.matrix)
  }
  
  # Do hierarchical cluster analysis
  cluster.ward <- agnes(trajec.om, diss = TRUE, method = "ward")
  
  # Plot the hierarchy (tree)
  pdf(file = tree.filename)
  mainStr <- "Sequence Hierarchical Clustering"
  plot(cluster.ward, which.plots = 2, labels = FALSE, main = mainStr)
  dev.off()
  
  return(list(trajec.seq, cluster.ward))
}
