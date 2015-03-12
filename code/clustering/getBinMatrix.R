getBinMatrix <- function(trajec.matrix) {
  # Transform each visitor into binary matrices
  trajec.list <- list()
  actTypes <- names(table(trajec.matrix))
  for(i in 1:nrow(trajec.matrix)) {
    actMatrix <- matrix(0, nrow = nIntervals, ncol = length(actTypes))
    colnames(actMatrix) <- actTypes
    for(j in 1:ncol(trajec.matrix)) {
      colIndex <- which(actTypes == trajec.matrix[i, j])
      actMatrix[j, colIndex] <- 1
    }
    
    trajec.list[[i]] <- actMatrix
  }
  
  # Merge the binary matrices into one by vectorizing each of them
  firstVector <- c(t(trajec.list[[1]]))
  binMatrix <- matrix(0, nrow = nrow(trajec.matrix), ncol = length(firstVector))
  binMatrix[1, ] <- firstVector
  for(i in 2:nrow(binMatrix)) {
    actVector <- c(t(trajec.list[[i]]))
    binMatrix[i, ] <- actVector
  }
  
  return(binMatrix)
}
