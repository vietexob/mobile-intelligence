getTransitionMatrix <- function(trajec.data, is.joint=FALSE) {
  source("./code/sna/getWeightedEdges.R")
  
  edges <- getWeightedEdges(trajec.data)
  from.attrNames <- names(table(edges$fromAttrId))
  from.len <- length(from.attrNames)
  to.attrNames <- names(table(edges$toAttrId))
  to.len <- length(to.attrNames)
  
  trans.matrix <- matrix(data = 0, nrow = from.len, ncol = to.len)
  rownames(trans.matrix) <- from.attrNames
  colnames(trans.matrix) <- to.attrNames
  
  for(i in 1:nrow(edges)) { # go through each row of the weighted edges
    from <- edges$fromAttrId[i]
    to <- edges$toAttrId[i]
    weight <- edges$Freq[i]
    
    for(row in 1:from.len) {
      isFound <- FALSE
      if(from.attrNames[row] == from) {
        for(col in 1:to.len) {
          if(to.attrNames[col] == to) {
            trans.matrix[row, col] <- weight
            isFound <- TRUE
            break
          }
        }
      }
      
      if(isFound) {
        break
      }
    }
  }
  
  no.trans <- sum(trans.matrix) # total number of transitions
  if(is.joint) {
    for(i in 1:nrow(trans.matrix)) {
      for(j in 1:ncol(trans.matrix)) {
        trans.matrix[i, j] <- trans.matrix[i, j] / no.trans
      }
    }
  }
  else {
    for(i in 1:nrow(trans.matrix)) {
      trans.matrix[i, ] <- trans.matrix[i, ] / sum(trans.matrix[i, ])
    }
  }
  
  return(trans.matrix)
}
