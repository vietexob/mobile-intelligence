getWeightedEdges <- function(trajec.data) {
  # Returns the data frame that represents the
  # directed edges <from.attrId, to.attrId>
  
  from.attrId <- vector()
  to.attrId <- vector()
  userIds <- names(table(trajec.data$userId))
  counter <- 1
  
  for(i in 1:length(userIds)) {
    aUserId <- userIds[i]
    subset.userId <- subset(trajec.data, userId == aUserId)
    
    if(nrow(subset.userId) > 1) {
      attrIds <- subset.userId$attrId
      for(j in 1:(length(attrIds)-1)) {
        from <- attrIds[j]
        to <- attrIds[j+1]
        
        from.attrId[counter] <- from
        to.attrId[counter] <- to
        counter <- counter + 1
      }
    }
  }
  
  edges <- data.frame(fromAttrId = from.attrId, toAttrId = to.attrId)
  weightedEdges <- data.frame(table(edges))
  weightedEdges <- subset(weightedEdges, Freq > 0)
  
  return(weightedEdges)
}
