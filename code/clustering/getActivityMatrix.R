getActivityMatrix <- function(trajec.data, maxDurations,
                              startTime=0, endTime=600, interval=5) {
  # Transform each individual into a finite vector of observations
  nIntervals <- round((endTime - startTime) / interval)
  
  trajecs.matrix <- matrix(data = 0, nrow = nrow(trajec.data), ncol = nIntervals)
  for(i in 1:nrow(trajecs.matrix)) {
#     print(trajec.data$UserId[i])
    attrSeq <- trajec.data$AttrId[i]
    timeSeq <- trajec.data$TimeSeq[i]
    
    rowVector <- getAnObsVector(startTime, endTime, interval, attrSeq, timeSeq)
    trajecs.matrix[i, ] <- fillTheGaps(rowVector, interval, maxDurations)
  }
  
  return(trajecs.matrix)
}
