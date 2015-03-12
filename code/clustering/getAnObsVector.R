getAnObsVector <- function(startTime, endTime, interval, attrSeq, timeSeq) {
  if(endTime - startTime <= interval) {
    stop("endTime must exceed startTime")
  }
  
  attrSeqStr <- toString(attrSeq)
  attrSeqStr <- strsplit(attrSeqStr, ",")[[1]]
  seqLen <- length(attrSeqStr)
  
  timeSeqStr <- toString(timeSeq)
  timeSeqStr <- strsplit(timeSeqStr, ",")[[1]]
  timeSeq <- as.numeric(timeSeqStr)
  
  nIntervals <- round((endTime - startTime) / interval)
  actVector <- numeric(length = nIntervals) # activity vector
  for(i in 1:nIntervals) {
    intStartTime <- (i-1) * interval # interval startTime
    intEndTime <- i * interval # interval endTime
    
    notFound <- TRUE
    counter <- 1
    while(notFound && counter <= seqLen) {
      actTime <- timeSeq[counter]
      if(actTime >= intStartTime && actTime < intEndTime) {
        actVector[i] <- attrSeqStr[counter]
        notFound <- FALSE
      }
      
      counter <- counter + 1
    }
  }
  
  return(actVector)
}
