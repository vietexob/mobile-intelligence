fillTheGaps <- function(rowVector, interval, maxDurations) {
  if(length(rowVector) < 1) {
    stop("NULL vector input")
  }
  
  if(length(maxDurations) == 1) {
    maxNIntervals <- ceiling((maxDurations / interval)) # Max. # intervals
  }
  counter <- 0
  curAct <- 0 # current activity
  intervalCounter <- 0
  
  for(i in 1:length(rowVector)) {
    if(rowVector[i] > 0) {
      counter <- counter + 1
      curAct <- rowVector[i]
#       print(paste("curAct =", curAct))
      intervalCounter <- 1
      
    }
    else {
      if(counter > 0) {
        if(length(maxDurations) > 1) {
          curActIndex <- which(names(maxDurations) == curAct)
          maxNIntervals <- ceiling((maxDurations[curActIndex] / interval))
        }
        intervalCounter <- intervalCounter + 1
        if(intervalCounter <= maxNIntervals) {
          rowVector[i] <- curAct
        }
      }
    }
  }
  
  return(rowVector)
}
