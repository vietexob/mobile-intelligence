computeKLDivergence <- function(x.distr, y.distr) {
  # Returns the matrix representing the KL distances between
  # two input distributions x and y (distances are NOT symmetric).
  require(flexmix)
  
#   if(length(x.distr) != length(y.distr)) {
#     warning("Error: Input distributions are not of equal length!")
#   }
  
  # Get the probability densities
  # For the generated set
  x.dens <- density(x.distr)
  x.dens.x <- x.dens$x
  x.dens.y <- x.dens$y
  x.len <- length(x.dens.x)
  # For the test set
  y.dens <- density(y.distr)
  y.dens.x <- y.dens$x
  y.dens.y <- y.dens$y
  y.len <- length(y.dens.x)
  
  lowerBound <- max(x.dens.x[1], y.dens.x[1])
  upperBound <- min(x.dens.x[x.len], y.dens.x[y.len])
  x.lowerIndices <- which(x.dens.x >= lowerBound)
  x.upperIndices <- which(x.dens.x <= upperBound)
  x.indices <- intersect(x.lowerIndices, x.upperIndices)
  if(length(x.indices) == 0) {
    return(-1)
  }
  x.dens.x <- x.dens.x[x.indices] # select the overlap only
  x.len <- length(x.indices)
  
  y.lowerIndices <- which(y.dens.x >= lowerBound)
  y.upperIndices <- which(y.dens.x <= upperBound)
  y.indices <- intersect(y.lowerIndices, y.upperIndices)
  if(length(y.indices) == 0) {
    return(-1)
  }
  y.dens.x <- y.dens.x[y.indices] # select the overlap only
  y.len <- length(y.indices)
  
  if(x.len > y.len) { # shrink x.dens.y
    y.dens.y <- y.dens.y[y.indices]
    new.x.dens.y <- vector()
    
    for(i in 2:y.len) {
      lower <- y.dens.x[i-1]
      upper <- y.dens.x[i]
      lowerIndices <- which(x.dens.x >= lower)
      upperIndices <- which(x.dens.x < upper)
      x.indices <- intersect(lowerIndices, upperIndices)
      new.x.dens.y[i-1] <- mean(x.dens.y[x.indices])
    }
    
    x.indices <- which(x.dens.x >= upper)
    lastValue <- mean(x.dens.y[x.indices])
    if(is.nan(lastValue)) {
      lastValue <- 0
    }
    new.x.dens.y[y.len] <- lastValue
    x.dens.y <- new.x.dens.y
  }
  else if(y.len > x.len) { # shrink y.dens.y
    x.dens.y <- x.dens.y[x.indices]
    new.y.dens.y <- vector()
    
    for(i in 2:x.len) {
      lower <- x.dens.x[i-1]
      upper <- x.dens.x[i]
      lowerIndices <- which(y.dens.x >= lower)
      upperIndices <- which(y.dens.x < upper)
      y.indices <- intersect(lowerIndices, upperIndices)
      new.y.dens.y[i-1] <- mean(y.dens.y[y.indices])
    }
    
    y.indices <- which(y.dens.x >= upper)
    lastValue <- mean(y.dens.y[y.indices])
    if(is.nan(lastValue)) {
      lastValue <- 0
    }
    new.y.dens.y[x.len] <- lastValue
    y.dens.y <- new.y.dens.y
  }
  
  xy.distr <- cbind(x.dens.y, y.dens.y)
  return(KLdiv(xy.distr))
}
