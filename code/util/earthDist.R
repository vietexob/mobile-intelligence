earthDist <- function(lon1, lat1, lon2, lat2) {
  ## Computes the Euclidean distsance between 2 given points
  ## defined by their coordinates in kilometers.
  
  R <- 6371 # Earth's radius in kilometers
  rad <- pi / 180
  
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  
  return(d)
}
