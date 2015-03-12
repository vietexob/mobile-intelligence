afterPCA <- function(matAdjust = 'Centered matrix',
                     meanList = 'List of column means of original (unadjusted) matrix',
                     eigenList = 'PCA object returned from prcomp',
                     n = 'selected PC\'s',
                     specific_select = 'If True: just the n\'th column, else: n == 1:n') {
  # Activity reconstruction
  if(length(n) > ncol(matAdjust)) {
    stop("N is higher than the number of PC\'s")
  }
  if(!specific_select & length(n) > 1) {
    stop("Use a single number when selecting up to n\'th PC")
  }
  if(!specific_select) {
    n <- 1:n
  }
  
  meanMatrix <- matrix(meanList, nrow = nrow(matAdjust), ncol = ncol(matAdjust))
  (t(eigenList$rotation[, n] %*% (t(eigenList$rotation[, n]) %*% t(matAdjust)))
   + meanMatrix)
}
