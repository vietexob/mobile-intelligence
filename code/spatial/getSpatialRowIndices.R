getSpatialRowIndices <- function (call.data, cell_id.coord.rowIndex) {
  ## Match each cell_id in call.data with its corresponding rowIndex of 
  ## the cell tower coordinate table
  require(plyr)
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(nrow(call.data))
  rowIndices <- vector() # a vector of row indices
  nMatches <- 0 # count the number of matches
  
  for(i in 1:nrow(call.data)) {
    ## Have to convert numeric to string in order to do lookup
    cell_id <- toString(call.data$cell_id[i])
    if(nchar(cell_id) > 0) {
      ## Look up the (loaded) table
      rowIndex <- cell_id.coord.rowIndex[[cell_id]]
      if(is.null(rowIndex)) { # if matched
        rowIndices <- c(rowIndices, NA)
      } else { # if unmatched
        rowIndices <- c(rowIndices, rowIndex)
        nMatches <- nMatches + 1
      }
    } else {
      rowIndices <- c(rowIndices, NA)
    }
    
    progress.bar$step()
  }
  ## Calculate the matched percentage
  match.pct <- round(nMatches / nrow(call.data) * 100, 2)
  print(paste("Percent matched locations =", match.pct))
  
  return(rowIndices)
}
