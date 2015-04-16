getDistanceMatrix <- function (act.matrix, cell_id.coord.rowIndex,
                               cell.towers, unknown_distance=30) {
  ## This function returns a pairwise distance matrix between all possible locations in
  ## the input activity matrix. The specified unknown distance gives the assumed distance
  ## between a known location and an unknown one (coded as the symbol "0").
  require(plyr)
  source("./code/util/earthDist.R") # compute geographical distance between 2 given locations
  
  ## Get all possible location names from the activity matrix
  ## Suppose there are n of such locations, then the distance matrix is of dimension nxn
  act.cell_ids <- names(table(act.matrix))
  dist_matrix <- matrix(0, nrow=length(act.cell_ids), ncol=length(act.cell_ids))
  ## Assign row and column names to the distance matrix
  rownames(dist_matrix) <- colnames(dist_matrix) <- act.cell_ids
  
  progress.bar <- create_progress_bar("text")
  progress.bar$init(nrow(dist_matrix)*ncol(dist_matrix))
  for(i in 1:nrow(dist_matrix)) {
    from_cell_id <- act.cell_ids[i]
    from_row_index <- -1
    from_lon <- -1
    from_lat <- -1
    if(from_cell_id != "0") { # the location is known
      ## Retrieve the corresponding row index
      from_row_index <- cell_id.coord.rowIndex[[from_cell_id]]
      if(is.null(from_row_index)) {
        stop(from_cell_id)
      }
      ## Retrieve the lon and lat coordinates
      from_lon <- cell.towers$Longitude[from_row_index]
      from_lat <- cell.towers$Latitude[from_row_index]
    }
    
    for(j in 1:ncol(dist_matrix)) {
      if(dist_matrix[i, j] == 0) { # has not been populated
        to_cell_id <- act.cell_ids[j]
        if(from_cell_id != to_cell_id) {
          if(from_cell_id == "0") { # the location is unknown
            dist_matrix[i, j] <- unknown_distance
            dist_matrix[j, i] <- unknown_distance
          } else { # the location is known
            ## Retrieve its row index and spatial coordinates
            to_row_index <- cell_id.coord.rowIndex[[to_cell_id]]
            if(is.null(to_row_index)) {
              stop(to_cell_id)
            }
            to_lon <- cell.towers$Longitude[to_row_index]
            to_lat <- cell.towers$Latitude[to_row_index]
            
            ## Get the geographical distance between two known locations
            distance <- earthDist(from_lon, from_lat, to_lon, to_lat)
            ## Distance between 2 locations is symmetrical
            dist_matrix[i, j] <- distance
            dist_matrix[j, i] <- distance
          }
        }
      }
      progress.bar$step()
    }
  }
  
  return(dist_matrix)
}
