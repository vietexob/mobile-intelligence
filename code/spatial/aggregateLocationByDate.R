aggregateLocationByDate <- function(call.data, cell_id.coord.rowIndex, cell.towers) {
  ## This function aggregates the retrieved call data by each date.
  ## Specifically, it first sorts each call record by date and finds the coordinates
  ## of each cell_id where the call was made from. It then finds the frequency of calls
  ## made from a particular cell_id during the duration covered by the call data.
  
  dates <- names(table(call.data$date)) # get all the dates
  aggregate.data <- data.frame() # master data frame to store the records
  
  for(i in 1:length(dates)) { # for each date
    ## Subset the data by date
    subset.call.data <- subset(call.data, date == dates[i])
    ## Get all cell_ids of the subset data
    cellId.table <- table(subset.call.data$cell_id)
    cellId.table <- subset(cellId.table, cellId.table > 0)
    ## Get cell_id names and frequencies
    cellIds <- names(cellId.table)
    cellId.freq <- as.numeric(cellId.table)
    
    ## Retrieve the corresponding cell tower coordinates
    longitude <- vector()
    latitude <- vector()
    counter <- 0 # count the number of cell_id's that can be located (i.e., matched)
    for(j in 1:length(cellId.table)) { # go through each cell_id
      if(nchar(cellIds[j]) > 0) { # if it's not empty
        rowIndex <- cell_id.coord.rowIndex[[cellIds[j]]]
        if(!is.null(rowIndex)) { # if it can be located
          ## Get the longitude and latitude, make sure they are in numeric format
          aLongitude <- cell.towers$Longitude[rowIndex]
          if(!is.numeric(aLongitude)) {
            aLongitude <- as.numeric(aLongitude)
          }
          
          aLatitude <- cell.towers$Latitude[rowIndex]
          if(!is.numeric(aLatitude)) {
            aLatitude <- as.numeric(aLatitude)
          }
          
          longitude[j] <- as.numeric(aLongitude)
          latitude[j] <- as.numeric(aLatitude)
          counter <- counter + 1
        } else {
          stop(paste("Unmatched cell_id:", cellIds[j]))
        }
      } else {
        stop(paste("Zero-length cell_id:", j))
      }
    }
    
    ## Create an aggregate dataset
    dateStr <- toString(as.Date(dates[i], "%Y%m%d"))
    date.aggregate.data <- data.frame(cell_id = cellIds, freq = cellId.freq,
                                      longitude = longitude, latitude = latitude,
                                      date = rep(dateStr, counter))
    ## Combine each date with the master data frame
    aggregate.data <- rbind(aggregate.data, date.aggregate.data)
  }
  
  return(aggregate.data)
}
