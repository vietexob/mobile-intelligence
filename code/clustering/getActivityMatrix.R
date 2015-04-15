getActivityMatrix <- function (call.data, delta=15) {
  ## This function returns an activity matrix of dimension n x m, where n is the number of
  ## agents (i.e., individuals) and m is the number of discrete time periods of a day (24-hour
  ## period). Each activity is a known or unknown (which is specified by symbol '0') location
  ## of an individual recorded during a specific time period of a day. This assumes the input
  ## to be a call data frame and delta is the length (in minutes) of the time period that
  ## divides the day into. Default value of delta is 15 minutes.
  require(plyr)
  
  caller_id.table <- table(call.data$caller_id)
  caller_id.table <- subset(caller_id.table, caller_id.table > 0)
  caller_ids <- names(caller_id.table)
  
  periods <- seq(from=0, to=24*60, by=delta)
  act.matrix <- matrix("0", nrow=length(caller_ids), ncol=(length(periods)-1))
  rownames(act.matrix) <- caller_ids
  colnames(act.matrix) <- periods[1:(length(periods)-1)]
  
  ## Hash table to store "collided" activities
  collision <- new.env()
  keys <- vector()
  
  ## Populate the activity matrix
  progress.bar <- create_progress_bar("text")
  progress.bar$init(length(caller_ids))
  for(i in 1:length(caller_ids)) {
    a_caller_id <- caller_ids[i]
    subset.caller_id <- subset(call.data, caller_id == a_caller_id)
    
    for(j in 1:nrow(subset.caller_id)) {
      minute <- subset.caller_id$minute[j]
      lower_bound_index <- max(which(periods <= minute))
      if(act.matrix[i, lower_bound_index] != "0") { # has been allocated
        keyStr <- paste(i, lower_bound_index, sep=",")
        values <- collision[[keyStr]]
        new.value <- toString(subset.caller_id$cell_id[j])
        
        if(is.null(values)) {
          old.value <- act.matrix[i, lower_bound_index]
          values <- c(old.value, new.value)
        } else { # there are existing values
          values <- c(values, new.value)
        }
        
        keys <- c(keys, keyStr)
        collision[[keyStr]] <- values
      } else { # has not been used
        a_cell_id <- toString(subset.caller_id$cell_id[j])
        act.matrix[i, lower_bound_index] <- a_cell_id
      }
    }
    progress.bar$step()
  }
  
  ## Redo those entries with collisions
  all.keys <- names(table(keys))
  for(aKey in all.keys) {
    keyStr <- strsplit(aKey, ",")[[1]]
    rowIndex <- as.numeric(keyStr[1])
    colIndex <- as.numeric(keyStr[2])
    
    values <- collision[[aKey]]
    value.table <- table(values)
    sorted.values <- sort(value.table, decreasing=TRUE)
    max.value <- names(sorted.values)[1]
    act.matrix[rowIndex, colIndex] <- max.value
  }
  
  return(act.matrix)
}
