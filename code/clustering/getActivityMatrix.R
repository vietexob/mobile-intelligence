getActivityMatrix <- function (call.data, delta=15) {
  ## This function returns an activity matrix of dimension n x m, where n is the number of
  ## agents (i.e., individuals) and m is the number of discrete time periods of a day (24-hour
  ## period). Each activity is a known or unknown location (which is specified by symbol '0')
  ## of an individual recorded during a specific time period of a day. This assumes the input
  ## to be a call data frame and delta is the length (in minutes) of the time period that
  ## divides the day into. Default value of delta is 15 minutes.
  require(plyr)
  
  ## Get a list of caller ids
  caller_id.table <- table(call.data$caller_id)
  caller_id.table <- subset(caller_id.table, caller_id.table > 0)
  caller_ids <- names(caller_id.table)
  
  ## Construct an n x m activity matrix, where each row is an individual and each
  ## column is a discrete time period. Each entry of the matrix is the recorded location
  ## of each individual (row) during each discrete time period (column).
  periods <- seq(from=0, to=24*60, by=delta)
  act.matrix <- matrix("0", nrow=length(caller_ids), ncol=(length(periods)-1))
  rownames(act.matrix) <- caller_ids # name the rows by caller ids
  colnames(act.matrix) <- periods[1:(length(periods)-1)] # name the cols by periods
  
  ## Because during a time period, multiple different locations could be recorded
  ## for a given individual, we therefore need a hash table to store the "collided"
  ## activities (or locations)
  collision <- new.env()
  keys <- vector() # keys of the hash table (i.e., mapping)
  
  ## Populate the activity matrix
  progress.bar <- create_progress_bar("text")
  progress.bar$init(length(caller_ids))
  for(i in 1:length(caller_ids)) { # iterate through each caller id
    a_caller_id <- caller_ids[i]
    ## Get all records by that caller id only
    subset.caller_id <- subset(call.data, caller_id == a_caller_id)
    
    for(j in 1:nrow(subset.caller_id)) {
      ## Get the time of the call event (in minutes)
      minute <- subset.caller_id$minute[j]
      ## Determine the period that it belongs to: the maximum period that's below it
      lower_bound_index <- max(which(periods <= minute))
      ## Check if the period has been allocated any activity (location)
      if(act.matrix[i, lower_bound_index] != "0") { # has been allocated
        ## Create a mapping from the period to a vector of locations (the values)
        keyStr <- paste(i, lower_bound_index, sep=",")
        values <- collision[[keyStr]]
        new.value <- toString(subset.caller_id$cell_id[j])
        
        if(is.null(values)) { # the hash table doesn't contain the entry yet
          old.value <- act.matrix[i, lower_bound_index]
          values <- c(old.value, new.value)
        } else { # there are existing values (i.e., hash table has an entry)
          values <- c(values, new.value)
        }
        
        ## Put key-values pair to the hash table
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
  ## For each of those in the activity matrix whose entry has collisions, we take the
  ## activity that receives the highest frequency only (and discard the rest)
  all.keys <- names(table(keys))
  for(aKey in all.keys) {
    ## Get row and column index from the key string
    keyStr <- strsplit(aKey, ",")[[1]]
    rowIndex <- as.numeric(keyStr[1])
    colIndex <- as.numeric(keyStr[2])
    
    ## Retrieve the vector of values and take one with the highest frequency
    values <- collision[[aKey]]
    value.table <- table(values)
    ## Take the highest frequency
    sorted.values <- sort(value.table, decreasing=TRUE)
    max.value <- names(sorted.values)[1]
    ## Add the highest frequency value to the activity matrix
    act.matrix[rowIndex, colIndex] <- max.value
  }
  
  return(act.matrix)
}
