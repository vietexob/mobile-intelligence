getWeightedEdges <- function(call.data, lower.bound=0, upper.bound=50) {
  ## Returns a data frame that represents the weighted edges
  ## between callers and callees in the mobile phone data.
  
  ## Select the callers that are also callees
  call_ids <- intersect(call.data$caller_id, call.data$callee_id)
  ## TODO: Should reconsider this criteria to reduce data loss
  subset.call.data <- subset(call.data, caller_id %in% call_ids)
  subset.call.data <- subset(subset.call.data, callee_id %in% call_ids)
  
  ## Associate each caller with their **most frequented** location
  ## This is because each agent (caller/callee) is mobile, thus their location
  ## keeps changing throughout the duration, and we can only plot one of those
  ## during the duration.
  caller_id.cell_id <- new.env()
  for(a_call_id in call_ids) {
    subset.caller_id <- subset(subset.call.data, caller_id == a_call_id)
    if(nrow(subset.caller_id) > 0) {
      ## Find the most frequented location
      cell_id.table <- table(subset.caller_id$cell_id)
      cell_id.table <- subset(cell_id.table, cell_id.table > 0)
#       print(length(table(cell_id.table)))
#       most.freq.cell_id <- names(sort(cell_id.table, decreasing=TRUE))[1]
      if(length(cell_id.table) > 1) {
        print(a_call_id)
        print(names(cell_id.table))
      }
      caller_id.cell_id[[toString(a_call_id)]] <- names(cell_id.table)
    }
  }
  
  ## Create a data frame that stores the relationships (i.e., edges)
  ## among the agents. Strength of the relationship is represented by call frequency.
  edges <- data.frame(caller_id = subset.call.data$caller_id,
                      callee_id = subset.call.data$callee_id)
  weightedEdges <- data.frame(table(edges))
  weightedEdges <- subset(weightedEdges, Freq > lower.bound)
  weightedEdges <- subset(weightedEdges, Freq < upper.bound)
  
  ## Map each agent to a pair of spatial coordinates
  caller_location <- vector()
  callee_location <- vector()
  caller_locs <- vector()
  callee_locs <- vector()
  for(i in 1:nrow(weightedEdges)) {
    caller_id <- toString(weightedEdges$caller_id[i])
    caller_loc <- caller_id.cell_id[[caller_id]]
    if(is.null(caller_loc)) {
      caller_location[i] <- NA
    } else {
      if(length(caller_loc) == 1) {
        caller_location[i] <- caller_loc
      } else {
        caller_locs <- caller_loc
      }
    }
    
    callee_id <- toString(weightedEdges$callee_id[i])
    callee_loc <- caller_id.cell_id[[callee_id]]
    if(is.null(callee_loc)) {
      callee_location[i] <- NA
    } else {
      if(length(callee_loc) == 1) {
        callee_location[i] <- callee_loc
      } else {
        callee_locs <- callee_loc
      }
    }
    
    if(length(caller_locs) > 0 || length(callee_locs) > 0) {
      if(length(caller_locs) > 0 && length(callee_locs) > 0) {
        
      } else {
        if(length(caller_locs) > 0) {
          
        } else {
          print("This will never happen?")
        }
      }
    }
  }
  
  ## Combine the attributes into a data frame and return
  weightedEdges <- cbind(weightedEdges, caller_location, callee_location)
  return(weightedEdges)
}
