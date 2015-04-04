getWeightedEdges <- function(call.data, lower.bound=0, upper.bound=50) {
  ## Returns a data frame that represents the weighted edges
  ## between callers and callees in the mobile phone data.
  
  ## Select the callers that are also callees
  call_ids <- intersect(call.data$caller_id, call.data$callee_id)
  ## TODO: Should reconsider this criteria to reduce data loss
  subset.call.data <- subset(call.data, caller_id %in% call_ids)
  subset.call.data <- subset(subset.call.data, callee_id %in% call_ids)
  
  ## Associate each caller with their most frequented location
  caller_id.cell_id <- new.env()
  for(a_call_id in call_ids) {
    subset.caller_id <- subset(subset.call.data, caller_id == a_call_id)
    if(nrow(subset.caller_id) > 0) {
      ## Find the most frequented location
      cell_id.table <- table(subset.caller_id$cell_id)
      most.freq.cell_id <- names(sort(cell_id.table, decreasing=TRUE))[1]
      print(paste(a_call_id, most.freq.cell_id))
      caller_id.cell_id[[toString(a_call_id)]] <- most.freq.cell_id
    }
  }
  
  edges <- data.frame(caller_id = subset.call.data$caller_id,
                      callee_id = subset.call.data$callee_id)
  weightedEdges <- data.frame(table(edges))
  weightedEdges <- subset(weightedEdges, Freq > lower.bound)
  weightedEdges <- subset(weightedEdges, Freq < upper.bound)
  
  caller_location <- vector()
  callee_location <- vector()
  for(i in 1:nrow(weightedEdges)) {
    caller_id <- toString(weightedEdges$caller_id[i])
    caller_loc <- caller_id.cell_id[[caller_id]]
    if(is.null(caller_loc)) {
      caller_location[i] <- NA
    } else {
      caller_location[i] <- caller_loc
    }
    
    callee_id <- toString(weightedEdges$callee_id[i])
    callee_loc <- caller_id.cell_id[[callee_id]]
    if(is.null(callee_loc)) {
      callee_location[i] <- NA
    } else {
      callee_location[i] <- callee_loc
    }
  }
  
  weightedEdges <- cbind(weightedEdges, caller_location, callee_location)
  return(weightedEdges)
}
