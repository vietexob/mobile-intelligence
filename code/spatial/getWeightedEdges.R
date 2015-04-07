getWeightedEdges <- function(call.data, lower.bound=0, upper.bound=50) {
  ## Returns a data frame that represents the weighted edges
  ## between callers and callees in the mobile phone data.
  
  ## Select the callers that are also callees
  call_ids <- intersect(call.data$caller_id, call.data$callee_id)
  subset.call.data <- subset(call.data, caller_id %in% call_ids)
  subset.call.data <- subset(subset.call.data, callee_id %in% call_ids)
  
  ## Associate each caller with their **vector** of locations.
  caller_id.cell_id <- new.env()
  for(a_call_id in call_ids) {
    subset.caller_id <- subset(subset.call.data, caller_id == a_call_id)
    if(nrow(subset.caller_id) > 0) {
      ## Find the most frequented location
      cell_id.table <- table(subset.caller_id$cell_id)
      cell_id.table <- subset(cell_id.table, cell_id.table > 0)
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
  
  ## Map each agent to all of their spatial coordinates during the duration
  caller_location <- vector()
  callee_location <- vector()
  freq <- vector()
  for(i in 1:nrow(weightedEdges)) {
    a_freq <- weightedEdges$Freq[i]
    caller_id <- toString(weightedEdges$caller_id[i])
    caller_loc <- caller_id.cell_id[[caller_id]]
    callee_id <- toString(weightedEdges$callee_id[i])
    callee_loc <- caller_id.cell_id[[callee_id]]
    
    if(!is.null(caller_loc) && !is.null(callee_loc)) {
      if(length(caller_loc) == 1 && length(callee_loc) == 1) {
        ## Create an edge between the pair of locations
        caller_location <- c(caller_location, caller_loc)
        callee_location <- c(callee_location, callee_loc)
        freq <- c(freq, a_freq)
      } else { # at least one has multiple locations
        n_edges <-length(caller_loc) * length(callee_loc)
        a_freq <- ceiling(a_freq / n_edges)
        freq <- c(freq, rep(a_freq, n_edges))
        
        if(length(caller_loc) > 1 && length(callee_loc) > 1) {
          ## Create complete bipartite graph connecting the locations of callers
          ## to the locations of callees.
          for(j in 1:length(caller_loc)) {
            for(k in 1:length(callee_loc)) {
              caller_location <- c(caller_location, caller_loc[j])
              callee_location <- c(callee_location, callee_loc[k])
            }
          }
        } else {
          ## Either caller or callee has only one location (and the other has multiple)
          if(length(caller_loc) > 1) {
            for(j in 1:length(caller_loc)) {
              caller_location <- c(caller_location, caller_loc[j])
            }
            callee_location <- c(callee_location, rep(callee_loc, length(caller_loc)))
          } else { # length(callee_loc) > 1
            caller_location <- c(caller_location, rep(caller_loc, length(callee_loc)))
            for(j in 1:length(callee_loc)) {
              callee_location <- c(callee_location, callee_loc[j])
            }
          }
        }
      }
    }
  }
  
  ## Combine the attributes into a **new** data frame and return
  weightedEdges <- data.frame(caller_loc = caller_location, callee_loc = callee_location,
                              freq = freq)
  return(weightedEdges)
}
