rm(list = ls())

library(TraMineR)
library(cluster)
library(plyr)

source("./code/util/earthDist.R")

call.data <- read.csv(file="./data/mobile/my_call_data_90_100_500.csv", header=TRUE)

## Read the cell towers spatial coordinates
cell.towers <- read.csv(file="./data/mobile/cell_coord.csv", stringsAsFactors=FALSE)
## Convert from string to numeric format
cell.towers$Longitude <- as.numeric(cell.towers$Longitude)
cell.towers$Latitude <- as.numeric(cell.towers$Latitude)

## Load the cell_id rowIndex lookup table (from the previous
## tutorial: http://vietletruc.com/wp-content/uploads/2015/03/cell_locations.html)
load("./data/mobile/cell_id_rowIndex_mapping.RData")

## Match each cell_id with its corresponding rowIndex of the coordinate table
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

## Reduce call data to those that can be plotted on a map
my.call.data <- call.data[!is.na(rowIndices), ]
## Further reduce to include a certain date range only. Why?
(table(my.call.data$date))
my.call.data <- subset(my.call.data, date > 20080229)
my.call.data <- subset(my.call.data, date < 20080308)

## Calculate the number of minutes until event
minute <- vector()
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(my.call.data))
for(i in 1:nrow(my.call.data)) {
  timestamp <- toString(call.data$time[i])
  timestamp <- strsplit(timestamp, ":")[[1]]
  minutes <- round(as.numeric(timestamp[1])*60 + as.numeric(timestamp[2]) +
                     as.numeric(timestamp[3])/60, 2)
  minute[i] <- minutes
  progress.bar$step()
}
my.call.data$minute <- minute

## Create an activity matrix
## TODO: Extract function getActMatrix
caller_id.table <- table(my.call.data$caller_id)
caller_id.table <- subset(caller_id.table, caller_id.table > 0)
caller_ids <- names(caller_id.table)
delta <- 30 # minutes
periods <- seq(from=0, to=24*60, by=15)
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
  subset.caller_id <- subset(my.call.data, caller_id == a_caller_id)
  
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

## Compute the distance matrix between all pairs of cell_ids in act.matrix
## TODO: Extract function getDistMatrix
act.cell_ids <- names(table(act.matrix))
dist_matrix <- matrix(0, nrow=length(act.cell_ids), ncol=length(act.cell_ids))
rownames(dist_matrix) <- colnames(dist_matrix) <- act.cell_ids
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(dist_matrix)*ncol(dist_matrix))
for(i in 1:nrow(dist_matrix)) {
  from_cell_id <- act.cell_ids[i]
  from_row_index <- -1
  from_lon <- -1
  from_lat <- -1
  if(from_cell_id != "0") {
    from_row_index <- cell_id.coord.rowIndex[[from_cell_id]]
    if(is.null(from_row_index)) {
      stop(from_cell_id)
    }
    from_lon <- cell.towers$Longitude[from_row_index]
    from_lat <- cell.towers$Latitude[from_row_index]
  }
  
  for(j in 1:ncol(dist_matrix)) {
    if(dist_matrix[i, j] == 0) { # has not been populated
      to_cell_id <- act.cell_ids[j]
      if(from_cell_id != to_cell_id) {
        if(from_cell_id == "0") {
          dist_matrix[i, j] <- 10
          dist_matrix[j, i] <- 10
        } else {
          to_row_index <- cell_id.coord.rowIndex[[to_cell_id]]
          if(is.null(to_row_index)) {
            stop(to_cell_id)
          }
          to_lon <- cell.towers$Longitude[to_row_index]
          to_lat <- cell.towers$Latitude[to_row_index]
          
          distance <- earthDist(from_lon, from_lat, to_lon, to_lat)
          dist_matrix[i, j] <- distance
          dist_matrix[j, i] <- distance
        }
      }
    }
    progress.bar$step()
  }
}

## Define the sequences
n.states <- length(act.cell_ids)
## Select colors for the activities
cl <- colors()[seq(from = 1, by = 5, length.out = n.states)]
## xtstep = 2: tick-mark displayed every two positions
trajec.seq <- seqdef(act.matrix, xtstep = 2, cpal = cl)

# Compute the pairwise optimal matching (OM) distances between sequences
# with insertion/deletion cost of 1 and substitution cost matrix based on dist.matrix
trajec.om <- seqdist(trajec.seq, method = "OM", indel = 10, sm = dist_matrix)
trajec.om <- seqdist(trajec.seq, method = "OM", indel = 10, sm = "TRATE")

# Do hierarchical cluster analysis
mobile.cluster <- agnes(trajec.om, diss = TRUE, method = "ward")

# Plot the hierarchy (tree)
pdf(file = "./figures/mobile/mobile_clusters.pdf")
mainStr <- "Sequence Hierarchical Clustering"
plot(mobile.cluster, which.plots = 2, labels = FALSE, main = mainStr)
dev.off()
