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

## TODO: Compute the distance matrix between all pairs of cell_ids in my.call.data


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
caller_ids <- names(table(my.call.data$caller_id))
delta <- 30 # minutes
periods <- seq(from=0, to=24*60, by=15)
act.matrix <- matrix(0, nrow=length(caller_ids), ncol=(length(periods)-1))
rownames(act.matrix) <- caller_ids
colnames(act.matrix) <- periods[1:(length(periods)-1)]

## Populate the activity matrix
progress.bar <- create_progress_bar("text")
progress.bar$init(length(caller_ids))
for(i in 1:length(caller_ids)) {
  a_caller_id <- caller_ids[i]
  subset.caller_id <- subset(my.call.data, caller_id == a_caller_id)
  for(j in 1:nrow(subset.caller_id)) {
    minute <- subset.caller_id$minute[j]
    upper_bound_index <- min(which(periods > minute))
    lower_bound_index <- max(which(periods <= minute))
    print(paste(lower_bound_index, upper_bound_index))
  }
  progress.bar$step()
}

