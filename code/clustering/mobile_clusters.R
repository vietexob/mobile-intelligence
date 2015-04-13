rm(list = ls())

library(TraMineR)
library(cluster)
library(ggplot2)
library(ggmap)
library(plyr)

source("./code/spatial/getSpatialRowIndices.R")
source("./code/clustering/getActivityMatrix.R")
source("./code/clustering/getDistanceMatrix.R")
source("./code/util/fivethirtyeight_theme.R")

call.data <- read.csv(file="./data/mobile/my_call_data_90_100_500.csv", header=TRUE)

## Read the cell towers spatial coordinates
cell.towers <- read.csv(file="./data/mobile/cell_coord.csv", stringsAsFactors=FALSE)
## Convert from string to numeric format
cell.towers$Longitude <- as.numeric(cell.towers$Longitude)
cell.towers$Latitude <- as.numeric(cell.towers$Latitude)

## Load the cell_id rowIndex lookup table (from the previous
## tutorial: http://vietletruc.com/wp-content/uploads/2015/03/cell_locations.html)
load("./data/mobile/cell_id_rowIndex_mapping.RData")
rowIndices <- getSpatialRowIndices(call.data, cell_id.coord.rowIndex)

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
act.matrix <- getActivityMatrix(my.call.data, delta=30)

## Compute the distance matrix between all pairs of cell_ids in act.matrix
unknown_distance <- 30
dist_matrix <- getDistanceMatrix(act.matrix, cell_id.coord.rowIndex,
                                 cell.towers, unknown_distance)

## Define the sequences
n.states <- length(table(act.matrix))
## Select colors for the activities
cl <- colors()[seq(from = 1, by = 2, length.out = n.states)]
## xtstep = 2: tick-mark displayed every two positions
trajec.seq <- seqdef(act.matrix, xtstep = 2, cpal = cl)

# Compute the pairwise optimal matching (OM) distances between sequences
# with insertion/deletion cost of 1 and substitution cost matrix based on dist.matrix
trajec.om <- seqdist(trajec.seq, method = "OM", indel = unknown_distance, sm = dist_matrix)

# Do hierarchical cluster analysis
mobile.cluster <- agnes(trajec.om, diss = TRUE, method = "ward")
# Plot the hierarchy (tree)
pdf(file = "./figures/clustering/mobile_cl_tree.pdf")
mainStr <- "Sequence Hierarchical Clustering"
plot(mobile.cluster, which.plots = 2, labels = FALSE, main = mainStr)
dev.off()

K <- 2 # number of clusters
trajec.cl <- cutree(mobile.cluster, K)

pdf(file = "./figures/clustering/mobile_trajec_cl.pdf", width=20, height=10)
seqdplot(trajec.seq, group = trajec.cl, border = NA, title="Cluster", withlegend="auto")
dev.off()

## Map each caller_id to their cluster label
caller_ids <- rownames(act.matrix)
caller_id.cluster <- new.env()
for(i in 1:length(caller_ids)) {
  a_caller_id <- caller_ids[i]
  caller_id.cluster[[a_caller_id]] <- trajec.cl[i]
}
save(caller_id.cluster, file="./data/mobile/caller_id_cluster_mapping.RData")

## Plot the spatial locations of each cluster for one-day duration
## Assign cluster label and add longitude and latitude coordinates to my call data frame
cluster <- vector()
longitude <- vector()
latitude <- vector()
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(my.call.data))
for(i in 1:nrow(my.call.data)) {
  caller_id <- toString(my.call.data$caller_id[i])
  cluster[i] <- caller_id.cluster[[caller_id]]
  
  cell_id <- toString(my.call.data$cell_id[i])
  rowIndex <- cell_id.coord.rowIndex[[cell_id]]
  longitude[i] <- cell.towers$Longitude[rowIndex]
  latitude[i] <- cell.towers$Latitude[rowIndex]
  progress.bar$step()
}
my.cluster.data <- data.frame(longitude=longitude, latitude=latitude, cluster=cluster)
my.cluster.data <- data.frame(table(my.cluster.data))
my.cluster.data <- subset(my.cluster.data, Freq > 0)

## Convert longitude and latitude to numeric format
num_longitude <- vector()
num_latitude <- vector()
for(i in 1:nrow(my.cluster.data)) {
  lonStr <- toString(my.cluster.data$longitude[i])
  num_longitude[i] <- as.numeric(lonStr)
  latStr <- toString(my.cluster.data$latitude[i])
  num_latitude[i] <- as.numeric(latStr)
}
my.cluster.data$longitude <- num_longitude
my.cluster.data$latitude <- num_latitude

## Calculate the mean longitude
mean.longitude <- mean(my.cluster.data$longitude)
## Calculate the mean latitude
mean.latitude <- mean(my.cluster.data$latitude)

## Retrieved a map centered at the given location
location <- c(mean.longitude, mean.latitude)
cluster.map <- get_map(location, maptype = "terrain", zoom = 10, scale=2)
cluster.map <- ggmap(cluster.map, extent = 'device', legend = 'none')

## Add the "bubbles" to the map, and size them by frequency.
cluster.map <- cluster.map + geom_point(data = my.cluster.data,
                                        aes(x = longitude, y = latitude, size = Freq,
                                            fill=cluster), alpha=0.80, shape=21)
## Remove any legends from the plot
cluster.map <- cluster.map + guides(fill=FALSE, alpha=FALSE, size=FALSE)
## Add a title to the plot
cluster.map <- cluster.map + ggtitle("Clusters of Call Events over the Week")
## Add a fancy theme to it (this step is optional)
cluster.map <- cluster.map + fivethirtyeight_theme()
## Display the plot (or else it won't show)
print(cluster.map)

## Save the plot on disk
ggsave(filename="./figures/clustering/my_clustered_bubbles.png", width=10, height=10)
