rm(list = ls())

## This script demonstrates how to match cell_id's in the mobile phone dataset with
## their real-world spatial locations. Note that not all cell_id's can be matched and
## located. Ignore those that are unmatched.

library(rmongodb)
library(ggplot2)
library(ggmap)

## Read the cell towers spatial coordinates
cell.towers <- read.csv(file="./data/mobile/cell_coord.csv", stringsAsFactors=FALSE)
cell.names <- cell.towers$Cell

## Take only the last 4 digits
cell.names.short <- vector()
for(i in 1:length(cell.names)) {
  cell.names.short[i] <- substr(cell.names[i], start=4, stop=nchar(cell.names[i]))
}
## Convert from hex into decimal
cell.names.dec <- strtoi(cell.names.short, 16L)

## Login credentials
host <- "heinz-tjle.heinz.cmu.edu"
username <- "student"
password <- "helloWorld"
db <- "admin"

## Connect to MongoDB remote server
mongo <- mongo.create(host = host, db = db, username = username, password = password)
## Check if we are successfully connected
mongo.is.connected(mongo)

## The database we're working with is 'admin' and the collection is 'cellular'
collection <- "cellular"
namespace <- paste(db, collection, sep=".")

## Get the cell tower locations
cell.loc <- mongo.distinct(mongo, namespace, "cell_id")
## Convert into vector
cell.loc <- unlist(cell.loc)
## Convert from hex into decimal
cell.loc.dec <- strtoi(cell.loc, 16L)

## Create a mapping from cell_id to the coordinate table (row index)
cell_id.coord.rowIndex <- new.env()
## Create a data frame to store and visualize the locations
cell.loc.data <- data.frame(stringsAsFactors=FALSE)
counter <- 0
for(i in 1:length(cell.loc)) {
  if(cell.loc.dec[i] %in% cell.names.dec) {
    rowIndices <- which(cell.names.dec == cell.loc.dec[i])
    if(length(rowIndices) == 1) {
      rowIndex <- rowIndices
    }
    else { # matched with more than one row
      ## Take the first one that's matched
      rowIndex <- rowIndices[1]
    }
    
    longitude <- as.numeric(cell.towers$Longitude[rowIndex])
    latitude <- as.numeric(cell.towers$Latitude[rowIndex])
    if(!is.na(longitude) && !is.na(latitude)) { # some of the coords are corrupted
      cell_id.coord.rowIndex[[toString(cell.loc[i])]] <- rowIndex
      loc.row.data <- data.frame(cell=cell.loc[i], longitude=longitude, latitude=latitude)
      cell.loc.data <- rbind(cell.loc.data, loc.row.data)
      counter <- counter + 1
    }
  }
}
## How much percent is matched?
(round(counter / length(cell.loc) * 100, 2))
## Save the mapping to disk for future use
save(cell_id.coord.rowIndex, file="./data/mobile/cell_id_rowIndex_mapping.RData")

## Specify a map with center at the center of all the coordinates
cell.loc.map <- get_map(location = c(lon = mean(cell.loc.data$longitude),
                                     lat = mean(cell.loc.data$latitude)),
                        zoom = 11, scale = 2)
## Make a map that plots each cell tower
cell.tower.pts <- geom_point(data = cell.loc.data, aes(x = longitude, y = latitude,
                                                       fill = "red", alpha = 0.80),
                             size = 1, shape = 21)
cell.loc.map <- ggmap(cell.loc.map) + cell.tower.pts + guides(fill = FALSE, alpha = FALSE,
                                                              size = FALSE)
## Give the map a title
cell.loc.map <- cell.loc.map + ggtitle("Locations of Cell Towers")
## Add the density contours
cell.loc.map <- cell.loc.map + geom_density2d(data = cell.loc.data,
                                              aes(x = longitude, y = latitude))
## Save the plot on disk
ggsave(filename="./figures/mobile/cell_towers.pdf", width=10, height=10)
