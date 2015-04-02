rm(list = ls())

library(rmongodb)
library(ggplot2)
library(ggmap)
library(igraph)
library(popgraph)
library(scales)
library(plyr)

source("./code/util/fivethirtyeight_theme.R")
source("./code/spatial/getWeightedEdges.R")

## Read the cell towers spatial coordinates
cell.towers <- read.csv(file="./data/mobile/cell_coord.csv", stringsAsFactors=FALSE)
cell.names <- cell.towers$Cell
## Load the cell_id rowIndex mapping
load("./data/mobile/cell_id_rowIndex_mapping.RData")

## Read my call data
# call.data <- read.csv(file="./data/mobile/my_call_data.csv")

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

## Use the aggregation framework to find the distribution of all IMEI's
## First group the IMEI's by frequency
pipe_1 <- mongo.bson.from.JSON(
  '{"$group":
    {"_id": "$imei", "count": {"$sum": 1}}
  }'
)
## Get those whose call frequencies are at least 100
pipe_2 <- mongo.bson.from.JSON(
  '{"$match": {"count": {"$gte": 100}}}'
)
## And those whose call frequencies are no more than 300
pipe_3 <- mongo.bson.from.JSON(
  '{"$match": {"count": {"$lte": 300}}}'
)
pipeline <- list(pipe_1, pipe_2, pipe_3)
imei.distr <- mongo.aggregation(mongo, namespace, pipeline)

## Reshape the data to fit into an R data frame
limei.distr <- mongo.bson.value(imei.distr, "result")
mimei.distr <- sapply(limei.distr, function(x) return(c(toString(x[["_id"]]),
                                                      as.numeric(x[["count"]]))))
mimei.distr <- t(mimei.distr) # transpose the matrix
call.freq <- as.numeric(mimei.distr[, 2]) # convert frequencies into numeric
dimei.distr <- as.data.frame(mimei.distr) # convert matrix into data frame
colnames(dimei.distr) <- c("imei", "freq")
dimei.distr$freq <- call.freq

## Plot the histogram of the IMEI frequencies
(ggplot(dimei.distr, aes(freq)) + geom_histogram(binwidth=2, fill="#c0392b", alpha=0.75) +
   fivethirtyeight_theme() + 
   labs(title="Distribution of Call Frequencies in [100, 300]",
        x="Call Frequency", y="Frequency") + scale_x_continuous(labels=comma) +
   scale_y_continuous(labels=comma) + geom_hline(yintercept=0, size=0.4, color="black"))
ggsave(file="./figures/mobile/call_freq.png", width=4, height=3)

## Sort the data frame by frequency in decreasing order
dimei.distr <- dimei.distr[order(-dimei.distr$freq), ]
## Select the top IMEI's by frequency
top <- 300
top.imei <- as.character(dimei.distr$imei[1:top])

## Retrieve all call records from the top IMEI's
## Define a master data frame to store all the results
call.data <- data.frame(stringsAsFactors=FALSE)

## Define the fields to be returned
fields <- mongo.bson.buffer.create()
## '1L' means we want to turn this field on, '0L' to turn it off
mongo.bson.buffer.append(fields, "_id", 0L)
mongo.bson.buffer.append(fields, "imei", 1L)
mongo.bson.buffer.append(fields, "caller_id", 1L)
mongo.bson.buffer.append(fields, "callee_id", 1L)
mongo.bson.buffer.append(fields, "date", 1L)
mongo.bson.buffer.append(fields, "time", 1L)
mongo.bson.buffer.append(fields, "call_duration", 1L)
mongo.bson.buffer.append(fields, "cell_id", 1L)
## Make an object from the buffer
fields <- mongo.bson.from.buffer(fields)

## Create a progress bar
progress.bar <- create_progress_bar("text")
progress.bar$init(length(top.imei))
for(i in 1:length(top.imei)) {  
  ## Define the query
  query <- mongo.bson.from.list(list('imei'=as.numeric(top.imei[i])))
  ## Create the query cursor
  cursor <- mongo.find(mongo, namespace, query=query, fields=fields)  
  ## Iterate over the cursor
  while(mongo.cursor.next(cursor)) {
    ## Iterate and grab the next record
    value <- mongo.cursor.value(cursor)
    call <- mongo.bson.to.list(value)
    ## Make it a data frame
    call.df <- as.data.frame(t(unlist(call)), stringsAsFactors=FALSE)
    ## Bind to the master data frame
    call.data <- rbind.fill(call.data, call.df)
  }
  progress.bar$step()
}

## Save the call data to disk for further use
write.csv(call.data, file="./data/mobile/my_call_data.csv", row.names=FALSE)

## Release the resources attached to cursor on both client and server
mongo.cursor.destroy(cursor)
## Close the connection
mongo.disconnect(mongo)
mongo.destroy(mongo)

## Match each cell_id with its corresponding rowIndex
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(call.data))
rowIndices <- vector()
nMatches <- 0
for(i in 1:nrow(call.data)) {
  cell_id <- toString(call.data$cell_id[i])
  rowIndex <- cell_id.coord.rowIndex[[cell_id]]
  if(is.null(rowIndex)) {
    rowIndices <- c(rowIndices, NA)
  } else {
    rowIndices <- c(rowIndices, rowIndex)
    nMatches <- nMatches + 1
  }
  progress.bar$step()
}
match.pct <- round(nMatches / nrow(call.data) * 100, 2)
print(paste("Percent matched locations =", match.pct))

## Reduce call data to those that can be plotted on a map
my.call.data <- call.data[!is.na(rowIndices), ]
## Further reduce to include certain date range only
my.call.data <- subset(my.call.data, date > 20080229)
my.call.data <- subset(my.call.data, date < 20080308)

aggregateLocationByDate <- function(call.data, cell_id.coord.rowIndex, cell.towers) {
  dates <- names(table(call.data$date))
  aggregate.data <- data.frame()
  
  for(i in 1:length(dates)) {
    subset.call.data <- subset(call.data, date == dates[i])
    cellId.table <- table(subset.call.data$cell_id)
    cellId.table <- subset(cellId.table, cellId.table > 0)
    cellIds <- names(cellId.table)
    cellId.freq <- as.numeric(cellId.table)
    
    ## Retrieve the corresponding cell tower coordinates
    longitude <- vector()
    latitude <- vector()
    for(j in 1:length(cellId.table)) {
      rowIndex <- cell_id.coord.rowIndex[[cellIds[j]]]
      longitude[j] <- as.numeric(cell.towers$Longitude[rowIndex])
      latitude[j] <- as.numeric(cell.towers$Latitude[rowIndex])
    }
    
    dateStr <- toString(as.Date(dates[i], "%Y%m%d"))
    date.aggregate.data <- data.frame(cell_id = cellIds, freq = cellId.freq,
                                      longitude = longitude, latitude = latitude,
                                      date = rep(dateStr, length(cellId.table)))
    aggregate.data <- rbind(aggregate.data, date.aggregate.data)
  }
  
  return(aggregate.data)
}

aggregate.location <- aggregateLocationByDate(my.call.data, cell_id.coord.rowIndex,
                                              cell.towers)

## Create a spatial bubble chart
## First, need a location at the center of all cell towers in my call data
all.longitude <- names(table(aggregate.location$longitude))
all.longitude <- as.numeric(all.longitude)
mean.longitude <- mean(all.longitude)

all.latitude <- names(table(aggregate.location$latitude))
all.latitude <- as.numeric(all.latitude)
mean.latitude <- mean(all.latitude)

location <- c(mean.longitude, mean.latitude)
theme_set(theme_bw(16))
location.map <- get_map(location, maptype = "terrain", zoom = 11, scale=2)
location.map <- ggmap(location.map, extent = 'device', legend = 'none')
location.map <- location.map + geom_point(data = aggregate.location,
                                          aes(x = longitude, y = latitude, size = freq),
                                          fill="red", alpha=0.80, shape=21)
location.map <- location.map + guides(fill=FALSE, alpha=FALSE, size=FALSE)
location.map <- location.map + facet_wrap(~date)
print(location.map)

## Save the plot on disk
ggsave(filename="./figures/mobile/my_call_data_bubbles.png", width=11, height=11)

#################################################################################
weightedEdges <- getWeightedEdges(my.call.data)
## Reduce to only the locations that can be plotted
badIndices <- vector()
for(i in 1:nrow(weightedEdges)) {
  caller_loc <- toString(weightedEdges$caller_location[i])
  caller_rowIndex <- cell_id.coord.rowIndex[[caller_loc]]
  callee_loc <- toString(weightedEdges$callee_location[i])
  callee_rowIndex <- cell_id.coord.rowIndex[[callee_loc]]
  if(is.null(caller_rowIndex) || is.null(callee_rowIndex)) {
    badIndices <- c(badIndices, i)
  }
}
weightedEdges <- weightedEdges[-badIndices, ]
