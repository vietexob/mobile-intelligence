rm(list = ls())

## This tutorial explores basic spatial visualization techniques for the mobile phone
## dataset. Specifically, we will explore two visualization techniques: spatial bubble plot
## and population graph. Spatial bubble plot is a simple techinque to visualize the
## distribution of "events" over space, where each "event" is any incident of interest that
## can be described by its spatial coordinates and that happens during the specfied duration.
## Each bubble is visually scaled by the number of events that happen nearby and are mapped
## to its coordinates. Population graph (http://dyerlab.github.io/popgraph/) is a concept
## introduced by Dyer and Nason (http://www.ncbi.nlm.nih.gov/pubmed/15189198) to visualize
## both the distribution of events over space and the relationships among those events.
## In other words, it is a graph laid over a spatial map, where each node of the graph is
## an event and each edge represents the relationship between a pair of events. In this
## tutorial, we will use the popgraph package to plot population graphs.

## Load the required packages, assuming they have been installed
library(rmongodb)
library(ggplot2)
library(ggmap) ## for plotting maps
library(igraph) ## for plotting graphs
library(popgraph) ## for plotting population graphs
library(scales) ## for plot formatting 
library(plyr) ## for data manipulation

## Source the useful functions
source("./code/util/fivethirtyeight_theme.R") ## fancy-looking theme for the plot
source("./code/spatial/getWeightedEdges.R") ## create weighted call graph

## Read the cell towers spatial coordinates
cell.towers <- read.csv(file="./data/mobile/cell_coord.csv", stringsAsFactors=FALSE)
cell.towers$Longitude <- as.numeric(cell.towers$Longitude)
cell.towers$Latitude <- as.numeric(cell.towers$Latitude)

## Load the cell_id rowIndex lookup table (from the previous
## tutorial: http://vietletruc.com/wp-content/uploads/2015/03/cell_locations.html)
load("./data/mobile/cell_id_rowIndex_mapping.RData")

## Load the previously retrieved call data
call.data <- read.csv(file="./data/mobile/my_call_data.csv")

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

## Use the aggregation framework to retrieve the distribution of all IMEI's
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
## Combine the pipeline and execute the aggregation
pipeline <- list(pipe_1, pipe_2, pipe_3)
## NOTE: This execution will take some time.
imei.distr <- mongo.aggregation(mongo, namespace, pipeline)

## Reshape the data to fit into an R data frame
limei.distr <- mongo.bson.value(imei.distr, "result")
mimei.distr <- sapply(limei.distr, function(x) return(c(toString(x[["_id"]]),
                                                      as.numeric(x[["count"]]))))
mimei.distr <- t(mimei.distr) # transpose the matrix
call.freq <- as.numeric(mimei.distr[, 2]) # convert frequencies into numeric
dimei.distr <- as.data.frame(mimei.distr) # convert matrix into data frame
colnames(dimei.distr) <- c("imei", "freq") # name the columns
dimei.distr$freq <- call.freq

## Plot the histogram of the IMEI frequencies
(ggplot(dimei.distr, aes(freq)) + geom_histogram(binwidth=2, fill="#c0392b", alpha=0.75) +
   fivethirtyeight_theme() + 
   labs(title="Distribution of Call Frequencies in [100, 300]",
        x="Call Frequency", y="Frequency") + scale_x_continuous(labels=comma) +
   scale_y_continuous(labels=comma) + geom_hline(yintercept=0, size=0.4, color="black"))
## Save the plot to disk
ggsave(file="./figures/mobile/call_freq.png", width=4, height=3)

## Sort the data frame by frequency in decreasing order
dimei.distr <- dimei.distr[order(-dimei.distr$freq), ]
## Select the top IMEI's by frequency
## NOTE: The retrieval of records through IMEI (depending on many) may take a **very long**
## time. Consider changing the 'top' variable to a smaller number if it takes too much time.
## With this current setting, I left the laptop run overnight to retrieve all the records.
top <- 1500
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
## NOTE: It is a good practice to save the retrieved data to offline storage in order
## to avoid retrieving them again the next time, and save substantial time and effort
write.csv(call.data, file="./data/mobile/my_call_data.csv", row.names=FALSE)

## Release the resources attached to cursor on both client and server
mongo.cursor.destroy(cursor)
## Close the connection
mongo.disconnect(mongo)
mongo.destroy(mongo)

## Match each cell_id with its corresponding rowIndex of the coordinate table
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(call.data))
rowIndices <- vector() ## a vector of row indices
nMatches <- 0 ## count the number of matches
for(i in 1:nrow(call.data)) {
  ## Have to convert numeric to string in order to do lookup
  cell_id <- toString(call.data$cell_id[i])
  if(nchar(cell_id) > 0) {
    ## Look up the (loaded) table
    rowIndex <- cell_id.coord.rowIndex[[cell_id]]
    if(is.null(rowIndex)) { ## if matched
      rowIndices <- c(rowIndices, NA)
    } else { ## if unmatched
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

aggregateLocationByDate <- function(call.data, cell_id.coord.rowIndex, cell.towers) {
  ## This function aggregates the retrieved call data by each date.
  ## Specifically, it 
  dates <- names(table(call.data$date))
  aggregate.data <- data.frame() # master data frame to store the records
  
  for(i in 1:length(dates)) {
    subset.call.data <- subset(call.data, date == dates[i])
    cellId.table <- table(subset.call.data$cell_id)
    cellId.table <- subset(cellId.table, cellId.table > 0)
    cellIds <- names(cellId.table)
    cellId.freq <- as.numeric(cellId.table)
    
    ## Retrieve the corresponding cell tower coordinates
    longitude <- vector()
    latitude <- vector()
    counter <- 0
    for(j in 1:length(cellId.table)) {
      if(nchar(cellIds[j]) > 0) {
        rowIndex <- cell_id.coord.rowIndex[[cellIds[j]]]
        if(!is.null(rowIndex)) {
          aLongitude <- cell.towers$Longitude[rowIndex]
          if(!is.numeric(aLongitude)) {
            aLongitude <- as.numeric(aLongitude)
          }
          aLatitude <- cell.towers$Latitude[rowIndex]
          if(!is.numeric(aLatitude)) {
            aLatitude <- as.numeric(aLatitude)
          }
          
          longitude[j] <- as.numeric(aLongitude)
          latitude[j] <- as.numeric(aLatitude)
          counter <- counter + 1
        } else {
          stop(paste("Unmatched cell_id:", cellIds[j]))
        }
      } else {
        stop(paste("Zero-length cell_id:", j))
      }
    }
    
    dateStr <- toString(as.Date(dates[i], "%Y%m%d"))
    date.aggregate.data <- data.frame(cell_id = cellIds, freq = cellId.freq,
                                      longitude = longitude, latitude = latitude,
                                      date = rep(dateStr, counter))
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
location.map <- get_map(location, maptype = "terrain", zoom = 10, scale=2)
location.map <- ggmap(location.map, extent = 'device', legend = 'none')
location.map <- location.map + geom_point(data = aggregate.location,
                                          aes(x = longitude, y = latitude, size = freq),
                                          fill="red", alpha=0.80, shape=21)
location.map <- location.map + guides(fill=FALSE, alpha=FALSE, size=FALSE)
location.map <- location.map + ggtitle("Distribution of Call Events over the Week")
location.map <- location.map + fivethirtyeight_theme()
location.map <- location.map + facet_wrap(~date)
print(location.map)

## Save the plot on disk
ggsave(filename="./figures/mobile/my_call_bubbles.png", width=10, height=10)

##### POP GRAPH #####
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
if(length(badIndices) > 0) {
  weightedEdges <- weightedEdges[-badIndices, ]
}

## Convert into cell location compatible with the spreadsheet
caller_cell_loc <- vector()
callee_cell_loc <- vector()
for(i in 1:nrow(weightedEdges)) {
  caller_loc_str <- toString(weightedEdges$caller_location[i])
  rowIndex <- cell_id.coord.rowIndex[[caller_loc_str]]
  caller_cell_loc[i] <- cell.towers$Cell[rowIndex]
  
  callee_loc_str <- toString(weightedEdges$callee_location[i])
  rowIndex <- cell_id.coord.rowIndex[[callee_loc_str]]
  callee_cell_loc[i] <- cell.towers$Cell[rowIndex]
}
## Merge with the weighted edges
weightedEdges$caller_cell_loc <- caller_cell_loc
weightedEdges$callee_cell_loc <- callee_cell_loc

## Create an igraph object from the weighted edges
weightedEdges.matrix <- as.matrix(weightedEdges[, 6:7])
g <- graph.edgelist(weightedEdges.matrix)
E(g)$weight <- weightedEdges$Freq # add weights to the edges

## Configure the graph for visualization
V(g)$size <- degree(g, mode = "all")
g <- decorate_graph(g, cell.towers, stratum = "Cell")

g.map <- get_map(location, maptype = "terrain", zoom = 10, scale=2)
g.map <- ggmap(g.map, extent = 'device', legend = 'none')
g.map <- g.map + geom_nodeset(aes(x = Longitude, y = Latitude, size=size),
                              graph=g, color="red")
g.map <- g.map + geom_edgeset(aes(x = Longitude, y = Latitude, size=weight),
                              graph=g, color="blue")
g.map <- g.map + ggtitle("Spatial Call Graph for One-week Duration")
g.map <- g.map + fivethirtyeight_theme()
g.map <- g.map + guides(fill=FALSE, alpha=FALSE, size=FALSE)

print(g.map)

## Save the plot on disk
ggsave(filename="./figures/mobile/my_call_graph.png", width=8, height=8)

