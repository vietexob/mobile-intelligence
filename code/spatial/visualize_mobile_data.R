rm(list = ls())

library(rmongodb)
library(ggplot2)
library(ggmap)
library(scales)
library(plyr)

source("./code/util/fivethirtyeight_theme.R")

## Read the cell towers spatial coordinates
cell.towers <- read.csv(file="./data/mobile/cell_coord.csv", stringsAsFactors=FALSE)
cell.names <- cell.towers$Cell

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
top <- 100
top.imei <- as.character(dimei.distr$imei[1:100])

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

## Plot the bubble charts

