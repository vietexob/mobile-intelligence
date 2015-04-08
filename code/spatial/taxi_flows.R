rm(list = ls())

library(plyr)
library(ggplot2)
library(maptools)
library(rmongodb)

## Login credentials
host <- "heinz-tjle.heinz.cmu.edu"
username <- "student"
password <- "helloWorld"
db <- "admin"

## Connect to MongoDB remote server
mongo <- mongo.create(host = host, db = db, username = username, password = password)
## Check if we are successfully connected
mongo.is.connected(mongo)

## The database we're working with is 'admin' and the collection is 'taxi'
collection <- "taxi"
namespace <- paste(db, collection, sep=".")

## Retrieve all ride records on Sept. 11, 2009
query <- mongo.bson.from.list(list('date'='2009-09-11', 'occupy'=1))
## Define the fields to be returned
fields <- mongo.bson.buffer.create()
## '1L' means we want to turn this field on, '0L' to turn it off
mongo.bson.buffer.append(fields, "_id", 0L)
mongo.bson.buffer.append(fields, "taxi_no", 1L)
mongo.bson.buffer.append(fields, "date", 1L)
mongo.bson.buffer.append(fields, "time", 1L)
mongo.bson.buffer.append(fields, "lon", 1L)
mongo.bson.buffer.append(fields, "lat", 1L)
## Make an object from the buffer
fields <- mongo.bson.from.buffer(fields)

## Create the query cursor
cursor <- mongo.find(mongo, namespace, query=query, fields=fields)
## Define a master data frame to store results
taxi.data <- data.frame(stringsAsFactors=FALSE)
## Iterate over the cursor
while(mongo.cursor.next(cursor)) {
  ## Iterate and grab the next record
  value <- mongo.cursor.value(cursor)
  taxi.record <- mongo.bson.to.list(value)
  ## Make it a data frame
  taxi.df <- as.data.frame(t(unlist(taxi.record)), stringsAsFactors=FALSE)
  ## Bind to the master data frame
  taxi.data <- rbind.fill(taxi.data, taxi.df)
}
## Release the resources attached to cursor on both client and server
done <- mongo.cursor.destroy(cursor)







