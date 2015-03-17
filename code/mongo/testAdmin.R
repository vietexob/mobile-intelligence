rm(list = ls())

library(rmongodb)

## Login credentials
host <- "heinz-tjle.heinz.cmu.edu"
username <- "student"
password <- "helloWorld"
db <- "admin"
## Connect to MongoDB remote server
mongo <- mongo.create(host = host, db = db, username = username, password = password)

## The database we're working with is 'admin' and the collection is 'example'
collection <- "cellular"
namespace <- paste(db, collection, sep=".")

## Get a list of collections within our namespace
mongo.get.database.collections(mongo, db)

## Find the number of documents in collection in the database
mongo.count(mongo, namespace, mongo.bson.empty())

## Declare the attribute names
attr.names <- c("caller_id", "county_id", "calltype_id", "callee_id", "date", "time",
                "call_duration", "roamcity_id", "roamtype_id", "tolltype_id",
                "adversary_id", "opp_city_id", "opposite_roamcity_id", "callmoment",
                "lac_id", "cell_id", "imei")

## Get a list of calls for phone's imei 355001000488650
cursor <- mongo.find(mongo, namespace, query=list(imei=355001000488650))
calls <- NULL
call.start_datetime <- NULL
while(mongo.cursor.next(cursor)) {
  value <- mongo.cursor.value(cursor)
  calls <- c(calls, list(mongo.bson.to.list(value)))
}

## Convert to dataframe
call.data <- data.frame(matrix(unlist(calls), nrow=length(calls), byrow=TRUE))
## Remove the first column '_id'
call.data <- call.data[, -1]
## Rename the columns
colnames(call.data) <- attr.names

## Build a query to find all calls on 03/01/2008
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "start_datetime")
mongo.bson.buffer.append(buf, "$in", "20080301")
mongo.bson.buffer.finish.object(buf)
criteria <- mongo.bson.from.buffer(buf)

## Retrieve the first 50
calls <- list()
max.count <- 50
counter <- 0
cursor <- mongo.find(mongo, namespace, bson)
while(mongo.cursor.next(cursor)) {
  val <- mongo.cursor.value(cursor)
  numbers[[length(numbers)+1]] <- mongo.bson.value(val, "imei")
  counter <- counter + 1
  if(counter == max.count) {
    break
  }
}










