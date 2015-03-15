rm(list = ls())

library(rmongodb)

## Login credentials
host <- "heinz-tjle.heinz.cmu.edu"
username <- "tjle"
password <- "x3V3WYg4"
db <- "admin"
## Connect to MongoDB remote server
mongo <- mongo.create(host = host, db = db, username = username, password = password)

## The database we're working with is 'admin' and the collection is 'example'
collection <- "example"
namespace <- paste(db, collection, sep=".")

## Get a list of collections within our namespace
mongo.get.database.collections(mongo, db)
## Find the number of documents in collection in the database
mongo.count(mongo, namespace, mongo.bson.empty())

## Create a document to insert
b <- mongo.bson.from.list(list(platform="Compose", language="R", number=1))
## Insert the document into the spacename
ok <- mongo.insert(mongo, namespace, b)

## Insert a few more documents
for(i in 2:50) {
  b <- mongo.bson.from.list(list(platform="Compose", language="R", number=i))
  mongo.insert(mongo, namespace, b)
}

## Build a query to find all language R
buf <- mongo.bson.buffer.create()
mongo.bson.buffer.append(buf, "language", "R")
query <- mongo.bson.from.buffer(buf)

## Get the count
count <- mongo.count(mongo, namespace, query)
## Bring them back into a list
numbers <- list()
cursor <- mongo.find(mongo, namespace, query)
while(mongo.cursor.next(cursor)) {
  val <- mongo.cursor.value(cursor)
  numbers[[length(numbers)+1]] <- mongo.bson.value(val, "number")
}
