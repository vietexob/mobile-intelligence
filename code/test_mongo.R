rm(list = ls())

library(rmongodb)

locations <- read.csv(file="./data/cell_coord.csv", header=TRUE)

## Login credentials
host <- "heinz-tjle.heinz.cmu.edu"
username <- "tjle"
password <- "x3V3WYg4"
db <- "admin"
## Connect to MongoDB remote server
mongo <- mongo.create(host = host, db = db, username = username, password = password)
## This is the namespace
collection <- "example"
ns <- paste(db, collection, sep=".")

## Test if we have successfully connected
mongo.is.connected(mongo)

## Insert an object defined using JSON notation
json <- '{"a":1, "b":2, "c":{"d":3, "e":4}}'
bson <- mongo.bson.from.JSON(json)

## Data structure that's closest to to a JSON in R is a list
list <- list(a=2, b=3, c=list(d=4, e=5))
bson <- mongo.bson.from.list(list)
mongo.insert(mongo, ns, bson)

## Retrieve data from MongoDB
## Query for documents whose field "a" holds a value greater or equal to 1
json <- '{"a":{"$gte":1}}'
bson <- mongo.bson.from.JSON(json)
cursor <- mongo.find(mongo, ns, bson)
while(mongo.cursor.next(cursor)) {
  value <- mongo.cursor.value(cursor)
  list <- mongo.bson.to.list(value)
  str(list)
}

json <- '{"_id":1}'
bson <- mongo.bson.from.JSON(json)
cursor <- mongo.find(mongo, ns, bson)
mongo.cursor.next(cursor)

value <- mongo.cursor.value(cursor)
list <- mongo.bson.to.list(value)
print(list)
str(list)




