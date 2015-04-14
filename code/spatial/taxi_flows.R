rm(list = ls())

library(plyr)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)
# library(rmongodb)

source("./code/util/fivethirtyeight_theme.R")

## Load all taxi GPS traces on Sept. 11, 2009 when occupied
taxi.data <- read.csv(file="./data/taxi/taxi_gps_2009_09_11.csv", header=TRUE)

# ## Login credentials
# host <- "heinz-tjle.heinz.cmu.edu"
# username <- "student"
# password <- "helloWorld"
# db <- "admin"
# 
# ## Connect to MongoDB remote server
# mongo <- mongo.create(host = host, db = db, username = username, password = password)
# ## Check if we are successfully connected
# mongo.is.connected(mongo)
# 
# ## The database we're working with is 'admin' and the collection is 'taxi'
# collection <- "taxi"
# namespace <- paste(db, collection, sep=".")
# 
# ## Retrieve all ride records on Sept. 11, 2009
# query <- mongo.bson.from.list(list('date'='2009-09-11', 'occupy'=1))
# ## Define the fields to be returned
# fields <- mongo.bson.buffer.create()
# ## '1L' means we want to turn this field on, '0L' to turn it off
# mongo.bson.buffer.append(fields, "_id", 0L)
# mongo.bson.buffer.append(fields, "taxi_no", 1L)
# mongo.bson.buffer.append(fields, "date", 1L)
# mongo.bson.buffer.append(fields, "time", 1L)
# mongo.bson.buffer.append(fields, "lon", 1L)
# mongo.bson.buffer.append(fields, "lat", 1L)
# ## Make an object from the buffer
# fields <- mongo.bson.from.buffer(fields)
# 
# ## Create the query cursor
# cursor <- mongo.find(mongo, namespace, query=query, fields=fields)
# ## Define a master data frame to store results
# taxi.data <- data.frame(stringsAsFactors=FALSE)
# ## Iterate over the cursor
# while(mongo.cursor.next(cursor)) {
#   ## Iterate and grab the next record
#   value <- mongo.cursor.value(cursor)
#   taxi.record <- mongo.bson.to.list(value)
#   ## Make it a data frame
#   taxi.df <- as.data.frame(t(unlist(taxi.record)), stringsAsFactors=FALSE)
#   ## Bind to the master data frame
#   taxi.data <- rbind.fill(taxi.data, taxi.df)
# }
# 
# ## Release the resources attached to cursor on both client and server
# mongo.cursor.destroy(cursor)
# ## Close the connection
# mongo.disconnect(mongo)
# mongo.destroy(mongo)

## Compute the duration between each timestamp
duration <- vector()
trip_indicator <- vector()
threshold <- 15*60 # seconds

## Create a progress bar
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(taxi.data)-1)
for(i in 1:(nrow(taxi.data)-1)) {
  this_taxi_no <- taxi.data$taxi_no[i]
  next_taxi_no <- taxi.data$taxi_no[i+1]
  
  if(this_taxi_no == next_taxi_no) {
    this_timestamp <- toString(taxi.data$time[i])
    this_timestamp <- strsplit(this_timestamp, ":")[[1]]
    this_second <- as.numeric(this_timestamp[1])*3600 + as.numeric(this_timestamp[2])*60 + as.numeric(this_timestamp[3])
    
    next_timestamp <- toString(taxi.data$time[i+1])
    next_timestamp <- strsplit(next_timestamp, ":")[[1]]
    next_second <- as.numeric(next_timestamp[1])*3600 + as.numeric(next_timestamp[2])*60 + as.numeric(next_timestamp[3])
    
    duration[i] <- next_second - this_second
    if(i == 1) {
      trip_indicator[i] <- "start"
    } else {
      if(trip_indicator[i-1] == "end" || trip_indicator[i-1] == "error") {
        trip_indicator[i] <- "start"
      } else {
        if(duration[i] >= threshold) {
          trip_indicator[i] <- "end"
        } else {
          trip_indicator[i] <- "going"
        }
      }
    }
  } else {
    duration[i] <- NA
    if(trip_indicator[i-1] == "end") {
      trip_indicator[i] <- "error"
    } else {
      trip_indicator[i] <- "end"
    }
  }
  
  progress.bar$step()
}

duration[nrow(taxi.data)] <- NA
trip_indicator[nrow(taxi.data)] <- "end"
taxi.data$duration <- duration
taxi.data$indicator <- trip_indicator

## Create a progress bar
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(taxi.data))
or.data <- data.frame() # data frame for the origins
dest.data <- data.frame() # data frame for the destinations
for(i in 1:nrow(taxi.data)) {
  indicator <- taxi.data$indicator[i]
  if(indicator == "start") {
    or_lon <- taxi.data$lon[i]
    or_lat <- taxi.data$lat[i]
    or_data <- data.frame(or_lon = or_lon, or_lat = or_lat)
    or.data <- rbind(or.data, or_data)
  } else if(indicator == "end") {
    dest_lon <- taxi.data$lon[i]
    dest_lat <- taxi.data$lat[i]
    dest_data <- data.frame(dest_lon = dest_lon, dest_lat = dest_lat)
    dest.data <- rbind(dest.data, dest_data)
  } else {
    ## Do nothing
  }
  
  progress.bar$step()
}

## Combine the OD pairs
od.data <- cbind(or.data, dest.data)
## Add artificial group to the OD pairs in order to be compatitable with the shapefiles
od.data$group <- seq(1:nrow(od.data))

## Remove the axes in the resulting plot
x_quiet <- scale_x_continuous("", breaks=NULL)
y_quiet <- scale_y_continuous("", breaks=NULL)
quiet <- list(x_quiet, y_quiet)

## Read the shapefiles
## The spatial object wouldn't have a coordinate system assigned to it.
## We can check it by proj4string(sz_bou). We thus need to assign a CRS
## (coordinate reference system) to the object before we can plot it.
## Here we use the WGS84 standard (the World Geodetic System proposed in 1984)
sz_bou <- readOGR(dsn="./data/sz_shp/", layer="sz_bou")
proj4string(sz_bou) <- CRS("+init=epsg:4326")

sz_road <- readOGR(dsn="./data/sz_shp/", layer="sz_road")
proj4string(sz_road) <- CRS("+init=epsg:4326")

sz_veg <- readOGR(dsn="./data/sz_shp/", layer="sz_veg")
proj4string(sz_veg) <- CRS("+init=epsg:4326")

sz_wat <- readOGR(dsn="./data/sz_shp/", layer="sz_wat")
proj4string(sz_wat) <- CRS("+init=epsg:4326")

## Convert shapefiles into data frames so that they can be plotted using ggplot
sz_bou.data <- fortify(sz_bou)
sz_road.data <- fortify(sz_road) ## this will take a while
sz_veg.data <- fortify(sz_veg)
sz_wat.data <- fortify(sz_wat)

## Plot the shapfiles using ggplot
shenzhen <- ggplot(data=sz_bou.data, aes(x=long, y=lat,
                                         group=group)) + geom_polygon(fill="lightblue") +
  ggtitle("Map of Shenzhen + All Taxi OD Pairs on 09/11/2009")
shenzhen <- shenzhen + geom_polygon(data=sz_wat.data,
                                    aes(x=long, y=lat, group=group),
                                    fill="blue", alpha=0.75)
shenzhen <- shenzhen + geom_polygon(data=sz_veg.data,
                                    aes(x=long, y=lat, group=group),
                                    fill="darkgreen", alpha=0.75)
shenzhen <- shenzhen + geom_polygon(data=sz_road.data,
                                    aes(x=long, y=lat, group=group),
                                    color="darkgrey", fill=NA)
## Add OD pairs to the shapefiles
shenzhen.od <- shenzhen + geom_segment(data=od.data, aes(x=or_lon, y=or_lat,
                                                         xend=dest_lon, yend=dest_lat,
                                                         group=group),
                                       alpha=0.8, col="red") + quiet + coord_equal() +
  fivethirtyeight_theme()
print(shenzhen.od)
ggsave(filename="./figures/spatial/shenzhen_od_pairs.png", scale = 3, dpi = 400)
