rm(list = ls())

library(ggplot2)
library(ggmap) # for plotting maps
library(scales) # for plot formatting 
library(plyr) # for data manipulation

source("./code/util/fivethirtyeight_theme.R")

## See http://www.thebureauinvestigates.com/2014/05/23/get-the-data-what-the-drones-strike/

drone.data <- read.csv(file="./data/drone_strikes.csv")
## Break down by year
year <- vector()
for(i in 1:nrow(drone.data)) {
  dateStr <- toString(drone.data$Date[i])
  dateStrSplit <- strsplit(dateStr, "/")[[1]]
  year[i] <- as.numeric(dateStrSplit[3])
}
drone.data$year <- year
## Subset the data by year
subset.drone.data <- subset(drone.data, year >= 2008)
## Convert year to factor
subset.drone.data$year <- as.factor(subset.drone.data$year)

## Specify a map with center at the center of all the coordinates
mean.longitude <- mean(subset.drone.data$Longitude)
mean.latitude <- mean(subset.drone.data$Latitude)
drone.map <- get_map(location = c(mean.longitude, mean.latitude),
                     zoom = 9, scale = 2)
## Plot the strike locations, colored by time period and sized by minimum # casualties
strike.pts <- geom_point(data = subset.drone.data, aes(x = Longitude, y = Latitude,
                                                       fill = Time.Period,
                                                       size = Minimum.Total.Reported.Killed),
                         alpha = 0.80, shape = 21)
drone.map <- ggmap(drone.map) + strike.pts + guides(size=FALSE, alpha = FALSE)
## Give the map a title
drone.map <- drone.map + ggtitle("US Drone Strikes in Pakistan from 2008 to 2013 (sized by # casualties)")
## Add the density contours
drone.map <- drone.map + geom_density2d(data = subset.drone.data,
                                        aes(x = Longitude, y = Latitude))
drone.map <- drone.map + facet_wrap(~year)
print(drone.map)
## Save the plot on disk
ggsave(filename="./figures/spatial/drone_strikes.pdf", width=20, height=15)
