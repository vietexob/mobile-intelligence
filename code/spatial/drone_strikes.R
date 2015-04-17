rm(list = ls())

library(ggplot2)
library(ggmap) # for plotting maps

source("./code/util/fivethirtyeight_theme.R")

## See http://www.thebureauinvestigates.com/2014/05/23/get-the-data-what-the-drones-strike/

drone.data <- read.csv(file="./data/drone/drone_strikes.csv")
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
drone.map <- ggmap(drone.map, extent="device", legend="none")
## Plot a heat map layer: Polygons with fill color based on relative frequency of events
drone.map <- drone.map + stat_density2d(data=subset.drone.data,
                                        aes(x=Longitude, y=Latitude, fill=..level..,
                                            alpha=..level..), geom="polygon")
## Define the colors to fill the density contours
drone.map <- drone.map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
## Add the strike points, color them red and define round shape
drone.map <- drone.map + geom_point(data=subset.drone.data,
                                    aes(x=Longitude, y=Latitude),
                                    fill="red", shape=21, alpha=0.8)
## Remove any legend and apply fancy theme
drone.map <- drone.map + guides(size=FALSE, alpha = FALSE) + fivethirtyeight_theme()
## Give the map a title
drone.map <- drone.map + ggtitle("US Drone Strikes in Pakistan from 2008 to 2013")

## Plot strikes by each year
drone.map <- drone.map + facet_wrap(~year)
print(drone.map)
## Save the plot on disk
ggsave(filename="./figures/spatial/drone_strikes.png", width=12, height=9)
