rm(list = ls())

library(ggplot2)
library(ggmap) # for plotting maps
library(scales) # for plot formatting 
library(plyr) # for data manipulation

## http://www.thebureauinvestigates.com/2014/05/23/get-the-data-what-the-drones-strike/

drone.data <- read.csv(file="./data/drone_strikes.csv")

## Specify a map with center at the center of all the coordinates
mean.longitude <- mean(drone.data$Longitude)
mean.latitude <- mean(drone.data$Latitude)
drone.map <- get_map(location = c(mean.longitude, mean.latitude),
                     zoom = 9, scale = 2)

strike.pts <- geom_point(data = drone.data, aes(x = Longitude, y = Latitude,
                                                fill = "red", alpha = 0.80),
                         size = 2, shape = 21)
drone.map <- ggmap(drone.map) + strike.pts + guides(fill = FALSE, alpha = FALSE,
                                                    size = FALSE)
## Give the map a title
drone.map <- drone.map + ggtitle("Locations of Drone Strikes")
## Add the density contours
drone.map <- drone.map + geom_density2d(data = drone.data,
                                        aes(x = Longitude, y = Latitude))
print(drone.map)
## Save the plot on disk
ggsave(filename="./figures/spatial/drone_strikes.pdf", width=12, height=10)
