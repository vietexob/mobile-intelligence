rm(list = ls())

library(plyr)
library(ggplot2)
library(maptools)

## Load the flow data -- OD points are needed
input <- read.csv(file="./data/london/mapping_flows.csv", header=TRUE)

## Load the lookup table that provides coordinates of area codes
centroids <- read.csv(file="./data/london/msoa_popweightedcentroids.csv", header=TRUE)

## Joining to get the XY coordinates of OD points
or.xy <- merge(input, centroids, by.x="origin", by.y="Code")
names(or.xy) <- c("origin", "destination", "trips", "o_name", "oX", "oY")
dest.xy <- merge(or.xy, centroids, by.x="destination", by.y="Code")
names(dest.xy) <- c("origin", "destination", "trips", "o_name", "oX", "oY",
                    "d_name", "dX", "dY")

## Remove the axes in the resulting plot
x_quiet <- scale_x_continuous("", breaks=NULL)
y_quiet <- scale_y_continuous("", breaks=NULL)
quiet <- list(x_quiet, y_quiet)

(ggplot(dest.xy[which(dest.xy$trips > 10), ], aes(oX, oY)) + # exclude flows <= 10
   geom_segment(aes(x=oX, y=oY, xend=dX, yend=dY, alpha=trips), col="white") +
   scale_alpha_continuous(range=c(0.03, 0.3)) + # set line transparency to make the plot readable
   theme(panel.background=element_rect(fill="black", color="black")) + # set black background
   quiet + coord_equal() + guides(alpha=FALSE)) # remove axes and fix aspect ratio

ggsave(filename="./figures/spatial/london_flows.pdf", width=10, height=12)
