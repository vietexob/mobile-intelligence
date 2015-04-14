rm(list = ls())

library(maptools)
library(ggplot2)
library(rgdal)
library(rgeos)

## INSTALLATION INSTRUCTIONS FOR OS X
## (1) Install Homebrew: http://brew.sh
## (2) Install 'geos': brew install geos
## (3) Download and install 'rgeos' from CRAN: R CMD INSTALL ./rgeos_0.3-8.tar
## (4) Follow: http://spatial.ly/2010/11/installing-rgdal-on-mac-os-x/

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

sz_poi <- readOGR(dsn="./data/sz_shp/", layer="sz_poi")
proj4string(sz_poi) <- CRS("+init=epsg:4326")

sz_wat <- readOGR(dsn="./data/sz_shp/", layer="sz_wat")
proj4string(sz_wat) <- CRS("+init=epsg:4326")

sz_tow <- readOGR(dsn="./data/sz_shp/", layer="sz_tow")
## Assign WGS84 coordinate system to the object
proj4string(sz_tow) <- CRS("+init=epsg:4326")

## Plot the shapefiles the traditional way
pdf(file="./figures/sz_shp/sz_shp_plot.pdf", width=42, height=21)
plot(sz_bou, col="lightblue", main="Map of Shenzhen")
plot(sz_veg, col="darkgreen", add=TRUE)
plot(sz_wat, col="blue", add=TRUE)
lines(sz_road, col="darkgrey")
points(sz_tow, col="red", pch=20, cex=0.8)
dev.off()

## Convert shapefiles into data frames so that they can be plotted using ggplot
sz_bou.data <- fortify(sz_bou)
sz_road.data <- fortify(sz_road) ## this will take some time
sz_veg.data <- fortify(sz_veg)
sz_wat.data <- fortify(sz_wat)

## Points, as opposed to polygons, can be directly converted into data frames
## without transformation. Otherwise, an error will result if attempt to transform
sz_poi.data <- as.data.frame(sz_poi)
sz_tow.data <- as.data.frame(sz_tow)

## Plot the shapfiles using ggplot
shenzhen <- ggplot(data=sz_bou.data, aes(x=long, y=lat,
                                         group=group)) + geom_polygon(fill="lightblue") +
  ggtitle("Map of Shenzhen")
shenzhen <- shenzhen + geom_polygon(data=sz_wat.data,
                                    aes(x=long, y=lat, group=group),
                                    fill="blue", alpha=0.75)
shenzhen <- shenzhen + geom_polygon(data=sz_veg.data,
                                    aes(x=long, y=lat, group=group),
                                    fill="darkgreen", alpha=0.75)
shenzhen <- shenzhen + geom_polygon(data=sz_road.data,
                                    aes(x=long, y=lat, group=group), color="darkgrey", fill=NA)
shenzhen <- shenzhen + geom_point(data=sz_tow.data,
                                  aes(x=coords.x1, y=coords.x2, group=NULL), color="red",
                                  fill="red", alpha=0.8, size=2, shape=20)
print(shenzhen, scale=3, dpi=400)

## Save the plot as a file on disk
ggsave(filename="./figures/sz_shp/sz_shp_ggplot.pdf", scale = 3, dpi = 400)
