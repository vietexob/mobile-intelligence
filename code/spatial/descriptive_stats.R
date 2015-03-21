rm(list = ls())

library(maptools)
library(rgdal)
library(ggplot2)
library(rgeos)
library(ggmap)
library(plyr)


## INSTALLATION INSTRUCTIONS FOR OS X
## (1) Install Homebrew: http://brew.sh
## (2) Install 'geos': brew install geos
## (3) Download and install 'rgeos' from CRAN: R CMD INSTALL ./rgeos_0.3-8.tar
## (4) http://spatial.ly/2010/11/installing-rgdal-on-mac-os-x/

## 1. Read the shapefiles
filepath <- "./data/sz_shp/"
sz_bou <- readOGR(dsn=filepath, layer="sz_bou")
sz_road <- readOGR(dsn=filepath, layer="sz_road")
sz_veg <- readOGR(dsn=filepath, layer="sz_veg")
sz_poi <- readOGR(dsn=filepath, layer="sz_poi")
sz_tow <- readOGR(dsn=filepath, layer="sz_tow")
sz_wat <- readOGR(dsn=filepath, layer="sz_wat")

## What is going on here?
proj4string(sz_bou) <- CRS("+init=epsg:27700")

## Convert shapefiles into data frames
sz_bou.data <- fortify(sz_bou)
sz_road.data <- fortify(sz_road)
sz_veg.data <- fortify(sz_veg)
sz_poi.data <- fortify(sz_poi)
sz_tow.data <- fortify(sz_tow)
sz_wat.data <- fortify(sz_wat)







