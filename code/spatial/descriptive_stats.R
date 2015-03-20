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

## 1. Loading Spatial Data
filepath <- "./data/sz_shp/"
sz_bou <- readOGR(dsn = filepath, layer = "sz_bou")
