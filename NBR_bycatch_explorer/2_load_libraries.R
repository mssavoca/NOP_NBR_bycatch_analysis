# File load_Functions.R - file of all small functions used in code. These were put into a separate file so that they can be loaded at the beginning of a session and used for all other script files.


# #1. pkgTest
# # Function pkgTest - An R function to test if a package is installed. If not, the package and all dependencies will be installed from the default CRAN mirror.
# ## Code taken from Stack Overflow - http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
# # 
# pkgTest <- function(x)
# {
#   if (!require(x,character.only = TRUE))
#   {
#     install.packages(x,dep=TRUE)
#     if(!require(x,character.only = TRUE)) stop("Package not found")
#   }
# }
# 
# 
# pkgTest("gmt")
# pkgTest("SDMTools")
# pkgTest("ncdf4")
# pkgTest("RCurl")
# pkgTest("raster")
# pkgTest("data.table")
# pkgTest("lunar")
# pkgTest("R.utils")
# pkgTest("RColorBrewer")
# pkgTest("gbm")
# pkgTest("colorRamps")
# pkgTest("maps")
# pkgTest("mapdata")
# pkgTest("tweedie")
# pkgTest("mgcv")
# pkgTest("gamm4")
# pkgTest("sp")
# pkgTest("rgdal")
# pkgTest("fields")
# pkgTest("maptools")
# pkgTest("adehabitatHR")
# pkgTest("adehabitatHS")
# pkgTest("adehabitatLT")
# pkgTest("adehabitatMA")
# pkgTest("tiff")
# pkgTest("shiny")
# pkgTest("leaflet")
# pkgTest("dplyr")
# pkgTest("urltools")
# pkgTest("jsonlite")
# pkgTest('geojsonio')
# pkgTest('proj4')

library(data.table)
library(R.utils)
library(maps)
library(mapdata)
library(mgcv)
library(gamm4)
library(sp)
library(rgdal)
library(maptools)
library(shiny)
library(leaflet)
library(dplyr)
#library(urltools)
library(jsonlite)
library(geojsonio)
library(tidyverse)
library(RcppEigen)
library(shinythemes)
library(shinydashboard)
library(miniUI)
library(shinyjs)
library(DT)
library(htmlwidgets)
library(lubridate)

# pkgTest("miniUI")
# pkgTest("shinyjs")


