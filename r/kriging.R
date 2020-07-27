###############################################################################
#                            Kriging Th-234 Data
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -----------------------------------------------------------------
library(readxl) # reading Excel data
library(sp) # spatial library
library(sf) # working with shape files
library(gstat) # functionality for kriging
library(raster) 
library(rgdal) # working with shape files and geospatial data
library(rgeos) # working with geospatial data
library(automap) # for fitting the variogram
suppressPackageStartupMessages({
  library(dplyr) # for using glimpse to see data
  library(ggplot2) # for plotting
  library(scales) # for using comma
  library(magrittr)
})

# Data Wrangling --------------------------------------------------------------
# Turn off scientific notation and set degit max:
options(scipen = 999)
options(digits=22)

# Import all data from Excel:
th234_data_all <- read_excel("data/merged_th234_data_220720.xlsx", 
                             sheet = "metadata&data", na = "nd")

# Import only Pacific Ocean data:
th234_pacific <- th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean"),]
th234_pacific <- rbind(th234_pacific, th234_data_all[which(th234_data_all$Ocean == "Pacifc Ocean"),])
th234_pacific <- rbind(th234_pacific, th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean & Artic Ocean")])

# Remove NA from data:
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lat_decimal_degrees)), ] # latitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lon_decimal_degrees)), ] # longitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$part_234Th_small_dpmL)), ] # small particulate Th234
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$part_234Th_large_dpmL)), ] # large particulate Th234
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$depth_m)), ] # depth of each measurement

# QUESTION: Should remove all data that doesn't have small and large fractions? As otherwise we don't have total Th234 data... 
# QUESTION:  What does slurp, surface ( = 0?), and bottomn (with no bottom measurement) mean? How to treat?

# Variogram -------------------------------------------------------------------
# Convert to SPDF (x + y + z):
coordinates(th234_pacific) <- ~ lat_decimal_degrees + lon_decimal_degrees + depth_m

# Calculate variogram:
th234.vgm <- variogram(log(part_234Th_small_dpmL)~1, th234_pacific)

# Fit a model to variogram:
th234.fit <- autofitVariogram(log(part_234Th_small_dpmL)~1,
                              th234_pacific,
                              model = c("Sph"),
                              kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                              fix.values = c(NA, NA, NA),
                              start_vals = c(NA,NA,NA),
                              verbose = T)

# Plot model and variogram:
plot(th234.vgm, th234.fit$var_model, main = "Fitted variogram")

# Prediction Grid -------------------------------------------------------------
# Load in Longhust Province shape file:
oceans <- shapefile("longhurst/Longhurst_world_v4_2010.shp")

# Kriging ---------------------------------------------------------------------



