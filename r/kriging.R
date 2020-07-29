###############################################################################
#                            Kriging Th-234 Data
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading Excel data
  library(sp) # spatial library
  library(sf) # working with shape files
  library(gstat) # functionality for kriging
  library(maptools)
  library(tidyverse)
  library(raster) 
  library(lwgeom)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rgdal) # working with shape files and geospatial data
  library(rgeos) # working with geospatial data
  library(automap) # for fitting the variogram
  library(dplyr) # for using glimpse to see data
  library(ggplot2) # for plotting
  library(ggthemes)
  library(ggalt)
  library(scales) # for using comma
  library(magrittr)
  library(gridExtra)
})

# Data Wrangling --------------------------------------------------------------
# Do you want to plot? 0 for no. 1 for yes. 
plotting = 1

# Data Wrangling --------------------------------------------------------------
# Turn off scientific notation and set digit and print max:
options(scipen = 999)
options(digits=22)
options(max.print=999999)

# Important notes on Data Wrangling:
# (0) All commas and blanks replaced with nd.
# (1) Depth: Bottom at (-49.33,2.50) and (-49.17,2.08) were taken to be at those depths given by: https://maps.ngdc.noaa.gov/viewers/bathymetry/
# (2) Depth: Surface was taken to be 3 m
# (3) Depth: All X_Y data was taken to be greatest value, Y. 
# (3.5) Slurp and Slurp_estimated taken to be 100m per Buesseler correspondence. 
# (4) Lat: TT043 station lat of , changed to nd
# (5) Lon: #VALUE replaced with nd
# (6) TotTh234 and uncert totth234: #DIV/0! replaced with nd
# QUESTION: Uranium has X-Y values... how to deal with 

# Import all data from Excel:
th234_data_all <- read_excel("data/th234_data_220720.xlsx", 
                             sheet = "metadata&data", na = "nd")

# Import only Pacific Ocean data:
th234_pacific <- th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean"),]
th234_pacific <- rbind(th234_pacific, th234_data_all[which(th234_data_all$Ocean == "Pacifc Ocean"),])
th234_pacific <- rbind(th234_pacific, th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean & Artic Ocean")])

# Remove NA from data (may want to use na.omit eventually on th234_pacific):
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lat_decimal_degrees)), ] # latitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lon_decimal_degrees)), ] # longitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$`total_234Th(dpm/L)`)), ] # total Th234
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$`part_234Th_small(dpm/L)`)), ] # total Th234
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$`238U(dpm/L)`)), ] # total U238
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$depth_m)), ] # depth of each measurement

# Variogram -------------------------------------------------------------------
# Convert to SPDF:
coordinates(th234_pacific) <- c("lat_decimal_degrees", "lon_decimal_degrees", "depth_m")
  
# Calculate variogram:
th234 <- th234_pacific$`part_234Th_small(dpm/L)`
th234.vgm <- variogram(th234 ~ lat_decimal_degrees + lon_decimal_degrees + depth_m, th234_pacific) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# QUESTION: ~1 means assume constant trend. Is this correct?

# Fit a model to variogram:
th234.fit <- autofitVariogram(th234 ~ lat_decimal_degrees + lon_decimal_degrees + depth_m,
                              th234_pacific,
                              model = c("Sph"),
                              kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                              fix.values = c(NA, NA, NA),
                              verbose = FALSE,
                              GLS.model = NA,
                              start_vals = c(NA,NA,NA))

# Plot model and variogram:
plot(th234.vgm, th234.fit$var_model, main = "Fitted variogram")

# Prediction Grid - Longhurst -------------------------------------------------
# Shape files are from: https://www.marineregions.org/downloads.php
# Load in Longhust Province shape file:
oceans <- readOGR("shapes/longhurst/Longhurst_world_v4_2010.shp")

# The provinces that I will be kriging over are:
#   [42] ALSK - Coastal, Alaska Downwelling Coastal Province
#   [30] PSAE - Westerlies, Pacific Subarctic Gyres Province (East)
#   [33] NPPF - Westerlies, N. Pacific Polar Front Province
#   [43] CCAL - Coastal, California Upwelling Coastal Province
#   [37] NPTG - Trades, N. Pacific Tropical Gyre Province
#   [44] CAMR - Coastal, Central American Coastal Province
#   [38] PNEC - Trades, N. Pacific Equatorial Countercurrent Province
#   [39] PEQD - Trades, Pacific Equatorial Divergence Province
#   [36] SPSG - Westerlies, S. Pacific Subtropical Gyre Province
#   [45] CHIL - Coastal, Chile-Peru Current Coastal Province
 
# Isolate each region:
alsk <- oceans[43,]
psae <- oceans[31,]
nppf <- oceans[34,]
ccal <- oceans[44,]
nptg <- oceans[38,]
camr <- oceans[45,]
pnec <- oceans[39,]
peqd <- oceans[40,]
spsg <- oceans[37,]
chil <- oceans[46,]

# Turn into the Pacific Ocean of interpolation:
regions <- rbind(alsk, psae)
regions <- rbind(regions, nppf)
regions <- rbind(regions, ccal)
regions <- rbind(regions, nptg)
regions <- rbind(regions, camr)
regions <- rbind(regions, pnec)
regions <- rbind(regions, peqd)
regions <- rbind(regions, spsg)
regions <- rbind(regions, chil)

# Generate points ever 2.8 degrees:
pts <- spsample(regions, cellsize=c(2.8,2.8), type="regular")

# Plot pacific:
if (plotting == 1) {
  plot(regions) ; points(pts, col='red', pch=3, cex=0.5)
}

# Make depth bins:
watercolumn = array(dim=34)
for (i in 1:34) {
  if (i <= 30) {
    watercolumn[i] = i*10
  }
  else {
    watercolumn[i] = 300 + (i-30)*50
  }
}

# Combine into one array:
x <- pts@coords[,1]
y <- pts@coords[,2]
z <- watercolumn

pacific <- matrix(ncol = 3, nrow = dim(pts@coords)[1]*length(watercolumn))
k = 1
for (i in 1:dim(pts@coords)[1]) {
  for (j in 1:length(watercolumn)) {
    pacific[k,1] <- x[i]
    pacific[k,2] <- y[i]
    pacific[k,3] <- watercolumn[j]
    k = k + 1
  }
}

# Make into a SPDF:
data = data.frame(ID = 1:(k-1))
th234_pacific_1 <- SpatialPoints(pacific)
pacific_234th <- SpatialPointsDataFrame(th234_pacific_1, data)

# Plotting new pacific SPDF:
if (plotting == 1) {
  plot(pacific_234th)
}

# Kriging ---------------------------------------------------------------------
th234.kriged <- krige(th234~1, th234_pacific, pacific_234th, model=th234.fit$var_model)


