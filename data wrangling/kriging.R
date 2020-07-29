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
  library(stringr)
  library(maps)
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
  library(plotly)
  library(leaflet)
  library(mapview)
})

# Data Wrangling --------------------------------------------------------------
# Do you want to plot? 0 for no. 1 for yes. 
plotting = 0

# Data Wrangling --------------------------------------------------------------
# Turn off scientific notation and set digit and print max:
options(scipen = 999)
options(digits=22)
options(max.print=999999)

# Import all data from Excel:
th234_data_all <- read_excel("data/th234_data_220720.xlsx", 
                             sheet = "metadata&data", na = "nd")

# Import only Pacific Ocean data:
th234_pacific <- th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean"),]
th234_pacific <- rbind(th234_pacific, 
                       th234_data_all[which(th234_data_all$Ocean == "Pacifc Ocean"),])
th234_pacific <- rbind(th234_pacific, 
                       th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean & Artic Ocean")])

# Make Th_tot data ------------------------------------------------------------
unknown <- which(is.na(th234_pacific$`total_234Th(dpm/L)`) == TRUE)
unknown_before <- length(unknown)
i = 1
for (i in 1:dim(th234_pacific)[1]) {
  if (is.na(th234_pacific$`total_234Th(dpm/L)`[i]) == TRUE) {
    if (is.na(th234_pacific$`part_234Th_small(dpm/L)`[i]) == FALSE) {
      if (is.na(th234_pacific$`part_234Th_large(dpm/L)`[i]) == FALSE) {
        if (is.na(th234_pacific$`diss_234Th(dpm/L)`[i]) == FALSE) {
          th234_pacific$`total_234Th(dpm/L)`[i] <- th234_pacific$`part_234Th_small(dpm/L)`[i] +
            th234_pacific$`part_234Th_large(dpm/L)`[i] +
            th234_pacific$`diss_234Th(dpm/L)`[i]
        }
      }
    }
  }
}
# Did it work?
unknown <- which(is.na(th234_pacific$`total_234Th(dpm/L)`) == TRUE)
unknown_after <- length(unknown)
change_th234_d <- unknown_before - unknown_after

# Remove NA from data (may want to use na.omit eventually on th234_pacific) ---
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lat_decimal_degrees)), ] # latitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lon_decimal_degrees)), ] # longitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$`total_234Th(dpm/L)`)), ] # total Th234
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$`238U(dpm/L)`)), ] # total U238
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$depth_m)), ] # depth of each measurement
th234_pacific <- th234_pacific[-which(th234_pacific$`total_234Th(dpm/L)` > 10), ] # th234_tot that are too big
th234_pacific <- th234_pacific[-which(th234_pacific$lon_decimal_degrees > 0), ] # long in region we are interested in

# Make data for total original locations:
th234_locations_total <- th234_data_all[-which(is.na(th234_data_all$lat_decimal_degrees)), ] # get rid of na in latitude
th234_locations_total <- th234_data_all[-which(is.na(th234_data_all$lon_decimal_degrees)), ] # get rid of na in longitude
th234_locations_total <- th234_locations_total[-which(abs(th234_locations_total$lat_decimal_degrees) > 90), ] # proper latitude
th234_locations_total <- th234_locations_total[-which(abs(th234_locations_total$lon_decimal_degrees) > 180), ] # proper longitude

# Make data frames:
th234_locations_old <- data.frame(LAT = th234_locations_total$lat_decimal_degrees, 
                                  LON = th234_locations_total$lon_decimal_degrees)
th234_locations_new <- data.frame(LAT = th234_pacific$lat_decimal_degrees, 
                                  LON = th234_pacific$lon_decimal_degrees)

# Plot data that we have ------------------------------------------------------
world <- map_data("world")
worldplot <- ggplot() +
             geom_polygon(data = world, 
                          aes(x = long, 
                              y = lat, 
                              group = group)) + 
             coord_fixed(1.3) + 
             geom_point(data = th234_locations_old, 
                        aes(x = LON, 
                            y = LAT, 
                            color = 'Old Th-234 Stations'),
                        size = 1, 
                        stroke = 0, 
                        shape = 16) + 
             geom_point(data = th234_locations_new, 
                        aes(x = LON, 
                            y = LAT, 
                            color = 'New Th-234 Stations'), 
                        size = 1, 
                        stroke = 0, 
                        shape = 16) + 
             labs(title='Global Th-234 Stations', 
                  x = "Longitude", 
                  y = "Latitude", 
                  color = "Th-234 Station Types") + 
             theme(plot.title=element_text(size=10, face="bold"), 
                   axis.text.x=element_text(size=10), 
                   axis.text.y=element_text(size=10),
                   axis.title.x=element_text(size=10, face="bold"),
                   axis.title.y=element_text(size=10, face="bold")) 
print(worldplot)
# Warning message: Removed 14 rows containing missing values (geom_point). I do not know why. 
if (plotting == 1) {
  dev.copy(pdf, 'figures/th234_tot/th234_global_sites.pdf')
  dev.off()
}

# Variogram -------------------------------------------------------------------
# Make only one data column:
th234_total <- data.frame(TH234_TOT = th234_pacific$`total_234Th(dpm/L)`, 
                          LAT = th234_pacific$lat_decimal_degrees, 
                          LON = th234_pacific$lon_decimal_degrees, 
                          DEPTH = th234_pacific$depth_m )

# Convert to SPDF:
coordinates(th234_total) <- ~ LAT + LON + DEPTH # c("lat_decimal_degrees", "lon_decimal_degrees", "depth_m")

# As (lon,lat), use Web Mercater this is the most common spatial reference system for the entire world:
proj4string(th234_total) <- CRS("+init=epsg:3857")

# View the stations:
mapview(th234_total, map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"))

# Calculate variogram:
th234 <- th234_total$TH234_TOT
th234.vgm <- variogram(th234 ~ LAT + LON + DEPTH, th234_total) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# Fit a model to variogram:
th234.fit <- autofitVariogram(th234 ~ LAT + LON + DEPTH,
                              th234_total,
                              model = c("Sph", 
                                        "Exp", 
                                        "Gau", 
                                        "Ste", 
                                        "Mat"),
                              kappa = c(0.05, 
                                        seq(0.2, 2, 0.1), 
                                        5, 
                                        10),
                              fix.values = c(NA, NA, NA),
                              verbose = TRUE,
                              GLS.model = NA,
                              start_vals = c(NA,NA,NA))

# Plot model and variogram:
plot(th234.vgm, 
     th234.fit$var_model, 
     main = "Fitted variogram",
     ylab = "Semivariance",
     xlab = "Distance"
     )
if (plotting == 1) {
  dev.copy(pdf, 'figures/th234_tot/fitted_variogram.pdf')
  dev.off()
}

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

# As (lon,lat), use Web Mercater this is the most common spatial reference system for the entire world:
regions <- spTransform(regions, CRS("+init=epsg:3857"))

# Generate points ever 2.8 degrees:
pts <- spsample(regions, cellsize=c(2.8,2.8), type="regular")

# Plot pacific regions --------------------------------------------------------
# Associate data with the polygons each represents (the numbers all the way to the left originally):
regions@data$id <- rownames(regions@data)
region_points <- fortify(regions, region = "id") # make a data.frame from an SPDF sorted by each polygon (a total of 9 here)
longhurst_df <- merge(region_points, regions@data, by = "id") # merge all these into one long data.frame with coordinates and polygon information

# Associate data with points:
interp_points <- data.frame(LAT = pts@coords[,2], LON = pts@coords[,1])

# Make the plot:
ggRegions <- ggplot() +
             geom_polygon(data = world, 
                          aes(x = long, 
                              y = lat, 
                              group = group)) + 
             geom_polygon(data = longhurst_df, 
                          aes(x=long, 
                              y=lat, 
                              group = group, 
                              fill = 'Longhurst Regions')) + 
             geom_point(data = interp_points, 
                        aes(x=LON,
                            y=LAT, 
                            fill = 'Interpolation Points'), 
                        size = 0.75, 
                        stroke = 0, 
                        shape = 16, 
                        color='red') +
             coord_fixed(1.3) + 
             labs(title='Interpolation Region', 
                  x = "Longitude", 
                  y = "Latitude", 
                  fill = "") + 
             theme(plot.title=element_text(size=10, face="bold"), 
                   axis.text.x=element_text(size=10), 
                   axis.text.y=element_text(size=10),
                   axis.title.x=element_text(size=10, face="bold"),
                   axis.title.y=element_text(size=10, face="bold")) 
print(ggRegions)
if (plotting == 1) {
  dev.copy(pdf, 'figures/kriging/interpolation_region.pdf')
  dev.off()
}

# Plot queuing vs data points:
ggPoints <- ggplot() + 
            geom_polygon(data = world, 
                         aes(x = long, 
                             y = lat, 
                             group = group)) + 
            geom_point(data = interp_points, 
                       aes(x=LON, 
                           y=LAT, 
                           fill = 'Interpolation Points'), 
                       size = 0.75, 
                       stroke = 0, 
                       shape = 16, 
                       color='red') +
            geom_point(data = th234_locations_new, 
                       aes(x = LON, 
                           y = LAT, 
                           fill = 'New Th-234 Stations'), 
                       size = 1, 
                       stroke = 0, 
                       shape = 16, 
                       color='lightblue') + 
            coord_fixed(1.3) + 
            labs(title='Prediction Grid and Th-234 Stations', 
                 x = "Longitude", 
                 y = "Latitude", 
                 fill = "") + 
            theme(plot.title=element_text(size=10, face="bold"), 
                  axis.text.x=element_text(size=10), 
                  axis.text.y=element_text(size=10),
                  axis.title.x=element_text(size=10, face="bold"),
                  axis.title.y=element_text(size=10, face="bold")) 
print(ggPoints)
if (plotting == 1) {
  dev.copy(pdf, 'figures/kriging/interpolation_prediction.pdf')
  dev.off()
}

# Make prediction grid --------------------------------------------------------
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
data = data.frame(ID = 1:dim(pacific)[1])
th234_pacific_1 <- SpatialPoints(pacific)
pacific_234th <- SpatialPointsDataFrame(th234_pacific_1, data)

# Use GPS, world-wide reference projection:
proj4string(pacific_234th) <- CRS("+init=epsg:3857")

# Plot Prediction Points:
prediction <- plot_ly(x=pacific_234th@coords[,2], 
                      y=pacific_234th@coords[,1], 
                      z=-pacific_234th@coords[,3], 
                      type='scatter3d', 
                      mode="markers", 
                      color=pacific_234th@coords[,3])
prediction <- prediction %>% layout(title = 'Prediction Grid Points',
                                    xaxis = list(title = 'Longitude'), 
                                    yaxis = list(title = 'Latitude'))
print(prediction)

# Kriging ---------------------------------------------------------------------
# Krige Type 1:
th234_total_kriged <- autoKrige(th234 ~ th234_total@coords[,1] + th234_total@coords[,2] + th234_total@coords[,3],
                                th234_total,
                                th234_pacific_1,
                                block = 0,
                                model = c("Sph", "Exp", "Gau", "Ste"),
                                kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                                fix.values = c(NA,NA,NA),
                                remove_duplicates = TRUE,
                                verbose = FALSE,
                                GLS.model = NA)

# Krige Type 2:
th234_total_kriged <- krige(th234 ~ th234_total@coords[,1] + th234_total@coords[,2] + th234_total@coords[,3],
                            th234_total,
                            pacific_234th,
                            model = th234.fit$var_model,
                            nmax = 0)


