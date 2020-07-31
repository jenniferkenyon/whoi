###############################################################################
#                                 Kriging Data
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading Excel data
  library(rgdal) # for shape file reading
  library(sp) # spatial library
  library(maps) # for map data
  library(ggplot2) # for plotting
})

###################### Prediction Grid - Longhurst ############################
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

# Remove misc:
rm(alsk,
   camr,
   ccal,
   chil,
   nppf,
   nptg,
   peqd,
   pnec,
   psae,
   spsg,
   oceans)

# Generate points ever 2.8 degrees -------------------------------------------
points <- spsample(regions, cellsize=c(2.8,2.8), type="regular")

# Plot pacific regions --------------------------------------------------------
# Associate data with the polygons each represents (the numbers all the way to the left originally):
regions@data$id <- rownames(regions@data)
region_points <- fortify(regions, region = "id") # make a data.frame from an SPDF sorted by each polygon (a total of 9 here)
longhurst_df <- merge(region_points, regions@data, by = "id") # merge all these into one long data.frame with coordinates and polygon information

# Associate data with points:
interp_points <- data.frame(LAT = points@coords[,2], LON = points@coords[,1])

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
  ggsave('figures/kriging/interpolation_region.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
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
  ggsave('figures/kriging/interpolation_prediction.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
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

###############################################################################
#                                  End Program
###############################################################################