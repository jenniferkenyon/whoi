###############################################################################
#                            Krige: Prediction Grid
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
############################################################################### << make sure (x,y) is (lon, lat)
# Libraries -------------------------------------------------------------------
# Open libraries:
suppressPackageStartupMessages({
  library(readxl) # reading Excel data
  library(writexl) # writing Excel data
  library(rgdal) # for shape file reading
  library(sp) # spatial library
  library(maps) # for map data
  library(ggplot2) # for plotting
  library(maptools) # for map plotting
  library(gstat) # for geographical statistics,
  library(gstat) # for geographical statistics
  library(plotly) # 3D function
})

# Do you want to print? -------------------------------------------------------
printing = 1

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
 
############################## Generate Points ###############################
# Generate points ever 2.8 degrees -------------------------------------------
# Can be done with: 
points <- spsample(regions, cellsize=c(2.8,2.8), type="regular")

# WARNING: In proj4string(obj) : CRS object has comment, which is lost in output. Should not affect output. 

# Rename:
colnames(points@coords) <- c("Longitude","Latitude") 

# As (lon,lat), use GPS, the most common spatial reference system for the entire world:
interp_grid <- spTransform(points, CRS("+init=epsg:4326"))

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
   oceans,
   points)

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
x <- interp_grid@coords[,1]
y <- interp_grid@coords[,2]
z <- watercolumn
pacific <- matrix(ncol = 3, 
                  nrow = dim(interp_grid@coords)[1]*length(watercolumn))
k = 1
for (i in 1:dim(interp_grid@coords)[1]) {
  for (j in 1:length(watercolumn)) {
    pacific[k,1] <- x[i]
    pacific[k,2] <- y[i]
    pacific[k,3] <- watercolumn[j]
    k = k + 1
  }
}

# Make into a SPDF:
data = data.frame(ID = 1:dim(pacific)[1])
th234_pacific <- SpatialPoints(pacific)
prediction_grid <- SpatialPointsDataFrame(th234_pacific, data)

# Rename:
colnames(prediction_grid@coords) <- c("Longitude","Latitude","Depth") 

# Use GPS, world-wide reference projection:
proj4string(prediction_grid) <- CRS("+init=epsg:4326")

# Save to excel:
if (printing == 1) {
  write_xlsx(as.data.frame(prediction_grid), 
             "data/output/krige/prediction_grid.xlsx" 
  )
}

# Remove misc:
rm(i,j,k,
   watercolumn,
   x,y,z,
   th234_pacific,
   pacific,
   data)

########################### Plot Points and Regions ###########################
# Associate data with the polygons each represents (the numbers all the way to the left originally):
regions@data$id <- rownames(regions@data)
region_points <- fortify(regions, region = "id") # make a data.frame from an SPDF sorted by each polygon (a total of 9 here)
longhurst <- merge(region_points, regions@data, by = "id") # merge all these into one long data.frame with coordinates and polygon information

# Associate data with points:
interp_points <- data.frame(Latitude = interp_grid@coords[,2], Longitude = interp_grid@coords[,1])

# Shift to over the pacific:
interp_points$Longitude <- ifelse(interp_points$Longitude < -25, 
                                  interp_points$Longitude + 360, 
                                  interp_points$Longitude)
longhurst$long <- ifelse(longhurst$long < -25, 
                         longhurst$long + 360, 
                         longhurst$long)

# Make world data -------------------------------------------------------------
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 

# Make the plot:
ggRegions <- ggplot() +
  geom_polygon(data = world,
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_polygon(data = longhurst,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = 'Longhurst Regions')) +
  geom_point(data = interp_points,
             aes(x = Longitude,
                 y = Latitude,
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
        axis.title.y=element_text(size=10, face="bold")) +
  scale_x_continuous(breaks=seq(0,360,30),
                     labels=c(0,30,60,90,
                              120,150,180,
                              -150,-120,-90,
                              -60,-30,0))
print(ggRegions)

# Printing:
if (printing == 1) {
  ggsave('figures/kriging/interpolation_region.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

# Plot queuing vs data points -------------------------------------------------
# Load 234Th data:
th234_data <- read_excel("data/output/excel/th234_data.xlsx")
u238_data <- read_excel("data/output/excel/u238_data.xlsx")
ratio_data <- read_excel("data/output/excel/ratio_data.xlsx")

# Shift over the pacific:
th234_data$Longitude <- ifelse(th234_data$Longitude < -25, 
                               th234_data$Longitude + 360, 
                               th234_data$Longitude)
u238_data$Longitude <- ifelse(u238_data$Longitude < -25, 
                              u238_data$Longitude + 360, 
                              u238_data$Longitude)
ratio_data$Longitude <- ifelse(ratio_data$Longitude < -25, 
                               ratio_data$Longitude + 360, 
                               ratio_data$Longitude)

# Plot 234Th and Prediction Grid:
ggPoints <- ggplot() +
  geom_polygon(data = world,
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_point(data = interp_points,
             aes(x = Longitude,
                 y = Latitude,
                 color = 'Interpolation Points'),
             size = 0.75,
             stroke = 0,
             shape = 16) +
  geom_point(data = th234_data,
             aes(x = Longitude,
                 y = Latitude,
                 color = '234Th Data Points'),
             size = 1,
             stroke = 0,
             shape = 16) +
  coord_fixed(1.3) +
  labs(title='Prediction Grid and 234Th Data Points',
       x = "Longitude",
       y = "Latitude",
       color = "") +
  theme(plot.title=element_text(size=10, face="bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10, face="bold"),
        axis.title.y=element_text(size=10, face="bold"))
print(ggPoints)
if (printing == 1) {
  ggsave('figures/kriging/interpolation_prediction_234th.pdf',
         width = 7,
         height = 3,
         dpi = 300)
}

# Remove grid:
rm(ggPoints)

# Plot 238U and Prediction Grid:
ggPoints <- ggplot() +
  geom_polygon(data = world,
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_point(data = interp_points,
             aes(x = Longitude,
                 y = Latitude,
                 color = 'Interpolation Points'),
             size = 0.75,
             stroke = 0,
             shape = 16) +
  geom_point(data = u238_data,
             aes(x = Longitude,
                 y = Latitude,
                 color = '238U Data Points'),
             size = 1,
             stroke = 0,
             shape = 16) +
  coord_fixed(1.3) +
  labs(title='Prediction Grid and 238U Data Points',
       x = "Longitude",
       y = "Latitude",
       color = "") +
  theme(plot.title=element_text(size=10, face="bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10, face="bold"),
        axis.title.y=element_text(size=10, face="bold"))
print(ggPoints)
if (printing == 1) {
  ggsave('figures/kriging/interpolation_prediction_238u.pdf',
         width = 7,
         height = 3,
         dpi = 300)
}

# Remove grid:
rm(ggPoints)

# Plot Ratio and Prediction Grid:
ggPoints <- ggplot() +
  geom_polygon(data = world,
               aes(x = long,
                   y = lat,
                   group = group)) +
  geom_point(data = interp_points,
             aes(x = Longitude,
                 y = Latitude,
                 color = 'Interpolation Points'),
             size = 0.75,
             stroke = 0,
             shape = 16) +
  geom_point(data = ratio_data,
             aes(x = Longitude,
                 y = Latitude,
                 color = 'POC/234Th Data Points'),
             size = 1,
             stroke = 0,
             shape = 16) +
  coord_fixed(1.3) +
  labs(title='Prediction Grid and POC/234Th Data Points',
       x = "Longitude",
       y = "Latitude",
       color = "") +
  theme(plot.title=element_text(size=10, face="bold"),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=10, face="bold"),
        axis.title.y=element_text(size=10, face="bold"))
print(ggPoints)
if (printing == 1) {
  ggsave('figures/kriging/interpolation_prediction_ratio.pdf',
         width = 7,
         height = 3,
         dpi = 300)
}

# Plot Prediction Points  --------------------------------------------------------
prediction <- plot_ly(x=prediction_grid@coords[,2],
                      y=prediction_grid@coords[,1],
                      z=-prediction_grid@coords[,3],
                      type='scatter3d',
                      mode="markers",
                      color=prediction_grid@coords[,3])
prediction <- prediction %>% layout(title = 'Prediction Grid Points',
                                    xaxis = list(title = 'Longitude'),
                                    yaxis = list(title = 'Latitude'))
print(prediction)

# Remove misc:
rm(region_points,
   longhurst,
   printing)

###############################################################################
#                                  End Program
###############################################################################