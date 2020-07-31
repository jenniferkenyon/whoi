###############################################################################
#                               Plotting Data
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading Excel data
  library(sp) # creating spatial data
  library(ggplot2) # plotting data  
  library(maps) # gives map data
})

# Printing configuration ------------------------------------------------------
# Do you want to print? 1 means yes. 0 means no. 
printing = 1

# Import data -----------------------------------------------------------------
pacific_data <- read_excel("data/output/excel/pacific_data.xlsx")
th234_data <- read_excel("data/output/excel/th234_data.xlsx")
u238_data <- read_excel("data/output/excel/u238_data.xlsx")
ratio_data <- read_excel("data/output/excel/ratio_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(pacific_data) <- ~ Latitude + Longitude + Depth
coordinates(th234_data) <- ~ Latitude + Longitude + Depth
coordinates(u238_data) <- ~ Latitude + Longitude + Depth
coordinates(ratio_data) <- ~ Latitude + Longitude + Depth

############################## Plot all Stations ##############################
# Create points:
all_locations <- data.frame(Latitude = pacific_data@coords[,1], 
                            Longitude = pacific_data@coords[,2])

# Shift to be over the Pacific:
all_locations$Longitude <- ifelse(all_locations$Longitude < -25, 
                                  all_locations$Longitude + 360, 
                                  all_locations$Longitude)

# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = all_locations, 
             aes(x = Longitude,
                 y = Latitude, 
                 color = 'Total 234Th Stations'),
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='Global 234Th Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "234Th Station Types") + 
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
print(ggLocations)

# Remove misc:
rm(ggLocations, 
   all_locations)

if (printing == 1) {
  ggsave('figures/th234_tot/all_234th_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

######################## Plot Usable 234Th Stations ###########################
# Create points:
usable_locations <- data.frame(Latitude = th234_data@coords[,1], 
                               Longitude = th234_data@coords[,2])

# Shift to be over the Pacific:
usable_locations$Longitude <- ifelse(usable_locations$Longitude < -25, 
                                     usable_locations$Longitude + 360, 
                                     usable_locations$Longitude)

# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = usable_locations, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable 234Th Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='Usable 234Th Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "234Th Station Types") + 
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
print(ggLocations)

# Remove misc:
rm(ggLocations, 
   usable_locations)

if (printing == 1) {
  ggsave('figures/th234_tot/usable_th234_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

############### Plot all locations vs usable 234Th Stations ###################
# Create points:
all_locations <- data.frame(Latitude = pacific_data@coords[,1], 
                            Longitude = pacific_data@coords[,2])
usable_locations <- data.frame(Latitude = th234_data@coords[,1], 
                               Longitude = th234_data@coords[,2])

# Shift to be over the Pacific:
all_locations$Longitude <- ifelse(all_locations$Longitude < -25, 
                                  all_locations$Longitude + 360, 
                                  all_locations$Longitude)
usable_locations$Longitude <- ifelse(usable_locations$Longitude < -25, 
                                     usable_locations$Longitude + 360, 
                                     usable_locations$Longitude)

# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
               geom_polygon(data = world, 
                            aes(x = long, 
                                y = lat, 
                                group = group)) + 
               coord_fixed(1.3) + 
               geom_point(data = all_locations, 
                          aes(x = Longitude,
                              y = Latitude, 
                              color = 'Total 234Th Stations'),
                         size = 0.75, 
                         stroke = 0, 
                         shape = 16) + 
               geom_point(data = usable_locations, 
                          aes(x = Longitude, 
                              y = Latitude, 
                              color = 'Usable 234Th Stations'), 
                          size = 0.75, 
                          stroke = 0, 
                          shape = 16) + 
               labs(title='All 234Th Stations vs Usable Stations', 
                    x = "Longitude", 
                    y = "Latitude", 
                    color = "234Th Station Types") + 
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
print(ggLocations)

# Remove misc:
rm(ggLocations, 
   all_locations, 
   usable_locations)

if (printing == 1) {
  ggsave('figures/th234_tot/comparison_234th_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}


############################## Plot 238U Stations #############################
# Create points:
usable_locations <- data.frame(Latitude = u238_data@coords[,1], 
                               Longitude = u238_data@coords[,2])

# Shift to be over the Pacific:
usable_locations$Longitude <- ifelse(usable_locations$Longitude < -25, 
                                     usable_locations$Longitude + 360, 
                                     usable_locations$Longitude)


# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = usable_locations, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable 238U Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='Usable 238U Stations Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "238U Station Types") + 
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
print(ggLocations)


# Remove misc:
rm(ggLocations, 
   usable_locations)

if (printing == 1) {
  ggsave('figures/u238/usable_238U_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

##################### Plot Usable and All 238U Stations #######################
# Create points:
all_locations <- data.frame(Latitude = pacific_data@coords[,1], 
                            Longitude = pacific_data@coords[,2])
usable_locations <- data.frame(Latitude = u238_data@coords[,1], 
                               Longitude = u238_data@coords[,2])

# Shift to be over the Pacific:
all_locations$Longitude <- ifelse(all_locations$Longitude < -25, 
                                  all_locations$Longitude + 360, 
                                  all_locations$Longitude)
usable_locations$Longitude <- ifelse(usable_locations$Longitude < -25, 
                                     usable_locations$Longitude + 360, 
                                     usable_locations$Longitude)


# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = all_locations, 
             aes(x = Longitude,
                 y = Latitude, 
                 color = 'Total 238U Stations'),
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  geom_point(data = usable_locations, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable 238U Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='All 238U Stations vs Usable Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "238U Station Types") + 
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
print(ggLocations)


# Remove misc:
rm(ggLocations, 
   all_locations)

if (printing == 1) {
  ggsave('figures/u238/comparison_238U_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

############################## Plot Ratio Stations ############################
# Create points:
all_locations <- data.frame(Latitude = pacific_data@coords[,1], 
                            Longitude = pacific_data@coords[,2])


# Shift to be over the Pacific:
all_locations$Longitude <- ifelse(all_locations$Longitude < -25, 
                                  all_locations$Longitude + 360, 
                                  all_locations$Longitude)

# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = usable_locations, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable POC/234Th Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='Usable POC/234Th Stations Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "POC/234Th Station Types") + 
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
print(ggLocations)


# Remove misc:
rm(ggLocations, 
   usable_locations)

if (printing == 1) {
  ggsave('figures/ratio/usable_POC_234Th_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

################## Plot Usable and All POC/234Th Stations #####################
# Create points:
all_locations <- data.frame(Latitude = pacific_data@coords[,1], 
                            Longitude = pacific_data@coords[,2])
usable_locations <- data.frame(Latitude = ratio_data@coords[,1], 
                               Longitude = ratio_data@coords[,2])

# Shift to be over the Pacific:
all_locations$Longitude <- ifelse(all_locations$Longitude < -25, 
                                  all_locations$Longitude + 360, 
                                  all_locations$Longitude)
usable_locations$Longitude <- ifelse(usable_locations$Longitude < -25, 
                                     usable_locations$Longitude + 360, 
                                     usable_locations$Longitude)


# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = all_locations, 
             aes(x = Longitude,
                 y = Latitude, 
                 color = 'Total POC/234Th Stations'),
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  geom_point(data = usable_locations, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable POC/234Th Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='All POC/234Th Stations vs Usable Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "POC/234Th Station Types") + 
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
print(ggLocations)


# Remove misc:
rm(ggLocations, 
   usable_locations,
   all_locations)

if (printing == 1) {
  ggsave('figures/ratio/comparison_POC_234Th_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

#################### 234Th, 238U, and POC/234Th Stations ######################
# Create points:
usable_locations_th <- data.frame(Latitude = th234_data@coords[,1], 
                                  Longitude = th234_data@coords[,2])
usable_locations_u <- data.frame(Latitude = u238_data@coords[,1], 
                                 Longitude = u238_data@coords[,2])
usable_locations_ratio <- data.frame(Latitude = ratio_data@coords[,1], 
                                     Longitude = ratio_data@coords[,2])

# Shift to be over the Pacific:
usable_locations_th$Longitude <- ifelse(usable_locations_th$Longitude < -25, 
                                        usable_locations_th$Longitude + 360, 
                                        usable_locations_th$Longitude)
usable_locations_u$Longitude <- ifelse(usable_locations_u$Longitude < -25, 
                                       usable_locations_u$Longitude + 360, 
                                       usable_locations_u$Longitude)
usable_locations_ratio$Longitude <- ifelse(usable_locations_ratio$Longitude < -25, 
                                           usable_locations_ratio$Longitude + 360, 
                                           usable_locations_ratio$Longitude)

# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = usable_locations_th, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable 234Th Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  geom_point(data = usable_locations_u, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable 238U Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  geom_point(data = usable_locations_ratio, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable POC/234Th Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='All 234Th, 238U, and POC/234Th Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "Station Types") + 
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
print(ggLocations)

# Remove misc:
rm(ggLocations, 
   usable_locations_th,
   usable_locations_u,
   usable_locations_ratio)

if (printing == 1) {
  ggsave('figures/th234_tot/all_types_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

############################### All Stations ##################################
# Create points:
usable_locations_th <- data.frame(Latitude = th234_data@coords[,1], 
                                  Longitude = th234_data@coords[,2])
usable_locations_u <- data.frame(Latitude = u238_data@coords[,1], 
                                 Longitude = u238_data@coords[,2])
usable_locations_ratio <- data.frame(Latitude = ratio_data@coords[,1], 
                                     Longitude = ratio_data@coords[,2])
all_locations <- data.frame(Latitude = pacific_data@coords[,1], 
                            Longitude = pacific_data@coords[,2])

# Shift to be over the Pacific:
usable_locations_th$Longitude <- ifelse(usable_locations_th$Longitude < -25, 
                                        usable_locations_th$Longitude + 360, 
                                        usable_locations_th$Longitude)
usable_locations_u$Longitude <- ifelse(usable_locations_u$Longitude < -25, 
                                       usable_locations_u$Longitude + 360, 
                                       usable_locations_u$Longitude)
usable_locations_ratio$Longitude <- ifelse(usable_locations_ratio$Longitude < -25, 
                                           usable_locations_ratio$Longitude + 360, 
                                           usable_locations_ratio$Longitude)
all_locations$Longitude <- ifelse(all_locations$Longitude < -25, 
                                  all_locations$Longitude + 360, 
                                  all_locations$Longitude)

# Plot:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 
ggLocations <- ggplot() + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  geom_point(data = all_locations, 
             aes(x = Longitude,
                 y = Latitude, 
                 color = 'Total Stations'),
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  geom_point(data = usable_locations_th, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable 234Th Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  geom_point(data = usable_locations_u, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable 238U Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  geom_point(data = usable_locations_ratio, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = 'Usable POC/234Th Stations'), 
             size = 0.75, 
             stroke = 0, 
             shape = 16) + 
  labs(title='All Stations', 
       x = "Longitude", 
       y = "Latitude", 
       color = "Station Types") + 
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
print(ggLocations)

if (printing == 1) {
  ggsave('figures/th234_tot/comparison_all_types_stations.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

# Remove misc:
rm(ggLocations, 
   printing,
   world,
   all_locations,
   usable_locations_th,
   usable_locations_u,
   usable_locations_ratio)

###############################################################################
#                                  End Program
###############################################################################