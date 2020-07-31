###############################################################################
#                                     Kriging 
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading excel
  library(writexl) # writing Excel data
  library(automap) # autokriging
  library(gstat) # functionality for kriging
  library(ggplot2) # for plotting
  library(maps) # for using maps in plots
})

# Do you want to plot? --------------------------------------------------------
printing = 1

################################ Kriging: 234Th ###############################
# Read in data  ---------------------------------------------------------------
th234_data <- read_excel("data/output/excel/th234_data.xlsx")
prediction_grid <- read_excel("data/output/krige/prediction_grid.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(th234_data) <- ~ Longitude + Latitude + Depth
coordinates(prediction_grid) <- ~ Longitude + Latitude + Depth

# Specify data to be interpolated:
th234 <- th234_data$Th_total_dpm_L

# Variogram:
th234.vgm <- variogram(th234 ~ 1, # same result as with ~ Longitude + Latitude + Depth
                       data = th234_data,
                       alpha = 90)

# Fit:
th234.fit <- fit.variogram(th234.vgm, 
                           model = vgm(c("Sph",
                                         "Exp",
                                         "Gau",
                                         "Ste",
                                         "Mat"
                           )
                           )
)

# Krige:
th234_kriged <- krige(th234 ~ 1,
                      th234_data,
                      prediction_grid,
                      model = th234.fit,
                      nmax = 1)

# Save:
if (printing == 1) {
  write_xlsx(as.data.frame(th234_kriged), 
             "data/output/krige/th234_kriged_2.8.xlsx" 
  )
}

# Plot ------------------------------------------------------------------------
# Set to 10 meters:
depth = 10
th234_kriged_plot <- na.omit(as.data.frame(th234_kriged[which(th234_kriged@coords[,3] == depth),]))
colnames(th234_kriged_plot) = c("Longitude", "Latitude", "Depth", "Th234", "Th234_1")

# Center over Pacific:
th234_kriged_plot$Longitude <- ifelse(th234_kriged_plot$Longitude < -25, 
                                      th234_kriged_plot$Longitude + 360, 
                                      th234_kriged_plot$Longitude)

# Get world data:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 

# Plot kriged data at 10m:
ggth234 <- ggplot(th234_kriged_plot,
                  aes(x = Longitude, 
                      y = Latitude)) + 
           geom_tile( aes(fill = Th234)) + 
           geom_polygon(data = world, 
                        aes(x = long, 
                            y = lat, 
                            group = group)) + 
           coord_fixed(1.3) + 
           scale_fill_gradient(low = "blue", high="red") +
           labs(title='Global 234Th Stations', 
                x = "Longitude", 
                y = "Latitude") + 
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
print(ggth234)

if (printing == 1) {
  ggsave('figures/th234_tot/234th_kriged.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

# Remove misc:
rm(ggth234,
   prediction_grid,
   th234_data,
   th234_kriged,
   th234_kriged_plot,
   th234.fit,
   th234.vgm,
   depth,
   th234,
   world)

################################ Kriging: U238 ################################
# Read in data  ---------------------------------------------------------------
u238_data <- read_excel("data/output/excel/u238_data.xlsx")
prediction_grid <- read_excel("data/output/krige/prediction_grid.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(u238_data) <- ~ Longitude + Latitude + Depth
coordinates(prediction_grid) <- ~ Longitude + Latitude + Depth

# Specify data to be interpolated:
u238 <- u238_data$U_dpm_L

# Variogram:
u238.vgm <- variogram(u238 ~ 1, # same result as with ~ Longitude + Latitude + Depth
                      data = u238_data,
                      alpha = 120)

# Fit:
u238.fit <- fit.variogram(u238.vgm, 
                          model = vgm(c("Sph",
                                        "Exp",
                                        "Gau",
                                        "Ste",
                                        "Mat"),
                                      anis = c(90, 340, 0, 0.5, 1))
)

# Krige:
u238_kriged <- krige(u238 ~ 1,
                     u238_data,
                     prediction_grid,
                     model = u238.fit,
                     nmax = 1)

# Save:
if (printing == 1) {
  write_xlsx(as.data.frame(u238_kriged), 
             "data/output/krige/u238_kriged_2.8.xlsx" 
  )
}

# Plot ------------------------------------------------------------------------
# Set to 10 meters:
depth = 10
u238_kriged_plot <- na.omit(as.data.frame(u238_kriged[which(u238_kriged@coords[,3] == depth),]))
colnames(u238_kriged_plot) = c("Longitude", "Latitude", "Depth", "U238", "U238_1")

# Center over Pacific:
u238_kriged_plot$Longitude <- ifelse(u238_kriged_plot$Longitude < -25, 
                                     u238_kriged_plot$Longitude + 360, 
                                     u238_kriged_plot$Longitude)

# Get world data:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 

# Plot kriged data at 10m:
ggu238 <- ggplot(u238_kriged_plot,
                  aes(x = Longitude, 
                      y = Latitude)) + 
  geom_tile(aes(fill = U238)) + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(low = "blue", high="red") +
  labs(title='Global 238U Stations', 
       x = "Longitude", 
       y = "Latitude") + 
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
print(ggu238)

if (printing == 1) {
  ggsave('figures/u238/238u_kriged.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

# Remove misc:
rm(ggu238,
   prediction_grid,
   u238_data,
   u238_kriged,
   u238_kriged_plot,
   u238.fit,
   u238.vgm,
   depth,
   u238,
   world)

############################# Kriging: POC/234Th ##############################
# Read in data  ---------------------------------------------------------------
ratio_data <- read_excel("data/output/excel/ratio_data.xlsx")
prediction_grid <- read_excel("data/output/krige/prediction_grid.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(ratio_data) <- ~ Longitude + Latitude + Depth
coordinates(prediction_grid) <- ~ Longitude + Latitude + Depth

# Specify data to be interpolated:
ratio <- ratio_data$POC_Th_tot_umol_dpm

ratio.vgm <- variogram(ratio  ~ Longitude + Latitude + Depth, # same result as with ~ Longitude + Latitude + Depth
                       data = ratio_data,
                       alpha = 100)

# Fit:
ratio.fit <- fit.variogram(ratio.vgm, 
                           model = vgm(c("Sph",
                                         "Exp",
                                         "Gau",
                                         "Ste",
                                         "Mat"),
                                       anis = c(100, 350, 0, 0.5, 1))
)

# Krige:
ratio_kriged <- krige(ratio ~ 1,
                      ratio_data,
                      prediction_grid,
                      model = ratio.fit,
                      nmax = 1)

# Save:
if (printing == 1) {
  write_xlsx(as.data.frame(ratio_kriged), 
             "data/output/krige/ratio_kriged_2.8.xlsx" 
  )
}

# Plot ------------------------------------------------------------------------
# Set to 10 meters:
depth = 10
ratio_kriged_plot <- na.omit(as.data.frame(ratio_kriged[which(ratio_kriged@coords[,3] == depth),]))
colnames(ratio_kriged_plot) = c("Longitude", "Latitude", "Depth", "POC_234Th_Ratio", "POC_234Th_Ratio_1")

# Center over Pacific:
ratio_kriged_plot$Longitude <- ifelse(ratio_kriged_plot$Longitude < -25, 
                                      ratio_kriged_plot$Longitude + 360, 
                                      ratio_kriged_plot$Longitude)

# Get world data:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 

# Plot kriged data at 10m:
ggratio <- ggplot(ratio_kriged_plot,
                 aes(x = Longitude, 
                     y = Latitude)) + 
  geom_tile(aes(fill = POC_234Th_Ratio)) + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(low = "blue", high="red") +
  labs(title='Global POC/234Th Stations', 
       x = "Longitude", 
       y = "Latitude") + 
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
print(ggratio)

if (printing == 1) {
  ggsave('figures/ratio/ratio_kriged.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

# Remove misc:
rm(ggratio,
   prediction_grid,
   ratio_data,
   ratio_kriged,
   ratio_kriged_plot,
   ratio.fit,
   ratio.vgm,
   depth,
   ratio,
   world,
   printing)

###############################################################################
#                                  End Program
###############################################################################