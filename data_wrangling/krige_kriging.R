###############################################################################
#                                     Kriging 
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading excel
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

################################ Kriging: U238 ################################
# Read in data  ---------------------------------------------------------------
u238_data <- read_excel("data/output/excel/u238_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(th234_data) <- ~ Latitude + Longitude + Depth

# Specify data to be interpolated:
u238 <- u238_data$U_dpm_L

# Krige Type 1:
u238_total_kriged <- autoKrige(u238 ~ Latitude + Longitude + Depth,
                                u238_data,
                                #<NEEDS TO BE CREATED>,
                                block = 30,
                                model = c("Sph", "Exp", "Gau", "Ste"),
                                kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                                fix.values = c(NA,NA,NA),
                                remove_duplicates = TRUE,
                                verbose = FALSE,
                                GLS.model = NA)

# Krige Type 2:
u238_total_kriged <- krige(u238 ~ Latitude + Longitude + Depth,
                            u238_data,
                            #<NEEDS TO BE CREATED>,
                            model = u238.fit$var_model,
                            nmax = 30)

############################# Kriging: POC/234Th ##############################
# Read in data  ---------------------------------------------------------------
ratio_data <- read_excel("data/output/excel/ratio_data")

# Make SPDFs ------------------------------------------------------------------
coordinates(th234_data) <- ~ Latitude + Longitude + Depth

# Specify data to be interpolated:
ratio <- ratio_data$POC_Th_tot_umol_dpm

# Krige Type 1:
ratio_total_kriged <- autoKrige(ratio ~ Latitude + Longitude + Depth,
                                ratio_data,
                                #<NEEDS TO BE CREATED>,
                                block = 30,
                                model = c("Sph", "Exp", "Gau", "Ste"),
                                kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
                                fix.values = c(NA,NA,NA),
                                remove_duplicates = TRUE,
                                verbose = FALSE,
                                GLS.model = NA)

# Krige Type 2:
ratio_total_kriged <- krige(ratio ~ Latitude + Longitude + Depth,
                            ratio_data,
                            #<NEEDS TO BE CREATED>,
                            model = ratio.fit$var_model,
                            nmax = 30)

###############################################################################
#                                  End Program
###############################################################################