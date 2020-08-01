###############################################################################
#                                     Kriging 
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading excel
  library(ggplot2) # plotting
  library(maps) # world maps
})

# Do you want to plot? --------------------------------------------------------
printing = 1

############################## Plotting POC Fluxes ############################
# Read in data  ---------------------------------------------------------------
poc_flux <- read_excel("output/flux_100m.xlsx")
colnames(poc_flux) <- c("Longitude", "Latitude", "Depth", "POC_flux_mgC_m2_d")

# Plot ------------------------------------------------------------------------
# Center over Pacific:
poc_flux$Longitude <- ifelse(poc_flux$Longitude < -25, 
                             poc_flux$Longitude + 360, 
                             poc_flux$Longitude)

# Get world data:
world <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) 

# Plot kriged data at 10m:
ggPOC_flux <- ggplot(poc_flux,
                     aes(x = Longitude, 
                         y = Latitude)) + 
  geom_tile(aes(fill = POC_flux_mgC_m2_d)) + 
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group)) + 
  coord_fixed(1.3) + 
  scale_fill_gradient(low = "blue", high="red") +
  labs(title='Pacific POC Fluxs at 100m', 
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
print(ggPOC_flux)

if (printing == 1) {
  ggsave('figures/poc_flux_100m.pdf', 
         width = 7, 
         height = 3, 
         dpi = 300)
}

# Remove misc:
rm(ggPOC_flux,
   printing,
   world)

###############################################################################
#                                  End Program
###############################################################################