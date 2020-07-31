###############################################################################
#                            Kriging U-238 Data
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
plotting = 1

# Data Wrangling --------------------------------------------------------------
# Turn off scientific notation and set digit and print max:
options(scipen = 999)
options(digits=22)
options(max.print=999999)

# Import all data from Excel:
th234_data_ceballos <- read_excel("data/th234_data_220720_edited.xlsx", 
                                  sheet = "metadata&data", na = "") # Cabellos 
# WARNING: There were 50 or more warnings (use warnings() to see the first 50)                                                                  
th234_data_exports <- read_excel("data/Buesseler_Elementa_Tables v6.xlsx", 
                                 sheet = "Table S1") # Buesseler

# Import only Pacific Ocean data:
th234_pacific <- th234_data_ceballos[which(th234_data_ceballos$Ocean == "Pacific Ocean"),]
th234_pacific <- rbind(th234_pacific, 
                       th234_data_ceballos[which(th234_data_ceballos$Ocean == "Pacifc Ocean"),])
th234_pacific <- rbind(th234_pacific, 
                       th234_data_ceballos[which(th234_data_ceballos$Ocean == "Pacific Ocean & Artic Ocean")])

# Make U234 data --------------------------------------------------------------
unknown <- which(is.na(th234_pacific$`Total_234Th_dpm/L`) == TRUE)
unknown_before <- length(unknown)
for (i in 1:dim(th234_pacific)[1]) { # across all data in dataset
  if (is.na(th234_pacific$`238U_dpm/L`[i]) == TRUE) { # if uranium data doesnt exist
    if (is.na(th234_pacific$Salinity[i]) == FALSE) { # and salinity data exists
      th234_pacific$`238U_dpm/L`[i] = 0.0786*th234_pacific$Salinity[i] - 0.315 # make uranium from salinity
    }
  }
}
unknown <- which(is.na(th234_pacific$`Total_234Th_dpm/L`) == TRUE)
unknown_after <- length(unknown)
change_th234 <- unknown_before - unknown_after

# Remove NA from data ---------------------------------------------------------
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lat_decimal_degrees)), ] # latitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$lon_decimal_degrees)), ] # longitude
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$depth_m)), ] # depth of each measurement
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$`238U(dpm/L)`)), ] # total U238



