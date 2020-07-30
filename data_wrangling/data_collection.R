###############################################################################
#                        Data Import and Quality Control
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading Excel data
  library(writexl) # writing Excel data
  library(sp) # creating spatial data
  library(rgdal) # writing shapefiles 
})

############################### EXPORTS DATA ##################################
# Import data -----------------------------------------------------------------
# Read in from excel:
import <- read_excel("data/input/exports_v6.xlsx", # path
                     sheet = "Table S1", # sheet
                     na = "") # NA type

# Turn into a data_frame:
exports <- as.data.frame(import)

# Remove import:
rm(import)

# Give serial numbers ---------------------------------------------------------
# Find end of array:
end <- dim(exports)[1]

# Give serial number of form X_Y: X = data set, Y = cell within data set:
# 1 = EXPORTS data
# 2 = GP15 RR1814 data
# 3 = GP15 RR1815 data 
# 4 = CGODB (Comprehensive Global Oceanic Database of 234Th Data) data

j = 1
i = 1
for (i in 1:end) {
  exports$`SR#`[i] <- paste0(j, # data base
                                   "_",
                                   i) # cell number
}

# Remove misc:
rm(end, i, j)

# Collect data into one array -------------------------------------------------
pacific_1 <- data.frame(Serial_Number = exports$`SR#`, # serial number from above
                        Latitude = exports$Latitude, # latitude
                        Longitude = exports$Longitude, # longitude
                        Depth = exports$`Bottle Depth`, # depth of measurement
                        U_dpm_L = exports$`238U_dpm/L`, # 238uranium measurements
                        Th_tot_dpm_L = exports$`234Th_dpm/L`, # 234thorium measurements
                        Th_tot_1SD_dpm_L =  exports$`234Th_ obs_uncert`, # 234thorium uncertainty measurements
                        POC_Th_tot_umol_dpm = exports$`POC/234Th_μmol/dpm`, # POC/234Th measurements
                        POC_Th_tot_1SD_umol_dpm =  exports$`st dev` # POC/234Th uncertainty measurements
)

############################## GP15 DATA RR1814 ###############################
# Import data -----------------------------------------------------------------
# Read in from excel:
import <- read_excel("data/input/gp15_RR1814.xlsx", # path
                     na = "") # NA type

# Turn into a data_frame:
gp15_RR1814 <- as.data.frame(import)

# Remove import:
rm(import)

# Give serial numbers ---------------------------------------------------------
# Find end of array:
end <- dim(gp15_RR1814)[1]

# Give serial number of form X_Y: X = data set, Y = cell within data set:
j = 2
i = 1
for (i in 1:end) {
  gp15_RR1814$`SR#`[i] <- paste0(j, # data base
                                "_",
                                i) # cell number
}

# Remove end:
rm(end, i, j)

# Collect data into one array -------------------------------------------------
convert <- 0.06 # for converting mBq/kg to dpm/L
pacific_2 <- data.frame(Serial_Number = gp15_RR1814$`SR#`, # serial number from above
                        Latitude = gp15_RR1814$`Start Latitude`, # latitude
                        Longitude = gp15_RR1814$`Start Longitude`, # longitude
                        Depth = gp15_RR1814$Depth_m, # depth of measurement
                        U_dpm_L = '', # 234uranium measurements
                        Th_tot_dpm_L = convert*gp15_RR1814$`Th_234_T_CONC_BOTTLE_mBq/kg`, # 234thorium measurements
                        Th_tot_1SD_dpm_L =  convert*gp15_RR1814$`1SD_Th_234_T_CONC_BOTTLE_mBq/kg`, # 234thorium uncertainty measurements
                        POC_Th_tot_umol_dpm = '', # POC/234Th measurements
                        POC_Th_tot_1SD_umol_dpm = ''  # POC/234Th uncertainty measurements
)

############################## GP15 DATA RR1815 ###############################
# Import data -----------------------------------------------------------------
# Read in from excel:
import <- read_excel("data/input/gp15_RR1815.xlsx", # path
                     na = "") # NA type

# Turn into a data_frame:
gp15_RR1815 <- as.data.frame(import)

# Remove import:
rm(import)

# Give serial numbers ---------------------------------------------------------
# Find end of array:
end <- dim(gp15_RR1815)[1]

# Give serial number of form X_Y: X = data set, Y = cell within data set:
j = 3
i = 1
for (i in 1:end) {
  gp15_RR1815$`SR#`[i] <- paste0(j, # data base
                                       "_",
                                       i) # cell number
}

# Remove end:
rm(end, i, j)

# Collect data into one array -------------------------------------------------
pacific_3 <- data.frame(Serial_Number = gp15_RR1815$`SR#`, # serial number from above
                        Latitude = gp15_RR1815$`Start Latitude`, # latitude
                        Longitude = gp15_RR1815$`Start Longitude`, # longitude
                        Depth = gp15_RR1815$Depth_m, # depth of measurement
                        U_dpm_L = '', # 234uranium measurements
                        Th_tot_dpm_L = convert*gp15_RR1815$`Th_234_T_CONC_BOTTLE_mBq/kg`, # 234thorium measurements
                        Th_tot_1SD_dpm_L =  convert*gp15_RR1815$`1SD_Th_234_T_CONC_BOTTLE_mBq/kg`, # 234thorium uncertainty measurements
                        POC_Th_tot_umol_dpm = '', # POC/234Th measurements
                        POC_Th_tot_1SD_umol_dpm = ''  # POC/234Th uncertainty measurements
)

# Remove misc:
rm(convert)

################################ CEBALLOS DATA ################################
# Import data -----------------------------------------------------------------
# Read in from excel:
import <- read_excel("data/input/cgodb_220720.xlsx", # path
                     sheet = "metadata&data", # sheet
                     na = "nd") # NA type

# WARNING: There were 50 or more warnings (use warnings() to see the first 50). This is all coersion. 

# Turn into a data_frame:
cgodb <- as.data.frame(import)

# Remove import:
rm(import)

# Give serial numbers ---------------------------------------------------------
# Find end of array:
end <- dim(cgodb)[1]

# Give serial number of form X_Y: X = data set, Y = cell within data set:
j = 4
i = 1
for (i in 1:end) {
  cgodb$`SR#`[i] <- paste0(j, # data base
                                       "_",
                                       i) # cell number
}

# Remove end:
rm(end, i, j)

# Import only Pacific Ocean data ----------------------------------------------
cgodb <- cgodb[which(cgodb$Ocean == "Pacific Ocean"),]
cgodb <- rbind(cgodb, 
               cgodb[which(cgodb$Ocean == "Pacifc Ocean"),])
cgodb <- rbind(cgodb, 
               cgodb[which(cgodb$Ocean == "Pacific Ocean & Arctic Ocean")])
cgodb <- rbind(cgodb, 
               cgodb[which(cgodb$Ocean == "Atlantic Ocean & Pacific Ocean")])

# Exclude Chinese and Japanese Seas -------------------------------------------
cgodb <- cgodb[-which(cgodb$Region == "South China Sea"), ]
cgodb <- cgodb[-which(cgodb$Region == "Funka Bay"), ]
cgodb <- cgodb[-which(cgodb$Region == "Funka Bay of Hokkaido, Japan"), ]
cgodb <- cgodb[-which(cgodb$Region == "Southern South China Sea"), ]
cgodb <- cgodb[-which(cgodb$Region == "oligotrophic northern South China Sea"), ]
cgodb <- cgodb[-which(cgodb$Region == "main steam of the Kuroshio Current"), ]
cgodb <- cgodb[-which(cgodb$Region == "southern South China Sea"), ]
cgodb <- cgodb[-which(cgodb$Project_Name == "JGOFS-South East Asia Time-Series Station (SEATS)"), ]
cgodb <- cgodb[-which(cgodb$Region == "Ulleung Basin of the East/Japan Sea"), ]
cgodb <- cgodb[-which(cgodb$Region == "Western North Pacific"), ]
cgodb <- cgodb[-which(cgodb$Cruise_ID_1 == "MR05-04"), ]
cgodb <- cgodb[-which(cgodb$Project_Name == "Carbon Cycling in the China Seas: Budget, Controls and Ocean Acidification (CHOICE-C)"), ]
cgodb <- cgodb[-which(cgodb$Region ==  "Central North Pacific: Manzanillo (Mexico) and Hawaii"), ]

# Make 234Th_total data from 234Th_diss + 234Th_part_small + 234Th_part_large:
unknown <- which(is.na(cgodb$`total_234Th(dpm/L)`) == TRUE)
unknown_before <- length(unknown)
i = 1
for (i in 1:dim(cgodb)[1]) { # loop over all data in data set
  if (is.na(cgodb$`total_234Th(dpm/L)`[i]) == TRUE) { # if total thorium doesn't exist
    if (is.na(cgodb$`part_234Th_small(dpm/L)`[i]) == FALSE) { # and small particulate thorium exists
      if (is.na(cgodb$`part_234Th_large(dpm/L)`[i]) == FALSE) { # and large particulate thorium exists
        if (is.na(cgodb$`diss_234Th(dpm/L)`[i]) == FALSE) { # and dissolved thorium exists
          cgodb$`total_234Th(dpm/L)`[i] <- cgodb$`part_234Th_small(dpm/L)`[i] + # add together all thorium
                                           cgodb$`part_234Th_large(dpm/L)`[i] +
                                           cgodb$`diss_234Th(dpm/L)`[i]
          if (is.na(cgodb$uncert_total234Th [i]) == TRUE) { # if total thorium doesn't exist
            if (is.na(cgodb$uncert_part234Th_small[i]) == FALSE) { # and uncertainty in small particulate thorium exists
              if (is.na(cgodb$uncert_part234Th_small[i]) == FALSE) { # and uncertainty in large particulate thorium exists
                if (is.na(cgodb$uncert_diss234Th[i]) == FALSE) { # and uncertainty in dissolved thorium exists
                  cgodb$uncert_total234Th[i] <- cgodb$uncert_part234Th_small[i] + # add together uncertainties
                                                cgodb$uncert_part234Th_large[i] +
                                                cgodb$uncert_diss234Th[i]
                }
              }
            }
          }
        }
      }
    }
  }
}
unknown <- which(is.na(cgodb$`total_234Th(dpm/L)`) == TRUE)
unknown_after <- length(unknown)
change_234th <- unknown_before - unknown_after

# Remove unnecessary variables:
rm(unknown, unknown_before, unknown_after, i, change_234th)

# Make 238U data from salinity ------------------------------------------------
unknown <- which(is.na(cgodb$`238U(dpm/L)`) == TRUE)
unknown_before <- length(unknown)
i = 1
for (i in 1:dim(cgodb)[1]) { # across all data in data set
  if (is.na(cgodb$`238U(dpm/L)`[i]) == TRUE) { # if uranium data doesn't exist
    if (is.na(cgodb$salinity[i]) == FALSE) { # and salinity data exists
      cgodb$`238U(dpm/L)`[i] = 0.0786*cgodb$salinity[i] - 0.315 # make uranium from salinity
    }
  }
}
unknown <- which(is.na(cgodb$`238U(dpm/L)`) == TRUE)
unknown_after <- length(unknown)
change_238u <- unknown_before - unknown_after

# Remove unnecessary variables:
rm(unknown, unknown_before, unknown_after, i, change_238u)

# Collect data into one array -------------------------------------------------
pacific_4 <- data.frame(Serial_Number = cgodb$`SR#`, # serial number from above
                        Latitude = cgodb$lat_decimal_degrees, # latitude
                        Longitude = cgodb$lon_decimal_degrees, # longitude
                        Depth = cgodb$`depth(m)`, # depth of measurement
                        U_dpm_L = cgodb$`238U(dpm/L)`, # 238uranium measurements
                        Th_tot_dpm_L = cgodb$`total_234Th(dpm/L)` , # 234thorium measurements
                        Th_tot_1SD_dpm_L =  cgodb$uncert_total234Th , # 234thorium uncertainty measurements
                        POC_Th_tot_umol_dpm = cgodb$`POC/Th_small(umol/dpm)` + 
                                              cgodb$`POC/Th_large(umol/dpm)`, # POC/234Th measurements
                        POC_Th_tot_1SD_umol_dpm = cgodb$`uncert_POC/Th_small` + 
                                                  cgodb$`uncert_POC/Th_large` # POC/234Th uncertainty measurements
)

############################# Compiling all data ##############################
# Bind together:
pacific_data <- rbind(pacific_1,
                      pacific_2, 
                      pacific_3, 
                      pacific_4
)

# Remove misc:
rm(pacific_1, pacific_2, pacific_3, pacific_4)

############################ Data Quality Control #############################
# Remove NAs from coordninate data:
pacific_data <- pacific_data[-which(is.na(pacific_data$Latitude)), ] # latitude
pacific_data <- pacific_data[-which(is.na(pacific_data$Longitude)), ] # longitude
pacific_data <- pacific_data[-which(is.na(pacific_data$Depth)), ] # depth of each measurement

# Turn into a SPDF:
coordinates(pacific_data) = ~ Latitude + Longitude + Depth

########################### Collect Specific Data #############################
# 234Th -----------------------------------------------------------------------
th234 <- pacific_data[-which(is.na(pacific_data$Th_tot_dpm_L)), ] # total 234Th
if (length(which(pacific_data$Th_tot_dpm_L > 10)) > 0 ) {
  pacific_data[-which(pacific_data$Th_tot_dpm_L > 10), ]
}
latitude <- th234$Latitude
longitude <- th234$Longitude
depth <- th234$Depth
th234_data <- data.frame(Latitude = latitude, 
                         Longitude = longitude, 
                         Depth = depth, 
                         Th_total_dpm_L = th234$Th_tot_dpm_L
)

# Turn into a SPDF:
coordinates(th234_data) = ~ Latitude + Longitude + Depth

# Remove misc:
rm(latitude, longitude, depth, th234)

# 238U ------------------------------------------------------------------------
u238 <- pacific_data[-which(is.na(pacific_data$U_dpm_L)), ] # 238U
latitude <- u238$Latitude
longitude <- u238$Longitude
depth <- u238$Depth
u238_data <- data.frame(Latitude = latitude, 
                        Longitude = longitude, 
                        Depth = depth, 
                        U_dpm_L = u238$U_dpm_L
)

# Turn into a SPDF:
coordinates(u238_data) = ~ Latitude + Longitude + Depth

# Remove misc:
rm(latitude, longitude, depth, u238)

# POC/234Th -------------------------------------------------------------------
ratio <- pacific_data[-which(is.na(pacific_data$POC_Th_tot_umol_dpm)), ] # POC/234Th ratio
latitude <- ratio$Latitude
longitude <- ratio$Longitude
depth <- ratio$Depth
ratio_data <- data.frame(Latitude = latitude, 
                         Longitude = longitude, 
                         Depth = depth, 
                         POC_Th_tot_umol_dpm = ratio$POC_Th_tot_umol_dpm
)

# Turn into a SPDF:
coordinates(ratio_data) = ~ Latitude + Longitude + Depth

# Remove misc:
rm(latitude, longitude, depth, ratio)

############################## Writing all data ###############################
# Write all data to .xlsx files:
write_xlsx(cgodb, 
           "data/output/excel/cgodb.xlsx" 
)
write_xlsx(exports, 
           "data/output/excel/exports.xlsx" 
)
write_xlsx(gp15_RR1814, 
           "data/output/excel/gp15_RR1814.xlsx" 
)
write_xlsx(gp15_RR1815, 
           "data/output/excel/gp15_RR1815.xlsx" 
)
write_xlsx(as.data.frame(pacific_data), 
           "data/output/excel/pacific_data.xlsx" 
)
write_xlsx(as.data.frame(th234_data), 
           "data/output/excel/th234_data.xlsx" 
)
write_xlsx(as.data.frame(u238_data), 
           "data/output/excel/u238_data.xlsx" 
)
write_xlsx(as.data.frame(ratio_data), 
           "data/output/excel/ratio_data.xlsx" 
)

# Export SPDFs as shapefiles:
writeOGR(obj = pacific_data, 
         dsn = "data/output/pacific_data_shapefile", 
         layer = "pacific_data_all", 
         driver = "ESRI Shapefile"
)
writeOGR(obj = th234_data, 
         dsn = "data/output/th234_data_shapefile", 
         layer = "th234_data", 
         driver = "ESRI Shapefile"
)
writeOGR(obj = u238_data, 
         dsn = "data/output/u238_data_shapefile", 
         layer = "u238_data", 
         driver = "ESRI Shapefile"
)
writeOGR(obj = ratio_data, 
         dsn = "data/output/ratio_data_shapefile", 
         layer = "ratio_data", 
         driver = "ESRI Shapefile"
)

###############################################################################
#                                  End Program
###############################################################################