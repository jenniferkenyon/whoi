###############################################################################
#                                  Krige: Variogams 
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
  library(maptools) # for map plotting
  library(gstat) # for geographical statistics
})

############################### 234Th Variogram ###############################
# Read in data  ---------------------------------------------------------------
th234_data <- read_excel("data/output/excel/th234_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(th234_data) <- ~ Latitude + Longitude + Depth

# As (lon,lat), use Web Mercater this is the most common spatial reference system for the entire world:
proj4string(th234_data) <- CRS("+init=epsg:3857")

# Calculate variogram:
th234 <- th234_data$Th_total_dpm_L
th234.vgm <- variogram(th234 ~ Latitude + Longitude + Depth, th234_data) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# Fit a model to variogram:
th234.fit <- autofitVariogram(th234 ~ Latitude + Longitude + Depth,
                              th234_data,
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
     main = "234Th Fitted variogram",
     ylab = "Semivariance",
     xlab = "Distance"
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/th234_tot/234th_fitted_variogram.pdf')
  dev.off()
}

############################### 238U Variogram ################################
# Read in data  ---------------------------------------------------------------
u238_data <- read_excel("data/output/excel/u238_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(u238_data) <- ~ Latitude + Longitude + Depth

# As (lon,lat), use Web Mercater this is the most common spatial reference system for the entire world:
proj4string(u238_data) <- CRS("+init=epsg:3857")

# Calculate variogram:
u238 <- u238_data$U_dpm_L
u238.vgm <- variogram(u238 ~ Latitude + Longitude + Depth, u238_data) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# Fit a model to variogram:
u238.fit <- autofitVariogram(u238 ~ Latitude + Longitude + Depth,
                              u238_data,
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
plot(u238.vgm, 
     u238.fit$var_model, 
     main = "238U Fitted variogram",
     ylab = "Semivariance",
     xlab = "Distance"
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/u238/u238_fitted_variogram.pdf')
  dev.off()
}

######################### POC/234Th Ratio Variogram ###########################
# Read in data  ---------------------------------------------------------------
ratio_data <- read_excel("data/output/excel/ratio_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(ratio_data) <- ~ Latitude + Longitude + Depth

# As (lon,lat), use Web Mercater this is the most common spatial reference system for the entire world:
proj4string(ratio_data) <- CRS("+init=epsg:3857")

# Calculate variogram:
ratio <- ratio_data$POC_Th_tot_umol_dpm
ratio.vgm <- variogram(ratio ~ Latitude + Longitude + Depth, ratio_data) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# Fit a model to variogram:
ratio.fit <- autofitVariogram(ratio ~ Latitude + Longitude + Depth,
                              ratio_data,
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
plot(ratio.vgm, 
     ratio.fit$var_model, 
     main = "POC/234Th Fitted variogram",
     ylab = "Semivariance",
     xlab = "Distance"
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/ratio/poc_234th_fitted_variogram.pdf')
  dev.off()
}

###############################################################################
#                                  End Program
###############################################################################