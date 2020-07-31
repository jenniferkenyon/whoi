###############################################################################
#                                  Krige: Variograms 
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading Excel data
  library(writexl) # writing Excel data
  library(rgdal) # for shape file reading
  library(sp) # spatial library
  library(maps) # for map data
  library(ggplot2) # for plotting
  library(maptools) # for map plotting
  library(automap) # variogram fitting
  library(gstat) # for geographical statistics
})

# Do you want to plot? --------------------------------------------------------
plotting = 1

############################### 234Th Variogram ###############################
# Read in data  ---------------------------------------------------------------
th234_data <- read_excel("data/output/excel/th234_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(th234_data) <- ~ Longitude + Latitude + Depth

# As (lon,lat), use GPS, the most common spatial reference system for the entire world:
proj4string(th234_data) <- CRS("+init=epsg:4326")

# Calculate variogram: np = number of points in lag (distance between two points), dist = ave. distance between points in the lag, gamma = mean of lag
th234 <- th234_data$Th_total_dpm_L
th234.vgm <- variogram(th234 ~ 1, # start with simple kriging (Z(s) = \mu + \epsilon(s)) with \mu the regressor and \epsilon the response vector (residuals)
                       data = th234_data) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# Fit a model to variogram:
th234.fit <- fit.variogram(th234.vgm, 
                           model = vgm(c("Sph",
                                         "Exp",
                                         "Gau",
                                         "Ste",
                                         "Mat"
                                         )
                                       )
                           )

# Plot model and variogram:
plot(th234.vgm, 
     th234.fit, 
     main = "234Th Fitted variogram",
     ylab = "Semivariance",
     xlab = "Distance"
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/th234_tot/234th_fitted_variogram.pdf')
  dev.off()
}

# Check for anistrophy --------------------------------------------------------
# Great a gstat object:
anistrophy <- gstat(id = "Th234",
                    formula = th234 ~ Longitude + Latitude + Depth,
                    data = th234_data)

# Create a variogram:
variogram_anis <- variogram(anistrophy, # gstat object
                            map =TRUE,
                            cutoff = 4000, # cutoff distance between points put into lags
                            width = 150) # distance between points in the lag

# Plot:
plot(variogram_anis,
     theshold = 10)
if (plotting == 1) {
  dev.copy(pdf, 'figures/th234_tot/234th_anistrophy.pdf')
  dev.off()
}

# This graph shows that there is not much autocorrelation, however there is more in the W-E/E-W direction than any other.
# Let's fit a variogram to this, using degrees of 70, 80, 90, 100, 110, and 120 from North/y-axis:
th234_variogram <- variogram(anistrophy,
                             alpha = c(70,80,90,100,110,120))

# Create a new model:
th234.vgm2 <- vgm(model = "Sph",
                  anis = c(90, 0, 0, 0.5, 1)
                 )

# Fit this new model:
th234.fit2 <- fit.variogram(th234_variogram,
                            model = th234.vgm2)

# Plot new model:
plot(th234_variogram,
     model = th234.fit2,
     as.table = TRUE
    )
if (plotting == 1) {
  dev.copy(pdf, 'figures/th234_tot/234th_new_fitting.pdf')
  dev.off()
}

rm(th234_variogram,
   th234.fit,
   th234.fit2,
   th234.vgm,
   th234.vgm2,
   variogram_anis,
   anistrophy)

# Thus, I will update our model with alpha = 90 and anis = c(90, 0, 0, 0.5, 1):
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

# Plot new model:
plot(th234.vgm,
     model = th234.fit
    )
if (plotting == 1) {
  dev.copy(pdf, 'figures/th234_tot/234th_final_fitting.pdf')
  dev.off()
}

# Save as excel:
if (plotting == 1) {
  write_xlsx(th234.fit, 
             "data/output/krige/th234_fit.xlsx" 
  )
  write_xlsx(th234.vgm, 
             "data/output/krige/th234_vgm.xlsx" 
  )
}

# Remove misc:
rm(th234_data,
   th234,
   th234.vgm, 
   th234.fit)

############################### 238U Variogram ################################
# Read in data  ---------------------------------------------------------------
u238_data <- read_excel("data/output/excel/u238_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(u238_data) <- ~ Longitude + Latitude + Depth

# As (lon,lat), use GPS, the most common spatial reference system for the entire world:
proj4string(u238_data) <- CRS("+init=epsg:4326")

# Calculate variogram:
u238 <- u238_data$U_dpm_L
u238.vgm <- variogram(u238 ~ 1, 
                      u238_data) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# Fit a model to variogram:
u238.fit <- fit.variogram(u238.vgm, 
                          model = vgm(c("Sph", 
                                        "Exp", 
                                        "Gau", 
                                        "Ste", 
                                        "Mat"
                                        )
                                      )
                          )

# Plot model and variogram:
plot(u238.vgm, 
     u238.fit, 
     main = "238U Fitted variogram",
     ylab = "Semivariance",
     xlab = "Distance"
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/u238/u238_fitted_variogram.pdf')
  dev.off()
}

# Check for anistrophy --------------------------------------------------------
# Great a gstat object:
anistrophy <- gstat(id = "U238",
                    formula = u238 ~ Longitude + Latitude + Depth,
                    data = u238_data)

# Create a variogram:
variogram_anis <- variogram(anistrophy, # gstat object
                            map =TRUE,
                            cutoff = 4000, # cutoff distance between points put into lags
                            width = 150) # distance between points in the lag

# Plot:
plot(variogram_anis,
     theshold = 10)
if (plotting == 1) {
  dev.copy(pdf, 'figures/u238/238u_anistrophy.pdf')
  dev.off()
}

# This graph shows that there is not much autocorrelation, however there is more in the W-E/E-W direction than any other.
# Let's fit a variogram to this, using degrees of 70, 80, 90, 100, 110, and 120 from North/y-axis:
u238_variogram <- variogram(anistrophy,
                            alpha = c(70,80,90,100,110,120))

# Create a new model:
u238.vgm2 <- vgm(model = "Sph",
                 anis = c(120, 340, 0, 0.5, 1)
)

# Fit this new model:
u238.fit2 <- fit.variogram(u238_variogram,
                           model = u238.vgm2)

# Plot new model:
plot(u238_variogram,
     model = u238.fit2,
     as.table = TRUE
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/u238/238u_new_fitting.pdf')
  dev.off()
}

rm(u238_variogram,
   u238.fit,
   u238.fit2,
   u238.vgm,
   u238.vgm2,
   variogram_anis,
   anistrophy)

# Thus, I will update our model with alpha = 120 and anis = c(90, 340, 0, 0.5, 1):
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

# Plot new model:
plot(u238.vgm,
     model = u238.fit
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/u238/u238_final_fitting.pdf')
  dev.off()
}

# Save as excel:
if (plotting == 1) {
  write_xlsx(u238.fit, 
             "data/output/krige/u238_fit.xlsx" 
  )
  write_xlsx(u238.vgm, 
             "data/output/krige/u238_vgm.xlsx" 
  )
}

# Remove misc:
rm(u238_data,
   u238,
   u238.vgm,
   u238.fit)

######################### POC/234Th Ratio Variogram ###########################
# Read in data  ---------------------------------------------------------------
ratio_data <- read_excel("data/output/excel/ratio_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(ratio_data) <- ~ Longitude + Latitude + Depth

# As (lon,lat), use GPS, the most common spatial reference system for the entire world:
proj4string(ratio_data) <- CRS("+init=epsg:4326")

# Calculate variogram:
ratio <- ratio_data$POC_Th_tot_umol_dpm
ratio.vgm <- variogram(ratio ~ Longitude + Latitude + Depth, 
                       ratio_data) # in a regression, y = ax + b. y is the response vector and x is the regressor (independent variable).

# Fit a model to variogram:
ratio.fit <- fit.variogram(ratio.vgm, 
                           model = vgm(c("Sph", 
                                         "Exp", 
                                         "Gau", 
                                         "Ste", 
                                         "Mat"
                          )
                          )
)

# Plot model and variogram:
plot(ratio.vgm, 
     ratio.fit, 
     main = "POC/234Th Fitted variogram",
     ylab = "Semivariance",
     xlab = "Distance"
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/ratio/poc_234th_fitted_variogram.pdf')
  dev.off()
}

# Check for anistrophy --------------------------------------------------------
# Great a gstat object:
anistrophy <- gstat(id = "POC_Th234_Ratio",
                    formula = ratio ~ Longitude + Latitude + Depth,
                    data = ratio_data)

# Create a variogram:
variogram_anis <- variogram(anistrophy, # gstat object
                            map =TRUE,
                            cutoff = 4000, # cutoff distance between points put into lags
                            width = 150) # distance between points in the lag

# Plot:
plot(variogram_anis,
     theshold = 10)
if (plotting == 1) {
  dev.copy(pdf, 'figures/ratio/ratio_anistrophy.pdf')
  dev.off()
}

# This graph shows that there is not much autocorrelation, however there is more in the W-E/E-W direction than any other.
# Let's fit a variogram to this, using degrees of 70, 80, 90, 100, 110, and 120 from North/y-axis:
ratio_variogram <- variogram(anistrophy,
                             alpha = c(70,80,90,100,110,120))

# Create a new model:
ratio.vgm2 <- vgm(model = "Sph",
                  anis = c(90, 0, 0, 0.5, 1)
)

# Fit this new model:
ratio.fit2 <- fit.variogram(ratio_variogram,
                            model = ratio.vgm2)

# Plot new model:
plot(ratio_variogram,
     model = ratio.fit2,
     as.table = TRUE
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/ratio/ratio_new_fitting.pdf')
  dev.off()
}

rm(ratio_variogram,
   ratio.fit,
   ratio.fit2,
   ratio.vgm,
   ratio.vgm2,
   variogram_anis,
   anistrophy)

# Thus, I will update our model with alpha = 100 and anis = c(90, 350, 0, 0.5, 1):
# Variogram:
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

# Plot new model:
plot(ratio.vgm,
     model = ratio.fit
)
if (plotting == 1) {
  dev.copy(pdf, 'figures/ratio/ratio_final_fitting.pdf')
  dev.off()
}

# Save as excel:
if (plotting == 1) {
  write_xlsx(ratio.fit, 
             "data/output/krige/ratio_fit.xlsx" 
  )
  write_xlsx(ratio.vgm, 
             "data/output/krige/ratio_vgm.xlsx" 
  )
}

# Remove misc:
rm(ratio_data,
   ratio,
   ratio.vgm,
   ratio.fit,
   plotting)

###############################################################################
#                                  End Program
###############################################################################