###############################################################################
#                                     Kriging 
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(automap) # autokriging
  library(gstat) # functionality for kriging
})

################################ Kriging: 234Th ###############################
# Read in data  ---------------------------------------------------------------
th234_data <- read_excel("data/output/excel/th234_data.xlsx")

# Make SPDFs ------------------------------------------------------------------
coordinates(th234_data) <- ~ Latitude + Longitude + Depth

# Specify data to be interpolated:
th234 <- th234_data$Th_total_dpm_L

# Krige Type 1:
th234_total_kriged <- autoKrige(th234 ~ Latitude + Longitude + Depth, # formula
                                th234_data, # data being interpolated
                                #<NEEDS TO BE CREATED>, # prediction grid
                                block = 30, # blocked cells
                                model = c("Sph", "Exp", "Gau", "Ste"), # models to be tested
                                kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), # values to be tested for models
                                fix.values = c(NA,NA,NA),
                                remove_duplicates = TRUE,
                                verbose = FALSE,
                                GLS.model = NA)

# Krige Type 2:
th234_total_kriged <- krige(th234 ~ Latitude + Longitude + Depth,
                            th234_data,
                            #<NEEDS TO BE CREATED>,
                            model = th234.fit$var_model,
                            nmax = 30)

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