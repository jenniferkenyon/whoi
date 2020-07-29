###############################################################################
#                       Quality Control of Th-234 Data
#------------------------------------------------------------------------------
#                   Perrin Davidson | University of Chicago
###############################################################################
# Libraries -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl) # reading Excel data
  library(ggplot2) # plotting
})

# Set-Up ----------------------------------------------------------------------
# Do you want to plot? 0 for no. 1 for yes. 
plotting = 1

# Turn off scientific notation and set digit and print max:
options(scipen = 999)
options(digits=22)
options(max.print=999999)

# Data import -----------------------------------------------------------------
# Import all data from Excel:
th234_data_all <- read_excel("data/th234_data_220720.xlsx", 
                             sheet = "metadata&data", na = "nd")

# Import only Pacific Ocean data:
th234_pacific <- th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean"),]
th234_pacific <- rbind(th234_pacific, th234_data_all[which(th234_data_all$Ocean == "Pacifc Ocean"),])
th234_pacific <- rbind(th234_pacific, th234_data_all[which(th234_data_all$Ocean == "Pacific Ocean & Arctic Ocean")])
th234_pacific <- rbind(th234_pacific, th234_data_all[which(th234_data_all$Ocean == "Atlantic Ocean & Pacific Ocean")])


# Make Th_tot data ------------------------------------------------------------
unknown <- which(is.na(th234_pacific$`total_234Th(dpm/L)`) == TRUE)
unknown_before <- length(unknown)

i = 1
for (i in 1:dim(th234_pacific)[1]) {
  if (is.na(th234_pacific$`total_234Th(dpm/L)`[i]) == TRUE) {
    if (is.na(th234_pacific$`part_234Th_small(dpm/L)`[i]) == FALSE) {
      if (is.na(th234_pacific$`part_234Th_large(dpm/L)`[i]) == FALSE) {
        if (is.na(th234_pacific$`diss_234Th(dpm/L)`[i]) == FALSE) {
          th234_pacific$`total_234Th(dpm/L)`[i] <- th234_pacific$`part_234Th_small(dpm/L)`[i] +
                                                   th234_pacific$`part_234Th_large(dpm/L)`[i] +
                                                   th234_pacific$`diss_234Th(dpm/L)`[i]
        }
      }
    }
  }
}

unknown <- which(is.na(th234_pacific$`total_234Th(dpm/L)`) == TRUE)
unknown_after <- length(unknown)

change_th234_d <- unknown_before - unknown_after

# Remove NA Th_tot data -------------------------------------------------------
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$`total_234Th(dpm/L)`)), ]
th234_pacific <- th234_pacific[-which(is.na(th234_pacific$depth_m)), ]

# Make compiled data ----------------------------------------------------------
project_name <- th234_pacific$Project_Name
cruise_id <- th234_pacific$Cruise_ID
regions <- th234_pacific$Region
end <- dim(th234_pacific)[1]
index <- matrix(1:end, ncol = 1)
names <- matrix(1:100, nrow = 1)

j = 1
for (i in 2:end) {
  if (is.na(project_name[i]) == FALSE) {
    if (identical(project_name[i], project_name[i-1]) == FALSE) {
      j = j + 1
    }
    index[i] <- j
    names[j] <- project_name[i]
  } else if (is.na(cruise_id[i]) == FALSE) {
      if (identical(cruise_id[i], cruise_id[i-1]) == FALSE) {
        j = j + 1
      }
    index[i] = j
    names[j] <- cruise_id[i]
  } else if (is.na(regions[i]) == FALSE) {
      if (identical(regions[i], regions[i-1]) == FALSE) {
        j = j + 1
      }
    index[i] = j
    names[j] <- regions[i]
  }
}
index[1] = index[2] 
th234_pacific$PLOT_ID <- index
j_max <- j
names <- names[-(j_max+1):-100]

th234_depths = matrix(nrow = 1000, ncol = j_max)
th234_total = matrix(nrow = 1000, ncol = j_max)
max_num = matrix(1:j_max, ncol = j_max)
for (j in 1:j_max) {
  th234_depths[1:length(th234_pacific$depth_m[which(th234_pacific$PLOT_ID == j)]),j] <- th234_pacific$depth_m[which(th234_pacific$PLOT_ID == j)]
  th234_total[1:length(th234_pacific$`total_234Th(dpm/L)`[which(th234_pacific$PLOT_ID == j)]),j] <- th234_pacific$`total_234Th(dpm/L)`[which(th234_pacific$PLOT_ID == j)]
  max_num[j] <- length(th234_pacific$`total_234Th(dpm/L)`[which(th234_pacific$PLOT_ID == j)])
}

max_index = max(max_num)

th234_depths <- th234_depths[-max_index:-1000,]
th234_total <- th234_total[-max_index:-1000,]

colnames(th234_depths) <- names
colnames(th234_total) <- names

# Plot ------------------------------------------------------------------------
# Make a plotting space:
par(mfrow = c(9,5))
# Loop over j_max:
for (j in 1:j_max) {
  # Combine into one dataset:
  dataset_num = j
  end <- max(which(is.na(th234_depths[,dataset_num]) == FALSE))
  th234 <- data.frame(th234_depths[1:end,dataset_num], th234_total[1:end,dataset_num])
  names(th234)[1] <- "DEPTH"
  names(th234)[2] <- "TH234_TOTAL"
  # Plot:
  gg <- ggplot(th234, aes(x=TH234_TOTAL, y=DEPTH)) + geom_point(color='darkblue') + labs(title=names[dataset_num], x='[Th-234] (dpm/L)', y='Depth (m)') +
        theme(plot.title=element_text(size=10, face="bold"), 
              axis.text.x=element_text(size=10), 
              axis.text.y=element_text(size=10),
              axis.title.x=element_text(size=10, face="bold"),
              axis.title.y=element_text(size=10, face="bold")) 
  gg1 <- gg + scale_y_reverse() + scale_x_discrete(limits=c("1","2","3","4"))
  print(gg1)
  # For saving:
  dev.copy(pdf, paste0(names[j], ".pdf"))
  dev.off()
}

# To determine which are more than 5 dpm/L ------------------------------------
# The following values are unrealistic: 6.000000000000000000000, 13.039999999999999147349, 5.450000000000000177636, 398.000000000000000000000
bad_values <- th234_total[which(th234_total > 5)]

# These names for the first two are: "cruise 73 & cruise 86-1" and "VERTEX 2, VERTEX 3 and VERTEX 4"
bad_names1 <- th234_pacific$Cruise_ID[which(th234_pacific$`total_234Th(dpm/L)` > 5)] 

# These names for the second two are: "Funka Bay of Hokkaido, Japan" and Funka Bay" 
bad_names2 <- th234_pacific$Region[which(th234_pacific$`total_234Th(dpm/L)` > 5)]






