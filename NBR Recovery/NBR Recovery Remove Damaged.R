##### This takes input tables from NBR Recovery.R and removes damage from trajectories #####

### Inputs ###
library(raster) # Working with rasters
library(sf) # Working with vectors
setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
# setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# Table to be adjusted #
tbl <- read.csv('NBR Recovery/Period 4/P4_5_NBRecovery.csv')
colnames(tbl)[6:35] <- c('PF0', 'PF1', 'PF2', 'PF3', 'PF4', 'PF5','PF6', 'PF7', 'PF8', 
                         'PF9','PF10','PF11', 'PF12', 'PF13', 'PF14', 'PF15', 'PF16', 'PF17', 'PF18',
                         'PF19','PF20', 'PF21', 'PF22', 'PF23', 'PF24', 'PF25', 'PF26', 'PF27',
                         'PF28','PF29')
ncol <- 17
burn_per <- -25 # Burn % = % NBR recovery to full potential for dNBRs 100 and above (i.e., burned)

NBR_recovery_dif <- tbl[,6:ncol] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- tbl[j, i + 5] - tbl[j, i + 4]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= burn_per) { # Remove if damage more than Burn %
      tbl[j,i + 5:ncol] <- NA
    }
} # Works! But adds extra columns
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
tbl <- tbl[,1:ncol] # Remove excess columns

write.csv(tbl, 'P4_5_NBRecovery1.csv')