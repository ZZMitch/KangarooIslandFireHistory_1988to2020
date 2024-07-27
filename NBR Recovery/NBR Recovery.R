##### This code builds recovery trajectories for Kangaroo Island fires #####

### Inputs ###
library(raster) # Working with rasters
library(sf) # Working with vectors
setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
# setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# NBR Fitted Time-series from 1988 to 2020 #
KI_NBR_fitted <- stack('LandTrendr_Outputs/GEE_Data/Fitted/NBR_Fitted_88to20_UTM53S_1.tif')
KI_NBR_fitted
# crs(KI_NBR_fitted)
# plot(KI_NBR_fitted) # Takes a while to load

# Fire History Shapefiles from 1989 to 2020 #
Fires_P1 <- st_read('Shapefiles/Fire/Fire Season Periods/Period1_1989to1992/FireHistory_Period1.shp')
Fires_P1 <- st_transform(Fires_P1, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# crs(Fires_P1)
# plot(KI_NBR_fitted$Band_1)
# plot(Fires_P1, add = TRUE) # Confirming that shapefile plots over raster
# Period 1: 1989 - 1992
Fires_P2 <- st_read('Shapefiles/Fire/Fire Season Periods/Period2_1993to1997/FireHistory_Period2.shp')
Fires_P2 <- st_transform(Fires_P2, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# plot(Fires_P2)
# Period 2: 1993 - 1997
Fires_P3 <- st_read('Shapefiles/Fire/Fire Season Periods/Period3_1998to2007/FireHistory_Period3.shp')
Fires_P3 <- st_transform(Fires_P3, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# plot(Fires_P3)
# Period 3: 1998 - 2007
Fires_P4 <- st_read('Shapefiles/Fire/Fire Season Periods/Period4_2008/FireHistory_Period4.shp')
Fires_P4 <- st_transform(Fires_P4, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# plot(Fires_P4)
# Period 4: 2008
Fires_P5 <- st_read('Shapefiles/Fire/Fire Season Periods/Period5_2009to2019/FireHistory_Period5.shp')
Fires_P5 <- st_transform(Fires_P5, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# plot(Fires_P5)
# Period 5: 2009 - 2019
Fires_P6 <- st_read('Shapefiles/Fire/Fire Season Periods/Period6_2020/FireHistory_Period6.shp')
Fires_P6 <- st_transform(Fires_P6, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# plot(Fires_P6)
# Period 6: 2020
# Only 8 variables because no Stop_Year and accidentally deleted 'FINANCIALY' Field

### Individual Fire History Rasters from 1989 to 2020 ###
# Note: seperated by FID (see ArcGIS Pro attribute tables)
# Removes all pixels with dNBR < 200
# Creates table of all pixels with dNBR >= 200
# Extracts fitted NBR values from these pixels
# Adds maxNBR for full time-series (per pixel) to table
# Finds post-fire fitted NBR values until next burn/end of time-series
# Turns post-fire NBR into % NBR recovery* for each year
# % NBR recovery calulated based on maxNBR from entire time series...
# This accounts for fires that burn recently after previous fire...
# Since the pre-fire NBR would be lower and thus lead to faster recovery...
# So this variable is % recovery to an est. "max vegetation" setting/full potential.
# Rename? % NBR recovery to full potential: NBR recovery to full potential (%)

colnames1 = c('Eastings', 'Northings', 'dNBR') 
colnames2 = c('maxNBR', 'PreFireNBR', 'PostFireNBR' 'PF0', 'PF1', 'PF2', 'PF3', 'PF4', 'PF5','PF6', 'PF7', 'PF8', 
              'PF9','PF10','PF11', 'PF12', 'PF13', 'PF14', 'PF15', 'PF16', 'PF17', 'PF18',
              'PF19','PF20', 'PF21', 'PF22', 'PF23', 'PF24', 'PF25', 'PF26', 'PF27',
              'PF28','PF29')

# Period 1: 1989 - 1992
# P1_0
# dNBR: 1989 - 1991; Stop: 2019 (check 1994)
fire_raster <- raster('dNBR/Period 1/P1_dNBR89_91.tif')
fire_poly <- Fires_P1[1,] # Find polygon
P1_0 <- crop(fire_raster, fire_poly)
P1_0[P1_0 < 99.999] <- NA # Unburned dNBR classification
# plot(P1_0)

P1_0_tbl <- as.data.frame(P1_0, xy = TRUE, na.rm = TRUE)
colnames(P1_0_tbl) <- colnames1
P1_0_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P1_0_tbl[,1:2], method = 'simple') # Slow
P1_0_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,4:32] # Band 4 = 1991, Band 32 = 2019

# Single pixel test
# NBR_recovery <- NBR_postfire[3,2] - NBR_postfire[3,1]
# NBR_loss_from_max <- P1_0_tbl[3,4] - NBR_postfire[3,1] #Stagnant per pixel
# percentNBR_recoverytofullpotential <- NBR_recovery/NBR_loss_from_max * 100

# test_traj <- P1_0_tbl[106,5:33]
# for (i in 2:ncol(test_traj)) {
#   if(test_traj[i] - test_traj[i - 1] <= 25) {
#     test_traj[i:ncol(test_traj)] <- NA
#   }
# } # Successfully makes all time-steps NA after drop of 25% or more for single pixel
# test_traj

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) { 
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates recovery from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P1_0_tbl[j,4] - NBR_postfire[j,1]
} # Calculates fire damage from maxNBR for all rows and columns (same value for all columns)
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P1_0_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P1_0_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P1_0_tbl[j, i + 4] - P1_0_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -19.03) { # Remove if more than or equal to 19.01% damage
      P1_0_tbl[j,i + 4:ncol(P1_0_tbl)] <- NA
    } # 19.01% = unburned dNBR (99.9) / average dNBR (525.5)
} # Removes non-recovery noise. Adds extra columns. 
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P1_0_tbl <- P1_0_tbl[,1:33] # Remove excess columns
write.csv(P1_0_tbl, 'P1_0_NBRecovery2.csv')

# P1_1
# dNBR: 1989 - 1991; Stop: 2020 (check 2005)
fire_raster <- raster('dNBR/Period 1/P1_dNBR89_91.tif')
fire_poly <- Fires_P1[2,] # Find polygon
P1_1 <- crop(fire_raster, fire_poly)
P1_1[P1_1 < 99.999] <- NA
# plot(P1_1)

P1_1_tbl <- as.data.frame(P1_1, xy = TRUE, na.rm = TRUE)
colnames(P1_1_tbl) <- colnames1
P1_1_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P1_1_tbl[,1:2], method = 'simple') # Slow
P1_1_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,4:33] # Band 4 = 1991, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P1_1_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P1_1_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P1_1_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P1_1_tbl[j, i + 4] - P1_1_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P1_1_tbl[j,i + 4:ncol(P1_1_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P1_1_tbl <- P1_1_tbl[,1:34] # Remove excess columns
write.csv(P1_1_tbl, 'P1_1_NBRecovery2.csv')

# P1_2
# dNBR: 1989 - 1991; Stop: 2020
fire_raster <- raster('dNBR/Period 1/P1_dNBR89_91.tif')
fire_poly <- Fires_P1[3,] # Find polygon
P1_2 <- crop(fire_raster, fire_poly)
P1_2[P1_2 < 99.999] <- NA
# plot(P1_2)

P1_2_tbl <- as.data.frame(P1_2, xy = TRUE, na.rm = TRUE)
colnames(P1_2_tbl) <- colnames1
P1_2_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P1_2_tbl[,1:2], method = 'simple') # Slow
P1_2_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,4:33] # Band 4 = 1991, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P1_2_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P1_2_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P1_2_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P1_2_tbl[j, i + 4] - P1_2_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P1_2_tbl[j,i + 4:ncol(P1_2_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P1_2_tbl <- P1_2_tbl[,1:34] # Remove excess columns
write.csv(P1_2_tbl, 'P1_2_NBRecovery2.csv')

# P1_3
# dNBR: 1990 - 1992; Stop: 2007
P1_3 <- raster('dNBR/Period 1/P1_dNBR90_92.tif')
P1_3[P1_3 < 99.999] <- NA
# plot(P1_3)

P1_3_tbl <- as.data.frame(P1_3, xy = TRUE, na.rm = TRUE)
colnames(P1_3_tbl) <- colnames1
P1_3_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P1_3_tbl[,1:2], method = 'simple') # Slow
P1_3_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,5:20] # Band 5 = 1992, Band 20 = 2007

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P1_3_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P1_3_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P1_3_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P1_3_tbl[j, i + 4] - P1_3_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P1_3_tbl[j,i + 4:ncol(P1_3_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P1_3_tbl <- P1_3_tbl[,1:20] # Remove excess columns
write.csv(P1_3_tbl, 'P1_3_NBRecovery2.csv')

# P1_4
# dNBR: 1988 - 1991; Stop: 2007
P1_4 <- raster('dNBR/Period 1/P1_dNBR88_91.tif')
P1_4[P1_4 < 99.999] <- NA
# plot(P1_4)

P1_4_tbl <- as.data.frame(P1_4, xy = TRUE, na.rm = TRUE)
colnames(P1_4_tbl) <- colnames1
P1_4_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P1_4_tbl[,1:2], method = 'simple') # Slow
P1_4_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,4:20] # Band 4 = 1991, Band 20 = 2007

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P1_4_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P1_4_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P1_4_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P1_4_tbl[j, i + 4] - P1_4_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P1_4_tbl[j,i + 4:ncol(P1_4_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P1_4_tbl <- P1_4_tbl[,1:21] # Remove excess columns
write.csv(P1_4_tbl, 'P1_4_NBRecovery2.csv')

# P1_5
# dNBR: 1988 - 1989; Stop: 2007
P1_5 <- raster('dNBR/Period 1/P1_dNBR88_89.tif')
P1_5[P1_5 < 99.999] <- NA
# plot(P1_5)

P1_5_tbl <- as.data.frame(P1_5, xy = TRUE, na.rm = TRUE)
colnames(P1_5_tbl) <- colnames1
P1_5_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P1_5_tbl[,1:2], method = 'simple') # Slow
P1_5_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,2:20] # Band 2 = 1989, Band 20 = 2007

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P1_5_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P1_5_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P1_5_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P1_5_tbl[j, i + 4] - P1_5_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P1_5_tbl[j,i + 4:ncol(P1_5_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P1_5_tbl <- P1_5_tbl[,1:23] # Remove excess columns
write.csv(P1_5_tbl, 'P1_5_NBRecovery2.csv')

# Period 2: 1993 - 1997
# P2_0
# dNBR: 1993 - 1995; Stop: 2019 (check 2007)
fire_raster <- raster('dNBR/Period 2/P2_dNBR93_95.tif')
fire_poly <- Fires_P2[1,] # Find polygon
P2_0 <- crop(fire_raster, fire_poly)
P2_0[P2_0 < 99.999] <- NA
# plot(P2_0)

P2_0_tbl <- as.data.frame(P2_0, xy = TRUE, na.rm = TRUE)
colnames(P2_0_tbl) <- colnames1
P2_0_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P2_0_tbl[,1:2], method = 'simple') # Slow
P2_0_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,8:32] # Band 8 = 1995, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P2_0_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P2_0_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P2_0_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P2_0_tbl[j, i + 4] - P2_0_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P2_0_tbl[j,i + 4:ncol(P2_0_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P2_0_tbl <- P2_0_tbl[,1:29] # Remove excess columns
write.csv(P2_0_tbl, 'P2_0_NBRecovery2.csv')

# P2_1
# dNBR: 1991 - 1994; Stop: 2006
P2_1 <- raster('dNBR/Period 2/P2_dNBR91_94.tif')
P2_1[P2_1 < 99.999] <- NA
# plot(P2_1)

P2_1_tbl <- as.data.frame(P2_1, xy = TRUE, na.rm = TRUE)
colnames(P2_1_tbl) <- colnames1
P2_1_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P2_1_tbl[,1:2], method = 'simple') # Slow
P2_1_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,7:19] # Band 7 = 1994, Band 19 = 2006

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P2_1_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P2_1_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P2_1_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P2_1_tbl[j, i + 4] - P2_1_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P2_1_tbl[j,i + 4:ncol(P2_1_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P2_1_tbl <- P2_1_tbl[,1:17] # Remove excess columns
write.csv(P2_1_tbl, 'P2_1_NBRecovery2.csv')

# P2_2
# dNBR: 1993 - 1995; Stop: 2020
fire_raster <- raster('dNBR/Period 2/P2_dNBR93_95.tif')
fire_poly <- Fires_P2[3,] # Find polygon
P2_2 <- crop(fire_raster, fire_poly)
P2_2[P2_2 < 99.999] <- NA
# plot(P2_2)

P2_2_tbl <- as.data.frame(P2_2, xy = TRUE, na.rm = TRUE)
colnames(P2_2_tbl) <- colnames1
P2_2_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P2_2_tbl[,1:2], method = 'simple') # Slow
P2_2_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,8:33] # Band 8 = 1995, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P2_2_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P2_2_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P2_2_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P2_2_tbl[j, i + 4] - P2_2_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P2_2_tbl[j,i + 4:ncol(P2_2_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P2_2_tbl <- P2_2_tbl[,1:30] # Remove excess columns
write.csv(P2_2_tbl, 'P2_2_NBRecovery2.csv')

# P2_3
# dNBR: 1996 - 1998; Stop: 2020 (check 2007)
fire_raster <- raster('dNBR/Period 2/P2_dNBR96_98.tif')
fire_poly <- Fires_P2[4,] # Find polygon
P2_3 <- crop(fire_raster, fire_poly)
P2_3[P2_3 < 99.999] <- NA
# plot(P2_3)

P2_3_tbl <- as.data.frame(P2_3, xy = TRUE, na.rm = TRUE)
colnames(P2_3_tbl) <- colnames1
P2_3_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P2_3_tbl[,1:2], method = 'simple') # Slow
P2_3_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,11:33] # Band 11 = 1998, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P2_3_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P2_3_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P2_3_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P2_3_tbl[j, i + 4] - P2_3_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P2_3_tbl[j,i + 4:ncol(P2_3_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P2_3_tbl <- P2_3_tbl[,1:27] # Remove excess columns
write.csv(P2_3_tbl, 'P2_3_NBRecovery2.csv')

# P2_4
# dNBR: 1996 - 1998; Stop: 2007
fire_raster <- raster('dNBR/Period 2/P2_dNBR96_98.tif')
fire_poly <- Fires_P2[5,] # Find polygon
P2_4 <- crop(fire_raster, fire_poly)
P2_4[P2_4 < 99.999] <- NA
# plot(P2_4)

P2_4_tbl <- as.data.frame(P2_4, xy = TRUE, na.rm = TRUE)
colnames(P2_4_tbl) <- colnames1
P2_4_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P2_4_tbl[,1:2], method = 'simple') # Slow
P2_4_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,11:20] # Band 11 = 1998, Band 20 = 2007

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P2_4_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P2_4_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P2_4_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P2_4_tbl[j, i + 4] - P2_4_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P2_4_tbl[j,i + 4:ncol(P2_4_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P2_4_tbl <- P2_4_tbl[,1:14] # Remove excess columns
write.csv(P2_4_tbl, 'P2_4_NBRecovery2.csv')

# Period 3: 1998 - 2007
# P3_0
# dNBR: 2005 - 2007; Stop: 2020
P3_0 <- raster('dNBR/Period 3/P3_dNBR05_07.tif')
P3_0[P3_0 < 99.999] <- NA
# plot(P3_0)

P3_0_tbl <- as.data.frame(P3_0, xy = TRUE, na.rm = TRUE)
colnames(P3_0_tbl) <- colnames1
P3_0_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_0_tbl[,1:2], method = 'simple') # Slow
P3_0_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,20:33] # Band 20 = 2007, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_0_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_0_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_0_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_0_tbl[j, i + 4] - P3_0_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_0_tbl[j,i + 4:ncol(P3_0_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_0_tbl <- P3_0_tbl[,1:18] # Remove excess columns
write.csv(P3_0_tbl, 'P3_0_NBRecovery2.csv')

# P3_1
# dNBR: 2005 - 2006; Stop: 2019
P3_1 <- raster('dNBR/Period 3/P3_dNBR05_06.tif')
P3_1[P3_1 < 99.999] <- NA
# plot(P3_1)

P3_1_tbl <- as.data.frame(P3_1, xy = TRUE, na.rm = TRUE)
colnames(P3_1_tbl) <- colnames1
P3_1_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_1_tbl[,1:2], method = 'simple') # Slow
P3_1_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,19:32] # Band 19 = 2006, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_1_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_1_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_1_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_1_tbl[j, i + 4] - P3_1_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_1_tbl[j,i + 4:ncol(P3_1_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_1_tbl <- P3_1_tbl[,1:18] # Remove excess columns
write.csv(P3_1_tbl, 'P3_1_NBRecovery2.csv')

# P3_2
# dNBR: 2004 - 2006; Stop: 2019
fire_raster <- raster('dNBR/Period 3/P3_dNBR04_06.tif')
fire_poly <- Fires_P3[3,] # Find polygon
P3_2 <- crop(fire_raster, fire_poly)
P3_2[P3_2 < 99.999] <- NA
# plot(P3_2)

P3_2_tbl <- as.data.frame(P3_2, xy = TRUE, na.rm = TRUE)
colnames(P3_2_tbl) <- colnames1
P3_2_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_2_tbl[,1:2], method = 'simple') # Slow
P3_2_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,19:32] # Band 19 = 2006, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_2_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_2_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_2_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_2_tbl[j, i + 4] - P3_2_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) { # Remove if more than 25% damage
      P3_2_tbl[j,i + 4:ncol(P3_2_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_2_tbl <- P3_2_tbl[,1:18] # Remove excess columns
write.csv(P3_2_tbl, 'P3_2_NBRecovery2.csv')

# P3_3
# dNBR: 2004 - 2006; Stop: 2019
fire_raster <- raster('dNBR/Period 3/P3_dNBR04_06.tif')
fire_poly <- Fires_P3[4,] # Find polygon
P3_3 <- crop(fire_raster, fire_poly)
P3_3[P3_3 < 99.999] <- NA
# plot(P3_3)

P3_3_tbl <- as.data.frame(P3_3, xy = TRUE, na.rm = TRUE)
colnames(P3_3_tbl) <- colnames1
P3_3_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_3_tbl[,1:2], method = 'simple') # Slow
P3_3_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,19:32] # Band 19 = 2006, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_3_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_3_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_3_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_3_tbl[j, i + 4] - P3_3_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_3_tbl[j,i + 4:ncol(P3_3_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_3_tbl <- P3_3_tbl[,1:18] # Remove excess columns
write.csv(P3_3_tbl, 'P3_3_NBRecovery2.csv')

# P3_4
# dNBR: 2000 - 2001; Stop: 2019 (check 2007)
P3_4 <- raster('dNBR/Period 3/P3_dNBR00_01.tif')
P3_4[P3_4 < 99.999] <- NA
# plot(P3_4)

P3_4_tbl <- as.data.frame(P3_4, xy = TRUE, na.rm = TRUE)
colnames(P3_4_tbl) <- colnames1
P3_4_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_4_tbl[,1:2], method = 'simple') # Slow
P3_4_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,14:32] # Band 14 = 2001, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_4_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_4_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_4_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_4_tbl[j, i + 4] - P3_4_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_4_tbl[j,i + 4:ncol(P3_4_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_4_tbl <- P3_4_tbl[,1:23] # Remove excess columns
write.csv(P3_4_tbl, 'P3_4_NBRecovery2.csv')

# P3_5
# dNBR: 2003 - 2005; Stop: 2020
P3_5 <- raster('dNBR/Period 3/P3_dNBR03_05.tif')
P3_5[P3_5 < 99.999] <- NA
# plot(P3_5)

P3_5_tbl <- as.data.frame(P3_5, xy = TRUE, na.rm = TRUE)
colnames(P3_5_tbl) <- colnames1
P3_5_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_5_tbl[,1:2], method = 'simple') # Slow
P3_5_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,18:33] # Band 18 = 2005, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_5_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_5_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_5_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_5_tbl[j, i + 4] - P3_5_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_5_tbl[j,i + 4:ncol(P3_5_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_5_tbl <- P3_5_tbl[,1:20] # Remove excess columns
write.csv(P3_5_tbl, 'P3_5_NBRecovery2.csv')

# P3_6
# dNBR: 2002 - 2003; Stop: 2019
P3_6 <- raster('dNBR/Period 3/P3_dNBR02_03.tif')
P3_6[P3_6 < 99.999] <- NA
# plot(P3_6)

P3_6_tbl <- as.data.frame(P3_6, xy = TRUE, na.rm = TRUE)
colnames(P3_6_tbl) <- colnames1
P3_6_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_6_tbl[,1:2], method = 'simple') # Slow
P3_6_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,16:32] # Band 16 = 2003, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_6_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_6_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_6_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_6_tbl[j, i + 4] - P3_6_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_6_tbl[j,i + 4:ncol(P3_6_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_6_tbl <- P3_6_tbl[,1:21] # Remove excess columns
write.csv(P3_6_tbl, 'P3_6_NBRecovery2.csv')

# P3_7
# dNBR: 2001 - 2002; Stop: 2019
P3_7 <- raster('dNBR/Period 3/P3_dNBR01_02.tif')
P3_7[P3_7 < 99.999] <- NA
# plot(P3_7)

P3_7_tbl <- as.data.frame(P3_7, xy = TRUE, na.rm = TRUE)
colnames(P3_7_tbl) <- colnames1
P3_7_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_7_tbl[,1:2], method = 'simple') # Slow
P3_7_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,15:32] # Band 15 = 2002, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_7_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_7_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_7_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_7_tbl[j, i + 4] - P3_7_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_7_tbl[j,i + 4:ncol(P3_7_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_7_tbl <- P3_7_tbl[,1:22] # Remove excess columns
write.csv(P3_7_tbl, 'P3_7_NBRecovery2.csv')

# P3_8
# dNBR: 2006 - 2007; Stop: 2019
fire_raster <- raster('dNBR/Period 3/P3_dNBR06_07.tif')
fire_poly <- Fires_P3[9,] # Find polygon
P3_8 <- crop(fire_raster, fire_poly)
P3_8[P3_8 < 99.999] <- NA
# plot(P3_8)

P3_8_tbl <- as.data.frame(P3_8, xy = TRUE, na.rm = TRUE)
colnames(P3_8_tbl) <- colnames1
P3_8_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_8_tbl[,1:2], method = 'simple') # Slow
P3_8_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,20:32] # Band 20 = 2007, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_8_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_8_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_8_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_8_tbl[j, i + 4] - P3_8_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_8_tbl[j,i + 4:ncol(P3_8_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_8_tbl <- P3_8_tbl[,1:17] # Remove excess columns
write.csv(P3_8_tbl, 'P3_8_NBRecovery2.csv')

# P3_9
# dNBR: 2003 - 2004; Stop: 2020
P3_9 <- raster('dNBR/Period 3/P3_dNBR03_04.tif')
P3_9[P3_9 < 99.999] <- NA
# plot(P3_9)

P3_9_tbl <- as.data.frame(P3_9, xy = TRUE, na.rm = TRUE)
colnames(P3_9_tbl) <- colnames1
P3_9_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_9_tbl[,1:2], method = 'simple') # Slow
P3_9_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,17:33] # Band 17 = 2004, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_9_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_9_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_9_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_9_tbl[j, i + 4] - P3_9_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_9_tbl[j,i + 4:ncol(P3_9_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_9_tbl <- P3_9_tbl[,1:21] # Remove excess columns
write.csv(P3_9_tbl, 'P3_9_NBRecovery2.csv')

# P3_10
# dNBR: 2004 - 2006; Stop: 2020
fire_raster <- raster('dNBR/Period 3/P3_dNBR04_06.tif')
fire_poly <- Fires_P3[11,] # Find polygon
P3_10 <- crop(fire_raster, fire_poly)
P3_10[P3_10 < 99.999] <- NA
# plot(P3_10)

P3_10_tbl <- as.data.frame(P3_10, xy = TRUE, na.rm = TRUE)
colnames(P3_10_tbl) <- colnames1
P3_10_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_10_tbl[,1:2], method = 'simple') # Slow
P3_10_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,19:33] # Band 19 = 2006, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_10_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_10_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_10_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_10_tbl[j, i + 4] - P3_10_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_10_tbl[j,i + 4:ncol(P3_10_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_10_tbl <- P3_10_tbl[,1:19] # Remove excess columns
write.csv(P3_10_tbl, 'P3_10_NBRecovery2.csv')

# P3_11
# dNBR: 2006 - 2007; Stop: 2020
fire_raster <- raster('dNBR/Period 3/P3_dNBR06_07.tif')
fire_poly <- Fires_P3[12,] # Find polygon
P3_11 <- crop(fire_raster, fire_poly)
P3_11[P3_11 < 99.999] <- NA
# plot(P3_11)

P3_11_tbl <- as.data.frame(P3_11, xy = TRUE, na.rm = TRUE)
colnames(P3_11_tbl) <- colnames1
P3_11_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_11_tbl[,1:2], method = 'simple') # Slow
P3_11_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,20:33] # Band 20 = 2007, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_11_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_11_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_11_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_11_tbl[j, i + 4] - P3_11_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_11_tbl[j,i + 4:ncol(P3_11_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_11_tbl <- P3_11_tbl[,1:18] # Remove excess columns
write.csv(P3_11_tbl, 'P3_11_NBRecovery2.csv')

# P3_12
# dNBR: 2006 - 2007; Stop: 2019
fire_raster <- raster('dNBR/Period 3/P3_dNBR06_07.tif')
fire_poly <- Fires_P3[13,] # Find polygon
P3_12 <- crop(fire_raster, fire_poly)
P3_12[P3_12 < 99.999] <- NA
# plot(P3_12)

P3_12_tbl <- as.data.frame(P3_12, xy = TRUE, na.rm = TRUE)
colnames(P3_12_tbl) <- colnames1
P3_12_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P3_12_tbl[,1:2], method = 'simple') # Slow
P3_12_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,20:32] # Band 20 = 2007, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P3_12_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P3_12_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P3_12_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P3_12_tbl[j, i + 4] - P3_12_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P3_12_tbl[j,i + 4:ncol(P3_12_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P3_12_tbl <- P3_12_tbl[,1:17] # Remove excess columns
write.csv(P3_12_tbl, 'P3_12_NBRecovery2.csv')

# Period 4: 2008
# P4_0
# dNBR: 2006 - 2008; Stop: 2019
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[1,] # Find polygon
P4_0 <- crop(fire_raster, fire_poly)
P4_0[P4_0 < 99.999] <- NA
# plot(P4_0)

P4_0_tbl <- as.data.frame(P4_0, xy = TRUE, na.rm = TRUE)
colnames(P4_0_tbl) <- colnames1
P4_0_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P4_0_tbl[,1:2], method = 'simple') # Slow
P4_0_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,21:32] # Band 21 = 2008, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P4_0_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P4_0_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P4_0_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P4_0_tbl[j, i + 4] - P4_0_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P4_0_tbl[j,i + 4:ncol(P4_0_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P4_0_tbl <- P4_0_tbl[,1:16] # Remove excess columns
write.csv(P4_0_tbl, 'P4_0_NBRecovery2.csv')

# P4_1
# dNBR: 2006 - 2008; Stop: 2019
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[2,] # Find polygon
P4_1 <- crop(fire_raster, fire_poly)
P4_1[P4_1 < 99.999] <- NA
# plot(P4_1)

P4_1_tbl <- as.data.frame(P4_1, xy = TRUE, na.rm = TRUE)
colnames(P4_1_tbl) <- colnames1
P4_1_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P4_1_tbl[,1:2], method = 'simple') # Slow
P4_1_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,21:32] # Band 21 = 2008, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P4_1_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P4_1_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P4_1_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P4_1_tbl[j, i + 4] - P4_1_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P4_1_tbl[j,i + 4:ncol(P4_1_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P4_1_tbl <- P4_1_tbl[,1:16] # Remove excess columns
write.csv(P4_1_tbl, 'P4_1_NBRecovery2.csv')

# P4_2
# dNBR: 2006 - 2008; Stop: 2019
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[3,] # Find polygon
P4_2 <- crop(fire_raster, fire_poly)
P4_2 <- mask(P4_2, fire_poly) # Another fire in bounding box
P4_2[P4_2 < 99.999] <- NA
# plot(P4_2)

P4_2_tbl <- as.data.frame(P4_2, xy = TRUE, na.rm = TRUE)
colnames(P4_2_tbl) <- colnames1
P4_2_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P4_2_tbl[,1:2], method = 'simple') # Slow
P4_2_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,21:32] # Band 21 = 2008, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P4_2_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P4_2_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P4_2_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P4_2_tbl[j, i + 4] - P4_2_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P4_2_tbl[j,i + 4:ncol(P4_2_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P4_2_tbl <- P4_2_tbl[,1:16] # Remove excess columns
write.csv(P4_2_tbl, 'P4_2_NBRecovery2.csv')

# P4_3
# dNBR: 2006 - 2008; Stop: 2019
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[4,] # Find polygon
P4_3 <- crop(fire_raster, fire_poly)
P4_3[P4_3 < 99.999] <- NA
# plot(P4_3)

P4_3_tbl <- as.data.frame(P4_3, xy = TRUE, na.rm = TRUE)
colnames(P4_3_tbl) <- colnames1
P4_3_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P4_3_tbl[,1:2], method = 'simple') # Slow
P4_3_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,21:32] # Band 21 = 2008, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P4_3_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P4_3_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P4_3_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P4_3_tbl[j, i + 4] - P4_3_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P4_3_tbl[j,i + 4:ncol(P4_3_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P4_3_tbl <- P4_3_tbl[,1:16] # Remove excess columns
write.csv(P4_3_tbl, 'P4_3_NBRecovery2.csv')

# P4_4
# dNBR: 2006 - 2008; Stop: 2020 (check 2019)
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[5,] # Find polygon
P4_4 <- crop(fire_raster, fire_poly)
P4_4[P4_4 < 99.999] <- NA
# plot(P4_4)

P4_4_tbl <- as.data.frame(P4_4, xy = TRUE, na.rm = TRUE)
colnames(P4_4_tbl) <- colnames1
P4_4_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P4_4_tbl[,1:2], method = 'simple') # Slow
P4_4_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,21:33] # Band 21 = 2008, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P4_4_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P4_4_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P4_4_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P4_4_tbl[j, i + 4] - P4_4_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P4_4_tbl[j,i + 4:ncol(P4_4_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P4_4_tbl <- P4_4_tbl[,1:17] # Remove excess columns
write.csv(P4_4_tbl, 'P4_4_NBRecovery2.csv')

# P4_5
# dNBR: 2006 - 2008; Stop: 2019
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[6,] # Find polygon
P4_5 <- crop(fire_raster, fire_poly)
P4_5 <- mask(P4_5, fire_poly) # Another fire in bounding box
P4_5[P4_5 < 99.999] <- NA
# plot(P4_5)

P4_5_tbl <- as.data.frame(P4_5, xy = TRUE, na.rm = TRUE)
colnames(P4_5_tbl) <- colnames1
P4_5_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P4_5_tbl[,1:2], method = 'simple') # Slow
P4_5_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,21:32] # Band 21 = 2008, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P4_5_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P4_5_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P4_5_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P4_5_tbl[j, i + 4] - P4_5_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P4_5_tbl[j,i + 4:ncol(P4_5_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P4_5_tbl <- P4_5_tbl[,1:16] # Remove excess columns
write.csv(P4_5_tbl, 'P4_5_NBRecovery2.csv')

# Period 5: 2009 - 2019
# P5_0
# dNBR: 2010 - 2011; Stop: 2020
fire_raster <- raster('dNBR/Period 5/P5_dNBR10_11.tif')
fire_poly <- Fires_P5[1,] # Find polygon
P5_0 <- crop(fire_raster, fire_poly)
P5_0[P5_0 < 99.999] <- NA
# plot(P5_0)

P5_0_tbl <- as.data.frame(P5_0, xy = TRUE, na.rm = TRUE)
colnames(P5_0_tbl) <- colnames1
P5_0_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_0_tbl[,1:2], method = 'simple') # Slow
P5_0_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,24:33] # Band 24 = 2011, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_0_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_0_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_0_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_0_tbl[j, i + 4] - P5_0_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_0_tbl[j,i + 4:ncol(P5_0_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_0_tbl <- P5_0_tbl[,1:14] # Remove excess columns
write.csv(P5_0_tbl, 'P5_0_NBRecovery2.csv')

# P5_1
# dNBR: 2017 - 2019; Stop: 2019
# P5_1 # No recovery data available

# P5_2
# dNBR: 2011 - 2013; Stop: 2020
fire_raster <- raster('dNBR/Period 5/P5_dNBR11_13.tif')
fire_poly <- Fires_P5[3,] # Find polygon
P5_2 <- crop(fire_raster, fire_poly)
P5_2[P5_2 < 99.999] <- NA
# plot(P5_2)

P5_2_tbl <- as.data.frame(P5_2, xy = TRUE, na.rm = TRUE)
colnames(P5_2_tbl) <- colnames1
P5_2_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_2_tbl[,1:2], method = 'simple') # Slow
P5_2_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,26:33] # Band 26 = 2013, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_2_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_2_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_2_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_2_tbl[j, i + 4] - P5_2_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_2_tbl[j,i + 4:ncol(P5_2_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_2_tbl <- P5_2_tbl[,1:12] # Remove excess columns
write.csv(P5_2_tbl, 'P5_2_NBRecovery2.csv')

# P5_3
# dNBR: 2011 - 2013; Stop: 2019
fire_raster <- raster('dNBR/Period 5/P5_dNBR11_13.tif')
fire_poly <- Fires_P5[4,] # Find polygon
P5_3 <- crop(fire_raster, fire_poly)
P5_3[P5_3 < 99.999] <- NA
# plot(P5_3)

P5_3_tbl <- as.data.frame(P5_3, xy = TRUE, na.rm = TRUE)
colnames(P5_3_tbl) <- colnames1
P5_3_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_3_tbl[,1:2], method = 'simple') # Slow
P5_3_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,26:32] # Band 26 = 2013, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_3_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_3_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_3_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_3_tbl[j, i + 4] - P5_3_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_3_tbl[j,i + 4:ncol(P5_3_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_3_tbl <- P5_3_tbl[,1:11] # Remove excess columns
write.csv(P5_3_tbl, 'P5_3_NBRecovery2.csv')

# P5_4
# dNBR: 2015 - 2017; Stop: 2020
P5_4 <- raster('dNBR/Period 5/P5_dNBR15_17.tif')
P5_4[P5_4 < 99.999] <- NA
# plot(P5_4)

P5_4_tbl <- as.data.frame(P5_4, xy = TRUE, na.rm = TRUE)
colnames(P5_4_tbl) <- colnames1
P5_4_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_4_tbl[,1:2], method = 'simple') # Slow
P5_4_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,30:33] # Band 30 = 2017, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_4_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_4_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_4_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_4_tbl[j, i + 4] - P5_4_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_4_tbl[j,i + 4:ncol(P5_4_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_4_tbl <- P5_4_tbl[,1:8] # Remove excess columns
write.csv(P5_4_tbl, 'P5_4_NBRecovery2.csv')

# P5_5
# dNBR: 2014 - 2015; Stop: 2020 (check 2019)
P5_5 <- raster('dNBR/Period 5/P5_dNBR14_15.tif')
P5_5[P5_5 < 99.999] <- NA
# plot(P5_5)

P5_5_tbl <- as.data.frame(P5_5, xy = TRUE, na.rm = TRUE)
colnames(P5_5_tbl) <- colnames1
P5_5_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_5_tbl[,1:2], method = 'simple') # Slow
P5_5_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,28:33] # Band 28 = 2015, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_5_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_5_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_5_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_5_tbl[j, i + 4] - P5_5_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_5_tbl[j,i + 4:ncol(P5_5_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_5_tbl <- P5_5_tbl[,1:10] # Remove excess columns
write.csv(P5_5_tbl, 'P5_5_NBRecovery2.csv')

# P5_6
# dNBR: 2015 - 2016; Stop: 2020 (check 2019)
fire_raster <- raster('dNBR/Period 5/P5_dNBR15_16.tif')
fire_poly <- Fires_P5[7,] # Find polygon
P5_6 <- crop(fire_raster, fire_poly)
P5_6[P5_6 < 99.999] <- NA
# plot(P5_6)

P5_6_tbl <- as.data.frame(P5_6, xy = TRUE, na.rm = TRUE)
colnames(P5_6_tbl) <- colnames1
P5_6_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_6_tbl[,1:2], method = 'simple') # Slow
P5_6_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,29:33] # Band 29 = 2016, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_6_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_6_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_6_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_6_tbl[j, i + 4] - P5_6_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_6_tbl[j,i + 4:ncol(P5_6_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_6_tbl <- P5_6_tbl[,1:9] # Remove excess columns
write.csv(P5_6_tbl, 'P5_6_NBRecovery2.csv')

# P5_7
# dNBR: 2017 - 2018; Stop: 2020
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_18.tif')
fire_poly <- Fires_P5[8,] # Find polygon
P5_7 <- crop(fire_raster, fire_poly)
P5_7[P5_7 < 99.999] <- NA
# plot(P5_7)

P5_7_tbl <- as.data.frame(P5_7, xy = TRUE, na.rm = TRUE)
colnames(P5_7_tbl) <- colnames1
P5_7_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_7_tbl[,1:2], method = 'simple') # Slow
P5_7_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,31:33] # Band 31 = 2018, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_7_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_7_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_7_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_7_tbl[j, i + 4] - P5_7_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_7_tbl[j,i + 4:ncol(P5_7_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_7_tbl <- P5_7_tbl[,1:7] # Remove excess columns
write.csv(P5_7_tbl, 'P5_7_NBRecovery2.csv')

# P5_8
# dNBR: 2015 - 2016; Stop: 2019
fire_raster <- raster('dNBR/Period 5/P5_dNBR15_16.tif')
fire_poly <- Fires_P5[9,] # Find polygon
P5_8 <- crop(fire_raster, fire_poly)
P5_8[P5_8 < 99.999] <- NA
# plot(P5_8)

P5_8_tbl <- as.data.frame(P5_8, xy = TRUE, na.rm = TRUE)
colnames(P5_8_tbl) <- colnames1
P5_8_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_8_tbl[,1:2], method = 'simple') # Slow
P5_8_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,29:32] # Band 29 = 2016, Band 32 = 2019

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_8_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_8_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_8_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_8_tbl[j, i + 4] - P5_8_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_8_tbl[j,i + 4:ncol(P5_8_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_8_tbl <- P5_8_tbl[,1:8] # Remove excess columns
write.csv(P5_8_tbl, 'P5_8_NBRecovery2.csv')

# P5_9
# dNBR: 2017 - 2018; Stop: 2020
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_18.tif')
fire_poly <- Fires_P5[10,] # Find polygon
P5_9 <- crop(fire_raster, fire_poly)
P5_9[P5_9 < 99.999] <- NA
# plot(P5_9)

P5_9_tbl <- as.data.frame(P5_9, xy = TRUE, na.rm = TRUE)
colnames(P5_9_tbl) <- colnames1
P5_9_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_9_tbl[,1:2], method = 'simple') # Slow
P5_9_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,31:33] # Band 31 = 2018, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_9_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_9_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_9_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_9_tbl[j, i + 4] - P5_9_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_9_tbl[j,i + 4:ncol(P5_9_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_9_tbl <- P5_9_tbl[,1:7] # Remove excess columns
write.csv(P5_9_tbl, 'P5_9_NBRecovery2.csv')

# P5_10
# dNBR: 2010 - 2011; Stop: 2020
fire_raster <- raster('dNBR/Period 5/P5_dNBR10_11.tif')
fire_poly <- Fires_P5[11,] # Find polygon
P5_10 <- crop(fire_raster, fire_poly)
P5_10[P5_10 < 99.999] <- NA
# plot(P5_10)

P5_10_tbl <- as.data.frame(P5_10, xy = TRUE, na.rm = TRUE)
colnames(P5_10_tbl) <- colnames1
P5_10_tbl[,colnames2] <- NA
NBR_all <- extract(KI_NBR_fitted, P5_10_tbl[,1:2], method = 'simple') # Slow
P5_10_tbl[,4] <- apply(NBR_all[,1:33], 1, max)
NBR_postfire <- NBR_all[,24:33] # Band 24 = 2011, Band 33 = 2020

NBR_recovery <- NBR_postfire # "blank" table to be filled
NBR_loss_from_max <- NBR_postfire # "blank" table to be filled
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_recovery[j,i] <- NBR_postfire[j,i] - NBR_postfire[j,1]
} # Calculates difference from postfire year for all rows and columns
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    NBR_loss_from_max[j,i] <- P5_10_tbl[j,4] - NBR_postfire[j,1]
} # Calculates difference from maxNBR for all rows and columns 
for (i in 1:ncol(NBR_postfire)) {
  for (j in 1:nrow(NBR_postfire))
    P5_10_tbl[j,i + 4] <- NBR_recovery[j,i] / NBR_loss_from_max[j,i] * 100
} # Calculates NBR recovery to full potential (%) for all rows and columns
NBR_recovery_dif <- P5_10_tbl[,5:34] # "blank" table to be filled
for (i in 2:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    NBR_recovery_dif[j,i] <- P5_10_tbl[j, i + 4] - P5_10_tbl[j, i + 3]
} # Calculates year to year % NBR recovery difference for all rows and columns
for (i in 1:ncol(NBR_recovery_dif)) {
  for (j in 1:nrow(NBR_recovery_dif))
    if (NBR_recovery_dif[j,i] <= -25) {
      P5_10_tbl[j,i + 4:ncol(P5_10_tbl)] <- NA
    }
} # Works! But adds many extra columns and gives an error.
# Finds cell-timesteps where 25%+ damage occurs and removes that and subsequent timesteps
P5_10_tbl <- P5_10_tbl[,1:14] # Remove excess columns
write.csv(P5_10_tbl, 'P5_10_NBRecovery2.csv')

# P5_11
# dNBR: 2017 - 2019; Stop: 2020 (check 2019)
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_19.tif')
fire_poly <- Fires_P5[12,] # Find polygon
P5_11 <- crop(fire_raster, fire_poly)
P5_11[P5_11 < 200] <- NA
# plot(P5_11)
# No recovery data available

# P5_12
# dNBR: 2017 - 2018; Stop: 2019
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_18.tif')
fire_poly <- Fires_P5[13,] # Find polygon
P5_12 <- crop(fire_raster, fire_poly)
P5_12 <- mask(P5_12, fire_poly) # Another fire in bounding box
P5_12[P5_12 < 200] <- NA
# plot(P5_12)
# No recovery data available

# Period 6: 2020
# P6_0
# dNBR: 2019 - 2020
# P6_0 # No recovery data available

# P6_1
# dNBR: 2019 - 2020
# P6_1 # No recovery data available

# P6_2
# dNBR: 2019 - 2020
# P6_2 # No recovery data available

# P6_3
# dNBR: 2019 - 2020
# P6_3 # No recovery data available