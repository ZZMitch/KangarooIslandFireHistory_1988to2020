##### This code builds recovery trajectories for Kangaroo Island fires #####

### Inputs ###
library(raster) # Working with rasters
library(sf) # Working with vectors
# setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

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
# Removes all pixels with dNBR < 100
# Creates table of all pixels with dNBR >= 100
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
colnames2 = c('maxNBR', 'PreFireNBR', 'PostFireNBR', 'dNBR_Max', 'PF0', 'PF1', 'PF2', 'PF3',
              'PF4','PF5','PF6','PF7', 'PF8', 'PF9','PF10','PF11', 'PF12', 'PF13', 'PF14', 'PF15',
              'PF16','PF17', 'PF18','PF19','PF20', 'PF21', 'PF22', 'PF23', 'PF24', 'PF25', 'PF26',
              'PF27','PF28','PF29')

# Period 1: 1989 - 1992
# P1_0
fire_raster <- raster('dNBR/Period 1/P1_dNBR89_91.tif')
fire_poly <- Fires_P1[1,] # Find polygon
P1_0 <- crop(fire_raster, fire_poly)
P1_0[P1_0 < 99.999] <- NA # Unburned dNBR classification
plot(P1_0)

prefire_band = 2 # 1989
postfire_band = 4 # 1991
stop_band = 32 # 2019

# P1_1
fire_raster <- raster('dNBR/Period 1/P1_dNBR89_91.tif')
fire_poly <- Fires_P1[2,] # Find polygon
P1_1 <- crop(fire_raster, fire_poly)
P1_1[P1_1 < 99.999] <- NA
plot(P1_1)

prefire_band = 2 # 1989
postfire_band = 4 # 1991
stop_band = 33 # 2020

# P1_2
fire_raster <- raster('dNBR/Period 1/P1_dNBR89_91.tif')
fire_poly <- Fires_P1[3,] # Find polygon
P1_2 <- crop(fire_raster, fire_poly)
P1_2[P1_2 < 99.999] <- NA
plot(P1_2)

prefire_band = 2 # 1989
postfire_band = 4 # 1991
stop_band = 33 # 2020

# P1_3
P1_3 <- raster('dNBR/Period 1/P1_dNBR90_92.tif')
P1_3[P1_3 < 99.999] <- NA
plot(P1_3)

prefire_band = 3 # 1990
postfire_band = 5 # 1992
stop_band = 20 # 2007

# P1_4
P1_4 <- raster('dNBR/Period 1/P1_dNBR88_91.tif')
P1_4[P1_4 < 99.999] <- NA
plot(P1_4)

prefire_band = 1 # 1988
postfire_band = 4 # 1991
stop_band = 20 # 2007

# P1_5
P1_5 <- raster('dNBR/Period 1/P1_dNBR88_89.tif')
P1_5[P1_5 < 99.999] <- NA
plot(P1_5)

prefire_band = 1 # 1988
postfire_band = 2 # 1989
stop_band = 20 # 2007

# Period 2: 1993 - 1997
# P2_0
fire_raster <- raster('dNBR/Period 2/P2_dNBR93_95.tif')
fire_poly <- Fires_P2[1,] # Find polygon
P2_0 <- crop(fire_raster, fire_poly)
P2_0[P2_0 < 99.999] <- NA
plot(P2_0)

prefire_band = 6 # 1993
postfire_band = 8 # 1995
stop_band = 32 # 2019

# P2_1
P2_1 <- raster('dNBR/Period 2/P2_dNBR91_94.tif')
P2_1[P2_1 < 99.999] <- NA
plot(P2_1)

prefire_band = 4 # 1991
postfire_band = 7 # 1994
stop_band = 19 # 2006

# P2_2
fire_raster <- raster('dNBR/Period 2/P2_dNBR93_95.tif')
fire_poly <- Fires_P2[3,] # Find polygon
P2_2 <- crop(fire_raster, fire_poly)
P2_2[P2_2 < 99.999] <- NA
plot(P2_2)

prefire_band = 6 # 1993
postfire_band = 8 # 1995
stop_band = 33 # 2020

# P2_3
fire_raster <- raster('dNBR/Period 2/P2_dNBR96_98.tif')
fire_poly <- Fires_P2[4,] # Find polygon
P2_3 <- crop(fire_raster, fire_poly)
P2_3[P2_3 < 99.999] <- NA
plot(P2_3)

prefire_band = 9 # 1996
postfire_band = 11 # 1998
stop_band = 33 # 2020

# P2_4
fire_raster <- raster('dNBR/Period 2/P2_dNBR96_98.tif')
fire_poly <- Fires_P2[5,] # Find polygon
P2_4 <- crop(fire_raster, fire_poly)
P2_4[P2_4 < 99.999] <- NA
plot(P2_4)

prefire_band = 9 # 1996
postfire_band = 11 # 1998
stop_band = 20 # 2007

# Period 3: 1998 - 2007
# P3_0
P3_0 <- raster('dNBR/Period 3/P3_dNBR05_07.tif')
P3_0[P3_0 < 99.999] <- NA
plot(P3_0)

prefire_band = 18 # 2005
postfire_band = 20 # 2007
stop_band = 33 # 2020

# P3_1
P3_1 <- raster('dNBR/Period 3/P3_dNBR05_06.tif')
P3_1[P3_1 < 99.999] <- NA
plot(P3_1)

prefire_band = 18 # 2005
postfire_band = 19 # 2006
stop_band = 32 # 2019

# P3_2
fire_raster <- raster('dNBR/Period 3/P3_dNBR04_06.tif')
fire_poly <- Fires_P3[3,] # Find polygon
P3_2 <- crop(fire_raster, fire_poly)
P3_2[P3_2 < 99.999] <- NA
plot(P3_2)

prefire_band = 17 # 2004
postfire_band = 19 # 2006
stop_band = 32 # 2019

# P3_3
fire_raster <- raster('dNBR/Period 3/P3_dNBR04_06.tif')
fire_poly <- Fires_P3[4,] # Find polygon
P3_3 <- crop(fire_raster, fire_poly)
P3_3[P3_3 < 99.999] <- NA
plot(P3_3)

prefire_band = 17 # 2004
postfire_band = 19 # 2006
stop_band = 32 # 2019

# P3_4
P3_4 <- raster('dNBR/Period 3/P3_dNBR00_01.tif')
P3_4[P3_4 < 99.999] <- NA
plot(P3_4)

prefire_band = 13 # 2000
postfire_band = 14 # 2001
stop_band = 32 # 2019

# P3_5
P3_5 <- raster('dNBR/Period 3/P3_dNBR03_05.tif')
P3_5[P3_5 < 99.999] <- NA
plot(P3_5)

prefire_band = 16 # 2003
postfire_band = 18 # 2005
stop_band = 33 # 2020

# P3_6
P3_6 <- raster('dNBR/Period 3/P3_dNBR02_03.tif')
P3_6[P3_6 < 99.999] <- NA
plot(P3_6)

prefire_band = 15 # 2002
postfire_band = 16 # 2003
stop_band = 32 # 2019

# P3_7
P3_7 <- raster('dNBR/Period 3/P3_dNBR01_02.tif')
P3_7[P3_7 < 99.999] <- NA
plot(P3_7)

prefire_band = 14 # 2001
postfire_band = 15 # 2002
stop_band = 32 # 2019

# P3_8
fire_raster <- raster('dNBR/Period 3/P3_dNBR06_07.tif')
fire_poly <- Fires_P3[9,] # Find polygon
P3_8 <- crop(fire_raster, fire_poly)
P3_8[P3_8 < 99.999] <- NA
plot(P3_8)

prefire_band = 19 # 2006
postfire_band = 20 # 2007
stop_band = 32 # 2019

# P3_9
P3_9 <- raster('dNBR/Period 3/P3_dNBR03_04.tif')
P3_9[P3_9 < 99.999] <- NA
plot(P3_9)

prefire_band = 16 # 2003
postfire_band = 17 # 2004
stop_band = 33 # 2020

# P3_10
fire_raster <- raster('dNBR/Period 3/P3_dNBR04_06.tif')
fire_poly <- Fires_P3[11,] # Find polygon
P3_10 <- crop(fire_raster, fire_poly)
P3_10[P3_10 < 99.999] <- NA
plot(P3_10)

prefire_band = 17 # 2004
postfire_band = 19 # 2006
stop_band = 33 # 2020

# P3_11
fire_raster <- raster('dNBR/Period 3/P3_dNBR06_07.tif')
fire_poly <- Fires_P3[12,] # Find polygon
P3_11 <- crop(fire_raster, fire_poly)
P3_11[P3_11 < 99.999] <- NA
plot(P3_11)

prefire_band = 19 # 2006
postfire_band = 20 # 2007
stop_band = 33 # 2020

# P3_12
fire_raster <- raster('dNBR/Period 3/P3_dNBR06_07.tif')
fire_poly <- Fires_P3[13,] # Find polygon
P3_12 <- crop(fire_raster, fire_poly)
P3_12[P3_12 < 99.999] <- NA
plot(P3_12)

prefire_band = 19 # 2006
postfire_band = 20 # 2007
stop_band = 32 # 2019

# Period 4: 2008
# P4_0
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[1,] # Find polygon
P4_0 <- crop(fire_raster, fire_poly)
P4_0[P4_0 < 99.999] <- NA
plot(P4_0)

prefire_band = 19 # 2006
postfire_band = 21 # 2008
stop_band = 32 # 2019

# P4_1
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[2,] # Find polygon
P4_1 <- crop(fire_raster, fire_poly)
P4_1[P4_1 < 99.999] <- NA
plot(P4_1)

prefire_band = 19 # 2006
postfire_band = 21 # 2008
stop_band = 32 # 2019

# P4_2
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[3,] # Find polygon
P4_2 <- crop(fire_raster, fire_poly)
P4_2 <- mask(P4_2, fire_poly) # Another fire in bounding box
P4_2[P4_2 < 99.999] <- NA
plot(P4_2)

prefire_band = 19 # 2006
postfire_band = 21 # 2008
stop_band = 32 # 2019

# P4_3
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[4,] # Find polygon
P4_3 <- crop(fire_raster, fire_poly)
P4_3[P4_3 < 99.999] <- NA
plot(P4_3)

prefire_band = 19 # 2006
postfire_band = 21 # 2008
stop_band = 32 # 2019

# P4_4
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[5,] # Find polygon
P4_4 <- crop(fire_raster, fire_poly)
P4_4[P4_4 < 99.999] <- NA
plot(P4_4)

prefire_band = 19 # 2006
postfire_band = 21 # 2008
stop_band = 33 # 2020

# P4_5
fire_raster <- raster('dNBR/Period 4/P4_dNBR06_08.tif')
fire_poly <- Fires_P4[6,] # Find polygon
P4_5 <- crop(fire_raster, fire_poly)
P4_5 <- mask(P4_5, fire_poly) # Another fire in bounding box
P4_5[P4_5 < 99.999] <- NA
plot(P4_5)

prefire_band = 19 # 2006
postfire_band = 21 # 2008
stop_band = 32 # 2019

# Period 5: 2009 - 2019
# P5_0
fire_raster <- raster('dNBR/Period 5/P5_dNBR10_11.tif')
fire_poly <- Fires_P5[1,] # Find polygon
P5_0 <- crop(fire_raster, fire_poly)
P5_0[P5_0 < 99.999] <- NA
plot(P5_0)

prefire_band = 23 # 2010
postfire_band = 24 # 2011
stop_band = 33 # 2020

# P5_1
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_19.tif')
fire_poly <- Fires_P5[2,] # Find polygon
P5_1 <- crop(fire_raster, fire_poly)
P5_1[P5_1 < 99.999] <- NA
plot(P5_1)

prefire_band = 30 # 2017
postfire_band = 32 # 2019
# No recovery data available

# P5_2
fire_raster <- raster('dNBR/Period 5/P5_dNBR11_13.tif')
fire_poly <- Fires_P5[3,] # Find polygon
P5_2 <- crop(fire_raster, fire_poly)
P5_2[P5_2 < 99.999] <- NA
plot(P5_2)

prefire_band = 24 # 2011
postfire_band = 26 # 2013
stop_band = 33 # 2020

# P5_3
fire_raster <- raster('dNBR/Period 5/P5_dNBR11_13.tif')
fire_poly <- Fires_P5[4,] # Find polygon
P5_3 <- crop(fire_raster, fire_poly)
P5_3[P5_3 < 99.999] <- NA
plot(P5_3)

prefire_band = 24 # 2011
postfire_band = 26 # 2013
stop_band = 32 # 2019

# P5_4
P5_4 <- raster('dNBR/Period 5/P5_dNBR15_17.tif')
P5_4[P5_4 < 99.999] <- NA
plot(P5_4)

prefire_band = 28 # 2015
postfire_band = 30 # 2017
stop_band = 33 # 2020

# P5_5
P5_5 <- raster('dNBR/Period 5/P5_dNBR14_15.tif')
P5_5[P5_5 < 99.999] <- NA
plot(P5_5)

prefire_band = 27 # 2014
postfire_band = 28 # 2015
stop_band = 33 # 2020

# P5_6
fire_raster <- raster('dNBR/Period 5/P5_dNBR15_16.tif')
fire_poly <- Fires_P5[7,] # Find polygon
P5_6 <- crop(fire_raster, fire_poly)
P5_6[P5_6 < 99.999] <- NA
plot(P5_6)

prefire_band = 28 # 2015
postfire_band = 29 # 2016
stop_band = 33 # 2020

# P5_7
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_18.tif')
fire_poly <- Fires_P5[8,] # Find polygon
P5_7 <- crop(fire_raster, fire_poly)
P5_7[P5_7 < 99.999] <- NA
plot(P5_7)

prefire_band = 30 # 2017
postfire_band = 31 # 2018
stop_band = 33 # 2020

# P5_8
fire_raster <- raster('dNBR/Period 5/P5_dNBR15_16.tif')
fire_poly <- Fires_P5[9,] # Find polygon
P5_8 <- crop(fire_raster, fire_poly)
P5_8[P5_8 < 99.999] <- NA
plot(P5_8)

prefire_band = 28 # 2015
postfire_band = 29 # 2016
stop_band = 32 # 2019

# P5_9
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_18.tif')
fire_poly <- Fires_P5[10,] # Find polygon
P5_9 <- crop(fire_raster, fire_poly)
P5_9[P5_9 < 99.999] <- NA
plot(P5_9)

prefire_band = 30 # 2017
postfire_band = 31 # 2018
stop_band = 33 # 2020

# P5_10
fire_raster <- raster('dNBR/Period 5/P5_dNBR10_11.tif')
fire_poly <- Fires_P5[11,] # Find polygon
P5_10 <- crop(fire_raster, fire_poly)
P5_10[P5_10 < 99.999] <- NA
plot(P5_10)

prefire_band = 23 # 2010
postfire_band = 24 # 2011
stop_band = 33 # 2020

# P5_11
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_19.tif')
fire_poly <- Fires_P5[12,] # Find polygon
P5_11 <- crop(fire_raster, fire_poly)
P5_11[P5_11 < 99.999] <- NA
plot(P5_11)

prefire_band = 30 # 2017
postfire_band = 32 # 2019
# No recovery data available

# P5_12
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_18.tif')
fire_poly <- Fires_P5[13,] # Find polygon
P5_12 <- crop(fire_raster, fire_poly)
P5_12 <- mask(P5_12, fire_poly) # Another fire in bounding box
P5_12[P5_12 < 99.999] <- NA
plot(P5_12)

prefire_band = 30 # 2017
postfire_band = 31 # 2018
# No recovery data available

# Period 6: 2020
# P6_0
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[1,] # Find polygon
P6_0 <- crop(fire_raster, fire_poly)
P6_0 <- mask(P6_0, fire_poly) # Another fire in bounding box
P6_0[P6_0 < 99.999] <- NA
plot(P6_0)

prefire_band = 32 # 2019
postfire_band = 33 # 2020
# No recovery data available

# P6_1
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[2,] # Find polygon
P6_1 <- crop(fire_raster, fire_poly)
P6_1 <- mask(P6_1, fire_poly) # Another fire in bounding box
P6_1[P6_1 < 99.999] <- NA
plot(P6_1)

prefire_band = 32 # 2019
postfire_band = 33 # 2020
# No recovery data available

# P6_2
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[3,] # Find polygon
P6_2 <- crop(fire_raster, fire_poly)
P6_2 <- mask(P6_2, fire_poly) # Another fire in bounding box
P6_2[P6_2 < 99.999] <- NA
plot(P6_2)

prefire_band = 32 # 2019
postfire_band = 33 # 2020
# No recovery data available

# P6_3
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[4,] # Find polygon
P6_3 <- crop(fire_raster, fire_poly)
P6_3 <- mask(P6_3, fire_poly) # Another fire in bounding box
P6_3[P6_3 < 99.999] <- NA
plot(P6_3)

prefire_band = 32 # 2019
postfire_band = 33 # 2020
# No recovery data available

### Build initial table and timeseries data ###
tbl <- as.data.frame(P6_3, xy = TRUE, na.rm = TRUE) # Raster to table, adds coorinates, removes unburned
# Update fire ID each time

colnames(tbl) <- colnames1 # Adds column names for original table
tbl[,colnames2] <- NA # Adds column names to be filled

NBR_all <- extract(KI_NBR_fitted, tbl[,1:2], method = 'simple') # Slow
# Extracts full fitted NBR timeseries (1988-2020) for pixels corresponding to fire

tbl[,4] <- apply(NBR_all[,1:33], 1, max) # Extracts MaxNBR for all pixels
MaxNBR <- tbl[,4]
tbl[,5] <- NBR_all[,prefire_band] # Extracts PreFireNBR for all pixels
PreFireNBR <- tbl[,5]
tbl[,6] <- NBR_all[,postfire_band] # Extracts PostFireNBR for all pixels
PostFireNBR <- tbl[,6]

NBR_postfire_ts <- NBR_all[,postfire_band:stop_band] # Subset postfire bands from full NBR timeseries

### Calculate NBR Recovery to max vegetation (%) for all pixels ###
# Calculates difference between maxNBR and postfire NBR for all pixels
for (j in 1:nrow(tbl)) { # For all pixels
  tbl[j,7] <- MaxNBR[j] - PostFireNBR[j] # Distance 1 (Bright et al., 2019), but from max vegetation
}
dNBR_fromMax <- tbl[,7]

# Calculates dNBR from postfire year X 
NBR_recovery <- NBR_postfire_ts # "blank" table to be filled
for (i in 1:ncol(NBR_postfire_ts)) { # For all time-steps
  for (j in 1:nrow(NBR_postfire_ts)) # And for all pixels
    NBR_recovery[j,i] <- NBR_postfire_ts[j,i] - PostFireNBR[j] # Distance 2 (Bright et al., 2019)
} 

# Calculates NBR recovery to full potential (%)
for (i in 1:ncol(NBR_postfire_ts)) { # For all time-steps
  for (j in 1:nrow(NBR_postfire_ts)) # And for all pixels
    tbl[j,i + 7] <- NBR_recovery[j,i] / dNBR_fromMax[j] * 100 # Skip 7 columns at beginning
} 

### Remove pre-end damage and subsequent time-steps from each pixels (if required) ###
# Calculates year to year post-fire NBR difference
NBR_postfire_ts_dif <- NBR_recovery
for (i in 2:ncol(NBR_postfire_ts_dif)) { # For all time-steps
  for (j in 1:nrow(NBR_postfire_ts_dif)) # And for all pixels
    NBR_postfire_ts_dif[j,i] <- NBR_postfire_ts[j,i] - NBR_postfire_ts[j,i - 1]
} 

# Finds when damage occurs (50+ NBR loss) and removes that and subsequent timesteps
length = stop_band - postfire_band
length_plus8 = length + 8

for (i in 1:ncol(NBR_postfire_ts_dif)) { # For all time-steps
  for (j in 1:nrow(NBR_postfire_ts_dif)) # And for all pixels
    if (NBR_postfire_ts_dif[j,i] <= -50) { 
      tbl[j,i + 7:length_plus8] <- NA # Skip 7 columns at beginning
    } 
} # Removes non-recovery noise. Adds extra columns. 

tbl <- tbl[,1:7] # Remove excess columns

### Export ###
write.csv(tbl, 'P6_3_NBRecovery3.csv')
