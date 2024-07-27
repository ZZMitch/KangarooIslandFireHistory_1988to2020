##### This code creates dNBR Maps for Kangaroo Island Fire Events 1989 - 2020 #####

### Inputs ###
library(raster) # Working with rasters
library(sf) # Working with vectors
#setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# NBR Fitted Time-series from 1988 to 2020 #
KI_NBR_fitted <- stack('LandTrendr_Outputs/GEE_Data/Fitted/NBR_Fitted_88to20_UTM53S_1.tif')
KI_NBR_fitted
# crs(KI_NBR_fitted)
# plot(KI_NBR_fitted) #Takes a while to load

# Fire History Shapefiles from 1989 to 2020 #
Fires_P1 <- st_read('Shapefiles/Fire/Fire Season Periods/Period1_1989to1992/FireHistory_Period1.shp')
Fires_P1 <- st_transform(Fires_P1, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# crs(Fires_P1)
# plot(KI_NBR_fitted$Band_1)
# plot(Fires_P1, add = TRUE) #Confirming that shapefile plots over raster
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

### Create dNBR Maps by Fire Event ###
### Period 1 ###
# 1988 - 1989
dNBR88_89 <- KI_NBR_fitted$Band_1 - KI_NBR_fitted$Band_2 # Band 1 = 1988, Band 2 = 1989
# plot(dNBR88_89)
P188_89 <- Fires_P1[6,] # Find polygon(s)
# plot(P188_89)
P1_dNBR88_89 <- crop(dNBR88_89, P188_89) # Initial clip
P1_dNBR88_89 <- mask(P1_dNBR88_89, P188_89) # Remove bounding box
# plot(P1_dNBR88_89)
# writeRaster(P1_dNBR88_89, 'P1_dNBR88_89.tif') 
# Aligns perfect, values correct, outside polygon = NoData

# 1988 - 1991
dNBR88_91 <- KI_NBR_fitted$Band_1 - KI_NBR_fitted$Band_4 # Band 1 = 1988, Band 4 = 1991
# plot(dNBR88_91)
P188_91 <- Fires_P1[5,] # Find polygon(s)
# plot(P188_91)
P1_dNBR88_91 <- crop(dNBR88_91, P188_91) # Initial clip
P1_dNBR88_91 <- mask(P1_dNBR88_91, P188_91) # Remove bounding box
# plot(P1_dNBR88_91)
# writeRaster(P1_dNBR88_91, 'P1_dNBR88_91.tif') 

# 1989 - 1991
dNBR89_91 <- KI_NBR_fitted$Band_2 - KI_NBR_fitted$Band_4 # Band 2 = 1989, Band 4 = 1991
# plot(dNBR89_91)
P189_91 <- Fires_P1[1:3,] # Find polygon(s)
# plot(P189_91)
P1_dNBR89_91 <- crop(dNBR89_91, P189_91) # Initial clip
P1_dNBR89_91 <- mask(P1_dNBR89_91, P189_91) # Remove bounding box
# plot(P1_dNBR89_91)
# writeRaster(P1_dNBR89_91, 'P1_dNBR89_91.tif')

# 1990 - 1992
dNBR90_92 <- KI_NBR_fitted$Band_3 - KI_NBR_fitted$Band_5 # Band 3 = 1990, Band 5 = 1992
# plot(dNBR90_92)
P190_92 <- Fires_P1[4,] # Find polygon(s)
# plot(P190_92)
P1_dNBR90_92 <- crop(dNBR90_92, P190_92) # Initial clip
P1_dNBR90_92 <- mask(P1_dNBR90_92, P190_92) # Remove bounding box
# plot(P1_dNBR90_92)
# writeRaster(P1_dNBR90_92, 'P1_dNBR90_92.tif')

### Period 2 ###
# 1991 - 1994
dNBR91_94 <- KI_NBR_fitted$Band_4 - KI_NBR_fitted$Band_7 # Band 4 = 1991, Band 7 = 1994
# plot(dNBR91_94)
P291_94 <- Fires_P2[2,] # Find polygon(s)
# plot(P291_94)
P2_dNBR91_94 <- crop(dNBR91_94, P291_94) # Initial clip
P2_dNBR91_94 <- mask(P2_dNBR91_94, P291_94) # Remove bounding box
# plot(P2_dNBR91_94)
#writeRaster(P2_dNBR91_94, 'P2_dNBR91_94.tif')

# 1993 - 1995
dNBR93_95 <- KI_NBR_fitted$Band_6 - KI_NBR_fitted$Band_8 # Band 6 = 1993, Band 8 = 1995
# plot(dNBR93_95)
P293_95 <- Fires_P2[c(1,3),] # Find polygon(s)
# plot(P293_95)
P2_dNBR93_95 <- crop(dNBR93_95, P293_95) # Initial clip
P2_dNBR93_95 <- mask(P2_dNBR93_95, P293_95) # Remove bounding box
# plot(P2_dNBR93_95)
# writeRaster(P2_dNBR93_95, 'P2_dNBR93_95.tif')

# 1996 - 1998
dNBR96_98 <- KI_NBR_fitted$Band_9 - KI_NBR_fitted$Band_11 # Band 9 = 1996, Band 11 = 1998
# plot(dNBR96_98)
P296_98 <- Fires_P2[4:5,] # Find polygon(s)
# plot(P296_98)
P2_dNBR96_98 <- crop(dNBR96_98, P296_98) # Initial clip
P2_dNBR96_98 <- mask(P2_dNBR96_98, P296_98) # Remove bounding box
# plot(P2_dNBR96_98)
# writeRaster(P2_dNBR96_98, 'P2_dNBR96_98.tif')

### Period 3 ###
# 2000 - 2001
dNBR00_01 <- KI_NBR_fitted$Band_13 - KI_NBR_fitted$Band_14 # Band 13 = 2000, Band 14 = 2001
# plot(dNBR00_01)
P300_01 <- Fires_P3[5,] # Find polygon(s)
# plot(P300_01)
P3_dNBR00_01 <- crop(dNBR00_01, P300_01) # Initial clip
P3_dNBR00_01 <- mask(P3_dNBR00_01, P300_01) # Remove bounding box
# plot(P3_dNBR00_01)
# writeRaster(P3_dNBR00_01, 'P3_dNBR00_01.tif')

# 2001 - 2002
dNBR01_02 <- KI_NBR_fitted$Band_14 - KI_NBR_fitted$Band_15 # Band 14 = 2001, Band 15 = 2002
# plot(dNBR01_02)
P301_02 <- Fires_P3[8,] # Find polygon(s)
# plot(P301_02)
P3_dNBR01_02 <- crop(dNBR01_02, P301_02) # Initial clip
P3_dNBR01_02 <- mask(P3_dNBR01_02, P301_02) # Remove bounding box
# plot(P3_dNBR01_02)
# writeRaster(P3_dNBR01_02, 'P3_dNBR01_02.tif')

# 2002 - 2003
dNBR02_03 <- KI_NBR_fitted$Band_15 - KI_NBR_fitted$Band_16 # Band 14 = 2002, Band 15 = 2003
# plot(dNBR02_03)
P302_03 <- Fires_P3[7,] # Find polygon(s)
# plot(P302_03)
P3_dNBR02_03 <- crop(dNBR02_03, P302_03) # Initial clip
P3_dNBR02_03 <- mask(P3_dNBR02_03, P302_03) # Remove bounding box
# plot(P3_dNBR02_03)
# writeRaster(P3_dNBR02_03, 'P3_dNBR02_03.tif')

# 2003 - 2004
dNBR03_04 <- KI_NBR_fitted$Band_16 - KI_NBR_fitted$Band_17 # Band 16 = 2003, Band 17 = 2004
# plot(dNBR03_04)
P303_04 <- Fires_P3[10,] # Find polygon(s)
# plot(P303_04)
P3_dNBR03_04 <- crop(dNBR03_04, P303_04) # Initial clip
P3_dNBR03_04 <- mask(P3_dNBR03_04, P303_04) # Remove bounding box
# plot(P3_dNBR03_04)
#writeRaster(P3_dNBR03_04, 'P3_dNBR03_04.tif')

# 2003 - 2005
dNBR03_05 <- KI_NBR_fitted$Band_16 - KI_NBR_fitted$Band_18 # Band 16 = 2003, Band 18 = 2005
# plot(dNBR03_05)
P303_05 <- Fires_P3[6,] # Find polygon(s)
# plot(P303_05)
P3_dNBR03_05 <- crop(dNBR03_05, P303_05) # Initial clip
P3_dNBR03_05 <- mask(P3_dNBR03_05, P303_05) # Remove bounding box
# plot(P3_dNBR03_05)
#writeRaster(P3_dNBR03_05, 'P3_dNBR03_05.tif')

# 2004 - 2006
dNBR04_06 <- KI_NBR_fitted$Band_17 - KI_NBR_fitted$Band_19 # Band 17 = 2004, Band 19 = 2006
# plot(dNBR04_06)
P304_06 <- Fires_P3[c(3,4,11),] # Find polygon(s)
# plot(P304_06)
P3_dNBR04_06 <- crop(dNBR04_06, P304_06) # Initial clip
P3_dNBR04_06 <- mask(P3_dNBR04_06, P304_06) # Remove bounding box
# plot(P3_dNBR04_06)
writeRaster(P3_dNBR04_06, 'P3_dNBR04_06.tif')

# 2005 - 2006
dNBR05_06 <- KI_NBR_fitted$Band_18 - KI_NBR_fitted$Band_19 # Band 18 = 2005, Band 19 = 2006
# plot(dNBR05_06)
P305_06 <- Fires_P3[2,] # Find polygon(s)
# plot(P305_06)
P3_dNBR05_06 <- crop(dNBR05_06, P305_06) # Initial clip
P3_dNBR05_06 <- mask(P3_dNBR05_06, P305_06) # Remove bounding box
# plot(P3_dNBR05_06)
# writeRaster(P3_dNBR05_06, 'P3_dNBR05_06.tif')

# 2005 - 2007
dNBR05_07 <- KI_NBR_fitted$Band_18 - KI_NBR_fitted$Band_20 # Band 18 = 2005, Band 20 = 2007
# plot(dNBR05_07)
P305_07 <- Fires_P3[1,] # Find polygon(s)
# plot(P305_07)
P3_dNBR05_07 <- crop(dNBR05_07, P305_07) # Initial clip
P3_dNBR05_07 <- mask(P3_dNBR05_07, P305_07) # Remove bounding box
# plot(P3_dNBR05_07)
# writeRaster(P3_dNBR05_07, 'P3_dNBR05_07.tif')

# 2006 - 2007
dNBR06_07 <- KI_NBR_fitted$Band_19 - KI_NBR_fitted$Band_20 # Band 19 = 2006, Band 20 = 2007
# plot(dNBR06_07)
P306_07 <- Fires_P3[c(9,12,13),] # Find polygon(s)
# plot(P306_07)
P3_dNBR06_07 <- crop(dNBR06_07, P306_07) # Initial clip
P3_dNBR06_07 <- mask(P3_dNBR06_07, P306_07) # Remove bounding box
# plot(P3_dNBR06_07)
# writeRaster(P3_dNBR06_07, 'P3_dNBR06_07.tif')

### Period 4 ###
# 2006 - 2008
dNBR06_08 <- KI_NBR_fitted$Band_19 - KI_NBR_fitted$Band_21 # Band 19 = 2006, Band 21 = 2008
# plot(dNBR06_08)
# plot(Fires_P4) #All polygons in this period have same dNBR years
P4_dNBR06_08 <- crop(dNBR06_08, Fires_P4) # Initial clip
P4_dNBR06_08 <- mask(P4_dNBR06_08, Fires_P4) # Remove bounding box
# plot(P4_dNBR06_08)
#writeRaster(P4_dNBR06_08, 'P3_dNBR06_08.tif')

### Period 5 ###
# 2010 - 2011
dNBR10_11 <- KI_NBR_fitted$Band_23 - KI_NBR_fitted$Band_24 # Band 23 = 2010, Band 24 = 2011
# plot(dNBR10_11)
P510_11 <- Fires_P5[c(1,11),] # Find polygon(s)
# plot(P510_11)
P5_dNBR10_11 <- crop(dNBR10_11, P510_11) # Initial clip
P5_dNBR10_11 <- mask(P5_dNBR10_11, P510_11) # Remove bounding box
# plot(P5_dNBR10_11)
#writeRaster(P5_dNBR10_11, 'P5_dNBR10_11.tif')

# 2011 - 2013
dNBR11_13 <- KI_NBR_fitted$Band_24 - KI_NBR_fitted$Band_26 # Band 24 = 2011, Band 26 = 2013
# plot(dNBR11_13)
P511_13 <- Fires_P5[3:4,] # Find polygon(s)
# plot(P511_13)
P5_dNBR11_13 <- crop(dNBR11_13, P511_13) # Initial clip
P5_dNBR11_13 <- mask(P5_dNBR11_13, P511_13) # Remove bounding box
# plot(P5_dNBR11_13)
# writeRaster(P5_dNBR11_13, 'P5_dNBR11_13.tif')

# 2014 - 2015
dNBR14_15 <- KI_NBR_fitted$Band_27 - KI_NBR_fitted$Band_28 # Band 27 = 2014, Band 28 = 2015
# plot(dNBR14_15)
P514_15 <- Fires_P5[6,] # Find polygon(s)
# plot(P514_15)
P5_dNBR14_15 <- crop(dNBR14_15, P514_15) # Initial clip
P5_dNBR14_15 <- mask(P5_dNBR14_15, P514_15) # Remove bounding box
# plot(P5_dNBR14_15)
# writeRaster(P5_dNBR14_15, 'P5_dNBR14_15.tif')

# 2015 - 2016
dNBR15_16 <- KI_NBR_fitted$Band_28 - KI_NBR_fitted$Band_29 # Band 28 = 2015, Band 29 = 2016
# plot(dNBR15_16)
P515_16 <- Fires_P5[c(7,9),] # Find polygon(s)
# plot(P515_16)
P5_dNBR15_16 <- crop(dNBR15_16, P515_16) # Initial clip
P5_dNBR15_16 <- mask(P5_dNBR15_16, P515_16) # Remove bounding box
# plot(P5_dNBR15_16)
# writeRaster(P5_dNBR15_16, 'P5_dNBR15_16.tif')

# 2015 - 2017
dNBR15_17 <- KI_NBR_fitted$Band_28 - KI_NBR_fitted$Band_30 # Band 28 = 2015, Band 30 = 2017
# plot(dNBR15_17)
P515_17 <- Fires_P5[5,] # Find polygon(s)
# plot(P515_17)
P5_dNBR15_17 <- crop(dNBR15_17, P515_17) # Initial clip
P5_dNBR15_17 <- mask(P5_dNBR15_17, P515_17) # Remove bounding box
# plot(P5_dNBR15_17)
# writeRaster(P5_dNBR15_17, 'P5_dNBR15_17.tif')

# 2017 - 2018
dNBR17_18 <- KI_NBR_fitted$Band_30 - KI_NBR_fitted$Band_31 # Band 30 = 2017, Band 31 = 2018
# plot(dNBR17_18)
P517_18 <- Fires_P5[c(8,10,13),] # Find polygon(s)
# plot(P517_18)
P5_dNBR17_18 <- crop(dNBR17_18, P517_18) # Initial clip
P5_dNBR17_18 <- mask(P5_dNBR17_18, P517_18) # Remove bounding box
# plot(P5_dNBR17_18)
# writeRaster(P5_dNBR17_18, 'P5_dNBR17_18.tif')

# 2017 - 2019
dNBR17_19 <- KI_NBR_fitted$Band_30 - KI_NBR_fitted$Band_32 # Band 30 = 2017, Band 32 = 2019
# plot(dNBR17_19)
P517_19 <- Fires_P5[c(2,12),] # Find polygon(s)
# plot(P517_19)
P5_dNBR17_19 <- crop(dNBR17_19, P517_19) # Initial clip
P5_dNBR17_19 <- mask(P5_dNBR17_19, P517_19) # Remove bounding box
# plot(P5_dNBR17_19)
# writeRaster(P5_dNBR17_19, 'P5_dNBR17_19.tif')

### Period 6 ###
# 2019 - 2020
dNBR19_20 <-  KI_NBR_fitted$Band_32 - KI_NBR_fitted$Band_33 # Band 32 = 2019, Band 33 = 2020
# plot(dNBR19_20)
# plot(Fires_P6) #All polygons in this period have same dNBR years
P6_dNBR19_20 <- crop(dNBR19_20, Fires_P6) # Initial clip
P6_dNBR19_20 <- mask(P6_dNBR19_20, Fires_P6) # Remove bounding box
# plot(P6_dNBR19_20)
# writeRaster(P6_dNBR19_20, 'P6_dNBR19_20.tif')
