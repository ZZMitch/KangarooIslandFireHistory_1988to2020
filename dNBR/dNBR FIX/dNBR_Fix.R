### Fix FOR NO-RECOVERY FIRES (BEFORE MIN = 200) ###

### Inputs ###
library(raster) # Working with rasters
library(sf) # Working with vectors
library(spatialEco) # TPI from here

setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
# setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# NBR Fitted Time-series from 1988 to 2020 #
KI_NBR_fitted <- stack('LandTrendr_Outputs/GEE_Data/Fitted/NBR_Fitted_88to20_UTM53S_1.tif')
KI_NBR_fitted
# crs(KI_NBR_fitted)
# plot(KI_NBR_fitted) # Takes a while to load

Fires_P5 <- st_read('Shapefiles/Fire/Fire Season Periods/Period5_2009to2019/FireHistory_Period5.shp')
Fires_P5 <- st_transform(Fires_P5, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# plot(Fires_P5)
# Period 5: 2009 - 2019
Fires_P6 <- st_read('Shapefiles/Fire/Fire Season Periods/Period6_2020/FireHistory_Period6.shp')
Fires_P6 <- st_transform(Fires_P6, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# plot(Fires_P6)
# Period 6: 2020
# Only 8 variables because no Stop_Year and accidentally deleted 'FINANCIALY' Field

### 1. Recreate Tables with correct dNBR range ###
#####
# P5_1
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_19.tif')
fire_poly <- Fires_P5[2,] # Find polygon
P5_1 <- crop(fire_raster, fire_poly)
P5_1[P5_1 < 99.999] <- NA 
plot(P5_1)
prefire_band = 30 # 2017

# P5_11
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_19.tif')
fire_poly <- Fires_P5[12,] # Find polygon
P5_11 <- crop(fire_raster, fire_poly)
P5_11[P5_11 < 99.999] <- NA
plot(P5_11)
prefire_band = 30 # 2017

# P5_12
fire_raster <- raster('dNBR/Period 5/P5_dNBR17_18.tif')
fire_poly <- Fires_P5[13,] # Find polygon
P5_12 <- crop(fire_raster, fire_poly)
P5_12 <- mask(P5_12, fire_poly) # Another fire in bounding box
P5_12[P5_12 < 99.999] <- NA
plot(P5_12)
prefire_band = 30 # 2017

# P6_0
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[1,] # Find polygon
P6_0 <- crop(fire_raster, fire_poly)
P6_0 <- mask(P6_0, fire_poly) # Another fire in bounding box
P6_0[P6_0 < 99.999] <- NA
plot(P6_0)
prefire_band = 32 # 2019

# P6_1
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[2,] # Find polygon
P6_1 <- crop(fire_raster, fire_poly)
P6_1 <- mask(P6_1, fire_poly) # Another fire in bounding box
P6_1[P6_1 < 99.999] <- NA
plot(P6_1)
prefire_band = 32 # 2019

# P6_2
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[3,] # Find polygon
P6_2 <- crop(fire_raster, fire_poly)
P6_2 <- mask(P6_2, fire_poly) # Another fire in bounding box
P6_2[P6_2 < 99.999] <- NA
plot(P6_2)
prefire_band = 32 # 2019

# P6_3
fire_raster <- raster('dNBR/Period 6/P6_dNBR19_20.tif')
fire_poly <- Fires_P6[4,] # Find polygon
P6_3 <- crop(fire_raster, fire_poly)
P6_3 <- mask(P6_3, fire_poly) # Another fire in bounding box
P6_3[P6_3 < 99.999] <- NA
plot(P6_3)
prefire_band = 32 # 2019

### Build initial table and timeseries data ###
colnames1 = c('Eastings', 'Northings', 'dNBR') 
colnames2 = c('mNBR', "preNBR")

tbl <- as.data.frame(P6_2, xy = TRUE, na.rm = TRUE) # Raster to table, adds coorinates, removes unburned
# Update fire ID each time

colnames(tbl) <- colnames1 # Adds column names for original table
tbl[,colnames2] <- NA # Adds column names to be filled

NBR_all <- extract(KI_NBR_fitted, tbl[,1:2], method = 'simple') # Slow
# Extracts full fitted NBR timeseries (1988-2020) for pixels corresponding to fire

tbl[,4] <- apply(NBR_all[,1:33], 1, max) # Extracts MaxNBR for all pixels
MaxNBR <- tbl[,4]
tbl[,5] <- NBR_all[,prefire_band] # Extracts PreFireNBR for all pixels
PreFireNBR <- tbl[,5] # REMEMBER TO CHANGE PREFIRE BAND #

### Export ###
write.csv(tbl, 'P6_2_dNBR_Fix.csv')
#####

tbl_P5_1 = read.csv("dNBR Fix/P5_1_dNBR_Fix2.csv")
tbl_P5_11 = read.csv("dNBR Fix/P5_11_dNBR_Fix2.csv")
tbl_P5_12 = read.csv("dNBR Fix/P5_12_dNBR_Fix2.csv")
tbl_P6_0 = read.csv("dNBR Fix/P6_0_dNBR_Fix2.csv")
tbl_P6_1 = read.csv("dNBR Fix/P6_1_dNBR_Fix2.csv")
tbl_P6_2 = read.csv("dNBR Fix/P6_2_dNBR_Fix2.csv")
tbl_P6_3 = read.csv("dNBR Fix/P6_3_dNBR_Fix2.csv")

#Test
tbl_P1_0 = read.csv("Fire History/Period 1/P1_0_FireHistory2.csv")

### 1.1. Calculate BExt, mNBR, preNBR ###
#####
# BExt
log10(nrow(tbl_P6_3) * 0.0009) # 0.0009 is size of a single Landsat pixel in km2

#dNBR
mean(tbl_P6_3$dNBR)

#mNBR
mean(tbl_P6_3$mNBR)

#preNBR
mean(tbl_P6_3$preNBR)
#####

### 2. New Burn History Calculations (BL50, BL40, SLB) ###
#####
# Fire Event Polygon and Table #
Fire_shp <- Fires_P6[4,] # Update for each fire
plot(Fire_shp)

# Fire Polygons for individual years #
FireHistory_ByYear <- st_read('Shapefiles/Fire/FireHistory_KangarooIsland_FireYear_UTM53S.shp')

### Create binary fire year rasters ###
# Create blank raster
rasterblank <- raster()
extent(rasterblank) <- extent(Fire_shp)
res(rasterblank) <- 30
crs(rasterblank) <- ' +proj=utm +zone=53 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

# Band 3: 1951
FireHistory_1951 <- FireHistory_ByYear[3,]
FireBinary_1951 <- rasterize(FireHistory_1951, rasterblank, 'Fire')
FireYear_1951 <- rasterize(FireHistory_1951, rasterblank, 'Year')

# Band 4: 1953
FireHistory_1953 <- FireHistory_ByYear[4,]
FireBinary_1953 <- rasterize(FireHistory_1953, rasterblank, 'Fire')
FireYear_1953 <- rasterize(FireHistory_1953, rasterblank, 'Year')

# Band 5: 1954
FireHistory_1954 <- FireHistory_ByYear[5,]
FireBinary_1954 <- rasterize(FireHistory_1954, rasterblank, 'Fire')
FireYear_1954 <- rasterize(FireHistory_1954, rasterblank, 'Year')

# Band 6: 1955
FireHistory_1955 <- FireHistory_ByYear[6,]
FireBinary_1955 <- rasterize(FireHistory_1955, rasterblank, 'Fire')
FireYear_1955 <- rasterize(FireHistory_1955, rasterblank, 'Year')

# Band 7: 1956
FireHistory_1956 <- FireHistory_ByYear[7,]
FireBinary_1956 <- rasterize(FireHistory_1956, rasterblank, 'Fire')
FireYear_1956 <- rasterize(FireHistory_1956, rasterblank, 'Year')

# Band 8: 1958
FireHistory_1958 <- FireHistory_ByYear[8,]
FireBinary_1958 <- rasterize(FireHistory_1958, rasterblank, 'Fire')
FireYear_1958 <- rasterize(FireHistory_1958, rasterblank, 'Year')

# Band 9: 1959
FireHistory_1959 <- FireHistory_ByYear[9,]
FireBinary_1959 <- rasterize(FireHistory_1959, rasterblank, 'Fire')
FireYear_1959 <- rasterize(FireHistory_1959, rasterblank, 'Year')

# Band 10: 1961
FireHistory_1961 <- FireHistory_ByYear[10,]
FireBinary_1961 <- rasterize(FireHistory_1961, rasterblank, 'Fire')
FireYear_1961 <- rasterize(FireHistory_1961, rasterblank, 'Year')

# Band 11: 1963
FireHistory_1963 <- FireHistory_ByYear[11,]
FireBinary_1963 <- rasterize(FireHistory_1963, rasterblank, 'Fire')
FireYear_1963 <- rasterize(FireHistory_1963, rasterblank, 'Year')

# Band 12: 1964
FireHistory_1964 <- FireHistory_ByYear[12,]
FireBinary_1964 <- rasterize(FireHistory_1964, rasterblank, 'Fire')
FireYear_1964 <- rasterize(FireHistory_1964, rasterblank, 'Year')

# Band 13: 1965
FireHistory_1965 <- FireHistory_ByYear[13,]
FireBinary_1965 <- rasterize(FireHistory_1965, rasterblank, 'Fire')
FireYear_1965 <- rasterize(FireHistory_1965, rasterblank, 'Year')

# Band 14: 1969
FireHistory_1969 <- FireHistory_ByYear[14,]
FireBinary_1969 <- rasterize(FireHistory_1969, rasterblank, 'Fire')
FireYear_1969 <- rasterize(FireHistory_1969, rasterblank, 'Year')

# Band 15: 1970
FireHistory_1970 <- FireHistory_ByYear[15,]
FireBinary_1970 <- rasterize(FireHistory_1970, rasterblank, 'Fire')
FireYear_1970 <- rasterize(FireHistory_1970, rasterblank, 'Year')

# Band 16: 1971
FireHistory_1971 <- FireHistory_ByYear[16,]
FireBinary_1971 <- rasterize(FireHistory_1971, rasterblank, 'Fire')
FireYear_1971 <- rasterize(FireHistory_1971, rasterblank, 'Year')

# Band 17: 1974
FireHistory_1974 <- FireHistory_ByYear[17,]
FireBinary_1974 <- rasterize(FireHistory_1974, rasterblank, 'Fire')
FireYear_1974 <- rasterize(FireHistory_1974, rasterblank, 'Year')

# Band 18: 1975
FireHistory_1975 <- FireHistory_ByYear[18,]
FireBinary_1975 <- rasterize(FireHistory_1975, rasterblank, 'Fire')
FireYear_1975 <- rasterize(FireHistory_1975, rasterblank, 'Year')

# Band 19: 1978
FireHistory_1978 <- FireHistory_ByYear[19,]
FireBinary_1978 <- rasterize(FireHistory_1978, rasterblank, 'Fire')
FireYear_1978 <- rasterize(FireHistory_1978, rasterblank, 'Year')

# Band 20: 1979
FireHistory_1979 <- FireHistory_ByYear[20,]
FireBinary_1979 <- rasterize(FireHistory_1979, rasterblank, 'Fire')
FireYear_1979 <- rasterize(FireHistory_1979, rasterblank, 'Year')

# Band 21: 1980
FireHistory_1980 <- FireHistory_ByYear[21,]
FireBinary_1980 <- rasterize(FireHistory_1980, rasterblank, 'Fire')
FireYear_1980 <- rasterize(FireHistory_1980, rasterblank, 'Year')

# Band 22: 1981
FireHistory_1981 <- FireHistory_ByYear[22,]
FireBinary_1981 <- rasterize(FireHistory_1981, rasterblank, 'Fire')
FireYear_1981 <- rasterize(FireHistory_1981, rasterblank, 'Year')

# Band 23: 1982
FireHistory_1982 <- FireHistory_ByYear[23,]
FireBinary_1982 <- rasterize(FireHistory_1982, rasterblank, 'Fire')
FireYear_1982 <- rasterize(FireHistory_1982, rasterblank, 'Year')

# Band 24: 1983
FireHistory_1983 <- FireHistory_ByYear[24,]
FireBinary_1983 <- rasterize(FireHistory_1983, rasterblank, 'Fire')
FireYear_1983 <- rasterize(FireHistory_1983, rasterblank, 'Year')

# Band 25: 1984
FireHistory_1984 <- FireHistory_ByYear[25,]
FireBinary_1984 <- rasterize(FireHistory_1984, rasterblank, 'Fire')
FireYear_1984 <- rasterize(FireHistory_1984, rasterblank, 'Year')

# Band 26: 1985
FireHistory_1985 <- FireHistory_ByYear[26,]
FireBinary_1985 <- rasterize(FireHistory_1985, rasterblank, 'Fire')
FireYear_1985 <- rasterize(FireHistory_1985, rasterblank, 'Year')

# Band 27: 1986
FireHistory_1986 <- FireHistory_ByYear[27,]
FireBinary_1986 <- rasterize(FireHistory_1986, rasterblank, 'Fire')
FireYear_1986 <- rasterize(FireHistory_1986, rasterblank, 'Year')

# Band 28: 1987
FireHistory_1987 <- FireHistory_ByYear[28,]
FireBinary_1987 <- rasterize(FireHistory_1987, rasterblank, 'Fire')
FireYear_1987 <- rasterize(FireHistory_1987, rasterblank, 'Year')

# Band 29: 1988
FireHistory_1988 <- FireHistory_ByYear[29,]
FireBinary_1988 <- rasterize(FireHistory_1988, rasterblank, 'Fire')
FireYear_1988 <- rasterize(FireHistory_1988, rasterblank, 'Year')

# Band 30: 1989
FireHistory_1989 <- FireHistory_ByYear[30,]
FireBinary_1989 <- rasterize(FireHistory_1989, rasterblank, 'Fire')
FireYear_1989 <- rasterize(FireHistory_1989, rasterblank, 'Year')

# Band 31: 1990
FireHistory_1990 <- FireHistory_ByYear[31,]
FireBinary_1990 <- rasterize(FireHistory_1990, rasterblank, 'Fire')
FireYear_1990 <- rasterize(FireHistory_1990, rasterblank, 'Year')

# Band 32: 1991
FireHistory_1991 <- FireHistory_ByYear[32,]
FireBinary_1991 <- rasterize(FireHistory_1991, rasterblank, 'Fire')
FireYear_1991 <- rasterize(FireHistory_1991, rasterblank, 'Year')

# Band 33: 1992
FireHistory_1992 <- FireHistory_ByYear[33,]
FireBinary_1992 <- rasterize(FireHistory_1992, rasterblank, 'Fire')
FireYear_1992 <- rasterize(FireHistory_1992, rasterblank, 'Year')

# Band 34: 1993
FireHistory_1993 <- FireHistory_ByYear[34,]
FireBinary_1993 <- rasterize(FireHistory_1993, rasterblank, 'Fire')
FireYear_1993 <- rasterize(FireHistory_1993, rasterblank, 'Year')

# Band 35: 1994
FireHistory_1994 <- FireHistory_ByYear[35,]
FireBinary_1994 <- rasterize(FireHistory_1994, rasterblank, 'Fire')
FireYear_1994 <- rasterize(FireHistory_1994, rasterblank, 'Year')

# Band 36: 1995
FireHistory_1995 <- FireHistory_ByYear[36,]
FireBinary_1995 <- rasterize(FireHistory_1995, rasterblank, 'Fire')
FireYear_1995 <- rasterize(FireHistory_1995, rasterblank, 'Year')

# Band 37: 1997
FireHistory_1997 <- FireHistory_ByYear[37,]
FireBinary_1997 <- rasterize(FireHistory_1997, rasterblank, 'Fire')
FireYear_1997 <- rasterize(FireHistory_1997, rasterblank, 'Year')

# Band 38: 2001
FireHistory_2001 <- FireHistory_ByYear[38,]
FireBinary_2001 <- rasterize(FireHistory_2001, rasterblank, 'Fire')
FireYear_2001 <- rasterize(FireHistory_2001, rasterblank, 'Year')

# Band 39: 2002
FireHistory_2002 <- FireHistory_ByYear[39,]
FireBinary_2002 <- rasterize(FireHistory_2002, rasterblank, 'Fire')
FireYear_2002 <- rasterize(FireHistory_2002, rasterblank, 'Year')

# Band 40: 2003
FireHistory_2003 <- FireHistory_ByYear[40,]
FireBinary_2003 <- rasterize(FireHistory_2003, rasterblank, 'Fire')
FireYear_2003 <- rasterize(FireHistory_2003, rasterblank, 'Year')

# Band 41: 2004
FireHistory_2004 <- FireHistory_ByYear[41,]
FireBinary_2004 <- rasterize(FireHistory_2004, rasterblank, 'Fire')
FireYear_2004 <- rasterize(FireHistory_2004, rasterblank, 'Year')

# Band 42: 2005
FireHistory_2005 <- FireHistory_ByYear[42,]
FireBinary_2005 <- rasterize(FireHistory_2005, rasterblank, 'Fire')
FireYear_2005 <- rasterize(FireHistory_2005, rasterblank, 'Year')

# Band 43: 2006
FireHistory_2006 <- FireHistory_ByYear[43,]
FireBinary_2006 <- rasterize(FireHistory_2006, rasterblank, 'Fire')
FireYear_2006 <- rasterize(FireHistory_2006, rasterblank, 'Year')

# Band 44: 2007
FireHistory_2007 <- FireHistory_ByYear[44,]
FireBinary_2007 <- rasterize(FireHistory_2007, rasterblank, 'Fire')
FireYear_2007 <- rasterize(FireHistory_2007, rasterblank, 'Year')

# Band 45: 2008
FireHistory_2008 <- FireHistory_ByYear[45,]
FireBinary_2008 <- rasterize(FireHistory_2008, rasterblank, 'Fire')
FireYear_2008 <- rasterize(FireHistory_2008, rasterblank, 'Year')

# Band 46: 2009
FireHistory_2009 <- FireHistory_ByYear[46,]
FireBinary_2009 <- rasterize(FireHistory_2009, rasterblank, 'Fire')
FireYear_2009 <- rasterize(FireHistory_2009, rasterblank, 'Year')

# Band 47: 2010
FireHistory_2010 <- FireHistory_ByYear[47,]
FireBinary_2010 <- rasterize(FireHistory_2010, rasterblank, 'Fire')
FireYear_2010 <- rasterize(FireHistory_2010, rasterblank, 'Year')

# Band 48: 2011
FireHistory_2011 <- FireHistory_ByYear[48,]
FireBinary_2011 <- rasterize(FireHistory_2011, rasterblank, 'Fire')
FireYear_2011 <- rasterize(FireHistory_2011, rasterblank, 'Year')

# Band 49: 2012
FireHistory_2012 <- FireHistory_ByYear[49,]
FireBinary_2012 <- rasterize(FireHistory_2012, rasterblank, 'Fire')
FireYear_2012 <- rasterize(FireHistory_2012, rasterblank, 'Year')

# Band 50: 2013
FireHistory_2013 <- FireHistory_ByYear[50,]
FireBinary_2013 <- rasterize(FireHistory_2013, rasterblank, 'Fire')
FireYear_2013 <- rasterize(FireHistory_2013, rasterblank, 'Year')

# Band 51: 2014
FireHistory_2014 <- FireHistory_ByYear[51,]
FireBinary_2014 <- rasterize(FireHistory_2014, rasterblank, 'Fire')
FireYear_2014 <- rasterize(FireHistory_2014, rasterblank, 'Year')

# Band 52: 2015
FireHistory_2015 <- FireHistory_ByYear[52,]
FireBinary_2015 <- rasterize(FireHistory_2015, rasterblank, 'Fire')
FireYear_2015 <- rasterize(FireHistory_2015, rasterblank, 'Year')

# Band 53: 2016
FireHistory_2016 <- FireHistory_ByYear[53,]
FireBinary_2016 <- rasterize(FireHistory_2016, rasterblank, 'Fire')
FireYear_2016 <- rasterize(FireHistory_2016, rasterblank, 'Year')

# Band 54: 2017
FireHistory_2017 <- FireHistory_ByYear[54,]
FireBinary_2017 <- rasterize(FireHistory_2017, rasterblank, 'Fire')
FireYear_2017 <- rasterize(FireHistory_2017, rasterblank, 'Year')

# Band 55: 2018
FireHistory_2018 <- FireHistory_ByYear[55,]
FireBinary_2018 <- rasterize(FireHistory_2018, rasterblank, 'Fire')
FireYear_2018 <- rasterize(FireHistory_2018, rasterblank, 'Year')

# Band 56: 2019
FireHistory_2019 <- FireHistory_ByYear[56,]
FireBinary_2019 <- rasterize(FireHistory_2019, rasterblank, 'Fire')
FireYear_2019 <- rasterize(FireHistory_2019, rasterblank, 'Year')

Fire_tbl <- tbl_P6_3 # Update for each fire

### Sum rasters together ###
CurrentFireYear <- 2020 # Update for each fire

# Number of fires in last 50 years (1970 - 2019) # Update for each fire
FireBinary_Stack50 <- stack(FireBinary_1970, FireBinary_1971, FireBinary_1974, 
                            FireBinary_1975, FireBinary_1978, FireBinary_1979, FireBinary_1980, FireBinary_1981, 
                            FireBinary_1982, FireBinary_1983, FireBinary_1984, FireBinary_1985, FireBinary_1986, 
                            FireBinary_1987, FireBinary_1988, FireBinary_1989, FireBinary_1990, FireBinary_1991,
                            FireBinary_1992, FireBinary_1993, FireBinary_1994, FireBinary_1995, FireBinary_1997,
                            FireBinary_2001, FireBinary_2002, FireBinary_2003, FireBinary_2004, FireBinary_2005,
                            FireBinary_2006, FireBinary_2007, FireBinary_2008, FireBinary_2009, FireBinary_2010,
                            FireBinary_2011, FireBinary_2012, FireBinary_2013, FireBinary_2014, FireBinary_2015,
                            FireBinary_2016, FireBinary_2017, FireBinary_2018, FireBinary_2019) 
FireBinary_Sum50 <- calc(FireBinary_Stack50, sum, na.rm = TRUE) # Sum of fires
plot(FireBinary_Sum50)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$BL50 <- extract(FireBinary_Sum50, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

# Number of Fires in last 40 years before fire (1980 - 2019) # Update for each fire
FireBinary_Stack40 <- stack(FireBinary_1980, FireBinary_1981, 
                            FireBinary_1982, FireBinary_1983, FireBinary_1984, FireBinary_1985, FireBinary_1986, 
                            FireBinary_1987, FireBinary_1988, FireBinary_1989, FireBinary_1990, FireBinary_1991,
                            FireBinary_1992, FireBinary_1993, FireBinary_1994, FireBinary_1995, FireBinary_1997,
                            FireBinary_2001, FireBinary_2002, FireBinary_2003, FireBinary_2004, FireBinary_2005,
                            FireBinary_2006, FireBinary_2007, FireBinary_2008, FireBinary_2009, FireBinary_2010,
                            FireBinary_2011, FireBinary_2012, FireBinary_2013, FireBinary_2014, FireBinary_2015,
                            FireBinary_2016, FireBinary_2017, FireBinary_2018, FireBinary_2019) 
FireBinary_Sum40 <- calc(FireBinary_Stack40, sum, na.rm = TRUE) # Sum of fires
plot(FireBinary_Sum40)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$BL40 <- extract(FireBinary_Sum40, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

### Years since last fire ### # (2020) Update for each fire
FireYear_Stack <- stack(FireYear_1951, FireYear_1953, FireYear_1954, FireYear_1955, FireYear_1956, 
                        FireYear_1958, FireYear_1959, FireYear_1961, FireYear_1963, FireYear_1964, 
                        FireYear_1965, FireYear_1969, FireYear_1970, FireYear_1971, FireYear_1974, 
                        FireYear_1975, FireYear_1978, FireYear_1979, FireYear_1980, FireYear_1981, 
                        FireYear_1982, FireYear_1983, FireYear_1984, FireYear_1985, FireYear_1986, 
                        FireYear_1987, FireYear_1988, FireYear_1989, FireYear_1990, FireYear_1991,
                        FireYear_1992, FireYear_1993, FireYear_1994, FireYear_1995, FireYear_1997,
                        FireYear_2001, FireYear_2002, FireYear_2003, FireYear_2004, FireYear_2005,
                        FireYear_2006, FireYear_2007, FireYear_2008, FireYear_2009, FireYear_2010,
                        FireYear_2011, FireYear_2012, FireYear_2013, FireYear_2014, FireYear_2015,
                        FireYear_2016, FireYear_2017, FireYear_2018, FireYear_2019)
FireYear_Max <- calc(FireYear_Stack, max) # Most recent fire year
YearSinceBurn <- CurrentFireYear - FireYear_Max # Current Year - Most recent fire year
plot(YearSinceBurn)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$SLB <- extract(YearSinceBurn, Fire_tbl[,2:3]) # Get fire counts based on burned pixels
Fire_tbl$SLB[is.na(Fire_tbl$SLB)] = 70 # Pixels with no previous fire given value of 70 (>70 yrs since fire)

### Export updated table ###
write.csv(Fire_tbl, 'P6_3_dNBR_Fix1.csv') # Update for each fire
#####

### 2.1 Calculate BL50, BL40, SLB ###
#####
# BL50
mean(tbl_P6_3$BL50)

# BL40
mean(tbl_P6_3$BL40)

# SLB
mean(tbl_P6_3$SLB)
#####

### 3. New Topography Calculations (ELV, SLP, ASP, TPIX, TWI) ###
#####
### DEM ###
DEM <- raster('DEM/Simple Boundary/KISimple_DEM1.tif')
plot(DEM)

### Slope & Aspect ###
Elev_Slope <- terrain(DEM, opt = 'slope', unit = "degrees")
plot(Elev_Slope)

Aspect <- terrain(DEM, opt = 'aspect', unit = "degrees")
plot(Aspect)

### TPI ### 
TPI_3x3 <- tpi(DEM, scale = 3, win = "rectangle") 
plot(TPI_3x3)

TPI_5x5 <- tpi(DEM, scale = 5, win = "rectangle") 
plot(TPI_5x5)

TPI_9x9 <- tpi(DEM, scale = 9, win = "rectangle") 
plot(TPI_9x9)

TPI_15x15 <- tpi(DEM, scale = 15, win = "rectangle") # Same window size as MSc project
plot(TPI_15x15)

TPI_31x31 <- tpi(DEM, scale = 31, win = "rectangle") 
plot(TPI_31x31)

TPI_65x65 <- tpi(DEM, scale = 65, win = "rectangle") 
plot(TPI_65x65)

TPI_111x111 <- tpi(DEM, scale = 111, win = "rectangle") 
plot(TPI_111x111)

### TWI ###
TWI <- raster('DEM/TWI_SAGA_DEMfilled1.tif')
plot(TWI)

Fire_shp <- Fires_P6[4,] # Update for each fire
Fire_tbl <- tbl_P6_3 # Update for each fire

# Elev
zoom(DEM, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$ELV <- extract(DEM, Fire_tbl[,3:4]) # Add to fire table

# Elev_Slope
zoom(Elev_Slope, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$SLP <- extract(Elev_Slope, Fire_tbl[,3:4]) # Add to fire table

# Aspect
zoom(Aspect, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$ASP <- extract(Aspect, Fire_tbl[,3:4]) # Add to fire table

# TPI3
zoom(TPI_3x3, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TPI3 <- extract(TPI_3x3, Fire_tbl[,3:4]) # Add to fire table

# TPI5
zoom(TPI_5x5, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TPI5 <- extract(TPI_5x5, Fire_tbl[,3:4]) # Add to fire table

# TPI9
zoom(TPI_9x9, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TPI9 <- extract(TPI_9x9, Fire_tbl[,3:4]) # Add to fire table

# TPI15
zoom(TPI_15x15, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TPI15 <- extract(TPI_15x15, Fire_tbl[,3:4]) # Add to fire table

# TPI31
zoom(TPI_31x31, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TPI31 <- extract(TPI_31x31, Fire_tbl[,3:4]) # Add to fire table

# TPI65
zoom(TPI_65x65, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TPI65 <- extract(TPI_65x65, Fire_tbl[,3:4]) # Add to fire table

# TPI111
zoom(TPI_111x111, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TPI111 <- extract(TPI_111x111, Fire_tbl[,3:4]) # Add to fire table

# TWI
zoom(TWI, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TWI <- extract(TWI, Fire_tbl[,3:4]) # Add to fire table

### Export updated table ###
write.csv(Fire_tbl, 'P6_3_dNBR_Fix2.csv') # Update for each fire
#####

### 3.1 Calculate ELV, SLP, ASP, TPIX, TWI ###
#####
# ELV
mean(tbl_P6_3$ELV)

# SLP
mean(tbl_P6_3$SLP)

# ASP
mean(tbl_P6_3$ASP)

# TPI3
mean(tbl_P6_3$TPI3)

# TPI5
mean(tbl_P6_3$TPI5)

# TPI9
mean(tbl_P6_3$TPI9)

# TPI15
mean(tbl_P6_3$TPI15)

# TPI31
mean(tbl_P6_3$TPI31)

# TPI65
mean(tbl_P6_3$TPI65, na.rm = TRUE)

# TPI111
mean(tbl_P6_3$TPI111, na.rm = TRUE)

# TWI
mean(tbl_P6_3$TWI, na.rm = TRUE)
#####