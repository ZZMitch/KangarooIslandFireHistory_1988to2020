### Burn History (Fire binaries, Years since last fire ###)

### Inputs ###
library(raster) # Working with rasters
library(sf) # Working with vectors
setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
# setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# Fire History Shapefiles from 1989 to 2020
# NBR Fitted Time-series from 1988 to 2020 #
KI_NBR_fitted <- stack('LandTrendr_Outputs/GEE_Data/Fitted/NBR_Fitted_88to20_UTM53S_1.tif')
KI_NBR_fitted

# Period 1: 1989 - 1992 
# Fires_P1 <- st_read('Shapefiles/Fire/Fire Season Periods/Period1_1989to1992/FireHistory_Period1.shp')
# Fires_P1 <- st_transform(Fires_P1, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 2: 1993 - 1997
#Fires_P2 <- st_read('Shapefiles/Fire/Fire Season Periods/Period2_1993to1997/FireHistory_Period2.shp')
# Fires_P2 <- st_transform(Fires_P2, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 3: 1998 - 2007
# Fires_P3 <- st_read('Shapefiles/Fire/Fire Season Periods/Period3_1998to2007/FireHistory_Period3.shp')
# Fires_P3 <- st_transform(Fires_P3, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 4: 2008
# Fires_P4 <- st_read('Shapefiles/Fire/Fire Season Periods/Period4_2008/FireHistory_Period4.shp')
# Fires_P4 <- st_transform(Fires_P4, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 5: 2009 - 2019
# Fires_P5 <- st_read('Shapefiles/Fire/Fire Season Periods/Period5_2009to2019/FireHistory_Period5.shp')
# Fires_P5 <- st_transform(Fires_P5, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 6: 2020
Fires_P6 <- st_read('Shapefiles/Fire/Fire Season Periods/Period6_2020/FireHistory_Period6.shp')
Fires_P6 <- st_transform(Fires_P6, crs(KI_NBR_fitted)) # Change projection to UTM 53S

# Fire Event Polygon and Table #
Fire_shp <- Fires_P6[4,] # Update for each fire
plot(Fire_shp)

Fire_tbl <- read.csv('NBR Recovery/Period 6/P6_3_NBRecovery3.csv') # Update for each fire

# Fire Polygons for individual years #
FireHistory_ByYear <- st_read('Shapefiles/Fire/FireHistory_KangarooIsland_FireYear_UTM53S.shp')

### Create binary fire year rasters ###
# Create blank raster
rasterblank <- raster()
extent(rasterblank) <- extent(Fire_shp)
res(rasterblank) <- 30
crs(rasterblank) <- ' +proj=utm +zone=53 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

# Band 1: 1932 - Not included: likely fires missed in between
# FireHistory_1932 <- FireHistory_ByYear[1,]
# FireBinary_1932 <- rasterize(FireHistory_1932, rasterblank, 'Fire')
# FireYear_1932 <- rasterize(FireHistory_1932, rasterblank, 'Year')

# Band 2: 1940 - Not included: likely fires missed in between
# FireHistory_1940 <- FireHistory_ByYear[2,]
# FireBinary_1940 <- rasterize(FireHistory_1940, rasterblank, 'Fire')
# FireYear_1940 <- rasterize(FireHistory_1940, rasterblank, 'Year')

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

# Band 57: 2020

### Sum rasters together ###
CurrentFireYear <- 2020 # Update for each fire

# Number of fires in last 70 years (1950 - 2019) # Update for each fire
FireBinary_Stack70 <- stack(FireBinary_1951, FireBinary_1953, FireBinary_1954, FireBinary_1955, FireBinary_1956,
                            FireBinary_1958, FireBinary_1959, FireBinary_1961, FireBinary_1963, FireBinary_1964, 
                            FireBinary_1965, FireBinary_1969, FireBinary_1970, FireBinary_1971, FireBinary_1974, 
                            FireBinary_1975, FireBinary_1978, FireBinary_1979, FireBinary_1980, FireBinary_1981, 
                            FireBinary_1982, FireBinary_1983, FireBinary_1984, FireBinary_1985, FireBinary_1986, 
                            FireBinary_1987, FireBinary_1988, FireBinary_1989, FireBinary_1990, FireBinary_1991,
                            FireBinary_1992, FireBinary_1993, FireBinary_1994, FireBinary_1995, FireBinary_1997,
                            FireBinary_2001, FireBinary_2002, FireBinary_2003, FireBinary_2004, FireBinary_2005,
                            FireBinary_2006, FireBinary_2007, FireBinary_2008, FireBinary_2009, FireBinary_2010,
                            FireBinary_2011, FireBinary_2012, FireBinary_2013, FireBinary_2014, FireBinary_2015,
                            FireBinary_2016, FireBinary_2017, FireBinary_2018, FireBinary_2019) 
FireBinary_Sum70 <- calc(FireBinary_Stack70, sum, na.rm = TRUE) # Sum of fires
plot(FireBinary_Sum70)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$Burned_Last70 <- extract(FireBinary_Sum70, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

# Number of fires in last 60 years (1960 - 2019) # Update for each fire
FireBinary_Stack60 <- stack(FireBinary_1961, FireBinary_1963, FireBinary_1964, 
                            FireBinary_1965, FireBinary_1969, FireBinary_1970, FireBinary_1971, FireBinary_1974, 
                            FireBinary_1975, FireBinary_1978, FireBinary_1979, FireBinary_1980, FireBinary_1981, 
                            FireBinary_1982, FireBinary_1983, FireBinary_1984, FireBinary_1985, FireBinary_1986, 
                            FireBinary_1987, FireBinary_1988, FireBinary_1989, FireBinary_1990, FireBinary_1991,
                            FireBinary_1992, FireBinary_1993, FireBinary_1994, FireBinary_1995, FireBinary_1997,
                            FireBinary_2001, FireBinary_2002, FireBinary_2003, FireBinary_2004, FireBinary_2005,
                            FireBinary_2006, FireBinary_2007, FireBinary_2008, FireBinary_2009, FireBinary_2010,
                            FireBinary_2011, FireBinary_2012, FireBinary_2013, FireBinary_2014, FireBinary_2015,
                            FireBinary_2016, FireBinary_2017, FireBinary_2018, FireBinary_2019) 
FireBinary_Sum60 <- calc(FireBinary_Stack60, sum, na.rm = TRUE) # Sum of fires
plot(FireBinary_Sum60)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$Burned_Last60 <- extract(FireBinary_Sum60, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

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
Fire_tbl$Burned_Last50 <- extract(FireBinary_Sum50, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

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
Fire_tbl$Burned_Last40 <- extract(FireBinary_Sum40, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

# Number of fires in last 30 years before fire (1990 - 2019) # Update for each fire
FireBinary_Stack30 <- stack(FireBinary_1990, FireBinary_1991,
                            FireBinary_1992, FireBinary_1993, FireBinary_1994, FireBinary_1995, FireBinary_1997,
                            FireBinary_2001, FireBinary_2002, FireBinary_2003, FireBinary_2004, FireBinary_2005,
                            FireBinary_2006, FireBinary_2007, FireBinary_2008, FireBinary_2009, FireBinary_2010,
                            FireBinary_2011, FireBinary_2012, FireBinary_2013, FireBinary_2014, FireBinary_2015,
                            FireBinary_2016, FireBinary_2017, FireBinary_2018, FireBinary_2019)
FireBinary_Sum30 <- calc(FireBinary_Stack30, sum, na.rm = TRUE) # Sum of fires
plot(FireBinary_Sum30)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$Burned_Last30 <- extract(FireBinary_Sum30, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

# Number of fires in last 20 years before fire (2000 - 2019) # Update for each fire
FireBinary_Stack20 <- stack(FireBinary_2001, FireBinary_2002, FireBinary_2003, FireBinary_2004, FireBinary_2005,
                            FireBinary_2006, FireBinary_2007, FireBinary_2008, FireBinary_2009, FireBinary_2010,
                            FireBinary_2011, FireBinary_2012, FireBinary_2013, FireBinary_2014, FireBinary_2015,
                            FireBinary_2016, FireBinary_2017, FireBinary_2018, FireBinary_2019)
FireBinary_Sum20 <- calc(FireBinary_Stack20, sum, na.rm = TRUE) # Sum of fires
plot(FireBinary_Sum20)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$Burned_Last20 <- extract(FireBinary_Sum20, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

# Number of fires in last 10 years before fire (2010 - 2019) # Update for each fire
FireBinary_Stack10 <- stack(FireBinary_2010,
                            FireBinary_2011, FireBinary_2012, FireBinary_2013, FireBinary_2014, FireBinary_2015,
                            FireBinary_2016, FireBinary_2017, FireBinary_2018, FireBinary_2019)
FireBinary_Sum10 <- calc(FireBinary_Stack10, sum, na.rm = TRUE) # Sum of fires
plot(FireBinary_Sum10)
points(Fire_tbl$Eastings, Fire_tbl$Northings, cex = 0.5) # Plots burned pixels
Fire_tbl$Burned_Last10 <- extract(FireBinary_Sum10, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

### Years since last fire ### # Update for each fire
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
Fire_tbl$SinceLastBurn <- extract(YearSinceBurn, Fire_tbl[,2:3]) # Get fire counts based on burned pixels

### Export updated table ###
write.csv(Fire_tbl, 'P6_3_FireHistory.csv') # Update for each fire
