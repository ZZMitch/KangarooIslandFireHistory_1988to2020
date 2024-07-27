### Calculates TPI and TWI for Kangaroo Island DEM ###

### Inputs ###
library(raster) # Working with rasters
library(spatialEco) # TPI from here
library(sf) # Working with vectors

setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
# setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

KI_NBR_fitted <- stack('LandTrendr_Outputs/GEE_Data/Fitted/NBR_Fitted_88to20_UTM53S_1.tif')
KI_NBR_fitted

### DEM ###
DEM <- raster('DEM/Simple Boundary/KISimple_DEM1.tif')
plot(DEM)

Elev3 <- focal(DEM, w = matrix(1,3,3), fun = mean) # 3x3 average
plot(Elev3)

### Slope & Aspect ###
Elev_Slope <- terrain(DEM, opt = 'slope', unit = "degrees")
plot(Elev_Slope)

Elev_Slope3 <- focal(Elev_Slope, w = matrix(1,3,3), fun = mean) # 3x3 average
plot(Elev_Slope3)

# Slope_rad <- terrain(DEM, opt = 'slope', unit = "radians")
# plot(Slope_rad)

Aspect <- terrain(DEM, opt = 'aspect', unit = "degrees")
plot(Aspect)

Aspect3 <- focal(Aspect, w = matrix(1,3,3), fun = mean) # 3x3 average
plot(Aspect3)

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

TWI3 <- focal(TWI, w = matrix(1,3,3), fun = mean) # 3x3 average
plot(TWI3)

### Add Values to Fire History Tables ###
# Fire History Shapefiles from 1989 to 2020
# Period 1: 1989 - 1992 
# Fires_P1 <- st_read('Shapefiles/Fire/Fire Season Periods/Period1_1989to1992/FireHistory_Period1.shp')
# Fires_P1 <- st_transform(Fires_P1, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 2: 1993 - 1997
# Fires_P2 <- st_read('Shapefiles/Fire/Fire Season Periods/Period2_1993to1997/FireHistory_Period2.shp')
# Fires_P2 <- st_transform(Fires_P2, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 3: 1998 - 2007
# Fires_P3 <- st_read('Shapefiles/Fire/Fire Season Periods/Period3_1998to2007/FireHistory_Period3.shp')
# Fires_P3 <- st_transform(Fires_P3, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 4: 2008
Fires_P4 <- st_read('Shapefiles/Fire/Fire Season Periods/Period4_2008/FireHistory_Period4.shp')
Fires_P4 <- st_transform(Fires_P4, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 5: 2009 - 2019
Fires_P5 <- st_read('Shapefiles/Fire/Fire Season Periods/Period5_2009to2019/FireHistory_Period5.shp')
Fires_P5 <- st_transform(Fires_P5, crs(KI_NBR_fitted)) # Change projection to UTM 53S
# Period 6: 2020
# Fires_P6 <- st_read('Shapefiles/Fire/Fire Season Periods/Period6_2020/FireHistory_Period6.shp')
# Fires_P6 <- st_transform(Fires_P6, crs(KI_NBR_fitted)) # Change projection to UTM 53S

Fire_shp <- Fires_P4[1,] # Update for each fire
Fire_tbl <- read.csv('Fire History/Period 6/P6_3_FireHistory.csv') # Update for each fire

# Elev
zoom(DEM, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$Elev <- extract(DEM, Fire_tbl[,3:4]) # Add to fire table

# Elev3
zoom(Elev3, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$Elev3 <- extract(Elev3, Fire_tbl[,3:4]) # Add to fire table

# Elev_Slope
zoom(Elev_Slope, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$Elev_Slope <- extract(Elev_Slope, Fire_tbl[,3:4]) # Add to fire table

# Elev_Slope3
zoom(Elev_Slope3, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$Elev_Slope3 <- extract(Elev_Slope3, Fire_tbl[,3:4]) # Add to fire table

# Aspect
zoom(Aspect, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$Aspect <- extract(Aspect, Fire_tbl[,3:4]) # Add to fire table

# Aspect3
zoom(Aspect3, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$Aspect3 <- extract(Aspect3, Fire_tbl[,3:4]) # Add to fire table

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

# TWI3
zoom(TWI3, ext = Fire_shp)
plot(Fire_shp, col = NA, add = TRUE)
Fire_tbl$TWI3 <- extract(TWI3, Fire_tbl[,3:4]) # Add to fire table

### Export updated table ###
write.csv(Fire_tbl, 'P6_3_FireHistory1.csv') # Update for each fire
