### Inputs ###
setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
# setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

library(raster) # Working with rasters
library(sf) # Working with vectors

### Forest % for all fire events ###
landcover = st_read('Shapefiles/Landuse/LandCover_KangarooIsland.shp')

rasterblank <- raster()
extent(rasterblank) <- extent(landcover)
res(rasterblank) <- 30
crs(rasterblank) <- ' +proj=utm +zone=53 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

landcoverr <- rasterize(landcover, rasterblank, 'LU_Class')
plot(landcoverr)
