# Script Name:		2017_extract_patch_zonal_stats.r
# Description:		Extract zonal stats for strata patches
# Author:			Ben Bright
# Date:				Oct 2017

library(raster)

zonal.means <- function(indir) {
setwd(indir)
# Create empty dataframe
id = raster("strata_id.tif")
var = c("curv","dist_unburn","dnbr","elev","gsp","map","mat","mmax",
"mmin","nbr1984","nbr1985","nbr1986","nbr1987","nbr1988","nbr1989","nbr1990",
"nbr1991","nbr1992","nbr1993","nbr1994","nbr1995","nbr1996","nbr1997","nbr1998","nbr1999","nbr2000","nbr2001","nbr2002",
"nbr2003","nbr2004","nbr2005","nbr2006","nbr2007","nbr2008","nbr2009","nbr2010","nbr2011","nbr2012","nbr2013","nbr2014",
"nbr2015","nbr2016","slope","strata","tpi","trasp","tri","winp","x","y","count")
nvar = length(var)
nrow = cellStats(id, max)
df = data.frame(matrix(nrow = nrow, ncol = nvar))
names(df) = c(var)
# Populate dataframe
for (i in 1:(nvar-1)) {
r = raster(paste(var[i],".tif",sep=""))
z = zonal(r, id, 'mean')
df[,i]=z[,2]
}
for (i in nvar) {
z = zonal(id, id, 'count')
df[,nvar]=z[,2]
}
write.csv(df,"zonal_means.csv")
}


# Black Mountain 2 and Cooney Ridge
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/black_mountain_2_cooney_ridge/")
date() # 1 min

# Cascade East Zone
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/cascade_east_zone/")
date() # 12 min

# Egley
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/egley/")
date() # 2 min

# Hayman
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/hayman/")
date() # 2 min

# Jasper
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/jasper/")
date() # 1 min

# Old
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/old/")
date() # 1.5 min

# School
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/school/")
date() # 1 min

# Wedge Canyon and Robert
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/wedge_canyon_robert/")
date() # 2 min

# Grand Prix
date()
zm = zonal.means("D:/data/jfsp_recovery/2017/grand_prix/")
date() # 1 min






# Cascade and East Zone NBR trajectories at our ground plots look different, so I'm going to distinguish them for Bright et al. Plus, in the original proposal, Cascade was considered dry mixed conifer and East Zone was considered moist mixed conifer.
zonal.means.separate.cez <- function(indir) {
setwd(indir)
# Create empty dataframe
id = raster("strata_id.tif")
var = c("curv","dist_unburn","dnbr","elev","gsp","map","mat","mmax",
"mmin","nbr1984","nbr1985","nbr1986","nbr1987","nbr1988","nbr1989","nbr1990",
"nbr1991","nbr1992","nbr1993","nbr1994","nbr1995","nbr1996","nbr1997","nbr1998","nbr1999","nbr2000","nbr2001","nbr2002",
"nbr2003","nbr2004","nbr2005","nbr2006","nbr2007","nbr2008","nbr2009","nbr2010","nbr2011","nbr2012","nbr2013","nbr2014",
"nbr2015","nbr2016","slope","strata","tpi","trasp","tri","winp","x","y","distinguish_cascade_east_zone","count")
nvar = length(var)
nrow = cellStats(id, max)
df = data.frame(matrix(nrow = nrow, ncol = nvar))
names(df) = c(var)
# Populate dataframe
for (i in 1:(nvar-1)) {
r = raster(paste(var[i],".tif",sep=""))
z = zonal(r, id, 'mean')
df[,i]=z[,2]
}
for (i in nvar) {
z = zonal(id, id, 'count')
df[,nvar]=z[,2]
}
write.csv(df,"zonal_means_distinguish_cascade_east_zone.csv")
}
# distinguish_cascade_east_zone.tif: 1=East Zone, 2=Cascade

# Cascade East Zone
date()
zm = zonal.means.separate.cez("D:/data/jfsp_recovery/2017/cascade_east_zone/")
date() # 12 min


# Now separate the two
setwd("D:/data/jfsp_recovery/2017/cascade_east_zone/")
cez = read.csv("zonal_means_distinguish_cascade_east_zone.csv")

idx.ez = which(cez$distinguish_cascade_east_zone == 1)
idx.cas = which(cez$distinguish_cascade_east_zone == 2)

c=cez[idx.cas,c(2:51,53)]
ez=cez[idx.ez,c(2:51,53)]

write.csv(c,"zonal_means_cascade.csv")
write.csv(ez,"zonal_means_east_zone.csv")




