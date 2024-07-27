# Script Name:		2017_veg_recovery_analysis.r
# Description:		Analyze NBR trajectories
# Author:			Ben Bright
# Date:				Oct 2017 (for AFE in Orlando) and 2018 (for paper)

setwd("D:/data/jfsp_recovery/2017/")

prep.for.combining <- function(zonal.mean.csv, fire.name, fire.year, nbr.fire.threshold) {

z = read.csv(zonal.mean.csv)

# Only want to analyze burned patches
z = z[which(z$strata>111 & z$strata <500),]

z$fire = fire.name
z$fire.year = fire.year
z$yrs.since.fire = 2016-fire.year

# Pre-fire NBR
if (fire.year == 2003) z$pre.fire.nbr = z$nbr2003 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$pre.fire.nbr = z$nbr2006 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$pre.fire.nbr = z$nbr2001
if (fire.year == 2000) z$pre.fire.nbr = z$nbr2000
if (fire.year == 2005) z$pre.fire.nbr = z$nbr2004 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$pre.fire.nbr = z$nbr2002 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Disturbance magnitude "dmag" - like dnbr but using fitted LandTrendr values so slightly different
if (fire.year == 2003) z$dmag = z$pre.fire.nbr - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$dmag = z$pre.fire.nbr - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$dmag = z$pre.fire.nbr - z$nbr2002
if (fire.year == 2000) z$dmag = z$pre.fire.nbr - z$nbr2001
if (fire.year == 2005) z$dmag = z$pre.fire.nbr - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$dmag = z$pre.fire.nbr - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Get rid of some areas that LandTrendr didn't detect as burned with MTBS dnbr threshold
z = z[which(z$dmag > nbr.fire.threshold),]

# Recovery magnitude "rmag" in terms of dnbr in 2016
if (fire.year == 2003) z$rmag2016 = z$nbr2016 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag2016 = z$nbr2016 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag2016 = z$nbr2016 - z$nbr2002
if (fire.year == 2000) z$rmag2016 = z$nbr2016 - z$nbr2001
if (fire.year == 2005) z$rmag2016 = z$nbr2016 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag2016 = z$nbr2016 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr in 2010
if (fire.year == 2003) z$rmag2010 = z$nbr2010 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag2010 = z$nbr2010 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag2010 = z$nbr2010 - z$nbr2002
if (fire.year == 2000) z$rmag2010 = z$nbr2010 - z$nbr2001
if (fire.year == 2005) z$rmag2010 = z$nbr2010 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag2010 = z$nbr2010 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire


# Recovery magnitude "rmag" in terms of dnbr 2 years post fire
if (fire.year == 2003) z$rmag2yrs = z$nbr2005 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag2yrs = z$nbr2009 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag2yrs = z$nbr2003 - z$nbr2002
if (fire.year == 2000) z$rmag2yrs = z$nbr2002 - z$nbr2001
if (fire.year == 2005) z$rmag2yrs = z$nbr2007 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag2yrs = z$nbr2005 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 3 years post fire
if (fire.year == 2003) z$rmag3yrs = z$nbr2006 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag3yrs = z$nbr2010 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag3yrs = z$nbr2004 - z$nbr2002
if (fire.year == 2000) z$rmag3yrs = z$nbr2003 - z$nbr2001
if (fire.year == 2005) z$rmag3yrs = z$nbr2008 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag3yrs = z$nbr2006 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 4 years post fire
if (fire.year == 2003) z$rmag4yrs = z$nbr2007 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag4yrs = z$nbr2011 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag4yrs = z$nbr2005 - z$nbr2002
if (fire.year == 2000) z$rmag4yrs = z$nbr2004 - z$nbr2001
if (fire.year == 2005) z$rmag4yrs = z$nbr2009 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag4yrs = z$nbr2007 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 5 years post fire
if (fire.year == 2003) z$rmag5yrs = z$nbr2008 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag5yrs = z$nbr2012 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag5yrs = z$nbr2006 - z$nbr2002
if (fire.year == 2000) z$rmag5yrs = z$nbr2005 - z$nbr2001
if (fire.year == 2005) z$rmag5yrs = z$nbr2010 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag5yrs = z$nbr2008 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 6 years post fire
if (fire.year == 2003) z$rmag6yrs = z$nbr2009 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag6yrs = z$nbr2013 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag6yrs = z$nbr2007 - z$nbr2002
if (fire.year == 2000) z$rmag6yrs = z$nbr2006 - z$nbr2001
if (fire.year == 2005) z$rmag6yrs = z$nbr2011 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag6yrs = z$nbr2009 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 7 years post fire
if (fire.year == 2003) z$rmag7yrs = z$nbr2010 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag7yrs = z$nbr2014 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag7yrs = z$nbr2008 - z$nbr2002
if (fire.year == 2000) z$rmag7yrs = z$nbr2007 - z$nbr2001
if (fire.year == 2005) z$rmag7yrs = z$nbr2012 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag7yrs = z$nbr2010 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 8 years post fire
if (fire.year == 2003) z$rmag8yrs = z$nbr2011 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag8yrs = z$nbr2015 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag8yrs = z$nbr2009 - z$nbr2002
if (fire.year == 2000) z$rmag8yrs = z$nbr2008 - z$nbr2001
if (fire.year == 2005) z$rmag8yrs = z$nbr2013 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag8yrs = z$nbr2011 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 9 years post fire
if (fire.year == 2003) z$rmag9yrs = z$nbr2012 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag9yrs = z$nbr2016 - z$nbr2008 # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag9yrs = z$nbr2010 - z$nbr2002
if (fire.year == 2000) z$rmag9yrs = z$nbr2009 - z$nbr2001
if (fire.year == 2005) z$rmag9yrs = z$nbr2014 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag9yrs = z$nbr2012 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 10 years post fire
if (fire.year == 2003) z$rmag10yrs = z$nbr2013 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag10yrs = NA # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag10yrs = z$nbr2011 - z$nbr2002
if (fire.year == 2000) z$rmag10yrs = z$nbr2010 - z$nbr2001
if (fire.year == 2005) z$rmag10yrs = z$nbr2015 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag10yrs = z$nbr2013 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 11 years post fire
if (fire.year == 2003) z$rmag11yrs = z$nbr2014 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag11yrs = NA # Fire spread over 2007-08 in CEZ & Egley NBR imagery, so use 2006 as prefire, 2008 as postfire
if (fire.year == 2002) z$rmag11yrs = z$nbr2012 - z$nbr2002
if (fire.year == 2000) z$rmag11yrs = z$nbr2011 - z$nbr2001
if (fire.year == 2005) z$rmag11yrs = z$nbr2016 - z$nbr2006 # Fire spread over 2005,2006 in SCHOOL, so use 2004 as prefire
if (fire.name == "WCR") z$rmag11yrs = z$nbr2014 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 12 years post fire
if (fire.year == 2003) z$rmag12yrs = z$nbr2015 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag12yrs = NA
if (fire.year == 2002) z$rmag12yrs = z$nbr2013 - z$nbr2002
if (fire.year == 2000) z$rmag12yrs = z$nbr2012 - z$nbr2001
if (fire.year == 2005) z$rmag12yrs = NA
if (fire.name == "WCR") z$rmag12yrs = z$nbr2015 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 13 years post fire
if (fire.year == 2003) z$rmag13yrs = z$nbr2016 - z$nbr2004 # Pre-fire imagery year for BMCR, OLD, SIMI is 2003
if (fire.year == 2007) z$rmag13yrs = NA
if (fire.year == 2002) z$rmag13yrs = z$nbr2014 - z$nbr2002
if (fire.year == 2000) z$rmag13yrs = z$nbr2013 - z$nbr2001
if (fire.year == 2005) z$rmag13yrs = NA
if (fire.name == "WCR") z$rmag13yrs = z$nbr2016 - z$nbr2004 # Fire spread over 2003,2004 in Wedge Canyon, Robert, so use 2002 as prefire

# Recovery magnitude "rmag" in terms of dnbr 14 years post fire
if (fire.year == 2003) z$rmag14yrs = NA
if (fire.year == 2007) z$rmag14yrs = NA
if (fire.year == 2002) z$rmag14yrs = z$nbr2015 - z$nbr2002
if (fire.year == 2000) z$rmag14yrs = z$nbr2014 - z$nbr2001
if (fire.year == 2005) z$rmag14yrs = NA
if (fire.name == "WCR") z$rmag14yrs = NA

# Recovery magnitude "rmag" in terms of dnbr 15 years post fire
if (fire.year == 2003) z$rmag15yrs = NA
if (fire.year == 2007) z$rmag15yrs = NA
if (fire.year == 2002) z$rmag15yrs = z$nbr2016 - z$nbr2002
if (fire.year == 2000) z$rmag15yrs = z$nbr2015 - z$nbr2001
if (fire.year == 2005) z$rmag15yrs = NA
if (fire.name == "WCR") z$rmag15yrs = NA

# Recovery magnitude "rmag" in terms of dnbr 16 years post fire
if (fire.year == 2003) z$rmag16yrs = NA
if (fire.year == 2007) z$rmag16yrs = NA
if (fire.year == 2002) z$rmag16yrs = NA
if (fire.year == 2000) z$rmag16yrs = z$nbr2016 - z$nbr2001
if (fire.year == 2005) z$rmag16yrs = NA
if (fire.name == "WCR") z$rmag16yrs = NA


# % recovery in 2016 (100 means fully recovered)
z$prec2016 = (z$rmag2016/z$dmag)*100
z$prec2016[which(z$prec2016 < 0)]=0
z$prec2016[which(z$prec2016 > 100)]=100

# % recovery in 2010 (100 means fully recovered)
z$prec2010 = (z$rmag2010/z$dmag)*100
z$prec2010[which(z$prec2010 < 0)]=0
z$prec2010[which(z$prec2010 > 100)]=100

# % recovery 2 yrs post fire (100 means fully recovered)
z$prec2yrs = (z$rmag2yrs/z$dmag)*100
z$prec2yrs[which(z$prec2yrs < 0)]=0
z$prec2yrs[which(z$prec2yrs > 100)]=100

# % recovery 3 yrs post fire (100 means fully recovered)
z$prec3yrs = (z$rmag3yrs/z$dmag)*100
z$prec3yrs[which(z$prec3yrs < 0)]=0
z$prec3yrs[which(z$prec3yrs > 100)]=100

# % recovery 4 yrs post fire (100 means fully recovered)
z$prec4yrs = (z$rmag4yrs/z$dmag)*100
z$prec4yrs[which(z$prec4yrs < 0)]=0
z$prec4yrs[which(z$prec4yrs > 100)]=100

# % recovery 5 yrs post fire (100 means fully recovered)
z$prec5yrs = (z$rmag5yrs/z$dmag)*100
z$prec5yrs[which(z$prec5yrs < 0)]=0
z$prec5yrs[which(z$prec5yrs > 100)]=100

# % recovery 6 yrs post fire (100 means fully recovered)
z$prec6yrs = (z$rmag6yrs/z$dmag)*100
z$prec6yrs[which(z$prec6yrs < 0)]=0
z$prec6yrs[which(z$prec6yrs > 100)]=100

# % recovery 7 yrs post fire (100 means fully recovered)
z$prec7yrs = (z$rmag7yrs/z$dmag)*100
z$prec7yrs[which(z$prec7yrs < 0)]=0
z$prec7yrs[which(z$prec7yrs > 100)]=100

# % recovery 8 yrs post fire (100 means fully recovered)
z$prec8yrs = (z$rmag8yrs/z$dmag)*100
z$prec8yrs[which(z$prec8yrs < 0)]=0
z$prec8yrs[which(z$prec8yrs > 100)]=100

# % recovery 9 yrs post fire (100 means fully recovered)
z$prec9yrs = (z$rmag9yrs/z$dmag)*100
z$prec9yrs[which(z$prec9yrs < 0)]=0
z$prec9yrs[which(z$prec9yrs > 100)]=100

# % recovery 10 yrs post fire (100 means fully recovered)
z$prec10yrs = (z$rmag10yrs/z$dmag)*100
z$prec10yrs[which(z$prec10yrs < 0)]=0
z$prec10yrs[which(z$prec10yrs > 100)]=100

# % recovery 11 yrs post fire (100 means fully recovered)
z$prec11yrs = (z$rmag11yrs/z$dmag)*100
z$prec11yrs[which(z$prec11yrs < 0)]=0
z$prec11yrs[which(z$prec11yrs > 100)]=100

# % recovery 12 yrs post fire (100 means fully recovered)
z$prec12yrs = (z$rmag12yrs/z$dmag)*100
z$prec12yrs[which(z$prec12yrs < 0)]=0
z$prec12yrs[which(z$prec12yrs > 100)]=100

# % recovery 13 yrs post fire (100 means fully recovered)
z$prec13yrs = (z$rmag13yrs/z$dmag)*100
z$prec13yrs[which(z$prec13yrs < 0)]=0
z$prec13yrs[which(z$prec13yrs > 100)]=100

# % recovery 14 yrs post fire (100 means fully recovered)
z$prec14yrs = (z$rmag14yrs/z$dmag)*100
z$prec14yrs[which(z$prec14yrs < 0)]=0
z$prec14yrs[which(z$prec14yrs > 100)]=100

# % recovery 15 yrs post fire (100 means fully recovered)
z$prec15yrs = (z$rmag15yrs/z$dmag)*100
z$prec15yrs[which(z$prec15yrs < 0)]=0
z$prec15yrs[which(z$prec15yrs > 100)]=100

# % recovery 16 yrs post fire (100 means fully recovered)
z$prec16yrs = (z$rmag16yrs/z$dmag)*100
z$prec16yrs[which(z$prec16yrs < 0)]=0
z$prec16yrs[which(z$prec16yrs > 100)]=100

# For areas that have fully recovered, recovery duration in years
z$rec.dur = NA
for (i in 1:nrow(z)) {
	if (z$prec2016[i] >= 100) {
		if (fire.year == 2003) z$rec.dur[i] = min(which(z[i,40:52] > z$nbr2003[i]))
		if (fire.year == 2007) z$rec.dur[i] = min(which(z[i,44:52] > z$nbr2006[i])) # use 2006 as prefire for CEZ & Egley
		if (fire.year == 2002) z$rec.dur[i] = min(which(z[i,39:52] > z$nbr2001[i]))
		if (fire.year == 2000) z$rec.dur[i] = min(which(z[i,37:52] > z$nbr2000[i]))
		if (fire.year == 2005) z$rec.dur[i] = min(which(z[i,42:52] > z$nbr2004[i]))
		if (fire.name == "WCR") z$rec.dur[i] = min(which(z[i,40:52] > z$nbr2002[i]))
	}
}

# Create dnbr variable columns
z$dnbr1985 = z$nbr1985 - z$nbr1984
z$dnbr1986 = z$nbr1986 - z$nbr1985
z$dnbr1987 = z$nbr1987 - z$nbr1986
z$dnbr1988 = z$nbr1988 - z$nbr1987
z$dnbr1989 = z$nbr1989 - z$nbr1988
z$dnbr1990 = z$nbr1990 - z$nbr1989
z$dnbr1991 = z$nbr1991 - z$nbr1990
z$dnbr1992 = z$nbr1992 - z$nbr1991
z$dnbr1993 = z$nbr1993 - z$nbr1992
z$dnbr1994 = z$nbr1994 - z$nbr1993
z$dnbr1995 = z$nbr1995 - z$nbr1994
z$dnbr1996 = z$nbr1996 - z$nbr1995
z$dnbr1997 = z$nbr1997 - z$nbr1996
z$dnbr1998 = z$nbr1998 - z$nbr1997
z$dnbr1999 = z$nbr1999 - z$nbr1998
z$dnbr2000 = z$nbr2000 - z$nbr1999
z$dnbr2001 = z$nbr2001 - z$nbr2000
z$dnbr2002 = z$nbr2002 - z$nbr2001
z$dnbr2003 = z$nbr2003 - z$nbr2002
z$dnbr2004 = z$nbr2004 - z$nbr2003
z$dnbr2005 = z$nbr2005 - z$nbr2004
z$dnbr2006 = z$nbr2006 - z$nbr2005
z$dnbr2007 = z$nbr2007 - z$nbr2006
z$dnbr2008 = z$nbr2008 - z$nbr2007
z$dnbr2009 = z$nbr2009 - z$nbr2008
z$dnbr2010 = z$nbr2010 - z$nbr2009
z$dnbr2011 = z$nbr2011 - z$nbr2010
z$dnbr2012 = z$nbr2012 - z$nbr2011
z$dnbr2013 = z$nbr2013 - z$nbr2012
z$dnbr2014 = z$nbr2014 - z$nbr2013
z$dnbr2015 = z$nbr2015 - z$nbr2014
z$dnbr2016 = z$nbr2016 - z$nbr2015

# Assign severities to each patch
low = which(z$strata > 199 & z$strata < 300)
mod = which(z$strata > 299 & z$strata < 400)
high = which(z$strata > 399)
z$sev = NA
z$sev[low]=1
z$sev[mod]=2
z$sev[high]=3

z
}


BMCR = prep.for.combining("black_mountain_2_cooney_ridge/zonal_means.csv", "BMCR", 2003, 155)
EGLEY = prep.for.combining("egley/zonal_means.csv", "EGLEY", 2007, 100)
HAYMAN = prep.for.combining("hayman/zonal_means.csv", "HAYMAN", 2002, 140)
JASPER = prep.for.combining("jasper/zonal_means.csv", "JASPER", 2000, 75)
OLD = prep.for.combining("old/zonal_means.csv", "OLD", 2003, 100)
SCHOOL = prep.for.combining("school/zonal_means.csv", "SCHOOL", 2005, 115)
WCR = prep.for.combining("wedge_canyon_robert/zonal_means.csv", "WCR", 2003, 110)
C = prep.for.combining("cascade_east_zone/zonal_means_cascade.csv", "CASCADE", 2007, 100)
EZ = prep.for.combining("cascade_east_zone/zonal_means_east_zone.csv", "EASTZONE", 2007, 100)
CEZ = rbind(C,EZ) # Decided to recombine
GP = prep.for.combining("grand_prix/zonal_means.csv", "GP", 2003, 100)
OGP = rbind(OLD,GP) # Added Grand Prix for better representation of conifer/oak/chaparral

PIPO = rbind(EGLEY, HAYMAN, JASPER)
MC = rbind(SCHOOL, BMCR, CEZ, WCR)



# ------------ Nov 2018
# ------------ Percent recovery 5, 10, and 13 years post-fire significantly different between severities? (reponse to editor comment to add to text)
shapiro.test(sample(PIPO$prec4yrs,5000)) # very non-normal P<2.2e-16, so to use nonparametric for tests

kruskal.test(PIPO$prec4yrs~factor(PIPO$sev))
kruskal.test(PIPO$prec8yrs~factor(PIPO$sev))
kruskal.test(PIPO$prec12yrs~factor(PIPO$sev))

kruskal.test(MC$prec4yrs~factor(MC$sev))
kruskal.test(MC$prec8yrs~factor(MC$sev))
kruskal.test(MC$prec12yrs~factor(MC$sev))

kruskal.test(OGP$prec4yrs~factor(OGP$sev))
kruskal.test(OGP$prec8yrs~factor(OGP$sev))
kruskal.test(OGP$prec12yrs~factor(OGP$sev))


wilcox.test(PIPO$prec4yrs[-which(PIPO$sev == 1)]~factor(PIPO$sev[-which(PIPO$sev == 1)]))
wilcox.test(PIPO$prec4yrs[-which(PIPO$sev == 2)]~factor(PIPO$sev[-which(PIPO$sev == 2)]))
wilcox.test(PIPO$prec4yrs[-which(PIPO$sev == 3)]~factor(PIPO$sev[-which(PIPO$sev == 3)]))

wilcox.test(PIPO$prec8yrs[-which(PIPO$sev == 1)]~factor(PIPO$sev[-which(PIPO$sev == 1)]))
wilcox.test(PIPO$prec8yrs[-which(PIPO$sev == 2)]~factor(PIPO$sev[-which(PIPO$sev == 2)]))
wilcox.test(PIPO$prec8yrs[-which(PIPO$sev == 3)]~factor(PIPO$sev[-which(PIPO$sev == 3)]))

wilcox.test(PIPO$prec12yrs[-which(PIPO$sev == 1)]~factor(PIPO$sev[-which(PIPO$sev == 1)]))
wilcox.test(PIPO$prec12yrs[-which(PIPO$sev == 2)]~factor(PIPO$sev[-which(PIPO$sev == 2)]))
wilcox.test(PIPO$prec12yrs[-which(PIPO$sev == 3)]~factor(PIPO$sev[-which(PIPO$sev == 3)]))

0.0001077

wilcox.test(MC$prec4yrs[-which(MC$sev == 1)]~factor(MC$sev[-which(MC$sev == 1)]))
wilcox.test(MC$prec4yrs[-which(MC$sev == 2)]~factor(MC$sev[-which(MC$sev == 2)]))
wilcox.test(MC$prec4yrs[-which(MC$sev == 3)]~factor(MC$sev[-which(MC$sev == 3)]))

wilcox.test(MC$prec8yrs[-which(MC$sev == 1)]~factor(MC$sev[-which(MC$sev == 1)]))
wilcox.test(MC$prec8yrs[-which(MC$sev == 2)]~factor(MC$sev[-which(MC$sev == 2)]))
wilcox.test(MC$prec8yrs[-which(MC$sev == 3)]~factor(MC$sev[-which(MC$sev == 3)]))

wilcox.test(MC$prec12yrs[-which(MC$sev == 1)]~factor(MC$sev[-which(MC$sev == 1)]))
wilcox.test(MC$prec12yrs[-which(MC$sev == 2)]~factor(MC$sev[-which(MC$sev == 2)]))
wilcox.test(MC$prec12yrs[-which(MC$sev == 3)]~factor(MC$sev[-which(MC$sev == 3)]))



wilcox.test(OGP$prec4yrs[-which(OGP$sev == 1)]~factor(OGP$sev[-which(OGP$sev == 1)]))
wilcox.test(OGP$prec4yrs[-which(OGP$sev == 2)]~factor(OGP$sev[-which(OGP$sev == 2)]))
wilcox.test(OGP$prec4yrs[-which(OGP$sev == 3)]~factor(OGP$sev[-which(OGP$sev == 3)]))

wilcox.test(OGP$prec8yrs[-which(OGP$sev == 1)]~factor(OGP$sev[-which(OGP$sev == 1)]))
wilcox.test(OGP$prec8yrs[-which(OGP$sev == 2)]~factor(OGP$sev[-which(OGP$sev == 2)]))
wilcox.test(OGP$prec8yrs[-which(OGP$sev == 3)]~factor(OGP$sev[-which(OGP$sev == 3)]))

wilcox.test(OGP$prec12yrs[-which(OGP$sev == 1)]~factor(OGP$sev[-which(OGP$sev == 1)]))
wilcox.test(OGP$prec12yrs[-which(OGP$sev == 2)]~factor(OGP$sev[-which(OGP$sev == 2)]))
wilcox.test(OGP$prec12yrs[-which(OGP$sev == 3)]~factor(OGP$sev[-which(OGP$sev == 3)]))






# ------------ Timeseries figure function (dNBR)
plot.dnbr.ts <- function(df,ylim) {
col=c("cyan3","yellow3","red3")
ylim = ylim
lwd = 1
cex = 1.2
# Create mean and sd vectors
dnbr.low.mean = apply(df[which(df$sev==1),107:123], 2, mean, na.rm=T) #77:93 originally
dnbr.mod.mean = apply(df[which(df$sev==2),107:123], 2, mean, na.rm=T)
dnbr.high.mean = apply(df[which(df$sev==3),107:123], 2, mean, na.rm=T)
dnbr.low.sd = apply(df[which(df$sev==1),107:123], 2, sd, na.rm=T)
dnbr.mod.sd = apply(df[which(df$sev==2),107:123], 2, sd, na.rm=T)
dnbr.high.sd = apply(df[which(df$sev==3),107:123], 2, sd, na.rm=T)
# Plot means
plot(2000:2016-.1, dnbr.low.mean, pch=18, type="l", axes=F, ann=F, col=col[1], ylim=ylim, lwd=lwd)
abline(h=0)
lines(2000:2016, dnbr.mod.mean, pch=18, type="l", col=col[2], ylab="", xlab="", ylim=ylim, lwd=lwd)
lines(2000:2016+.1,dnbr.high.mean, pch=18, type="l", col=col[3], ylab="", xlab="", ylim=ylim, lwd=lwd)
# Plot sds
arrows(2000:2016-.1, dnbr.low.mean+dnbr.low.sd, 2000:2016-.1, dnbr.low.mean-dnbr.low.sd, angle=90, code=3, length=0.02, col=col[1], ann=F, cex=2, lwd=lwd)
arrows(2000:2016, dnbr.mod.mean+dnbr.mod.sd, 2000:2016, dnbr.mod.mean-dnbr.mod.sd, angle=90, code=3, length=0.02, col=col[2], ann=F, cex=2, lwd=lwd)
arrows(2000:2016+.1, dnbr.high.mean+dnbr.high.sd, 2000:2016+.1, dnbr.high.mean-dnbr.high.sd, angle=90, code=3, length=0.02, col=col[3], ann=F, cex=2, lwd=lwd)

box()
axis(2,las=2,cex.axis=cex)
axis(1, at=c(2000,2004,2008,2012,2016), labels=c(2000,2004,2008,2012,2016),cex.axis=cex)
}


# Timeseries figure (dNBR)
tiff(filename = "C:/Users/benjamincbright/Desktop/dnbr_ts_fig.tif", width=165, height=170, units="mm", res=600, compression="lzw")

par(mfcol=c(4,2))
par(mar=c(3.8,4.7,1.7,.8)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.
cex = 1.2
col = c("cyan3","yellow3","red3")

plot.dnbr.ts(SCHOOL, c(-350,150))
title(main="School, WA", cex.main = cex)
title(ylab="dNBR",cex.lab=cex,line=3.3)
legend("bottomleft", c("High","Moderate","Low"), col=rev(col), bty="n", lty=1, lwd=1, title="Burn Severity", pt.cex=1, cex=.8)
mtext("a", side=3, line=.2, adj = -.18)

plot.dnbr.ts(EGLEY, c(-350,150))
title(main="Egley, OR", cex.main = cex)
title(ylab="dNBR",cex.lab=cex,line=3.3)
mtext("b", side=3, line=.2, adj = -.18)

plot.dnbr.ts(CEZ, c(-350,150))
title(main="Cascade and East Zone, ID", cex.main = cex)
title(ylab="dNBR",cex.lab=cex,line=3.3)
mtext("c", side=3, line=.2, adj = -.18)

plot.dnbr.ts(OGP, c(-350,150))
title(main="Old and Grand Prix, CA", cex.main = cex)
title(ylab="dNBR",cex.lab=cex,line=3.3)
title(xlab="Year",line=2.4,cex.lab=cex)
mtext("d", side=3, line=.2, adj = -.18)

plot.dnbr.ts(WCR, c(-350,150))
title(main="Wedge Canyon and Robert, MT", cex.main = cex)
mtext("e", side=3, line=.2, adj = -.18)

plot.dnbr.ts(BMCR, c(-350,150))
title(main="Black Mountain 2 and Cooney Ridge, MT", cex.main = cex)
mtext("f", side=3, line=.2, adj = -.18)

plot.dnbr.ts(JASPER, c(-350,150))
title(main="Jasper, SD", cex.main = cex)
mtext("g", side=3, line=.2, adj = -.18)

plot.dnbr.ts(HAYMAN, c(-350,150))
title(main="Hayman, CO", cex.main = cex)
title(xlab="Year",line=2.4,cex.lab = cex)
mtext("h", side=3, line=.2, adj = -.18)

dev.off()





# ------------ Timeseries figure function (% NBR recovery)
#columns = 77:91
#columns2 = 2:16
columns = 77:90
columns2 = 2:15
plot.pr.ts <- function(df,ylim) {
col=c("cyan3","yellow3","red3")
ylim = ylim
lwd = 1
cex = 1.2
# Create mean and sd vectors
pr.low.mean = apply(df[which(df$sev==1),columns], 2, mean, na.rm=T)
pr.mod.mean = apply(df[which(df$sev==2),columns], 2, mean, na.rm=T)
pr.high.mean = apply(df[which(df$sev==3),columns], 2, mean, na.rm=T)
pr.low.sd = apply(df[which(df$sev==1),columns], 2, sd, na.rm=T)
pr.mod.sd = apply(df[which(df$sev==2),columns], 2, sd, na.rm=T)
pr.high.sd = apply(df[which(df$sev==3),columns], 2, sd, na.rm=T)
# Plot means
plot(columns2-.1, pr.low.mean, pch=18, type="l", axes=F, ann=F, col=col[1], ylim=ylim, lwd=lwd)
lines(columns2, pr.mod.mean, pch=18, type="l", col=col[2], ylab="", xlab="", ylim=ylim, lwd=lwd)
lines(columns2+.1,pr.high.mean, pch=18, type="l", col=col[3], ylab="", xlab="", ylim=ylim, lwd=lwd)
# Plot sds
arrows(columns2-.1, pr.low.mean+pr.low.sd, columns2-.1, pr.low.mean-pr.low.sd, angle=90, code=3, length=0.02, col=col[1], ann=F, cex=2, lwd=lwd)
arrows(columns2, pr.mod.mean+pr.mod.sd, columns2, pr.mod.mean-pr.mod.sd, angle=90, code=3, length=0.02, col=col[2], ann=F, cex=2, lwd=lwd)
arrows(columns2+.1, pr.high.mean+pr.high.sd, columns2+.1, pr.high.mean-pr.high.sd, angle=90, code=3, length=0.02, col=col[3], ann=F, cex=2, lwd=lwd)

box()
axis(2,las=2,cex.axis=cex)
axis(1, at=c(2,4,6,8,10,12,14,16), labels=c(2,4,6,8,10,12,14,16),cex.axis=cex)
}


# Timeseries figure (% NBR recovery)
tiff(filename = "C:/Users/benjamincbright/Desktop/percent_recovery_ts_fig_rev1.tif", width=165, height=170, units="mm", res=600, compression="lzw")

par(mfcol=c(4,2))
par(mar=c(3.8,4.7,1.7,1)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.
cex = 1.2
col = c("cyan3","yellow3","red3")

plot.pr.ts(SCHOOL, c(0,100))
title(main="School, Washington", cex.main = cex)
title(ylab="NBR recovery (%)",cex.lab=cex,line=3.3)
legend("bottomright", c("High","Moderate","Low"), col=rev(col), bty="n", lty=1, lwd=1, title="Burn severity", pt.cex=1, cex=.8)
legend(.8,105, c("Mixed conifer","2005",expression(paste("6.6 ",degree,"C")),"554 mm"), cex=.8, bty="n")
#mtext("a", side=3, line=.2, adj = -.18)

plot.pr.ts(EGLEY, c(0,100))
title(main="Egley, Oregon", cex.main = cex)
title(ylab="NBR recovery (%)",cex.lab=cex,line=3.3)
legend(.8,105, c("Ponderosa pine","2007",expression(paste("5.8 ",degree,"C")),"328 mm"), cex=.8, bty="n")
#mtext("b", side=3, line=.2, adj = -.18)

plot.pr.ts(CEZ, c(0,100))
title(main="Cascade & East Zone, Idaho", cex.main = cex)
title(ylab="NBR recovery (%)",cex.lab=cex,line=3.3)
legend(.8,105, c("Mixed conifer","2007",expression(paste("2.0 ",degree,"C")),"555 mm"), cex=.8, bty="n")
#mtext("c", side=3, line=.2, adj = -.18)

plot.pr.ts(OGP, c(0,100))
title(main="Old & Grand Prix, California", cex.main = cex)
title(ylab="NBR recovery (%)",cex.lab=cex,line=3.3)
title(xlab="Years post fire",line=2.4,cex.lab=cex)
legend(.8,105, c("Conifer-oak-chaparral","2003",expression(paste("15.0 ",degree,"C")),"425 mm"), cex=.8, bty="n")
#mtext("d", side=3, line=.2, adj = -.18)

plot.pr.ts(WCR, c(0,100))
title(main="Wedge Canyon & Robert, Montana", cex.main = cex)
legend(.8,105, c("Mixed conifer","2003",expression(paste("3.6 ",degree,"C")),"662 mm"), cex=.8, bty="n")
#mtext("e", side=3, line=.2, adj = -.18)

plot.pr.ts(BMCR, c(0,100))
title(main="Black Mountain 2 & Cooney Ridge, Montana", cex.main = cex)
legend(.8,105, c("Mixed conifer","2003",expression(paste("3.8 ",degree,"C")),"517 mm"), cex=.8, bty="n")
#mtext("f", side=3, line=.2, adj = -.18)

plot.pr.ts(JASPER, c(0,100))
title(main="Jasper, South Dakota", cex.main = cex)
legend(.8,105, c("Ponderosa pine","2000",expression(paste("5.5 ",degree,"C")),"548 mm"), cex=.8, bty="n")
#mtext("g", side=3, line=.2, adj = -.18)

plot.pr.ts(HAYMAN, c(0,100))
title(main="Hayman, Colorado", cex.main = cex)
title(xlab="Years post fire",line=2.4,cex.lab = cex)
legend(.8,105, c("Ponderosa pine","2002",expression(paste("5.0 ",degree,"C")),"435 mm"), cex=.8, bty="n")
#mtext("h", side=3, line=.2, adj = -.18)

dev.off()


# Timeseries figure (% NBR recovery) Sept 2018 exploration
tiff(filename = "C:/Users/benjamincbright/Desktop/percent_recovery_ts_fig_rev2.tif", width=100, height=140, units="mm", res=600, compression="lzw")

par(mfcol=c(3,1))
par(mar=c(3.8,4.7,1.7,.8)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.
cex = 1.2
col = c("cyan3","yellow3","red3")

plot.pr.ts(PIPO, c(0,100))
title(main="Ponderosa pine", cex.main = cex)
title(ylab="NBR recovery (%)",cex.lab=cex,line=3.3)
legend("topleft", c("High","Moderate","Low"), col=rev(col), bty="n", lty=1, lwd=1, title="Burn severity", pt.cex=1, cex=.8)
#legend(.8,105, c("Mixed conifer","6.6","554"), cex=.8, bty="n")
#mtext("a", side=3, line=.2, adj = -.14)

plot.pr.ts(MC, c(0,100))
title(main="Mixed conifer", cex.main = cex)
title(ylab="NBR recovery (%)",cex.lab=cex,line=3.3)
#legend(.8,105, c("Ponderosa pine","5.8","328"), cex=.8, bty="n")
#mtext("b", side=3, line=.2, adj = -.14)

plot.pr.ts(OGP, c(0,100))
title(main="Conifer-oak-chaparral", cex.main = cex)
title(ylab="NBR recovery (%)",cex.lab=cex,line=3.3)
title(xlab="Years post fire",line=2.4,cex.lab=cex)
#legend(.8,105, c("Mixed conifer","2.0","555"), cex=.8, bty="n")
#mtext("c", side=3, line=.2, adj = -.14)

dev.off()


mean(PIPO$prec5yrs, na.rm = T)
mean(MC$prec5yrs, na.rm = T)
mean(OGP$prec5yrs, na.rm = T)

mean(PIPO$prec10yrs, na.rm = T)
mean(MC$prec10yrs, na.rm = T)
mean(OGP$prec10yrs, na.rm = T)

mean(PIPO$prec13yrs, na.rm = T)
mean(MC$prec13yrs, na.rm = T)
mean(OGP$prec13yrs, na.rm = T)





# --- Boxplots comparing % recovery in 2016
tiff(filename = "C:/Users/benjamincbright/Desktop/prec2016~sev_boxplots.tif", width=165, height=155, units="mm", res=600, compression="lzw")

par(mfcol=c(3,3))
par(mar=c(4.5,4.6,1.7,1)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.
par(mgp=c(3,1,0)) # The margin line (in mex units) for the axis title, axis labels and axis line.The default is c(3, 1, 0).
cex=1.2
names=c("Low","Mod.","High")

boxplot(prec2016~sev, data=SCHOOL, names=names, cex.axis=cex, las=1)
title(main="School, WA", ylab="NBR recovery (%)", cex.lab=cex, cex.main=cex)
mtext("a", side=3, line=.2, adj = -.3)

boxplot(prec2016~sev, data=EGLEY, names=names, cex.axis=cex, las=1)
title(main="Egley, OR", ylab="NBR recovery (%)", cex.lab=cex, cex.main=cex)
mtext("b", side=3, line=.2, adj = -.3)

boxplot(prec2016~sev, data=CEZ, names=names, cex.axis=cex, las=1)
title(main="Cascade, E. Zone, ID", ylab="NBR recovery (%)", xlab="Burn severity", cex.lab=cex, cex.main=cex)
mtext("c", side=3, line=.2, adj = -.3)

boxplot(prec2016~sev, data=OGP, names=names, cex.axis=cex, las=1)
title(main="Old, Grand Prix, CA", cex.lab=cex, cex.main=cex)
mtext("d", side=3, line=.2, adj = -.3)

boxplot(prec2016~sev, data=WCR, names=names, cex.axis=cex, las=1)
title(main="Wedge C., Robert, MT", cex.lab=cex, cex.main=cex)
mtext("e", side=3, line=.2, adj = -.3)

boxplot(prec2016~sev, data=BMCR, names=names, cex.axis=cex, las=1)
title(main="Black M. 2, Cooney R., MT", xlab="Burn severity", cex.lab=cex, cex.main=cex)
mtext("f", side=3, line=.2, adj = -.3)

boxplot(prec2016~sev, data=JASPER, names=names, cex.axis=cex, las=1)
title(main="Jasper, SD", cex.lab=cex, cex.main=cex)
mtext("g", side=3, line=.2, adj = -.3)

boxplot(prec2016~sev, data=HAYMAN, names=names, cex.axis=cex, las=1)
title(main="Hayman, CO", xlab="Burn severity", cex.lab=cex, cex.main=cex)
mtext("h", side=3, line=.2, adj = -.3)

dev.off()

mean(SCHOOL$prec9yrs)
mean(EGLEY$prec9yrs)
mean(CEZ$prec9yrs)
mean(OGP$prec9yrs)
mean(WCR$prec9yrs)
mean(BMCR$prec9yrs)
mean(JASPER$prec9yrs)
mean(HAYMAN$prec9yrs, na.rm=T)
# 33-70

mean(SCHOOL$prec13yrs)
mean(EGLEY$prec13yrs)
mean(CEZ$prec13yrs)
mean(OGP$prec13yrs)
mean(WCR$prec13yrs)
mean(BMCR$prec13yrs)
mean(JASPER$prec13yrs)
mean(HAYMAN$prec13yrs, na.rm=T)
# 42-77




# --- Percent recovered and recovery duration

rec.dur.stats <-function(df) {

n = nrow(df)

nlow = length(which(df$sev == 1))
nlow.rec = length(which(df$sev == 1 & is.na(df$rec.dur) == F & df$rec.dur != Inf))
plow.rec = nlow.rec/nlow*100
mean.dur.low = mean(df$rec.dur[which(df$sev == 1 & is.na(df$rec.dur) == F & df$rec.dur != Inf)])

nmod = length(which(df$sev == 2))
nmod.rec = length(which(df$sev == 2 & is.na(df$rec.dur) == F & df$rec.dur != Inf))
pmod.rec = nmod.rec/nmod*100
mean.dur.mod = mean(df$rec.dur[which(df$sev == 2 & is.na(df$rec.dur) == F & df$rec.dur != Inf)])

nhigh = length(which(df$sev == 3))
nhigh.rec = length(which(df$sev == 3 & is.na(df$rec.dur) == F & df$rec.dur != Inf))
phigh.rec = nhigh.rec/nhigh*100
mean.dur.high = mean(df$rec.dur[which(df$sev == 3 & is.na(df$rec.dur) == F & df$rec.dur != Inf)])

list(plow.rec=plow.rec, pmod.rec=pmod.rec, phigh.rec=phigh.rec, mean.dur.low=mean.dur.low, mean.dur.mod=mean.dur.mod, mean.dur.high=mean.dur.high, yrs.since.fire = df$yrs.since.fire[1])
}

rec.dur.stats(EGLEY)
rec.dur.stats(HAYMAN)
rec.dur.stats(JASPER)
rec.dur.stats(SCHOOL)
rec.dur.stats(BMCR)
rec.dur.stats(CEZ)
rec.dur.stats(WCR)
rec.dur.stats(OGP)


# Stacked versus side-by-side barplot:
# https://www.r-graph-gallery.com/211-basic-grouped-or-stacked-barplot/

# prec <- read.table(text = "Egley   Hayman   Jasper   School   CEZ   BMCR   Simi   Old   WCR 
# 1 4.3 4.4 1.8 3.5 2.3 4.9 2.4 8.6 6.4
# 2 4 2.1 3 2.7 2.3 5.8 2.2 12.5 7.4
# 3 1.2 .2 .4 1 .8 2.4 NA 4.9 3.7", header = TRUE)

prec <- read.table(text = "Egley  Hayman  Jasper  School  BMCR  CEZ  WCR  OGP 
1 11.9 12.1 7.2  8.7  15.7  7.0  18.9  27.6   
2 8.6  4.7  5.5  6.2  12.1  5.1  16.6  24.6
3 7.3  1.2  2.0  6.1  11.4  4.1  17.2  20.7", header = TRUE)

#col=c("darkgreen","aquamarine2","gold2","red")
#col=c("aquamarine2","gold2","red")
col=c("cyan3","yellow3","red3")

tiff(filename = "C:/Users/benjamincbright/Desktop/percent_patches_recovered.tif", width=80, height=70, units="mm", res=600, compression="lzw")

par(mar=c(5.5,3.5,.5,.5)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.
par(mgp=c(3,.8,0)) # The margin line (in mex units) for the axis title, axis labels and axis line. The default is c(3, 1, 0).
cex=.9

barplot(as.matrix(prec), col=col, las=1, ylim=c(0,35), beside=T, cex.axis=cex, cex.names=cex, las=2,
names.arg=c("Egley","Hayman","Jasper","School","BMCR","CEZ","WCR","OGP"))
title(ylab="Patches recovered (%)", cex.lab=cex, line=2)
title(xlab="Fire", cex.lab=cex, line=4)
legend(0, 35, legend=c("High","Moderate","Low"), fill=rev(col), cex=.8, bty = "n", title="Burn severity")
# Add years since fire above each bar
s = seq(from=2.5,to=35,by=4)
barht = c(11.9, 12.1, 7.2, 8.7, 15.7, 7.0, 18.9, 27.6) + 2
rec.dur = c(9,14,16,11,13,9,13,13)
text(s, barht, rec.dur, cex=cex)

dev.off()







# -----------
# -----------
# --- Climate and topographic analysis

# Boxplots showing climate anomaly distributions
tiff(filename = "C:/Users/benjamincbright/Desktop/climate_anomalies_boxplot.tif", width=165, height=155, units="mm", res=600, compression="lzw")

par(mfcol=c(3,3))
par(mar=c(4.5,4.6,1.7,1)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.
par(mgp=c(3,1,0)) # The margin line (in mex units) for the axis title, axis labels and axis line.The default is c(3, 1, 0).
cex=1.2
names=c("GSP","WINP","MAT","MMAX","MMIN")
ylim=c(-1,1)


boxplot(SCHOOL$gsp, SCHOOL$winp, SCHOOL$mat, SCHOOL$mmax, SCHOOL$mmin, names=names, cex.axis=cex, las=2, ylim=ylim)
title(main="School, WA", ylab="Post-fire anomaly", cex.lab=cex, cex.main=cex)
#mtext("a", side=3, line=.2, adj = -.3)
abline(h=0)

boxplot(EGLEY$gsp, EGLEY$winp, EGLEY$mat, EGLEY$mmax, EGLEY$mmin, names=names, cex.axis=cex, las=2, ylim=ylim)
title(main="Egley, OR", ylab="Post-fire anomaly", cex.lab=cex, cex.main=cex)
#mtext("b", side=3, line=.2, adj = -.3)
abline(h=0)

boxplot(CEZ$gsp, CEZ$winp, CEZ$mat, CEZ$mmax, CEZ$mmin, names=names, cex.axis=cex, las=2, ylim=c(-1.5,1))
title(main="CEZ, ID", ylab="Post-fire anomaly", cex.lab=cex, cex.main=cex)
#mtext("c", side=3, line=.2, adj = -.3)
abline(h=0)

boxplot(OGP$gsp, OGP$winp, OGP$mat, OGP$mmax, OGP$mmin, names=names, cex.axis=cex, las=2, ylim=ylim)
title(main="OGP, CA", cex.lab=cex, cex.main=cex)
#mtext("d", side=3, line=.2, adj = -.3)
abline(h=0)

boxplot(WCR$gsp, WCR$winp, WCR$mat, WCR$mmax, WCR$mmin, names=names, cex.axis=cex, las=2, ylim=ylim)
title(main="WCR, MT", cex.lab=cex, cex.main=cex)
#mtext("e", side=3, line=.2, adj = -.3)
abline(h=0)

boxplot(BMCR$gsp, BMCR$winp, BMCR$mat, BMCR$mmax, BMCR$mmin, names=names, cex.axis=cex, las=2, ylim=ylim)
title(main="BMCR, MT", cex.lab=cex, cex.main=cex)
#mtext("f", side=3, line=.2, adj = -.3)
abline(h=0)

boxplot(JASPER$gsp, JASPER$winp, JASPER$mat, JASPER$mmax, JASPER$mmin, names=names, cex.axis=cex, las=2, ylim=ylim)
title(main="Jasper, SD", cex.lab=cex, cex.main=cex)
#mtext("g", side=3, line=.2, adj = -.3)
abline(h=0)

boxplot(HAYMAN$gsp, HAYMAN$winp, HAYMAN$mat, HAYMAN$mmax, HAYMAN$mmin, names=names, cex.axis=cex, las=2, ylim=ylim)
title(main="Hayman, CO", cex.lab=cex, cex.main=cex)
#mtext("h", side=3, line=.2, adj = -.3)
abline(h=0)

dev.off()








rf.and.cor <- function(df, ntree, prop.sam.size, vars) {
library(randomForest)

s = sample(1:nrow(df), nrow(df)*prop.sam.size)
dfs = df[s,]

# Random forest modeling
#rf = randomForest(prec9yrs ~ curv + dist_unburn + dnbr + elev + gsp + mat + mmax + mmin + slope + trasp + winp + pre.fire.nbr, data=df, importance=T)
if (vars == "nbr") rf = randomForest(prec2010 ~ dnbr + pre.fire.nbr, data=dfs, importance=T)
if (vars == "nbr + climate") rf = randomForest(prec2010 ~ dnbr + gsp + map + mat + mmax + mmin + winp + pre.fire.nbr, data=dfs, importance=T)
if (vars == "all") rf = randomForest(prec2010 ~ curv + dist_unburn + dnbr + elev + gsp + map + mat + mmax + mmin + slope + trasp + winp + pre.fire.nbr, data=dfs, importance=T)

# % variance explained
pve = round(rf$rsq[ntree] * 100,0)

# Root mean square error
rmse = round(sqrt(rf$mse[ntree]),0)
prmse =  round(rmse/mean(dfs$prec2010)*100,0)

# Model improvement ratio for each variable
rf.imp <- rf$importance[, "%IncMSE"]
i <- round(rf.imp/max(rf.imp),1)

# Correlation analysis
v = c("prec2010","curv","dist_unburn","dnbr","elev","gsp","map","mat","mmax","mmin","slope","trasp","winp","pre.fire.nbr")
cor = round(cor(dfs[v],method="spearman")[,1],2)

list(pve = pve, rmse = rmse, prmse = prmse, i = i, cor = cor)

}

date()
rf.and.cor(EGLEY, 500, 1, "nbr")
rf.and.cor(EGLEY, 500, 1, "nbr + climate")
rf.and.cor(EGLEY, 500, 1, "all")
date() # 18 min

date()
HAYMAN = HAYMAN[-which(is.na(HAYMAN$gsp) == T),]
rf.and.cor(HAYMAN, 500, 1, "nbr")
rf.and.cor(HAYMAN, 500, 1, "nbr + climate")
rf.and.cor(HAYMAN, 500, 1, "all")
date() # 40 min

date()
JASPER = JASPER[-which(is.na(JASPER$gsp) == T),]
rf.and.cor(JASPER, 500, 1, "nbr")
rf.and.cor(JASPER, 500, 1, "nbr + climate")
rf.and.cor(JASPER, 500, 1, "all")
date() # 9 min

date()
rf.and.cor(SCHOOL, 500, 1, "nbr")
rf.and.cor(SCHOOL, 500, 1, "nbr + climate")
rf.and.cor(SCHOOL, 500, 1, "all")
date() # 4 min

date()
BMCR = BMCR[-which(is.na(BMCR$dist_unburn) == T),]
rf.and.cor(BMCR, 500, 1, "nbr")
rf.and.cor(BMCR, 500, 1, "nbr + climate")
rf.and.cor(BMCR, 500, 1, "all")
date() # 1 min

date()
CEZ = CEZ[-which(is.na(CEZ$curv) == T),]
CEZ = CEZ[-which(is.na(CEZ$prec2010) == T),]
rf.and.cor(CEZ, 500, 1, "nbr")
rf.and.cor(CEZ, 500, 1, "nbr + climate")
rf.and.cor(CEZ, 500, 1, "all")
date() # 6 hr

date()
WCR = WCR[-which(is.na(WCR$curv) == T),]
WCR = WCR[-which(is.na(WCR$dist_unburn) == T),]
WCR = WCR[-which(is.na(WCR$dnbr) == T),]
rf.and.cor(WCR, 500, 1, "nbnrowr")
rf.and.cor(WCR, 500, 1, "nbr + climate")
rf.and.cor(WCR, 500, 1, "all")
date() # 1 hr

date()
rf.and.cor(OGP, 500, 1, "nbr")
rf.and.cor(OGP, 500, 1, "nbr + climate")
rf.and.cor(OGP, 500, 1, "all")
date() # 




# Sept 2018 - Try combining forest types again for modeling analysis - drop some incomplete records
HAYMAN = HAYMAN[-which(is.na(HAYMAN$gsp) == T),]
JASPER = JASPER[-which(is.na(JASPER$gsp) == T),]
BMCR = BMCR[-which(is.na(BMCR$dist_unburn) == T),]
CEZ = CEZ[-which(is.na(CEZ$curv) == T),]
CEZ = CEZ[-which(is.na(CEZ$prec2010) == T),]
WCR = WCR[-which(is.na(WCR$curv) == T),]
WCR = WCR[-which(is.na(WCR$dist_unburn) == T),]
WCR = WCR[-which(is.na(WCR$dnbr) == T),]

# Combine
PIPO = rbind(EGLEY, HAYMAN, JASPER)
MC = rbind(SCHOOL, BMCR, CEZ, WCR)

# Sept 2018 version of the function
rf.and.cor <- function(df, ntree, prop.sam.size, vars) {
library(randomForest)

s = sample(1:nrow(df), nrow(df)*prop.sam.size)
dfs = df[s,]

# Random forest modeling
#rf = randomForest(prec9yrs ~ curv + dist_unburn + dnbr + elev + gsp + mat + mmax + mmin + slope + trasp + winp + pre.fire.nbr, data=df, importance=T)
if (vars == "nbr") rf = randomForest(prec9yrs ~ dnbr + pre.fire.nbr, data=dfs, importance=T)
if (vars == "nbr + climate") rf = randomForest(prec9yrs ~ dnbr + gsp + map + mat + mmax + mmin + winp + pre.fire.nbr, data=dfs, importance=T)
if (vars == "all") rf = randomForest(prec9yrs ~ curv + dist_unburn + dnbr + gsp + map + mat + mmax + mmin + slope + trasp + winp + pre.fire.nbr, data=dfs, importance=T)

# % variance explained
pve = round(rf$rsq[ntree] * 100,0)

# Root mean square error
rmse = round(sqrt(rf$mse[ntree]),0)
prmse =  round(rmse/mean(dfs$prec9yrs)*100,0)

# Model improvement ratio for each variable
rf.imp <- rf$importance[, "%IncMSE"]
i <- round(rf.imp/max(rf.imp),1)

# Correlation analysis
v = c("prec9yrs","curv","dist_unburn","dnbr","elev","gsp","map","mat","mmax","mmin","slope","trasp","winp","pre.fire.nbr")
cor = round(cor(dfs[v],method="spearman")[,1],2)

list(pve = pve, rmse = rmse, prmse = prmse, i = i, cor = cor)

}

date()
rf.and.cor(PIPO, 500, 1, "nbr")
rf.and.cor(PIPO, 500, 1, "nbr + climate")
rf.and.cor(PIPO, 500, 1, "all")
date() #

date()
rf.and.cor(MC, 500, .1, "nbr")
rf.and.cor(MC, 500, .1, "nbr + climate")
rf.and.cor(MC, 500, .1, "all")
date() #

date()
rf.and.cor(OGP, 500, 1, "nbr")
rf.and.cor(OGP, 500, 1, "nbr + climate")
rf.and.cor(OGP, 500, 1, "all")
date() # 




# Patch statistics

# Drop some patches with incomplete records
HAYMAN = HAYMAN[-which(is.na(HAYMAN$gsp) == T),]
JASPER = JASPER[-which(is.na(JASPER$gsp) == T),]
BMCR = BMCR[-which(is.na(BMCR$dist_unburn) == T),]
CEZ = CEZ[-which(is.na(CEZ$curv) == T),]
WCR = WCR[-which(is.na(WCR$curv) == T),]
WCR = WCR[-which(is.na(WCR$dist_unburn) == T),]
WCR = WCR[-which(is.na(WCR$dnbr) == T),]

ALL=rbind(EGLEY, HAYMAN, JASPER, SCHOOL, BMCR, CEZ, WCR, OGP)


patch.stats <-function(df) {

total = nrow(df)
s = summary(df$count)*900*.0001
med = median(df$count)*900*.0001
sd = sd(df$count)*900*.0001

list(total=total, s=s, med=med, sd=sd)

}

patch.stats(EGLEY)


















































# Drop some incomplete records
HAYMAN = HAYMAN[-which(is.na(HAYMAN$gsp) == T),]
JASPER = JASPER[-which(is.na(JASPER$gsp) == T),]
BMCR = BMCR[-which(is.na(BMCR$dist_unburn) == T),]
CEZ = CEZ[-which(is.na(CEZ$curv) == T),]
WCR = WCR[-which(is.na(WCR$curv) == T),]
WCR = WCR[-which(is.na(WCR$dist_unburn) == T),]
WCR = WCR[-which(is.na(WCR$dnbr) == T),]

ALL = rbind(EGLEY, HAYMAN, JASPER, SCHOOL, BMCR, CEZ, WCR, OGP)
ALL = ALL[-which(is.na(ALL$prec2010) == T),]




library(MASS)

k=kde2d(ALL$map, ALL$prec2010, n=100)
image(k, col=rev(heat.colors(24)))

k=kde2d(ALL$gsp, ALL$prec2010, n=100)
image(k, col=rev(heat.colors(24)))

k=kde2d(ALL$winp, ALL$prec9yrs, n=100)
image(k, col=rev(heat.colors(24)))

k=kde2d(ALL$mat, ALL$prec9yrs, n=100)
image(k, col=rev(heat.colors(24)))

k=kde2d(ALL$mmax, ALL$prec9yrs, n=100)
image(k, col=rev(heat.colors(24)))

k=kde2d(ALL$mmin, ALL$prec9yrs, n=100)
image(k, col=rev(heat.colors(24)))

k=kde2d(PIPO$mmin, PIPO$prec9yrs, n=100)
image(k, col=rev(heat.colors(24)))





library(spdep)
library(fmsb) # To calculate VIF for a lm()



# # Group different forest types
PIPO = rbind(EGLEY, JASPER, HAYMAN)
MC = rbind(SCHOOL, CEZ, BMCR, WCR)
DMC = rbind(SCHOOL, C, BMCR)
MMC = rbind(EZ, WCR)

# # Get rid of NAs
PIPO = PIPO[-which(is.na(PIPO$gsp) == T),]
DMC = DMC[-which(is.na(DMC$dist_unburn) == T),]
MMC = MMC[-which(is.na(MMC$tpi) == T),]
MMC = MMC[-which(is.na(MMC$dist_unburn) == T),]
MMC = MMC[-which(is.na(MMC$dnbr) == T),]
ALL = rbind(PIPO, DMC, MMC, OLD)

# Correlation analysis
#vars = c("curv","dist_unburn","dnbr","gsp","map","mat","mmax","mmin","slope","trasp","winp","pre.fire.nbr","prec9yrs")
#cor(PIPO[vars])

# Calculate necessary sample size (Generally very small, like < 1%)
E = 100*.03
((1.96 * sd(EGLEY$prec9yrs))/E)^2
((1.96 * sd(HAYMAN$prec9yrs))/E)^2
((1.96 * sd(JASPER$prec9yrs))/E)^2
((1.96 * sd(SCHOOL$prec9yrs))/E)^2


# Do several iterations of modeling
loop.spatial.filtered.lm <- function(df, prop.sam.size, iterations, name) {

vars = c("intercept","curv","dist_unburn","dnbr","gsp","mat","mmax","mmin","slope","trasp","winp","pre.fire.nbr")
out = data.frame()

for (i in 1:iterations) {
cat(i, "of", iterations, "\r") # print i
flush.console()

# Error handling: https://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error
sf = NULL
attempt = 0
while(is.null(sf) && attempt <=20) {
attempt <- attempt + 1
# Sample dataframe (can't have too many cases)
s = sample(1:nrow(df), nrow(df)*prop.sam.size)
dfs = df[s,]
# Perform spatial filtering
coords = cbind(dfs$x, dfs$y)
nb = tri2nb(coords)
try(sf <- SpatialFiltering(prec9yrs ~ curv + dist_unburn + dnbr + gsp + mat + mmax + mmin + slope + trasp + winp + pre.fire.nbr,
data=dfs, nb=nb, ExactEV=FALSE))
}


# Develop linear models with and without spatial filtering
lmsf = lm(prec9yrs ~ curv + dist_unburn + dnbr + gsp + mat + mmax + mmin + slope + trasp + winp + pre.fire.nbr + fitted(sf),
data=dfs)
lm = lm(prec9yrs ~ curv + dist_unburn + dnbr + gsp + mat + mmax + mmin + slope + trasp + winp + pre.fire.nbr,
data=dfs)

# Get model diagnostics
coefs = as.data.frame(round(summary(lmsf)$coefficients[1:12,],3))
coefs$sig = 0 # Add significance col
for (j in 1:nrow(coefs)) if (coefs[j,4] < 0.05) coefs$sig[j] = 1
coefs$lmsf_moran[1] = lm.morantest(lmsf, nb2listw(nb))$p.value
coefs$lm_moran[1] = round(lm.morantest(lm, nb2listw(nb))$p.value,5)
coefs$vif[1] = VIF(lmsf)
coefs$adjr2[1] = summary(lmsf)$adj.r.squared
coefs$iteration[1] = i

out = rbind(out,coefs)

}

out$var = rep(vars,iterations)
write.csv(out, paste("C:/bcb/workspace/",name,".csv",sep=""), row.names=T)

}

agg.models <- function(csv) {
z=read.csv(csv)
agg = aggregate(z, by=list(var=z$var), FUN=mean)
target = c("intercept","pre.fire.nbr","dnbr","gsp","winp","mat","mmax","mmin","dist_unburn","curv","slope","trasp")
agg[match(target, agg$var),]
}

# Residual plot
res = resid(lmsf)
plot(dfs$prec9yr, res)


# ----------- EGLEY
EGLEY$dnbr = log(EGLEY$dnbr)
# EGLEY$dist_unburn is highly skewed

date()
loop.spatial.filtered.lm(EGLEY, 0.01, 100, "EGLEY")
date() # 4 min for 1%,  >24 hr for 10%
agg.models("C:/bcb/workspace/EGLEY.csv")


# ----------- HAYMAN
date()
HAYMAN = HAYMAN[-which(is.na(HAYMAN$gsp) == T),]
loop.spatial.filtered.lm(HAYMAN, 0.01, 100, "HAYMAN")
date() # 15 min
agg.models("C:/bcb/workspace/HAYMAN.csv")


# ----------- JASPER
date()
JASPER = JASPER[-which(is.na(JASPER$gsp) == T),]
loop.spatial.filtered.lm(JASPER, 0.01, 100, "JASPER")
date()
agg.models("C:/bcb/workspace/JASPER.csv")


# ----------- SCHOOL
date()
loop.spatial.filtered.lm(SCHOOL, 0.01, 100, "SCHOOL")
date() # < 1 min
agg.models("C:/bcb/workspace/SCHOOL.csv")


# ----------- BMCR
date()
loop.spatial.filtered.lm(BMCR, 0.01, 100, "BMCR")
date() # < 1 min
agg.models("C:/bcb/workspace/BMCR.csv")


# ----------- Cascade
date()
loop.spatial.filtered.lm(C, 0.01, 100, "C")
date() # 3 hr
agg.models("C:/bcb/workspace/C.csv")


# ----------- East Zone
date()
EZ = EZ[-which(is.na(EZ$curv) == T),]
loop.spatial.filtered.lm(EZ, 0.01, 100, "EZ")
date() # 54 min
agg.models("C:/bcb/workspace/EZ.csv")

# ----------- WCR
date()
loop.spatial.filtered.lm(WCR, 0.01, 100, "WCR")
date() # 54 min
agg.models("C:/bcb/workspace/WCR.csv")

# ----------- OLD
date()
loop.spatial.filtered.lm(OLD, 0.01, 100, "OLD")
date() # 54 min
agg.models("C:/bcb/workspace/OLD.csv")





# date()
# loop.spatial.filtered.lm(PIPO, 0.01, 100, "PIPO")
# date()

# date()
# loop.spatial.filtered.lm(DMC, 0.01, 100, "DMC")
# date()

# date()
# loop.spatial.filtered.lm(MMC, 0.01, 100, "MMC")
# date()

# date()
# loop.spatial.filtered.lm(OLD, 0.01, 100, "OLD")
# date()

# setwd("C:/bcb/projects/jfsp_recovery/manuscript/")
# z=read.csv("PIPO.csv")
# PIPOmean = aggregate(z, by=list(var=z$var), FUN=mean)
# z=read.csv("DMC.csv")
# DMCmean = aggregate(z, by=list(var=z$var), FUN=mean)
# z=read.csv("MMC.csv")
# MMCmean = aggregate(z, by=list(var=z$var), FUN=mean)
# z=read.csv("OLD.csv")
# OLDmean = aggregate(z, by=list(var=z$var), FUN=mean)

# target = c("intercept","pre.fire.nbr","dnbr","map","gsp","winp","mat","mmax","mmin","dist_unburn","curv","slope","trasp")
# PIPOmean[match(target, PIPOmean$var),]
# DMCmean[match(target, DMCmean$var),]
# MMCmean[match(target, MMCmean$var),]
# OLDmean[match(target, OLDmean$var),]






























library(gam)

PIPOgam = gam(prec2016 ~ curv + dist_unburn + dnbr + gsp + map + mat + mmax + mmin + slope +
tpi + trasp + winp + pre.fire.nbr + yrs.since.fire, data=PIPO)

PIPOlm = lm(prec2016 ~ curv + dist_unburn + dnbr + gsp + map + mat + mmax + mmin + slope +
tpi + trasp + winp + pre.fire.nbr + yrs.since.fire, data=PIPO)

PIPOgam = gam(prec2016 ~ s(x,y) + dist_unburn + map + mat, data=PIPO)























v = c("curv","d100","dd0","dd5","dist_unburn","dnbr","elev","fday","ffp","gsdd5","gsp","map","mat",
"mmax","mmin","mmindd0","mtcm","mtwm","sday","slope","smrp","smrpb","sprp","tpi","trasp","tri","winp",
"pre.fire.nbr","prec2016")

cortests = function(df, method) {

a = cor.test(df$prec2016, df$pre.fire.nbr, na.rm=T, method=method)
b = cor.test(df$prec2016, df$dnbr, na.rm=T, method=method)

c = cor.test(df$prec2016, df$d100, na.rm=T, method=method)
d = cor.test(df$prec2016, df$dd0, na.rm=T, method=method)
e = cor.test(df$prec2016, df$dd5, na.rm=T, method=method)
f = cor.test(df$prec2016, df$fday, na.rm=T, method=method)
g = cor.test(df$prec2016, df$ffp, na.rm=T, method=method)
h = cor.test(df$prec2016, df$gsdd5, na.rm=T, method=method)
i = cor.test(df$prec2016, df$gsp, na.rm=T, method=method)
j = cor.test(df$prec2016, df$map, na.rm=T, method=method)
k = cor.test(df$prec2016, df$mat, na.rm=T, method=method)
l = cor.test(df$prec2016, df$mmax, na.rm=T, method=method)
m = cor.test(df$prec2016, df$mmin, na.rm=T, method=method)
n = cor.test(df$prec2016, df$mmindd0, na.rm=T, method=method)
o = cor.test(df$prec2016, df$mtcm, na.rm=T, method=method)
p = cor.test(df$prec2016, df$mtwm, na.rm=T, method=method)
q = cor.test(df$prec2016, df$sday, na.rm=T, method=method)
r = cor.test(df$prec2016, df$smrp, na.rm=T, method=method)
s = cor.test(df$prec2016, df$smrpb, na.rm=T, method=method)
t = cor.test(df$prec2016, df$sprp, na.rm=T, method=method)
u = cor.test(df$prec2016, df$winp, na.rm=T, method=method)

v = cor.test(df$prec2016, df$curv, na.rm=T, method=method)
w = cor.test(df$prec2016, df$dist_unburn, na.rm=T, method=method)
x = cor.test(df$prec2016, df$elev, na.rm=T, method=method)
y = cor.test(df$prec2016, df$slope, na.rm=T, method=method)
z = cor.test(df$prec2016, df$tpi, na.rm=T, method=method)
a1 = cor.test(df$prec2016, df$trasp, na.rm=T, method=method)
b1 = cor.test(df$prec2016, df$tri, na.rm=T, method=method)

list(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1)
}

cortests(SCHOOL, "spearman")
cortests(EGLEY, "spearman")
cortests(EZ, "spearman")
cortests(C, "spearman")
cortests(OLD, "spearman")
cortests(WCR, "spearman")
cortests(BMCR, "spearman")
cortests(JASPER, "spearman")
cortests(HAYMAN, "spearman")


ALL = rbind(SCHOOL, EGLEY, EZ, C, OLD, WCR, BMCR, JASPER, HAYMAN)


ALL = ALL[-which(is.na(ALL$curv) == T),]
ALL = ALL[-which(is.na(ALL$d100) == T),]

ALL = ALL[-which(is.na(ALL$dd0) == T),]
ALL = ALL[-which(is.na(ALL$dd5) == T),]
ALL = ALL[-which(is.na(ALL$dist_unburn) == T),]





cortests(ALL, "spearman")

LM = lm(prec2016 ~ curv + d100 + dd0 + dd5 + dist_unburn + dnbr + fday + ffp + gsdd5 + gsp + map + mat + mmax + mmin + mmindd0 + mtcm + mtwm + sday + slope + smrp + smrpb + sprp + tpi + trasp + tri + winp + yrs.since.fire + pre.fire.nbr, data=ALL)

library(MASS)
step <- stepAIC(LM, direction="both")
step$anova # display results




summary(LM)











































































# --- Bar graph showing number of patches for each fire
npatch <- read.table(text = "Egley   Hayman   Jasper   School   CEZ   BMCR   Simi   Old   WCR 
1 42674 62691 29157 18214 203444 6855 8100 23178 35423", header = TRUE)
sum(npatch) # 429736


tiff(filename = "C:/Users/benjamincbright/Desktop/npatches_for_each_fire.tif", width=21, height=7, units="cm", res=600, compression="lzw")

par(mar=c(4.5,4.5,1,1)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.

barplot(as.matrix(npatch)*.001, las=1)
title(ylab=expression("Number of patches " ~ (10^{3})), xlab="Fire")

dev.off()








plot.dnbr.ts <- function(df, ylim, name, filename) {

dnbr.low.mean = apply(df[which(df$sev==1),83:103], 2, mean, na.rm=T)
dnbr.mod.mean = apply(df[which(df$sev==2),83:103], 2, mean, na.rm=T)
dnbr.high.mean = apply(df[which(df$sev==3),83:103], 2, mean, na.rm=T)
dnbr.low.sd = apply(df[which(df$sev==1),83:103], 2, sd, na.rm=T)
dnbr.mod.sd = apply(df[which(df$sev==2),83:103], 2, sd, na.rm=T)
dnbr.high.sd = apply(df[which(df$sev==3),83:103], 2, sd, na.rm=T)

col=c("aquamarine2","gold2","red")
lwd = 2
ylim = ylim

tiff(filename = paste("C:/Users/benjamincbright/Desktop/",filename,sep=""), width=16, height=8, units="cm", res=600, compression="lzw")

par(mar=c(4,4,2,1)) # c(bottom, left, top, right), default is c(5, 4, 4, 2) + 0.1.

plot(1996:2016-.1, dnbr.low.mean, pch=18, type="l", axes=F, ylab="Differenced Normalized Burn Ratio", xlab="Year", col=col[1], ylim=ylim, lwd=lwd)
abline(h=0)
lines(1996:2016, dnbr.mod.mean, pch=18, type="l", col=col[2], ylab="", xlab="", ylim=ylim, lwd=lwd)
lines(1996:2016+.1,dnbr.high.mean, pch=18, type="l", col=col[3], ylab="", xlab="", ylim=ylim, lwd=lwd)

arrows(1996:2016-.1, dnbr.low.mean+dnbr.low.sd, 1996:2016-.1, dnbr.low.mean-dnbr.low.sd, angle=90, code=3, length=0.04, col=col[1], ann=F, cex=2, lwd=lwd)
arrows(1996:2016, dnbr.mod.mean+dnbr.mod.sd, 1996:2016, dnbr.mod.mean-dnbr.mod.sd, angle=90, code=3, length=0.04, col=col[2], ann=F, cex=2, lwd=lwd)
arrows(1996:2016+.1, dnbr.high.mean+dnbr.high.sd, 1996:2016+.1, dnbr.high.mean-dnbr.high.sd, angle=90, code=3, length=0.04, col=col[3], ann=F, cex=2, lwd=lwd)

box()
axis(2,las=2)
axis(1, at=c(1996,2000,2004,2008,2012,2016), labels=c(1996,2000,2004,2008,2012,2016))
title(main=name, cex.main = 1)

legend("bottomleft",c("High","Moderate","Low"), col=rev(col), bty="n", lty=1, lwd=2, title="Burn Severity", pt.cex=1, cex=.8)

dev.off()

}

plot.dnbr.ts(BMCR, c(-700,150), "Black Mountain 2 and Cooney Ridge, MT", "BMCR_dnbr_ts.tif")
plot.dnbr.ts(CEZ, c(-600,100), "Cascade and East Zone, ID", "CEZ_nbr_ts.tif")
plot.dnbr.ts(EGLEY, c(-600,100), "Egley, OR", "EGLEY_nbr_ts.tif")
plot.dnbr.ts(JASPER, c(-700,100), "Jasper, SD", "JASPER_nbr_ts.tif")
plot.dnbr.ts(OLD, c(-600,100), "Old, CA", "OLD_nbr_ts.tif")
plot.dnbr.ts(HAYMAN, c(-600,100), "Hayman, CO", "HAYMAN_nbr_ts.tif")
plot.dnbr.ts(WCR, c(-600,150), "Wedge Canyon and Robert, MT", "WCR_nbr_ts.tif")



# Random forest models that show which varibles are important predictors of % vegetation recovery
library(randomForest)
library(rfUtilities)

rf.imp.vars <- function(df, nsamples) {

# I took out "pre.fire.nbr"
v = c("curv","d100","dd0","dd5","dist_unburn","elev","fday","ffp","gsdd5","gsp","map","mat_tenths","mmax_tenths","mmin_tenths",
"mmindd0","mtcm_tenths","mtwm_tenths","sday","slope","smrp","smrpb","smrsprpb","sprp","tpi","trasp","tri","winp","yrs.since.fire",
"dmag","prec2016")

xy = df[v]

xy = xy[complete.cases(xy),] # Remove rows that contain NA measurements

s = sample(1:nrow(xy),nsamples)
xy = xy[s,]

rf.modelSel(xdata=xy[,-30], ydata=xy$prec2016, r=seq(.1,.9,.1), imp.scale="mir", parsimony=0.1, final.model=T)

}


date()
rf = rf.imp.vars(EGLEY,5000)
date()
rf = rf.imp.vars(HAYMAN,5000)
date()
rf = rf.imp.vars(JASPER,5000)
date()
rf = rf.imp.vars(SCHOOL,5000)
date()
rf = rf.imp.vars(CEZ,5000)
date()
rf = rf.imp.vars(BMCR,5000)
date()
rf = rf.imp.vars(WCR,5000)
date()
rf = rf.imp.vars(SIMI,5000)
date()
rf = rf.imp.vars(OLD,5000)
date()


ALL = rbind(EGLEY, HAYMAN, JASPER, SCHOOL, CEZ, BMCR, WCR, SIMI, OLD)
date()
rf = rf.imp.vars(ALL,10000)
date()






# Some variable exploration of important variables in RF models
library(MASS)

k=kde2d(EGLEY$prec2016, EGLEY$map, n=100) # Doesn't make sense, less precip., more recovered, maybe recovery had less distance to go in already dry sites?
image(k)

k=kde2d(HAYMAN$prec2016, HAYMAN$smrsprpb, n=100) # Makes sense, more rain in the summer, more recovered
image(k)

k=kde2d(JASPER$prec2016, JASPER$elev, n=100) # Higher elevation sites recovering more
image(k)
k=kde2d(JASPER$prec2016, JASPER$smrp, n=100) # Makes sense, more rain in summer, more recovered
image(k)

k=kde2d(SCHOOL$prec2016, SCHOOL$map, n=100) # Pattern unclear
image(k)

k=kde2d(CEZ$prec2016, CEZ$mtcm_tenths, n=100) # Doesn't make sense, colder sites more recovered, maybe same as EGLEY, less distance to recover in already colder sites?
image(k)

k=kde2d(BMCR$prec2016, BMCR$dd0, n=100) # Doesn't make sense, colder sites more recovered
image(k)

plot(WCR$prec2016, WCR$dd5) # Doesn't make sense, colder sites more recovered

plot(SIMI$prec2016, SIMI$mtwm) # Pattern unclear

k=kde2d(OLD$prec2016, OLD$elev, n=100)
image(k)
plot(OLD$prec2016, OLD$elev) # Higher sites more recovered































# Plot post-burn trajectories by stepping through rows - pretty variable, but I will assume linear recovery
plot(unlist(z[1,20:52]))


# Predict recovery duration (yrs) and rate (%) to pre-fire NBR with a linear model for each patch
# https://davetang.org/muse/2013/05/09/on-curve-fitting/
#z$rec.dur = rep(-1,nrow(z))
#z$rec.rate = rep(-1,nrow(z))
#for (i in 1:nrow(z)) {
#y=unlist(z[i,40:52])
#x=1:(length(y))
#fit = lm(y~x)
#p = predict(fit, data.frame(x=c(1:100))) # Predict NBR for 100 years post fire
#z$rec.dur[i] = min(which(p > z$nbr2003[i])) # Year at which NBR reaches pre-fire NBR
#z$rec.rate[i] = round(fit$coefficients[2]/z$dmag[i]*100,2) # Recovery rate (lm slope divided by disturbance magnitude)
#}
#inf = which(is.infinite(z$rec.dur) ==T ) # Patches where NBR is declining, flat, or will take longer than 100 years
#z = z[-inf,]