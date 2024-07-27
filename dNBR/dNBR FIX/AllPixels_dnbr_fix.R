### Inputs ###
#setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# Fire Event Tables
#####
# Period 1
P1_0_tbl <- read.csv('Fire History/Period 1/P1_0_FireHistory2.csv')
P1_1_tbl <- read.csv('Fire History/Period 1/P1_1_FireHistory2.csv')
P1_2_tbl <- read.csv('Fire History/Period 1/P1_2_FireHistory2.csv')
P1_3_tbl <- read.csv('Fire History/Period 1/P1_3_FireHistory2.csv')
P1_4_tbl <- read.csv('Fire History/Period 1/P1_4_FireHistory2.csv')
P1_5_tbl <- read.csv('Fire History/Period 1/P1_5_FireHistory2.csv')

# Period 2
P2_0_tbl <- read.csv('Fire History/Period 2/P2_0_FireHistory2.csv')
P2_1_tbl <- read.csv('Fire History/Period 2/P2_1_FireHistory2.csv')
P2_2_tbl <- read.csv('Fire History/Period 2/P2_2_FireHistory2.csv')
P2_3_tbl <- read.csv('Fire History/Period 2/P2_3_FireHistory2.csv')
P2_4_tbl <- read.csv('Fire History/Period 2/P2_4_FireHistory2.csv')

# Period 3
P3_0_tbl <- read.csv('Fire History/Period 3/P3_0_FireHistory2.csv')
P3_1_tbl <- read.csv('Fire History/Period 3/P3_1_FireHistory2.csv')
P3_2_tbl <- read.csv('Fire History/Period 3/P3_2_FireHistory2.csv')
P3_3_tbl <- read.csv('Fire History/Period 3/P3_3_FireHistory2.csv')
P3_4_tbl <- read.csv('Fire History/Period 3/P3_4_FireHistory2.csv')
P3_5_tbl <- read.csv('Fire History/Period 3/P3_5_FireHistory2.csv')
P3_6_tbl <- read.csv('Fire History/Period 3/P3_6_FireHistory2.csv')
P3_7_tbl <- read.csv('Fire History/Period 3/P3_7_FireHistory2.csv')
P3_8_tbl <- read.csv('Fire History/Period 3/P3_8_FireHistory2.csv')
P3_9_tbl <- read.csv('Fire History/Period 3/P3_9_FireHistory2.csv')
P3_10_tbl <- read.csv('Fire History/Period 3/P3_10_FireHistory2.csv')
P3_11_tbl <- read.csv('Fire History/Period 3/P3_11_FireHistory2.csv')
P3_12_tbl <- read.csv('Fire History/Period 3/P3_12_FireHistory2.csv')

# Period 4
P4_0_tbl <- read.csv('Fire History/Period 4/P4_0_FireHistory2.csv')
P4_1_tbl <- read.csv('Fire History/Period 4/P4_1_FireHistory2.csv')
P4_2_tbl <- read.csv('Fire History/Period 4/P4_2_FireHistory2.csv')
P4_3_tbl <- read.csv('Fire History/Period 4/P4_3_FireHistory2.csv')
P4_4_tbl <- read.csv('Fire History/Period 4/P4_4_FireHistory2.csv')
P4_5_tbl <- read.csv('Fire History/Period 4/P4_5_FireHistory2.csv')

# Period 5
P5_0_tbl <- read.csv('Fire History/Period 5/P5_0_FireHistory2.csv')
P5_1_tbl <- read.csv('dNBR FIX/P5_1_dNBR_Fix2.csv')
P5_2_tbl <- read.csv('Fire History/Period 5/P5_2_FireHistory2.csv')
P5_3_tbl <- read.csv('Fire History/Period 5/P5_3_FireHistory2.csv')
P5_4_tbl <- read.csv('Fire History/Period 5/P5_4_FireHistory2.csv')
P5_5_tbl <- read.csv('Fire History/Period 5/P5_5_FireHistory2.csv')
P5_6_tbl <- read.csv('Fire History/Period 5/P5_6_FireHistory2.csv')
P5_7_tbl <- read.csv('Fire History/Period 5/P5_7_FireHistory2.csv')
P5_8_tbl <- read.csv('Fire History/Period 5/P5_8_FireHistory2.csv')
P5_9_tbl <- read.csv('Fire History/Period 5/P5_9_FireHistory2.csv')
P5_10_tbl <- read.csv('Fire History/Period 5/P5_10_FireHistory2.csv')
P5_11_tbl <- read.csv('dNBR FIX/P5_11_dNBR_Fix2.csv')
P5_12_tbl <- read.csv('dNBR FIX/P5_12_dNBR_Fix2.csv')

# Period 6
P6_0_tbl <- read.csv('dNBR FIX/P6_0_dNBR_Fix2.csv')
P6_1_tbl <- read.csv('dNBR FIX/P6_1_dNBR_Fix2.csv')
P6_2_tbl <- read.csv('dNBR FIX/P6_2_dNBR_Fix2.csv')
P6_3_tbl <- read.csv('dNBR FIX/P6_3_dNBR_Fix2.csv')
#####

P1_0_dNBR = P1_0_tbl[,5, drop = FALSE]
P1_0_dNBR$ID = "P1_0"
P1_0_dNBR$LT = "Forest"
P1_0_dNBR$ForestPer = 95
P1_0_dNBR$SeasonEnd = 1990.1

P1_1_dNBR = P1_1_tbl[,5, drop = FALSE]
P1_1_dNBR$ID = "P1_1"
P1_1_dNBR$LT = "Mixed"
P1_1_dNBR$ForestPer = 90
P1_1_dNBR$SeasonEnd = 1990.2

P1_2_dNBR = P1_2_tbl[,5, drop = FALSE]
P1_2_dNBR$ID = "P1_2"
P1_2_dNBR$LT = "Mixed"
P1_2_dNBR$ForestPer = 81.3
P1_2_dNBR$SeasonEnd = 1991.2

P1_3_dNBR <- P1_3_tbl[,5, drop = FALSE]
P1_3_dNBR$ID <- "P1_3"
P1_3_dNBR$LT = "Forest"
P1_3_dNBR$ForestPer = 99.9
P1_3_dNBR$SeasonEnd = 1992

P1_4_dNBR = P1_4_tbl[,5, drop = FALSE]
P1_4_dNBR$ID = "P1_4"
P1_4_dNBR$LT = "Forest"
P1_4_dNBR$ForestPer = 99.4
P1_4_dNBR$SeasonEnd = 1991.1

P1_5_dNBR = P1_5_tbl[,5, drop = FALSE]
P1_5_dNBR$ID = "P1_5"
P1_5_dNBR$LT = "Forest"
P1_5_dNBR$ForestPer = 94.9
P1_5_dNBR$SeasonEnd = 1989

P2_0_dNBR = P2_0_tbl[,5, drop = FALSE]
P2_0_dNBR$ID = "P2_0"
P2_0_dNBR$LT = "Non-Forest"
P2_0_dNBR$ForestPer = 45.3
P2_0_dNBR$SeasonEnd = 1995

P2_1_dNBR = P2_1_tbl[,5, drop = FALSE]
P2_1_dNBR$ID = "P2_1"
P2_1_dNBR$LT = "Mixed"
P2_1_dNBR$ForestPer = 79.2
P2_1_dNBR$SeasonEnd = 1994.1

P2_2_dNBR = P2_2_tbl[,5, drop = FALSE]
P2_2_dNBR$ID = "P2_2"
P2_2_dNBR$LT = "Forest"
P2_2_dNBR$ForestPer = 95
P2_2_dNBR$SeasonEnd = 1994.2

P2_3_dNBR = P2_3_tbl[,5, drop = FALSE]
P2_3_dNBR$ID = "P2_3"
P2_3_dNBR$LT = "Forest"
P2_3_dNBR$ForestPer = 99.6
P2_3_dNBR$SeasonEnd = 1997.1

P2_4_dNBR = P2_4_tbl[,5, drop = FALSE]
P2_4_dNBR$ID = "P2_4"
P2_4_dNBR$LT = "Forest"
P2_4_dNBR$ForestPer = 99.8
P2_4_dNBR$SeasonEnd = 1997.2

P3_0_dNBR = P3_0_tbl[,5, drop = FALSE]
P3_0_dNBR$ID = "P3_0"
P3_0_dNBR$LT = "Forest"
P3_0_dNBR$ForestPer = 98
P3_0_dNBR$SeasonEnd = 2007.1

P3_1_dNBR = P3_1_tbl[,5, drop = FALSE]
P3_1_dNBR$ID = "P3_1"
P3_1_dNBR$LT = "Forest"
P3_1_dNBR$ForestPer = 91.6
P3_1_dNBR$SeasonEnd = 2006.2

P3_2_dNBR = P3_2_tbl[,5, drop = FALSE]
P3_2_dNBR$ID = "P3_2"
P3_2_dNBR$LT = "Non-Forest"
P3_2_dNBR$ForestPer = 31
P3_2_dNBR$SeasonEnd = 2005.2

P3_3_dNBR = P3_3_tbl[,5, drop = FALSE]
P3_3_dNBR$ID = "P3_3"
P3_3_dNBR$LT = "Non-Forest"
P3_3_dNBR$ForestPer = 49.5
P3_3_dNBR$SeasonEnd = 2005.2

P3_4_dNBR = P3_4_tbl[,5, drop = FALSE]
P3_4_dNBR$ID = "P3_4"
P3_4_dNBR$LT = "Forest"
P3_4_dNBR$ForestPer = 97.3
P3_4_dNBR$SeasonEnd = 2001.1

P3_5_dNBR = P3_5_tbl[,5, drop = FALSE]
P3_5_dNBR$ID = "P3_5"
P3_5_dNBR$LT = "Non-Forest"
P3_5_dNBR$ForestPer = 44.9
P3_5_dNBR$SeasonEnd = 2005.1

P3_6_dNBR = P3_6_tbl[,5, drop = FALSE]
P3_6_dNBR$ID = "P3_6"
P3_6_dNBR$LT = "Forest"
P3_6_dNBR$ForestPer = 94.1
P3_6_dNBR$SeasonEnd = 2003

P3_7_dNBR = P3_7_tbl[,5, drop = FALSE]
P3_7_dNBR$ID = "P3_7"
P3_7_dNBR$LT = "Mixed"
P3_7_dNBR$ForestPer = 79.8
P3_7_dNBR$SeasonEnd = 2001.2

P3_8_dNBR = P3_8_tbl[,5, drop = FALSE]
P3_8_dNBR$ID = "P3_8"
P3_8_dNBR$LT = "Forest"
P3_8_dNBR$ForestPer = 100
P3_8_dNBR$SeasonEnd = 2007.2

P3_9_dNBR = P3_9_tbl[,5, drop = FALSE]
P3_9_dNBR$ID = "P3_9"
P3_9_dNBR$LT = "Non-Forest"
P3_9_dNBR$ForestPer = 27.6
P3_9_dNBR$SeasonEnd = 2004

P3_10_dNBR = P3_10_tbl[,5, drop = FALSE]
P3_10_dNBR$ID = "P3_10"
P3_10_dNBR$LT = "Forest"
P3_10_dNBR$ForestPer = 95.5
P3_10_dNBR$SeasonEnd = 2006.1

P3_11_dNBR = P3_11_tbl[,5, drop = FALSE]
P3_11_dNBR$ID = "P3_11"
P3_11_dNBR$LT = "Mixed"
P3_11_dNBR$ForestPer = 78.8
P3_11_dNBR$SeasonEnd = 2007.1

P3_12_dNBR = P3_12_tbl[,5, drop = FALSE]
P3_12_dNBR$ID = "P3_12"
P3_12_dNBR$LT = "Mixed"
P3_12_dNBR$ForestPer = 91.1
P3_12_dNBR$SeasonEnd = 2007.1

P4_0_dNBR = P4_0_tbl[,5, drop = FALSE]
P4_0_dNBR$ID = "P4_0"
P4_0_dNBR$LT = "Mixed"
P4_0_dNBR$ForestPer = 75.6
P4_0_dNBR$SeasonEnd = 2008.2

P4_1_dNBR = P4_1_tbl[,5, drop = FALSE]
P4_1_dNBR$ID = "P4_1"
P4_1_dNBR$LT = "Mixed"
P4_1_dNBR$ForestPer = 77.2
P4_1_dNBR$SeasonEnd = 2008.2

P4_2_dNBR = P4_2_tbl[,5, drop = FALSE]
P4_2_dNBR$ID = "P4_2"
P4_2_dNBR$LT = "Forest"
P4_2_dNBR$ForestPer = 99
P4_2_dNBR$SeasonEnd = 2008.1

P4_3_dNBR = P4_3_tbl[,5, drop = FALSE]
P4_3_dNBR$ID = "P4_3"
P4_3_dNBR$LT = "Forest"
P4_3_dNBR$ForestPer = 98.4
P4_3_dNBR$SeasonEnd = 2008.2

P4_4_dNBR = P4_4_tbl[,5, drop = FALSE]
P4_4_dNBR$ID = "P4_4"
P4_4_dNBR$LT = "Non-Forest"
P4_4_dNBR$ForestPer = 28.4
P4_4_dNBR$SeasonEnd = 2008.2

P4_5_dNBR = P4_5_tbl[,5, drop = FALSE]
P4_5_dNBR$ID = "P4_5"
P4_5_dNBR$LT = "Forest"
P4_5_dNBR$ForestPer = 99.4
P4_5_dNBR$SeasonEnd = 2008.2

P5_0_dNBR = P5_0_tbl[,5, drop = FALSE]
P5_0_dNBR$ID = "P5_0"
P5_0_dNBR$LT = "Non-Forest"
P5_0_dNBR$ForestPer = 12.5
P5_0_dNBR$SeasonEnd = 2011.1

P5_1_dNBR = P5_1_tbl[,6, drop = FALSE]
P5_1_dNBR$ID = "P5_1"
P5_1_dNBR$LT = "Non-Forest"
P5_1_dNBR$ForestPer = 22.2
P5_1_dNBR$SeasonEnd = 2018.2

P5_2_dNBR = P5_2_tbl[,5, drop = FALSE]
P5_2_dNBR$ID = "P5_2"
P5_2_dNBR$LT = "Forest"
P5_2_dNBR$ForestPer = 100
P5_2_dNBR$SeasonEnd = 2012.1

P5_3_dNBR = P5_3_tbl[,5, drop = FALSE]
P5_3_dNBR$ID = "P5_3"
P5_3_dNBR$LT = "Mixed"
P5_3_dNBR$ForestPer = 84.7
P5_3_dNBR$SeasonEnd = 2012.2

P5_4_dNBR = P5_4_tbl[,5, drop = FALSE]
P5_4_dNBR$ID = "P5_4"
P5_4_dNBR$LT = "Forest"
P5_4_dNBR$ForestPer = 98.3
P5_4_dNBR$SeasonEnd = 2016.2

P5_5_dNBR = P5_5_tbl[,5, drop = FALSE]
P5_5_dNBR$ID = "P5_5"
P5_5_dNBR$LT = "Forest"
P5_5_dNBR$ForestPer = 94.5
P5_5_dNBR$SeasonEnd = 2014

P5_6_dNBR = P5_6_tbl[,5, drop = FALSE]
P5_6_dNBR$ID = "P5_6"
P5_6_dNBR$LT = "Forest"
P5_6_dNBR$ForestPer = 100
P5_6_dNBR$SeasonEnd = 2015

P5_7_dNBR = P5_7_tbl[,5, drop = FALSE]
P5_7_dNBR$ID = "P5_7"
P5_7_dNBR$LT = "Non-Forest"
P5_7_dNBR$ForestPer = 9.3
P5_7_dNBR$SeasonEnd = 2018.1

P5_8_dNBR = P5_8_tbl[,5, drop = FALSE]
P5_8_dNBR$ID = "P5_8"
P5_8_dNBR$LT = "Forest"
P5_8_dNBR$ForestPer = 92.1
P5_8_dNBR$SeasonEnd = 2016.1

P5_9_dNBR = P5_9_tbl[,5, drop = FALSE]
P5_9_dNBR$ID = "P5_9"
P5_9_dNBR$LT = "Forest"
P5_9_dNBR$ForestPer = 100
P5_9_dNBR$SeasonEnd = 2017

P5_10_dNBR = P5_10_tbl[,5, drop = FALSE]
P5_10_dNBR$ID = "P5_10"
P5_10_dNBR$LT = "Mixed"
P5_10_dNBR$ForestPer = 72.3
P5_10_dNBR$SeasonEnd = 2011.2

P5_11_dNBR = P5_11_tbl[,6, drop = FALSE]
P5_11_dNBR$ID = "P5_11"
P5_11_dNBR$LT = "Mixed"
P5_11_dNBR$ForestPer = 75.6
P5_11_dNBR$SeasonEnd = 2019

P5_12_dNBR = P5_12_tbl[,6, drop = FALSE]
P5_12_dNBR$ID = "P5_12"
P5_12_dNBR$LT = "Forest"
P5_12_dNBR$ForestPer = 97.7
P5_12_dNBR$SeasonEnd = 2017

P6_0_dNBR = P6_0_tbl[,6, drop = FALSE]
P6_0_dNBR$ID = "P6_0"
P6_0_dNBR$LT = "Non-Forest"
P6_0_dNBR$ForestPer = 26
P6_0_dNBR$SeasonEnd = 2020.1

P6_1_dNBR = P6_1_tbl[,6, drop = FALSE]
P6_1_dNBR$ID = "P6_1"
P6_1_dNBR$LT = "Mixed"
P6_1_dNBR$ForestPer = 70.6
P6_1_dNBR$SeasonEnd = 2020.2

P6_2_dNBR = P6_2_tbl[,6, drop = FALSE]
P6_2_dNBR$ID = "P6_2"
P6_2_dNBR$LT = "Mixed"
P6_2_dNBR$ForestPer = 72.9
P6_2_dNBR$SeasonEnd = 2020.2

P6_3_dNBR = P6_3_tbl[,6, drop = FALSE]
P6_3_dNBR$ID = "P6_3"
P6_3_dNBR$LT = "Forest"
P6_3_dNBR$ForestPer = 92.9
P6_3_dNBR$SeasonEnd = 2020.3

AllPixels_dnbr = rbind(P1_0_dNBR, P1_1_dNBR, P1_2_dNBR, P1_3_dNBR, P1_4_dNBR, P1_5_dNBR, P2_0_dNBR,
                          P2_1_dNBR, P2_2_dNBR, P2_3_dNBR, P2_4_dNBR, P3_0_dNBR, P3_1_dNBR, P3_2_dNBR,
                          P3_3_dNBR, P3_4_dNBR, P3_5_dNBR, P3_6_dNBR, P3_7_dNBR, P3_8_dNBR, P3_9_dNBR,
                          P3_10_dNBR, P3_11_dNBR, P3_12_dNBR, P4_0_dNBR, P4_1_dNBR, P4_2_dNBR, P4_3_dNBR,
                          P4_4_dNBR, P4_5_dNBR, P5_0_dNBR, P5_1_dNBR, P5_2_dNBR, P5_3_dNBR, P5_4_dNBR,
                          P5_5_dNBR, P5_6_dNBR, P5_7_dNBR, P5_8_dNBR, P5_9_dNBR, P5_10_dNBR, P5_11_dNBR,
                          P5_12_dNBR, P6_0_dNBR, P6_1_dNBR, P6_2_dNBR, P6_3_dNBR)

write.csv(AllPixels_dnbr, "AllPixels_dnbr_fix.csv")
