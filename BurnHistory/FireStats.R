### This code extracts useful statistics from fire events ###

### Inputs ###
#setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

library(ggplot2) # Figure creation
library(ggpmisc) # More figure creation
library(ggpubr) # More figure creation
library(cowplot) # More figure creation
library(RColorBrewer) # More figure creation
library(gridExtra) # More figure creation

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
P5_1_tbl <- read.csv('Fire History/Period 5/P5_1_FireHistory2.csv')
P5_2_tbl <- read.csv('Fire History/Period 5/P5_2_FireHistory2.csv')
P5_3_tbl <- read.csv('Fire History/Period 5/P5_3_FireHistory2.csv')
P5_4_tbl <- read.csv('Fire History/Period 5/P5_4_FireHistory2.csv')
P5_5_tbl <- read.csv('Fire History/Period 5/P5_5_FireHistory2.csv')
P5_6_tbl <- read.csv('Fire History/Period 5/P5_6_FireHistory2.csv')
P5_7_tbl <- read.csv('Fire History/Period 5/P5_7_FireHistory2.csv')
P5_8_tbl <- read.csv('Fire History/Period 5/P5_8_FireHistory2.csv')
P5_9_tbl <- read.csv('Fire History/Period 5/P5_9_FireHistory2.csv')
P5_10_tbl <- read.csv('Fire History/Period 5/P5_10_FireHistory2.csv')
P5_11_tbl <- read.csv('Fire History/Period 5/P5_11_FireHistory2.csv')
P5_12_tbl <- read.csv('Fire History/Period 5/P5_12_FireHistory2.csv')

# Period 6
P6_0_tbl <- read.csv('Fire History/Period 6/P6_0_FireHistory2.csv')
P6_1_tbl <- read.csv('Fire History/Period 6/P6_1_FireHistory2.csv')
P6_2_tbl <- read.csv('Fire History/Period 6/P6_2_FireHistory2.csv')
P6_3_tbl <- read.csv('Fire History/Period 6/P6_3_FireHistory2.csv')
#####

# Overall Stats 
AllFires_tbl <- read.csv('FireEvents_Variables.csv')
Groups <- read.csv('FireEvents_Groups.csv')
AllFires_tbl$MissingData = factor(AllFires_tbl$MissingData, 
                                            levels = c("0", "1"))

### Times burned in last X years ###
#####
# Average
# mean(P6_3_tbl$Burned_Last70, na.rm = TRUE)
# mean(P6_3_tbl$Burned_Last60, na.rm = TRUE)
# mean(P6_3_tbl$Burned_Last50, na.rm = TRUE)
# mean(P6_3_tbl$Burned_Last40, na.rm = TRUE)
# mean(P6_3_tbl$Burned_Last30, na.rm = TRUE)
# mean(P6_3_tbl$Burned_Last20, na.rm = TRUE)
# mean(P6_3_tbl$Burned_Last10, na.rm = TRUE)
# 
# # Standard Deviation
# sd(P6_3_tbl$Burned_Last70, na.rm = TRUE)
# sd(P6_3_tbl$Burned_Last60, na.rm = TRUE)
# sd(P6_3_tbl$Burned_Last50, na.rm = TRUE)
# sd(P6_3_tbl$Burned_Last40, na.rm = TRUE)
# sd(P6_3_tbl$Burned_Last30, na.rm = TRUE)
# sd(P6_3_tbl$Burned_Last20, na.rm = TRUE)
# sd(P6_3_tbl$Burned_Last10, na.rm = TRUE)
#####

### Years since last burn ###
#####
# Set all NAs to 70
# sum(is.na(P6_3_tbl$SinceLastBurn)) / nrow(P6_3_tbl) * 100 # % of Yrs since last burn that are NA
# mean(P6_3_tbl$SinceLastBurn, na.rm = TRUE) # Mean without NAs
# sd(P6_3_tbl$SinceLastBurn, na.rm = TRUE) # SD without NAs
# P6_3_tbl$SinceLastBurn[is.na(P6_3_tbl$SinceLastBurn)] <- 70 # Set NAs to 70 years
# mean(P6_3_tbl$SinceLastBurn) # Mean where NAs = 70 years
# sd(P6_3_tbl$SinceLastBurn) # SD where NAs = 70 years

# Percent burn by fire severity #
# burned_pixels <- nrow(P3_3_tbl)
# lowsev_pixels <- sum(P1_0_tbl$dNBR < 270)  # Low severity burn = 100 - 269.999 dNBR
#  lowmodsev_pixels <- sum(P6_3_tbl$dNBR < 660) # Low & Mod severity burn
# modhisev_pixels <- sum(P3_3_tbl$dNBR > 269.999) # Mod & High severity burn
# hisev_pixels <- sum(P6_3_tbl$dNBR > 659.999) # High severity burn
# modhisev_pixels / burned_pixels * 100
#####

### Topographic variables ###
#####
# mean(P6_3_tbl$Elev)
# mean(P6_3_tbl$Elev3)
# mean(P6_3_tbl$Elev_Slope)
# mean(P6_3_tbl$Elev_Slope3)
# mean(P6_3_tbl$Aspect)
# mean(P6_3_tbl$Aspect3)
# mean(P6_3_tbl$TPI3)
# mean(P6_3_tbl$TPI5)
# mean(P6_3_tbl$TPI9)
# mean(P6_3_tbl$TPI15)
# mean(P6_3_tbl$TPI31)
# mean(P6_3_tbl$TPI65, na.rm = TRUE)
# mean(P6_3_tbl$TPI111, na.rm = TRUE)
# mean(P6_3_tbl$TWI, na.rm = TRUE)
# mean(P6_3_tbl$TWI3, na.rm = TRUE)
# 
# sd(P6_3_tbl$Elev)
# sd(P6_3_tbl$Elev3)
# sd(P6_3_tbl$Elev_Slope)
# sd(P6_3_tbl$Elev_Slope3)
# sd(P6_3_tbl$Aspect)
# sd(P6_3_tbl$Aspect3)
# sd(P6_3_tbl$TPI3)
# sd(P6_3_tbl$TPI5)
# sd(P6_3_tbl$TPI9)
# sd(P6_3_tbl$TPI15)
# sd(P6_3_tbl$TPI31)
# sd(P6_3_tbl$TPI65, na.rm = TRUE)
# sd(P6_3_tbl$TPI111, na.rm = TRUE)
# sd(P6_3_tbl$TWI, na.rm = TRUE)
# sd(P6_3_tbl$TWI3, na.rm = TRUE)
#####

### Add 5 yr slope for all pixels with at least 5 years of recovery ###
#####
# Slope <- function(x) {
#   if(sum(is.na(x)) > 0) {
#     NA
#   } else {
#   TempDF <- data.frame(x, years = 0:5)
#   lm(x ~ years, data <- TempDF)$coefficients[2]
#   }
# }
# 
# TData <- as.data.frame(t(P5_10_tbl[,9:14]))
# P5_10_tbl$Fiveyr_Slope <- sapply(TData, Slope)  
# 
# mean(P5_10_tbl$Fiveyr_Slope, na.rm = TRUE)
# sd(P5_10_tbl$Fiveyr_Slope, na.rm = TRUE)

# P6_3_tbl$Fiveyr_Slope <- NA # If less than 5 years of recovery
# write.csv(P6_3_tbl, 'P6_3_FireHistory2.csv')
#####

### Correlation matrix for fire events ###
#####
# FireEvents_cor <- cor(AllFires_tbl[,2:143]) # No correction for NAs
# write.csv(FireEvents_cor, 'FireEvents_cor.csv')
# FireEvents_corNAs <- cor(AllFires_tbl[,2:146], use = "pairwise.complete.obs")
# write.csv(FireEvents_corNAs, 'FireEvents_corNAs5.csv')

### Create figures for significant correlations ###
# x <- AllFires_tbl$MissingData # Change for each figure
# y <- AllFires_tbl$PF10 # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval
# 
# ggplot(AllFires_tbl, aes(x = x, y = y)) +
#   geom_point(size = 2) +
#   geom_smooth(method = lm, color = "black") +
#   stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
#                formula = formula1, parse = TRUE, size = 5, label.y = 0.1) +
#   scale_x_continuous(name = 'MissingData', expand = c(0.01,0)) +
#   scale_y_continuous(name = 'PF10 (%)', expand = c(0.01,0)) +
#   #coord_cartesian(ylim = c(0, 95)) + # Adjusts visible range for y axis manually
#   #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
# theme(axis.title.x = element_text(size = 14),
#       axis.text.x = element_text(size = 12),
#       axis.title.y = element_text(size = 14),
#       axis.text.y = element_text(size = 12),
#       panel.background = element_blank(),
#       panel.border = element_rect(color = 'black', fill = NA))
#####

##### Extent/Severity/Recovery vs. Variables #####
# Extent table
AllFires_extent = as.data.frame(AllFires_tbl[,c(1,5:6,22:23)])
colnames(AllFires_extent) = c("EventID", "Burned_km2", "Burned_km2_log", "Landscape", "ForestPer")
AllFires_extent$Extent = Groups$ExtGroup
AllFires_extent$Extent = factor(AllFires_extent$Extent, levels = c("XS", "S", "M", "L"))
AllFires_extent$Landscape = factor(AllFires_extent$Landscape, levels = c("Non-Forest","Mixed","Forest"))

# Severity table
# L = < 270
# ML = 270 - 439
# MH = 440 - 659
# H = 660+
#####
P1_0_dNBR = P1_0_tbl[,5, drop = FALSE]
P1_0_dNBR$ID = "P1_0"
median(P1_0_dNBR$dNBR)
P1_0_dNBR$Severity = "L"
P1_0_dNBR$Extent = "S"
P1_0_dNBR$Burned_km2 = 1.89
P1_0_dNBR$Landscape = "Forest"
P1_0_dNBR$ForestPer = 95
P1_0_dNBR$SinceLastBurn = 23.74429
P1_0_dNBR$SinceBurnCat = "<30"
P1_0_dNBR$BurnedLast50 = NA
P1_0_dNBR$BurnedLast50Cat = NA
P1_0_dNBR$MissingData = "1"
P1_0_dNBR$SeasonEnd = 1990.1
P1_0_dNBR$PreFireNBR = 394.58453
P1_0_dNBR$PreFireCat = "350-500"
P1_0_dNBR$maxNBR = 480.6154
P1_0_dNBR$maxCat = "450-600"

P1_1_dNBR = P1_1_tbl[,5, drop = FALSE]
P1_1_dNBR$ID = "P1_1"
median(P1_1_dNBR$dNBR)
P1_1_dNBR$Severity = "ML"
P1_1_dNBR$Extent = "XS"
P1_1_dNBR$Burned_km2 = 0.855
P1_1_dNBR$Landscape = "Mixed"
P1_1_dNBR$ForestPer = 90
P1_1_dNBR$SinceLastBurn = 25.68421
P1_1_dNBR$SinceBurnCat = "<30"
P1_1_dNBR$BurnedLast50 = NA
P1_1_dNBR$BurnedLast50Cat = NA
P1_1_dNBR$MissingData = "1"
P1_1_dNBR$SeasonEnd = 1990.2
P1_1_dNBR$PreFireNBR = 341.74591
P1_1_dNBR$PreFireCat = "<350"
P1_1_dNBR$maxNBR = 512.7774
P1_1_dNBR$maxCat = "450-600"

P1_2_dNBR = P1_2_tbl[,5, drop = FALSE]
P1_2_dNBR$ID = "P1_2"
median(P1_2_dNBR$dNBR)
P1_2_dNBR$Severity = "MH"
P1_2_dNBR$Extent = "XS"
P1_2_dNBR$Burned_km2 = 0.7173
P1_2_dNBR$Landscape = "Mixed"
P1_2_dNBR$ForestPer = 81.3
P1_2_dNBR$SinceLastBurn = 42.41029
P1_2_dNBR$SinceBurnCat = "30-50"
P1_2_dNBR$BurnedLast50 = NA
P1_2_dNBR$BurnedLast50Cat = NA
P1_2_dNBR$MissingData = "1"
P1_2_dNBR$SeasonEnd = 1991.2
P1_2_dNBR$PreFireNBR = 252.86557
P1_2_dNBR$PreFireCat = "<350"
P1_2_dNBR$maxNBR = 358.1411
P1_2_dNBR$maxCat = "<450"

P1_3_dNBR <- P1_3_tbl[,5, drop = FALSE]
P1_3_dNBR$ID <- "P1_3"
median(P1_3_dNBR$dNBR)
P1_3_dNBR$Severity = "MH"
P1_3_dNBR$Extent = "L"
P1_3_dNBR$Burned_km2 = 194.616
P1_3_dNBR$Landscape = "Forest"
P1_3_dNBR$ForestPer = 99.9
P1_3_dNBR$SinceLastBurn = 22.16222
P1_3_dNBR$SinceBurnCat = "<30"
P1_3_dNBR$BurnedLast50 = NA
P1_3_dNBR$BurnedLast50Cat = NA
P1_3_dNBR$MissingData = "1"
P1_3_dNBR$SeasonEnd = 1992
P1_3_dNBR$PreFireNBR = 414.22687
P1_3_dNBR$PreFireCat = "350-500"
P1_3_dNBR$maxNBR = 628.0835
P1_3_dNBR$maxCat = ">600"

P1_4_dNBR = P1_4_tbl[,5, drop = FALSE]
P1_4_dNBR$ID = "P1_4"
median(P1_4_dNBR$dNBR)
P1_4_dNBR$Severity = "MH"
P1_4_dNBR$Extent = "L"
P1_4_dNBR$Burned_km2 = 90.9378
P1_4_dNBR$Landscape = "Forest"
P1_4_dNBR$ForestPer = 99.4
P1_4_dNBR$SinceLastBurn = 20.73332
P1_4_dNBR$SinceBurnCat = "<30"
P1_4_dNBR$BurnedLast50 = NA
P1_4_dNBR$BurnedLast50Cat = NA
P1_4_dNBR$MissingData = "1"
P1_4_dNBR$SeasonEnd = 1991.1
P1_4_dNBR$PreFireNBR = 505.89157
P1_4_dNBR$PreFireCat = ">500"
P1_4_dNBR$maxNBR = 664.8566
P1_4_dNBR$maxCat = ">600"

P1_5_dNBR = P1_5_tbl[,5, drop = FALSE]
P1_5_dNBR$ID = "P1_5"
median(P1_5_dNBR$dNBR)
P1_5_dNBR$Severity = "ML"
P1_5_dNBR$Extent = "XS"
P1_5_dNBR$Burned_km2 = 1.1502
P1_5_dNBR$Landscape = "Forest"
P1_5_dNBR$ForestPer = 94.9
P1_5_dNBR$SinceLastBurn = 43.35211
P1_5_dNBR$SinceBurnCat = "30-50"
P1_5_dNBR$BurnedLast50 = NA
P1_5_dNBR$BurnedLast50Cat = NA
P1_5_dNBR$MissingData = "0"
P1_5_dNBR$SeasonEnd = 1989
P1_5_dNBR$PreFireNBR = 485.00571
P1_5_dNBR$PreFireCat = "350-500"
P1_5_dNBR$maxNBR = 645.6741
P1_5_dNBR$maxCat = ">600"

P2_0_dNBR = P2_0_tbl[,5, drop = FALSE]
P2_0_dNBR$ID = "P2_0"
median(P2_0_dNBR$dNBR)
P2_0_dNBR$Severity = "MH"
P2_0_dNBR$Extent = "M"
P2_0_dNBR$Burned_km2 = 9.7875
P2_0_dNBR$Landscape = "Non-Forest"
P2_0_dNBR$ForestPer = 45.3
P2_0_dNBR$SinceLastBurn = 41.92680
P2_0_dNBR$SinceBurnCat = "30-50"
P2_0_dNBR$BurnedLast50 = NA
P2_0_dNBR$BurnedLast50Cat = NA
P2_0_dNBR$MissingData = "1"
P2_0_dNBR$SeasonEnd = 1995
P2_0_dNBR$PreFireNBR = 305.81093
P2_0_dNBR$PreFireCat = "<350"
P2_0_dNBR$maxNBR = 417.0020
P2_0_dNBR$maxCat = "<450"

P2_1_dNBR = P2_1_tbl[,5, drop = FALSE]
P2_1_dNBR$ID = "P2_1"
median(P2_1_dNBR$dNBR)
P2_1_dNBR$Severity = "L"
P2_1_dNBR$Extent = "XS"
P2_1_dNBR$Burned_km2 = 0.3519
P2_1_dNBR$Landscape = "Mixed"
P2_1_dNBR$ForestPer = 79.2
P2_1_dNBR$SinceLastBurn = 17.13043
P2_1_dNBR$SinceBurnCat = "<30"
P2_1_dNBR$BurnedLast50 = NA
P2_1_dNBR$BurnedLast50Cat = NA
P2_1_dNBR$MissingData = "1"
P2_1_dNBR$SeasonEnd = 1994.1
P2_1_dNBR$PreFireNBR = 290.07819
P2_1_dNBR$PreFireCat = "<350"
P2_1_dNBR$maxNBR = 481.7349
P2_1_dNBR$maxCat = "450-600"

P2_2_dNBR = P2_2_tbl[,5, drop = FALSE]
P2_2_dNBR$ID = "P2_2"
median(P2_2_dNBR$dNBR)
P2_2_dNBR$Severity = "ML"
P2_2_dNBR$Extent = "S"
P2_2_dNBR$Burned_km2 = 1.854
P2_2_dNBR$Landscape = "Forest"
P2_2_dNBR$ForestPer = 95
P2_2_dNBR$SinceLastBurn = 24.20971
P2_2_dNBR$SinceBurnCat = "<30"
P2_2_dNBR$BurnedLast50 = NA
P2_2_dNBR$BurnedLast50Cat = NA
P2_2_dNBR$MissingData = "0"
P2_2_dNBR$SeasonEnd = 1994.2
P2_2_dNBR$PreFireNBR = 475.53697
P2_2_dNBR$PreFireCat = "350-500"
P2_2_dNBR$maxNBR = 525.5035
P2_2_dNBR$maxCat = "450-600"

P2_3_dNBR = P2_3_tbl[,5, drop = FALSE]
P2_3_dNBR$ID = "P2_3"
median(P2_3_dNBR$dNBR)
P2_3_dNBR$Severity = "MH"
P2_3_dNBR$Extent = "L"
P2_3_dNBR$Burned_km2 = 194.3082
P2_3_dNBR$Landscape = "Forest"
P2_3_dNBR$ForestPer = 99.6
P2_3_dNBR$SinceLastBurn = 39.03297
P2_3_dNBR$SinceBurnCat = "30-50"
P2_3_dNBR$BurnedLast50 = NA
P2_3_dNBR$BurnedLast50Cat = NA
P2_3_dNBR$MissingData = "1"
P2_3_dNBR$SeasonEnd = 1997.1
P2_3_dNBR$PreFireNBR = 478.71445
P2_3_dNBR$PreFireCat = "350-500"
P2_3_dNBR$maxNBR = 544.4494
P2_3_dNBR$maxCat = "450-600"

P2_4_dNBR = P2_4_tbl[,5, drop = FALSE]
P2_4_dNBR$ID = "P2_4"
median(P2_4_dNBR$dNBR)
P2_4_dNBR$Severity = "ML"
P2_4_dNBR$Extent = "M"
P2_4_dNBR$Burned_km2 = 23.3037
P2_4_dNBR$Landscape = "Forest"
P2_4_dNBR$ForestPer = 99.8
P2_4_dNBR$SinceLastBurn = 15.10937
P2_4_dNBR$SinceBurnCat = "<30"
P2_4_dNBR$BurnedLast50 = NA
P2_4_dNBR$BurnedLast50Cat = NA
P2_4_dNBR$MissingData = "1"
P2_4_dNBR$SeasonEnd = 1997.2
P2_4_dNBR$PreFireNBR = 517.37550
P2_4_dNBR$PreFireCat = ">500"
P2_4_dNBR$maxNBR = 625.2113
P2_4_dNBR$maxCat = ">600"

P3_0_dNBR = P3_0_tbl[,5, drop = FALSE]
P3_0_dNBR$ID = "P3_0"
median(P3_0_dNBR$dNBR)
P3_0_dNBR$Severity = "H"
P3_0_dNBR$Extent = "S"
P3_0_dNBR$Burned_km2 = 2.0232
P3_0_dNBR$Landscape = "Forest"
P3_0_dNBR$ForestPer = 98
P3_0_dNBR$SinceLastBurn = 28.26824
P3_0_dNBR$SinceBurnCat = "<30"
P3_0_dNBR$BurnedLast50 = 1.076547
P3_0_dNBR$BurnedLast50Cat = ">1"
P3_0_dNBR$MissingData = "0"
P3_0_dNBR$SeasonEnd = 2007.1
P3_0_dNBR$PreFireNBR = 524.88910
P3_0_dNBR$PreFireCat = ">500"
P3_0_dNBR$maxNBR = 549.1970
P3_0_dNBR$maxCat = "450-600"

P3_1_dNBR = P3_1_tbl[,5, drop = FALSE]
P3_1_dNBR$ID = "P3_1"
median(P3_1_dNBR$dNBR)
P3_1_dNBR$Severity = "H"
P3_1_dNBR$Extent = "XS"
P3_1_dNBR$Burned_km2 = 0.891
P3_1_dNBR$Landscape = "Forest"
P3_1_dNBR$ForestPer = 91.6
P3_1_dNBR$SinceLastBurn = 62.37980
P3_1_dNBR$SinceBurnCat = ">50"
P3_1_dNBR$BurnedLast50 = 0.331313
P3_1_dNBR$BurnedLast50Cat = "<1"
P3_1_dNBR$MissingData = "0"
P3_1_dNBR$SeasonEnd = 2006.2
P3_1_dNBR$PreFireNBR = 392.88623
P3_1_dNBR$PreFireCat = "350-500"
P3_1_dNBR$maxNBR = 556.1561
P3_1_dNBR$maxCat = "450-600"

P3_2_dNBR = P3_2_tbl[,5, drop = FALSE]
P3_2_dNBR$ID = "P3_2"
median(P3_2_dNBR$dNBR)
P3_2_dNBR$Severity = "ML"
P3_2_dNBR$Extent = "S"
P3_2_dNBR$Burned_km2 = 5.4657
P3_2_dNBR$Landscape = "Non-Forest"
P3_2_dNBR$ForestPer = 31
P3_2_dNBR$SinceLastBurn = 60.19233
P3_2_dNBR$SinceBurnCat = ">50"
P3_2_dNBR$BurnedLast50 = 0.181624
P3_2_dNBR$BurnedLast50Cat = "<1"
P3_2_dNBR$MissingData = "0"
P3_2_dNBR$SeasonEnd = 2005.2
P3_2_dNBR$PreFireNBR = 268.53548
P3_2_dNBR$PreFireCat = "<350"
P3_2_dNBR$maxNBR = 379.8742
P3_2_dNBR$maxCat = "<450"

P3_3_dNBR = P3_3_tbl[,5, drop = FALSE]
P3_3_dNBR$ID = "P3_3"
median(P3_3_dNBR$dNBR)
P3_3_dNBR$Severity = "L"
P3_3_dNBR$Extent = "XS"
P3_3_dNBR$Burned_km2 = 0.2223
P3_3_dNBR$Landscape = "Non-Forest"
P3_3_dNBR$ForestPer = 49.5
P3_3_dNBR$SinceLastBurn = 70
P3_3_dNBR$SinceBurnCat = ">50"
P3_3_dNBR$BurnedLast50 = 0
P3_3_dNBR$BurnedLast50Cat = "0"
P3_3_dNBR$MissingData = "0"
P3_3_dNBR$SeasonEnd = 2005.2
P3_3_dNBR$PreFireNBR = 400.76065
P3_3_dNBR$PreFireCat = "350-500"
P3_3_dNBR$maxNBR = 501.2032
P3_3_dNBR$maxCat = "450-600"

P3_4_dNBR = P3_4_tbl[,5, drop = FALSE]
P3_4_dNBR$ID = "P3_4"
median(P3_4_dNBR$dNBR)
P3_4_dNBR$Severity = "H"
P3_4_dNBR$Extent = "M"
P3_4_dNBR$Burned_km2 = 19.7388
P3_4_dNBR$Landscape = "Forest"
P3_4_dNBR$ForestPer = 97.3
P3_4_dNBR$SinceLastBurn = 22.64901
P3_4_dNBR$SinceBurnCat = "<30"
P3_4_dNBR$BurnedLast50 = 2.724193
P3_4_dNBR$BurnedLast50Cat = ">1"
P3_4_dNBR$MissingData = "0"
P3_4_dNBR$SeasonEnd = 2001.1
P3_4_dNBR$PreFireNBR = 480.16510
P3_4_dNBR$PreFireCat = "350-500"
P3_4_dNBR$maxNBR = 664.3716
P3_4_dNBR$maxCat = ">600"

P3_5_dNBR = P3_5_tbl[,5, drop = FALSE]
P3_5_dNBR$ID = "P3_5"
median(P3_5_dNBR$dNBR)
P3_5_dNBR$Severity = "ML"
P3_5_dNBR$Extent = "XS"
P3_5_dNBR$Burned_km2 = 0.9378
P3_5_dNBR$Landscape = "Non-Forest"
P3_5_dNBR$ForestPer = 44.9
P3_5_dNBR$SinceLastBurn = 70.00000
P3_5_dNBR$SinceBurnCat = ">50"
P3_5_dNBR$BurnedLast50 = 0
P3_5_dNBR$BurnedLast50Cat = "0"
P3_5_dNBR$MissingData = "0"
P3_5_dNBR$SeasonEnd = 2005.1
P3_5_dNBR$PreFireNBR = 198.82707
P3_5_dNBR$PreFireCat = "<350"
P3_5_dNBR$maxNBR = 312.2755
P3_5_dNBR$maxCat = "<450"

P3_6_dNBR = P3_6_tbl[,5, drop = FALSE]
P3_6_dNBR$ID = "P3_6"
median(P3_6_dNBR$dNBR)
P3_6_dNBR$Severity = "H"
P3_6_dNBR$Extent = "M"
P3_6_dNBR$Burned_km2 = 62.6607
P3_6_dNBR$Landscape = "Forest"
P3_6_dNBR$ForestPer = 94.1
P3_6_dNBR$SinceLastBurn = 27.86831
P3_6_dNBR$SinceBurnCat = "<30"
P3_6_dNBR$BurnedLast50 = 2.545927
P3_6_dNBR$BurnedLast50Cat = ">1"
P3_6_dNBR$MissingData = "0"
P3_6_dNBR$SeasonEnd = 2003
P3_6_dNBR$PreFireNBR = 467.94942
P3_6_dNBR$PreFireCat = "350-500"
P3_6_dNBR$maxNBR = 633.4238
P3_6_dNBR$maxCat = ">600"


P3_7_dNBR = P3_7_tbl[,5, drop = FALSE]
P3_7_dNBR$ID = "P3_7"
median(P3_7_dNBR$dNBR)
P3_7_dNBR$Severity = "MH"
P3_7_dNBR$Extent = "S"
P3_7_dNBR$Burned_km2 = 8.1225
P3_7_dNBR$Landscape = "Mixed"
P3_7_dNBR$ForestPer = 79.8
P3_7_dNBR$SinceLastBurn = 42.88233
P3_7_dNBR$SinceBurnCat = "30-50"
P3_7_dNBR$BurnedLast50 = 1.261274
P3_7_dNBR$BurnedLast50Cat = ">1"
P3_7_dNBR$MissingData = "0"
P3_7_dNBR$SeasonEnd = 2001.2
P3_7_dNBR$PreFireNBR = 402.31872
P3_7_dNBR$PreFireCat = "350-500"
P3_7_dNBR$maxNBR = 628.2026
P3_7_dNBR$maxCat = ">600"

P3_8_dNBR = P3_8_tbl[,5, drop = FALSE]
P3_8_dNBR$ID = "P3_8"
median(P3_8_dNBR$dNBR)
P3_8_dNBR$Severity = "H"
P3_8_dNBR$Extent = "M"
P3_8_dNBR$Burned_km2 = 21.8097
P3_8_dNBR$Landscape = "Forest"
P3_8_dNBR$ForestPer = 100
P3_8_dNBR$SinceLastBurn = 26.76342
P3_8_dNBR$SinceBurnCat = "<30"
P3_8_dNBR$BurnedLast50 = 1.681674
P3_8_dNBR$BurnedLast50Cat = ">1"
P3_8_dNBR$MissingData = "0"
P3_8_dNBR$SeasonEnd = 2007.2
P3_8_dNBR$PreFireNBR = 474.24097
P3_8_dNBR$PreFireCat = "350-500"
P3_8_dNBR$maxNBR = 649.9294
P3_8_dNBR$maxCat = ">600"

P3_9_dNBR = P3_9_tbl[,5, drop = FALSE]
P3_9_dNBR$ID = "P3_9"
median(P3_9_dNBR$dNBR)
P3_9_dNBR$Severity = "ML"
P3_9_dNBR$Extent = "XS"
P3_9_dNBR$Burned_km2 = 1.2204
P3_9_dNBR$Landscape = "Non-Forest"
P3_9_dNBR$ForestPer = 27.6
P3_9_dNBR$SinceLastBurn = 57.52212
P3_9_dNBR$SinceBurnCat = ">50"
P3_9_dNBR$BurnedLast50 = 0.570797
P3_9_dNBR$BurnedLast50Cat = "<1"
P3_9_dNBR$MissingData = "0"
P3_9_dNBR$SeasonEnd = 2004
P3_9_dNBR$PreFireNBR = 91.31171
P3_9_dNBR$PreFireCat = "<350"
P3_9_dNBR$maxNBR = 241.2082
P3_9_dNBR$maxCat = "<450"

P3_10_dNBR = P3_10_tbl[,5, drop = FALSE]
P3_10_dNBR$ID = "P3_10"
median(P3_10_dNBR$dNBR)
P3_10_dNBR$Severity = "H"
P3_10_dNBR$Extent = "M"
P3_10_dNBR$Burned_km2 = 31.5414
P3_10_dNBR$Landscape = "Forest"
P3_10_dNBR$ForestPer = 95.5
P3_10_dNBR$SinceLastBurn = 45.87588
P3_10_dNBR$SinceBurnCat = "30-50"
P3_10_dNBR$BurnedLast50 = 0.899187
P3_10_dNBR$BurnedLast50Cat = "<1"
P3_10_dNBR$MissingData = "0"
P3_10_dNBR$SeasonEnd = 2006.1
P3_10_dNBR$PreFireNBR = 528.46038
P3_10_dNBR$PreFireCat = ">500"
P3_10_dNBR$maxNBR = 554.9903
P3_10_dNBR$maxCat = "450-600"

P3_11_dNBR = P3_11_tbl[,5, drop = FALSE]
P3_11_dNBR$ID = "P3_11"
median(P3_11_dNBR$dNBR)
P3_11_dNBR$Severity = "MH"
P3_11_dNBR$Extent = "S"
P3_11_dNBR$Burned_km2 = 5.3775
P3_11_dNBR$Landscape = "Mixed"
P3_11_dNBR$ForestPer = 78.8
P3_11_dNBR$SinceLastBurn = 56.11414
P3_11_dNBR$SinceBurnCat = ">50"
P3_11_dNBR$BurnedLast50 = 0.020753
P3_11_dNBR$BurnedLast50Cat = "0"
P3_11_dNBR$MissingData = "0"
P3_11_dNBR$SeasonEnd = 2007.1
P3_11_dNBR$PreFireNBR = 388.94549
P3_11_dNBR$PreFireCat = "350-500"
P3_11_dNBR$maxNBR = 539.3515
P3_11_dNBR$maxCat = "450-600"

P3_12_dNBR = P3_12_tbl[,5, drop = FALSE]
P3_12_dNBR$ID = "P3_12"
median(P3_12_dNBR$dNBR)
P3_12_dNBR$Severity = "MH"
P3_12_dNBR$Extent = "S"
P3_12_dNBR$Burned_km2 = 4.4442
P3_12_dNBR$Landscape = "Mixed"
P3_12_dNBR$ForestPer = 91.1
P3_12_dNBR$SinceLastBurn = 42.96395
P3_12_dNBR$SinceBurnCat = "30-50"
P3_12_dNBR$BurnedLast50 = 0.556906
P3_12_dNBR$BurnedLast50Cat = "<1"
P3_12_dNBR$MissingData = "0"
P3_12_dNBR$SeasonEnd = 2007.1
P3_12_dNBR$PreFireNBR = 511.71287
P3_12_dNBR$PreFireCat = ">500"
P3_12_dNBR$maxNBR = 613.7656
P3_12_dNBR$maxCat = ">600"

P4_0_dNBR = P4_0_tbl[,5, drop = FALSE]
P4_0_dNBR$ID = "P4_0"
median(P4_0_dNBR$dNBR)
P4_0_dNBR$Severity = "ML"
P4_0_dNBR$Extent = "XS"
P4_0_dNBR$Burned_km2 = 1.6083
P4_0_dNBR$Landscape = "Mixed"
P4_0_dNBR$ForestPer = 75.6
P4_0_dNBR$SinceLastBurn = 70.00000
P4_0_dNBR$SinceBurnCat = ">50"
P4_0_dNBR$BurnedLast50 = 0
P4_0_dNBR$BurnedLast50Cat = "0"
P4_0_dNBR$MissingData = "0"
P4_0_dNBR$SeasonEnd = 2008.2
P4_0_dNBR$PreFireNBR = 433.65767
P4_0_dNBR$PreFireCat = "350-500"
P4_0_dNBR$maxNBR = 571.3453
P4_0_dNBR$maxCat = "450-600"

P4_1_dNBR = P4_1_tbl[,5, drop = FALSE]
P4_1_dNBR$ID = "P4_1"
median(P4_1_dNBR$dNBR)
P4_1_dNBR$Severity = "MH"
P4_1_dNBR$Extent = "M"
P4_1_dNBR$Burned_km2 = 24.4746
P4_1_dNBR$Landscape = "Mixed"
P4_1_dNBR$ForestPer = 77.2
P4_1_dNBR$SinceLastBurn = 59.08274
P4_1_dNBR$SinceBurnCat = ">50"
P4_1_dNBR$BurnedLast50 = 0.098147
P4_1_dNBR$BurnedLast50Cat = "0"
P4_1_dNBR$MissingData = "0"
P4_1_dNBR$SeasonEnd = 2008.2
P4_1_dNBR$PreFireNBR = 454.18331
P4_1_dNBR$PreFireCat = "350-500"
P4_1_dNBR$maxNBR = 555.3918
P4_1_dNBR$maxCat = "450-600"

P4_2_dNBR = P4_2_tbl[,5, drop = FALSE]
P4_2_dNBR$ID = "P4_2"
median(P4_2_dNBR$dNBR)
P4_2_dNBR$Severity = "MH"
P4_2_dNBR$Extent = "S"
P4_2_dNBR$Burned_km2 = 8.4483
P4_2_dNBR$Landscape = "Forest"
P4_2_dNBR$ForestPer = 99
P4_2_dNBR$SinceLastBurn = 48.34409
P4_2_dNBR$SinceBurnCat = "30-50"
P4_2_dNBR$BurnedLast50 = 1.017791
P4_2_dNBR$BurnedLast50Cat = "<1"
P4_2_dNBR$MissingData = "0"
P4_2_dNBR$SeasonEnd = 2008.1
P4_2_dNBR$PreFireNBR = 506.06609
P4_2_dNBR$PreFireCat = ">500"
P4_2_dNBR$maxNBR = 625.5046
P4_2_dNBR$maxCat = ">600"

P4_3_dNBR = P4_3_tbl[,5, drop = FALSE]
P4_3_dNBR$ID = "P4_3"
median(P4_3_dNBR$dNBR)
P4_3_dNBR$Severity = "H"
P4_3_dNBR$Extent = "L"
P4_3_dNBR$Burned_km2 = 160.9758
P4_3_dNBR$Landscape = "Forest"
P4_3_dNBR$ForestPer = 98.4
P4_3_dNBR$SinceLastBurn = 18.71716
P4_3_dNBR$SinceBurnCat = "<30"
P4_3_dNBR$BurnedLast50 = 1.256354
P4_3_dNBR$BurnedLast50Cat = ">1"
P4_3_dNBR$MissingData = "0"
P4_3_dNBR$SeasonEnd = 2008.2
P4_3_dNBR$PreFireNBR = 418.43423
P4_3_dNBR$PreFireCat = "350-500"
P4_3_dNBR$maxNBR = 503.2789
P4_3_dNBR$maxCat = "450-600"

P4_4_dNBR = P4_4_tbl[,5, drop = FALSE]
P4_4_dNBR$ID = "P4_4"
median(P4_4_dNBR$dNBR)
P4_4_dNBR$Severity = "MH"
P4_4_dNBR$Extent = "M"
P4_4_dNBR$Burned_km2 = 18.9027
P4_4_dNBR$Landscape = "Non-Forest"
P4_4_dNBR$ForestPer = 28.4
P4_4_dNBR$SinceLastBurn = 48.82426
P4_4_dNBR$SinceBurnCat = "30-50"
P4_4_dNBR$BurnedLast50 = 0.299576
P4_4_dNBR$BurnedLast50Cat = "<1"
P4_4_dNBR$MissingData = "0"
P4_4_dNBR$SeasonEnd = 2008.2
P4_4_dNBR$PreFireNBR = 267.38887
P4_4_dNBR$PreFireCat = "<350"
P4_4_dNBR$maxNBR = 390.9523
P4_4_dNBR$maxCat = "<450"

P4_5_dNBR = P4_5_tbl[,5, drop = FALSE]
P4_5_dNBR$ID = "P4_5"
median(P4_5_dNBR$dNBR)
P4_5_dNBR$Severity = "H"
P4_5_dNBR$Extent = "L"
P4_5_dNBR$Burned_km2 = 599.8392
P4_5_dNBR$Landscape = "Forest"
P4_5_dNBR$ForestPer = 99.4
P4_5_dNBR$SinceLastBurn = 24.00714
P4_5_dNBR$SinceBurnCat = "<30"
P4_5_dNBR$BurnedLast50 = 2.625260
P4_5_dNBR$BurnedLast50Cat = ">1"
P4_5_dNBR$MissingData = "0"
P4_5_dNBR$SeasonEnd = 2008.2
P4_5_dNBR$PreFireNBR = 535.34295
P4_5_dNBR$PreFireCat = ">500"
P4_5_dNBR$maxNBR = 644.5226
P4_5_dNBR$maxCat = ">600"

P5_0_dNBR = P5_0_tbl[,5, drop = FALSE]
P5_0_dNBR$ID = "P5_0"
median(P5_0_dNBR$dNBR)
P5_0_dNBR$Severity = "L"
P5_0_dNBR$Extent = "XS"
P5_0_dNBR$Burned_km2 = 0.1629
P5_0_dNBR$Landscape = "Non-Forest"
P5_0_dNBR$ForestPer = 12.5
P5_0_dNBR$SinceLastBurn = 64.68508
P5_0_dNBR$SinceBurnCat = ">50"
P5_0_dNBR$BurnedLast50 = 0
P5_0_dNBR$BurnedLast50Cat = "0"
P5_0_dNBR$MissingData = "0"
P5_0_dNBR$SeasonEnd = 2011.1
P5_0_dNBR$PreFireNBR = 112.10290
P5_0_dNBR$PreFireCat = "<350"
P5_0_dNBR$maxNBR = 203.7541
P5_0_dNBR$maxCat = "<450"

P5_1_dNBR = P5_1_tbl[,5, drop = FALSE]
P5_1_dNBR$ID = "P5_1"
median(P5_1_dNBR$dNBR)
P5_1_dNBR$Severity = "L"
P5_1_dNBR$Extent = "XS"
P5_1_dNBR$Burned_km2 = 1.0557
P5_1_dNBR$Landscape = "Non-Forest"
P5_1_dNBR$ForestPer = 22.2
P5_1_dNBR$SinceLastBurn = 70.00000
P5_1_dNBR$SinceBurnCat = ">50"
P5_1_dNBR$BurnedLast50 = 0
P5_1_dNBR$BurnedLast50Cat = "0"
P5_1_dNBR$MissingData = "0"
P5_1_dNBR$SeasonEnd = 2018.2
P5_1_dNBR$PreFireNBR = 170.37683
P5_1_dNBR$PreFireCat = "<350"
P5_1_dNBR$maxNBR = 176.4446
P5_1_dNBR$maxCat = "<450"

P5_2_dNBR = P5_2_tbl[,5, drop = FALSE]
P5_2_dNBR$ID = "P5_2"
median(P5_2_dNBR$dNBR)
P5_2_dNBR$Severity = "MH"
P5_2_dNBR$Extent = "S"
P5_2_dNBR$Burned_km2 = 3.7215
P5_2_dNBR$Landscape = "Forest"
P5_2_dNBR$ForestPer = 100
P5_2_dNBR$SinceLastBurn = 15.00000
P5_2_dNBR$SinceBurnCat = "<30"
P5_2_dNBR$BurnedLast50 = 1.869649
P5_2_dNBR$BurnedLast50Cat = ">1"
P5_2_dNBR$MissingData = "1"
P5_2_dNBR$SeasonEnd = 2012.1
P5_2_dNBR$PreFireNBR = 410.99284
P5_2_dNBR$PreFireCat = "350-500"
P5_2_dNBR$maxNBR = 498.2982
P5_2_dNBR$maxCat = "450-600"

P5_3_dNBR = P5_3_tbl[,5, drop = FALSE]
P5_3_dNBR$ID = "P5_3"
median(P5_3_dNBR$dNBR)
P5_3_dNBR$Severity = "ML"
P5_3_dNBR$Extent = "S"
P5_3_dNBR$Burned_km2 = 1.9575
P5_3_dNBR$Landscape = "Mixed"
P5_3_dNBR$ForestPer = 84.7
P5_3_dNBR$SinceLastBurn = 66.10667
P5_3_dNBR$SinceBurnCat = ">50"
P5_3_dNBR$BurnedLast50 = 0
P5_3_dNBR$BurnedLast50Cat = "0"
P5_3_dNBR$MissingData = "0"
P5_3_dNBR$SeasonEnd = 2012.2
P5_3_dNBR$PreFireNBR = 431.42795
P5_3_dNBR$PreFireCat = "350-500"
P5_3_dNBR$maxNBR = 568.3110
P5_3_dNBR$maxCat = "450-600"

P5_4_dNBR = P5_4_tbl[,5, drop = FALSE]
P5_4_dNBR$ID = "P5_4"
median(P5_4_dNBR$dNBR)
P5_4_dNBR$Severity = "ML"
P5_4_dNBR$Extent = "XS"
P5_4_dNBR$Burned_km2 = 1.1034
P5_4_dNBR$Landscape = "Forest"
P5_4_dNBR$ForestPer = 98.3
P5_4_dNBR$SinceLastBurn = 38.41272
P5_4_dNBR$SinceBurnCat = "30-50"
P5_4_dNBR$BurnedLast50 = 0.981225
P5_4_dNBR$BurnedLast50Cat = "<1"
P5_4_dNBR$MissingData = "0"
P5_4_dNBR$SeasonEnd = 2016.2
P5_4_dNBR$PreFireNBR = 407.96241
P5_4_dNBR$PreFireCat = "350-500"
P5_4_dNBR$maxNBR = 469.7189
P5_4_dNBR$maxCat = "450-600"

P5_5_dNBR = P5_5_tbl[,5, drop = FALSE]
P5_5_dNBR$ID = "P5_5"
median(P5_5_dNBR$dNBR)
P5_5_dNBR$Severity = "MH"
P5_5_dNBR$Extent = "S"
P5_5_dNBR$Burned_km2 = 3.2085
P5_5_dNBR$Landscape = "Forest"
P5_5_dNBR$ForestPer = 94.5
P5_5_dNBR$SinceLastBurn = 45.05442
P5_5_dNBR$SinceBurnCat = "30-50"
P5_5_dNBR$BurnedLast50 = 1.019355
P5_5_dNBR$BurnedLast50Cat = "<1"
P5_5_dNBR$MissingData = "0"
P5_5_dNBR$SeasonEnd = 2014
P5_5_dNBR$PreFireNBR = 443.47863
P5_5_dNBR$PreFireCat = "350-500"
P5_5_dNBR$maxNBR = 485.1740
P5_5_dNBR$maxCat = "450-600"

P5_6_dNBR = P5_6_tbl[,5, drop = FALSE]
P5_6_dNBR$ID = "P5_6"
median(P5_6_dNBR$dNBR)
P5_6_dNBR$Severity = "MH"
P5_6_dNBR$Extent = "S"
P5_6_dNBR$Burned_km2 = 3.3534
P5_6_dNBR$Landscape = "Forest"
P5_6_dNBR$ForestPer = 100
P5_6_dNBR$SinceLastBurn = 7.00000
P5_6_dNBR$SinceBurnCat = "<30"
P5_6_dNBR$BurnedLast50 = 3.592593
P5_6_dNBR$BurnedLast50Cat = ">1"
P5_6_dNBR$MissingData = "0"
P5_6_dNBR$SeasonEnd = 2015
P5_6_dNBR$PreFireNBR = 533.27151
P5_6_dNBR$PreFireCat = ">500"
P5_6_dNBR$maxNBR = 567.2546
P5_6_dNBR$maxCat = "450-600"

P5_7_dNBR = P5_7_tbl[,5, drop = FALSE]
P5_7_dNBR$ID = "P5_7"
median(P5_7_dNBR$dNBR)
P5_7_dNBR$Severity = "L"
P5_7_dNBR$Extent = "XS"
P5_7_dNBR$Burned_km2 = 0.2547
P5_7_dNBR$Landscape = "Non-Forest"
P5_7_dNBR$ForestPer = 9.3
P5_7_dNBR$SinceLastBurn = 68.93993
P5_7_dNBR$SinceBurnCat = ">50"
P5_7_dNBR$BurnedLast50 = 0
P5_7_dNBR$BurnedLast50Cat = "0"
P5_7_dNBR$MissingData = "0"
P5_7_dNBR$SeasonEnd = 2018.1
P5_7_dNBR$PreFireNBR = 87.74885
P5_7_dNBR$PreFireCat = "<350"
P5_7_dNBR$maxNBR = 130.5080
P5_7_dNBR$maxCat = "<450"

P5_8_dNBR = P5_8_tbl[,5, drop = FALSE]
P5_8_dNBR$ID = "P5_8"
median(P5_8_dNBR$dNBR)
P5_8_dNBR$Severity = "H"
P5_8_dNBR$Extent = "XS"
P5_8_dNBR$Burned_km2 = 1.4589
P5_8_dNBR$Landscape = "Forest"
P5_8_dNBR$ForestPer = 92.1
P5_8_dNBR$SinceLastBurn = 40.02344
P5_8_dNBR$SinceBurnCat = "30-50"
P5_8_dNBR$BurnedLast50 = 0.772980
P5_8_dNBR$BurnedLast50Cat = "<1"
P5_8_dNBR$MissingData = "0"
P5_8_dNBR$SeasonEnd = 2016.1
P5_8_dNBR$PreFireNBR = 459.97129
P5_8_dNBR$PreFireCat = "350-500"
P5_8_dNBR$maxNBR = 557.2258
P5_8_dNBR$maxCat = "450-600"

P5_9_dNBR = P5_9_tbl[,5, drop = FALSE]
P5_9_dNBR$ID = "P5_9"
median(P5_9_dNBR$dNBR)
P5_9_dNBR$Severity = "MH"
P5_9_dNBR$Extent = "S"
P5_9_dNBR$Burned_km2 = 2.6136
P5_9_dNBR$Landscape = "Forest"
P5_9_dNBR$ForestPer = 100
P5_9_dNBR$SinceLastBurn = 8.95661
P5_9_dNBR$SinceBurnCat = "<30"
P5_9_dNBR$BurnedLast50 = 3.811639
P5_9_dNBR$BurnedLast50Cat = ">1"
P5_9_dNBR$MissingData = "0"
P5_9_dNBR$SeasonEnd = 2017
P5_9_dNBR$PreFireNBR = 608.59382
P5_9_dNBR$PreFireCat = ">500"
P5_9_dNBR$maxNBR = 648.7401
P5_9_dNBR$maxCat = ">600"

P5_10_dNBR = P5_10_tbl[,5, drop = FALSE]
P5_10_dNBR$ID = "P5_10"
median(P5_10_dNBR$dNBR)
P5_10_dNBR$Severity = "ML"
P5_10_dNBR$Extent = "S" 
P5_10_dNBR$Burned_km2 = 2.9619
P5_10_dNBR$Landscape = "Mixed"
P5_10_dNBR$ForestPer = 72.3
P5_10_dNBR$SinceLastBurn = 61.82376
P5_10_dNBR$SinceBurnCat = ">50"
P5_10_dNBR$BurnedLast50 = 0.325433
P5_10_dNBR$BurnedLast50Cat = "<1"
P5_10_dNBR$MissingData = "1"
P5_10_dNBR$SeasonEnd = 2011.2
P5_10_dNBR$PreFireNBR = 211.34447
P5_10_dNBR$PreFireCat = "<350"
P5_10_dNBR$maxNBR = 363.2369
P5_10_dNBR$maxCat = "<450"

P5_11_dNBR = P5_11_tbl[,5, drop = FALSE]
P5_11_dNBR$ID = "P5_11"
median(P5_11_dNBR$dNBR)
P5_11_dNBR$Severity = "ML"
P5_11_dNBR$Extent = "S"
P5_11_dNBR$Burned_km2 = 6.6915
P5_11_dNBR$Landscape = "Mixed"
P5_11_dNBR$ForestPer = 75.6
P5_11_dNBR$SinceLastBurn = 62.31352
P5_11_dNBR$SinceBurnCat = ">50"
P5_11_dNBR$BurnedLast50 = 0.077202
P5_11_dNBR$BurnedLast50Cat = "0"
P5_11_dNBR$MissingData = "0"
P5_11_dNBR$SeasonEnd = 2019
P5_11_dNBR$PreFireNBR = 495.71668
P5_11_dNBR$PreFireCat = "350-500"
P5_11_dNBR$maxNBR = 538.1287
P5_11_dNBR$maxCat = "450-600"

P5_12_dNBR = P5_12_tbl[,5, drop = FALSE]
P5_12_dNBR$ID = "P5_12"
median(P5_12_dNBR$dNBR)
P5_12_dNBR$Severity = "H"
P5_12_dNBR$Extent = "S"
P5_12_dNBR$Burned_km2 = 1.8297
P5_12_dNBR$Landscape = "Forest"
P5_12_dNBR$ForestPer = 97.7
P5_12_dNBR$SinceLastBurn = 9.00000
P5_12_dNBR$SinceBurnCat = "<30"
P5_12_dNBR$BurnedLast50 = 3.272012
P5_12_dNBR$BurnedLast50Cat = ">1"
P5_12_dNBR$MissingData = "0"
P5_12_dNBR$SeasonEnd = 2017
P5_12_dNBR$PreFireNBR = 608.06383
P5_12_dNBR$PreFireCat = ">500"
P5_12_dNBR$maxNBR = 618.3234
P5_12_dNBR$maxCat = ">600"

P6_0_dNBR = P6_0_tbl[,5, drop = FALSE]
P6_0_dNBR$ID = "P6_0"
median(P6_0_dNBR$dNBR)
P6_0_dNBR$Severity = "ML"
P6_0_dNBR$Extent = "S"
P6_0_dNBR$Burned_km2 = 1.8216
P6_0_dNBR$Landscape = "Non-Forest"
P6_0_dNBR$ForestPer = 26
P6_0_dNBR$SinceLastBurn = 70.00000
P6_0_dNBR$SinceBurnCat = ">50"
P6_0_dNBR$BurnedLast50 = 0
P6_0_dNBR$BurnedLast50Cat = "0"
P6_0_dNBR$MissingData = "0"
P6_0_dNBR$SeasonEnd = 2020.1
P6_0_dNBR$PreFireNBR = 244.23121
P6_0_dNBR$PreFireCat = "<350"
P6_0_dNBR$maxNBR = 289.2900
P6_0_dNBR$maxCat = "<450"

P6_1_dNBR = P6_1_tbl[,5, drop = FALSE]
P6_1_dNBR$ID = "P6_1"
median(P6_1_dNBR$dNBR)
P6_1_dNBR$Severity = "MH"
P6_1_dNBR$Extent = "L"
P6_1_dNBR$Burned_km2 = 118.8225
P6_1_dNBR$Landscape = "Mixed"
P6_1_dNBR$ForestPer = 70.6
P6_1_dNBR$SinceLastBurn = 54.07240
P6_1_dNBR$SinceBurnCat = ">50"
P6_1_dNBR$BurnedLast50 = 0.326317
P6_1_dNBR$BurnedLast50Cat = "<1"
P6_1_dNBR$MissingData = "0"
P6_1_dNBR$SeasonEnd = 2020.2
P6_1_dNBR$PreFireNBR = 475.14857
P6_1_dNBR$PreFireCat = "350-500"
P6_1_dNBR$maxNBR = 517.6539
P6_1_dNBR$maxCat = "450-600"

P6_2_dNBR = P6_2_tbl[,5, drop = FALSE]
P6_2_dNBR$ID = "P6_2"
median(P6_2_dNBR$dNBR)
P6_2_dNBR$Severity = "MH"
P6_2_dNBR$Extent = "XS"
P6_2_dNBR$Burned_km2 = 1.4859
P6_2_dNBR$Landscape = "Mixed"
P6_2_dNBR$ForestPer = 72.9
P6_2_dNBR$SinceLastBurn = 66.18837
P6_2_dNBR$SinceBurnCat = ">50"
P6_2_dNBR$BurnedLast50 = 0.009085
P6_2_dNBR$BurnedLast50Cat = "0"
P6_2_dNBR$MissingData = "0"
P6_2_dNBR$SeasonEnd = 2020.2
P6_2_dNBR$PreFireNBR = 425.21090
P6_2_dNBR$PreFireCat = "350-500"
P6_2_dNBR$maxNBR = 491.4804
P6_2_dNBR$maxCat = "450-600"

P6_3_dNBR = P6_3_tbl[,5, drop = FALSE]
P6_3_dNBR$ID = "P6_3"
median(P6_3_dNBR$dNBR)
P6_3_dNBR$Severity = "H"
P6_3_dNBR$Extent = "L"
P6_3_dNBR$Burned_km2 = 1285.178
P6_3_dNBR$Landscape = "Forest"
P6_3_dNBR$ForestPer = 92.9
P6_3_dNBR$SinceLastBurn = 34.95014
P6_3_dNBR$SinceBurnCat = "30-50"
P6_3_dNBR$BurnedLast50 = 1.311748
P6_3_dNBR$BurnedLast50Cat = ">1"
P6_3_dNBR$MissingData = "0"
P6_3_dNBR$SeasonEnd = 2020.3
P6_3_dNBR$PreFireNBR = 536.75790
P6_3_dNBR$PreFireCat = ">500"
P6_3_dNBR$maxNBR = 577.4840
P6_3_dNBR$maxCat = "450-600"
#####

AllFires_severity = rbind(P1_0_dNBR, P1_1_dNBR, P1_2_dNBR, P1_3_dNBR, P1_4_dNBR, P1_5_dNBR, P2_0_dNBR,
                          P2_1_dNBR, P2_2_dNBR, P2_3_dNBR, P2_4_dNBR, P3_0_dNBR, P3_1_dNBR, P3_2_dNBR,
                          P3_3_dNBR, P3_4_dNBR, P3_5_dNBR, P3_6_dNBR, P3_7_dNBR, P3_8_dNBR, P3_9_dNBR,
                          P3_10_dNBR, P3_11_dNBR, P3_12_dNBR, P4_0_dNBR, P4_1_dNBR, P4_2_dNBR, P4_3_dNBR,
                          P4_4_dNBR, P4_5_dNBR, P5_0_dNBR, P5_1_dNBR, P5_2_dNBR, P5_3_dNBR, P5_4_dNBR,
                          P5_5_dNBR, P5_6_dNBR, P5_7_dNBR, P5_8_dNBR, P5_9_dNBR, P5_10_dNBR, P5_11_dNBR,
                          P5_12_dNBR, P6_0_dNBR, P6_1_dNBR, P6_2_dNBR, P6_3_dNBR)
AllFires_severity$Severity = factor(AllFires_severity$Severity, levels = c("L", "ML", "MH", "H"))
AllFires_severity$Extent = factor(AllFires_severity$Extent, levels = c("XS", "S", "M", "L"))
AllFires_severity$Landscape = factor(AllFires_severity$Landscape, levels = c("Non-Forest", "Mixed", 
                                                                          "Forest"))
AllFires_severity$SinceBurnCat = factor(AllFires_severity$SinceBurnCat, 
                                        levels = c("<30", "30-50", ">50"))
AllFires_severity$BurnedLast50Cat = factor(AllFires_severity$BurnedLast50Cat, 
                                        levels = c("0", "<1", ">1"))
AllFires_severity$MissingData = factor(AllFires_severity$MissingData, levels = c("0", "1"))
AllFires_severity$PreFireCat = factor(AllFires_severity$PreFireCat, 
                                      levels = c("<350", "350-500", ">500"))
AllFires_severity$maxCat = factor(AllFires_severity$maxCat, 
                                      levels = c("<450", "450-600", ">600"))

AllFires_severity1 = read.csv('AllFires_severity1.csv') # Includes mean/median/severity level per fire
AllFires_severity1$Severity = factor(AllFires_severity1$Severity, levels = c("L", "ML", "MH", "H"))
AllFires_severity1$Extent = factor(AllFires_severity1$Extent, levels = c("XS", "S", "M", "L"))
AllFires_severity1$SinceBurnCat = factor(AllFires_severity1$SinceBurnCat, 
                                        levels = c("<30", "30-50", ">50"))
AllFires_severity1$BurnedLast50Cat = factor(AllFires_severity1$BurnedLast50Cat, 
                                         levels = c("0", "<1", ">1"))
AllFires_severity1$MissingData = factor(AllFires_severity1$MissingData, 
                                            levels = c("0", "1"))
AllFires_severity1$PreFireNBRCat = factor(AllFires_severity1$PreFireNBRCat, 
                                      levels = c("<350", "350-500", ">500"))
AllFires_severity1$maxCat = factor(AllFires_severity1$maxCat, 
                                  levels = c("<450", "450-600", ">600"))
  
# Recovery table
AllFires_recovery = as.data.frame(t(AllFires_tbl[,55:84]))
colnames(AllFires_recovery) = as.character(AllFires_tbl[,1])
AllFires_recovery$AvgAll = apply(AllFires_recovery[1:47], 1, mean, na.rm = TRUE)
AllFires_recovery$AvgAll_sd = apply(AllFires_recovery[1:47], 1, sd, na.rm = TRUE)

AllFires_recovery1 = read.csv("AllFires_recovery1.csv")
AllFires_recovery1$Severity = factor(AllFires_recovery1$Severity, levels = c("L", "ML", "MH", "H"))
AllFires_recovery1$Severity1 = factor(AllFires_recovery1$Severity1, levels = c("L", "ML", "MH", "H"))
AllFires_recovery1$Severity2 = factor(AllFires_recovery1$Severity2, levels = c("L", "ML", "MH", "H"))
AllFires_recovery1$Severity3 = factor(AllFires_recovery1$Severity3, levels = c("L", "M", "H"))
AllFires_recovery1$Severity4 = factor(AllFires_recovery1$Severity4, levels = c("L", "M", "H"))
AllFires_recovery1$Severity5 = factor(AllFires_recovery1$Severity5, levels = c("L", "M", "H"))
AllFires_recovery1$Extent = factor(AllFires_recovery1$Extent, levels = c("XS", "S", "M", "L"))
AllFires_recovery1$Landscape = factor(AllFires_recovery1$Landscape, levels = c("Non-Forest", "Mixed", 
                                                                             "Forest"))
AllFires_recovery1$SinceBurnCat = factor(AllFires_recovery1$SinceBurnCat, 
                                         levels = c("<30", "30-50", ">50"))
AllFires_recovery1$SinceBurnCat1 = factor(AllFires_recovery1$SinceBurnCat1, 
                                         levels = c("<30", "30-50", ">50"))
AllFires_recovery1$BurnedLast40 = factor(AllFires_recovery1$BurnedLast40, 
                                            levels = c("0", "<1", ">1"))
AllFires_recovery1$MissingData = factor(AllFires_recovery1$MissingData, 
                                        levels = c("0", "1"))
AllFires_recovery1$PostFireNBR = factor(AllFires_recovery1$PostFireNBR, 
                                          levels = c("<-200", "-200 - 0", ">0"))
AllFires_recovery1$BurnYear = factor(AllFires_recovery1$BurnYear,
                                     levels = c("1990s", "2000s", "2010s"))

  
### Default ###
# Extent
# Barplot - Actual Values
#####
extent_bar = ggplot(AllFires_extent, 
                    aes(x = reorder(EventID, Burned_km2), y = Burned_km2, fill = Extent)) +
  geom_bar(stat = "identity", color = "black", width = 0.75) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  ylab("Burned Area (km²)") + scale_y_continuous(expand = c(0, 1)) +
  coord_cartesian(ylim = c(0, 200)) +
  geom_text(x = 46, y = 191, label = "599.8", angle = 90, size = 4, fontface = "italic") +
  geom_text(x = 47, y = 191, label = "1285.2", angle = 90, size = 4, fontface = "italic") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.075, 0.85))
#####
pdf("Extent_AllFires.pdf", width = 11.599, height = 7) #16.57 # 10
extent_bar
dev.off()

# Barplot - ln Values
#####
extentln_bar = ggplot(AllFires_extent, 
                      aes(x = reorder(EventID, Burned_km2_log), y = Burned_km2_log, fill = Extent)) +
  geom_bar(stat = "identity", color = "black", width = 0.75) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  ylab("ln(Burned Area (km²))") +
  scale_y_continuous(expand = c(0, 0.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.075, 0.85))
#####
pdf("extentln_bar.pdf", width = 11.599, height = 7) #16.57 # 10
extentln_bar
dev.off()

# Severity
# Boxplot
#####
severity_box = ggplot(AllFires_severity, aes(x = reorder(ID, dNBR, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = Severity), color = "black") +
  scale_fill_manual(values = c("yellow2", "orange2", "orangered2", "darkred")) +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.075, 0.85))
#####
pdf("Severity_AllFires.pdf", width = 11.599, height = 7) #16.57 # 10
severity_box
dev.off()

# Recovery
# Timeseries plot
#####
recovery_ts = ggplot(AllFires_recovery, aes(x = 0:29)) + 
  geom_line(aes(y = P1_0, color = "gray80")) +
  geom_line(aes(y = P1_1, color = "gray80")) +
  geom_line(aes(y = P1_2, color = "gray80")) +
  geom_line(aes(y = P1_3, color = "gray80")) +
  geom_line(aes(y = P1_4, color = "gray80")) +
  geom_line(aes(y = P1_5, color = "gray80")) +
  geom_line(aes(y = P2_0, color = "gray80")) +
  geom_line(aes(y = P2_1, color = "gray80")) +
  geom_line(aes(y = P2_2, color = "gray80")) +
  geom_line(aes(y = P2_3, color = "gray80")) +
  geom_line(aes(y = P2_4, color = "gray80")) +
  geom_line(aes(y = P3_0, color = "gray80")) +
  geom_line(aes(y = P3_1, color = "gray80")) +
  geom_line(aes(y = P3_2, color = "gray80")) +
  geom_line(aes(y = P3_3, color = "gray80")) +
  geom_line(aes(y = P3_4, color = "gray80")) +
  geom_line(aes(y = P3_5, color = "gray80")) +
  geom_line(aes(y = P3_6, color = "gray80")) +
  geom_line(aes(y = P3_7, color = "gray80")) +
  geom_line(aes(y = P3_8, color = "gray80")) +
  geom_line(aes(y = P3_9, color = "gray80")) +
  geom_line(aes(y = P3_10, color = "gray80")) +
  geom_line(aes(y = P3_11, color = "gray80")) +
  geom_line(aes(y = P3_12, color = "gray80")) +
  geom_line(aes(y = P4_0, color = "gray80")) +
  geom_line(aes(y = P4_1, color = "gray80")) +
  geom_line(aes(y = P4_2, color = "gray80")) +
  geom_line(aes(y = P4_3, color = "gray80")) +
  geom_line(aes(y = P4_4, color = "gray80")) +
  geom_line(aes(y = P4_5, color = "gray80")) +
  geom_line(aes(y = P5_0, color = "gray80")) +
  geom_line(aes(y = P5_1, color = "gray80")) +
  geom_line(aes(y = P5_2, color = "gray80")) +
  geom_line(aes(y = P5_3, color = "gray80")) +
  geom_line(aes(y = P5_4, color = "gray80")) +
  geom_line(aes(y = P5_5, color = "gray80")) +
  geom_line(aes(y = P5_6, color = "gray80")) +
  geom_line(aes(y = P5_7, color = "gray80")) +
  geom_line(aes(y = P5_8, color = "gray80")) +
  geom_line(aes(y = P5_9, color = "gray80")) +
  geom_line(aes(y = P5_10, color = "gray80")) +
  geom_line(aes(y = P5_11, color = "gray80")) +
  geom_line(aes(y = P5_12, color = "gray80")) +
  geom_line(aes(y = P6_0, color = "gray80")) +
  geom_line(aes(y = P6_1, color = "gray80")) +
  geom_line(aes(y = P6_2, color = "gray80")) +
  geom_line(aes(y = P6_3, color = "gray80")) +
  geom_line(aes(y = AvgAll, color = "black"), size = 2.5) + 
  geom_errorbar(aes(ymin = AvgAll - AvgAll_sd, ymax = AvgAll + AvgAll_sd), 
                color = "black", size = 1.25, width = 0.35) +
  scale_color_identity(name = "Mean ± SD", 
                       breaks = c("black"),
                       labels = c("All Fires"),
                       guide = "legend") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.9, 0.1))  
#####
pdf("Recovery_AllFires.pdf", width = 11.599, height = 7) #16.57 # 10
recovery_ts
dev.off()

# Combine into one figure 
# default = ggarrange(recovery_ts, 
#                     ggarrange(extent_bar, severity_box, ncol = 2, labels = c("B", "C")),
#                     nrow = 2, labels = "A")
# default

### Vs. Extent ###
# Severity
# Scatterplot - Extent ln values
#####
# x <- AllFires_tbl$Burned_km2_log # Change for each figure
# y <- AllFires_tbl$dNBR # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

severity_extentln_scatter = ggplot(data = NULL, 
                                 aes(x = AllFires_extent$Burned_km2_log, 
                                     y = AllFires_severity1$dNBR_mean)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
               #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'Ln(Burned Area (km²))', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = -0.5, y = 850, label = expression(italic(R^2) * "= 0.39"^"****"), 
           size = 6) +
theme(axis.title.x = element_text(size = 18),
      axis.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 18),
      axis.text.y = element_text(size = 14),
      panel.background = element_blank(),
      panel.border = element_rect(color = 'black', fill = NA))

#####
pdf("Severity_Extentln_Scatterr39.pdf", width = 11.599, height = 7) #16.57 # 10
severity_extentln_scatter
dev.off()

# Boxplot
#####
# All Fire events
severity_extent_box = ggplot(AllFires_severity, 
                             aes(x = reorder(ID, Burned_km2, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = Extent), color = "black") +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) + 
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.075, 0.85))

# All pixels combined by extent
far_compare = list(c("XS","L"))
med1_compare = list(c("XS","M"))
med2_compare = list(c("S","L"))
clo_compare = list(c("XS","S"), c("S","M"), c("M","L"))

severity_extent_box_combineallpixels = ggplot(AllFires_severity, aes(x = Extent, y = dNBR)) +
  geom_violin(aes(fill = Extent), color = "black", show.legend = FALSE, trim = FALSE) +#_boxplot
  geom_boxplot(width = 0.20, color = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 1) +
  #stat_compare_means(method = "wilcox.test", #****
                     #comparisons = clo_compare, label = "p.signif", label.y = 1000) +
  #stat_compare_means(method = "wilcox.test", #****
                     #comparisons = med1_compare, label = "p.signif", label.y = 1080) +
  #stat_compare_means(method = "wilcox.test", #****
                     #comparisons = med2_compare, label = "p.signif", label.y = 1160) +
  #stat_compare_means(method = "wilcox.test", #****
                     #comparisons = far_compare, label = "p.signif", label.y = 1240) +
  stat_compare_means(method = "kruskal.test", 
                     label= "p.format", label.y = max(AllFires_severity$dNBR) - 50,
                     label.x.npc = "left", size = 5) + 
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

# Means combined by extent
severity_extent_box_combinebyfire = ggplot(AllFires_severity1, 
                                           aes(x = Extent, y = dNBR_mean)) + #dNBR_median
  geom_boxplot(aes(fill = Extent), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test",
                     comparisons = clo_compare, label = "p.signif", size = 5.5, 
                     label.y = max(AllFires_severity1$dNBR_mean) + 80) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = med1_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 160) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = med2_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 240) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 320) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 420, label.x = 1.35, size = 4.5) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
severity_extent_box
severity_extent_box_combineallpixels
severity_extent_box_combinebyfire

# Combine into one figure
pdf("Burnedkm2_dNBR_2box_allpixels.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(severity_extent_box, severity_extent_box_combineallpixels, #severity_extent_box_combinebyfire
          nrow = 1, align = "h", rel_widths = c(3.5, 1))  #labels = "auto"
dev.off()

# Recovery
# Scatterplot - PF1, PF5, PF10, PF15
#####
# x <- AllFires_tbl$Burned_km2_log # Change for each figure
# y <- AllFires_tbl$PF15 # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

PF1_extent_scatter = ggplot(data = NULL,
                              aes(x = AllFires_tbl$Burned_km2_log,
                                  y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(-0.8, 2.8)) + # ylim = c(0, 100)
  annotate(geom = "text", x = -0.55, y = 41, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = -0.5, y = 37, label = expression(italic(R^2) * "= 0"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_extent_scatter = ggplot(data = NULL,
                              aes(x = AllFires_tbl$Burned_km2_log,
                                  y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(-0.8, 2.8)) + # ylim = c(0, 100)
  annotate(geom = "text", x = -0.55, y = 84.5, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2.525, y = 30, label = expression(italic(R^2) * "= 0"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_extent_scatter = ggplot(data = NULL,
                               aes(x = AllFires_tbl$Burned_km2_log,
                                   y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(-0.8, 2.8)) + # ylim = c(0, 100)
  annotate(geom = "text", x = -0.5, y = 96.5, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = -0.375, y = 91.5, label = expression(italic(R^2) * "= 0.12"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_extent_scatter = ggplot(data = NULL,
                               aes(x = AllFires_tbl$Burned_km2_log,
                                   y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(-0.8, 2.8)) + # ylim = c(0, 100)
  annotate(geom = "text", x = -0.5, y = 95, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = -0.4, y = 93, label = expression(italic(R^2) * "= 0.22"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_extent_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_extent_scatter, PF5_extent_scatter, 
                   PF10_extent_scatter, PF15_extent_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("Ln(Burned Area (km²)", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

# Timeseries plot
#####
recovery_extent_ts_byFire = ggplot(data = AllFires_recovery1, 
                                    aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Extent), alpha = 0.333)  +
  stat_summary(aes(group = Extent, color = Extent), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette =  "Set1", direction = -1) +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.85))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                AllFires_recovery1$Extent == "XS")
Yr1_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
               AllFires_recovery1$Extent == "S")
Yr1_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Extent == "M")
Yr1_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Extent == "L")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Extent == "XS")
Yr2_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Extent == "S")
Yr2_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Extent == "M")
Yr2_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Extent == "L")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Extent == "XS")
Yr3_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Extent == "S")
Yr3_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Extent == "M")
Yr3_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Extent == "L")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Extent == "XS")
Yr4_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Extent == "S")
Yr4_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Extent == "M")
Yr4_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Extent == "L")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Extent == "XS")
Yr5_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Extent == "S")
Yr5_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Extent == "M")
Yr5_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Extent == "L")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Extent == "XS")
Yr6_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Extent == "S")
Yr6_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Extent == "M")
Yr6_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Extent == "L")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Extent == "XS")
Yr7_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Extent == "S")
Yr7_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Extent == "M")
Yr7_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Extent == "L")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Extent == "XS")
Yr8_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Extent == "S")
Yr8_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Extent == "M")
Yr8_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Extent == "L")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Extent == "XS")
Yr9_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Extent == "S")
Yr9_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Extent == "M")
Yr9_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Extent == "L")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_XS = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Extent == "XS")
Yr10_S = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                 AllFires_recovery1$Extent == "S")
Yr10_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                 AllFires_recovery1$Extent == "M")
Yr10_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                 AllFires_recovery1$Extent == "L")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 8))
colnames(sig_tbl) = c("Year", "XS.S", "S.M","M.L", "XS.M", "S.L", "XS.L", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_XS, Yr1_S) # ns 0.58
wilcox.test(Yr2_XS, Yr2_S) # ns 0.49
wilcox.test(Yr3_XS, Yr3_S) # ns 0.81
wilcox.test(Yr4_XS, Yr4_S) # ns 0.60
wilcox.test(Yr5_XS, Yr5_S) # ns 0.25
wilcox.test(Yr6_XS, Yr6_S) # ns 0.35
wilcox.test(Yr7_XS, Yr7_S) # ns 0.44
wilcox.test(Yr8_XS, Yr8_S) # ns 0.13
wilcox.test(Yr9_XS, Yr9_S) # ns 0.11
wilcox.test(Yr10_XS, Yr10_S) # ns 0.17
sig_tbl$`XS.S` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_S, Yr1_M) # ns 0.19
wilcox.test(Yr2_S, Yr2_M) # ns 0.44
wilcox.test(Yr3_S, Yr3_M) # ns 0.86
wilcox.test(Yr4_S, Yr4_M) # ns 0.86
wilcox.test(Yr5_S, Yr5_M) # ns 0.38
wilcox.test(Yr6_S, Yr6_M) # ns 0.49
wilcox.test(Yr7_S, Yr7_M) # ns 0.70
wilcox.test(Yr8_S, Yr8_M) # ns 0.42
wilcox.test(Yr9_S, Yr9_M) # ns 0.67
wilcox.test(Yr10_S, Yr10_M) # ns 0.96
sig_tbl$`S.M` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_M, Yr1_L) # ns 0.52
wilcox.test(Yr2_M, Yr2_L) # ns 0.22
wilcox.test(Yr3_M, Yr3_L) # ns 0.44
wilcox.test(Yr4_M, Yr4_L) # ns 0.62
wilcox.test(Yr5_M, Yr5_L) # ns 0.52
wilcox.test(Yr6_M, Yr6_L) # ns 0.44
wilcox.test(Yr7_M, Yr7_L) # ns 0.62
wilcox.test(Yr8_M, Yr8_L) # ns 0.52
wilcox.test(Yr9_M, Yr9_L) # ns 0.44
wilcox.test(Yr10_M, Yr10_L) # ns 0.43
sig_tbl$`M.L` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_XS, Yr1_M) # ns 0.86
wilcox.test(Yr2_XS, Yr2_M) # ns 0.80
wilcox.test(Yr3_XS, Yr3_M) # ns 0.43
wilcox.test(Yr4_XS, Yr4_M) # ns 0.76
wilcox.test(Yr5_XS, Yr5_M) # ns 0.90
wilcox.test(Yr6_XS, Yr6_M) # ns 0.76
wilcox.test(Yr7_XS, Yr7_M) # ns 0.63
wilcox.test(Yr8_XS, Yr8_M) # ns 0.41
wilcox.test(Yr9_XS, Yr9_M) # ns 0.32
wilcox.test(Yr10_XS, Yr10_M) # ns 0.09
sig_tbl$`XS.M` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_S, Yr1_L) # ns 0.89
wilcox.test(Yr2_S, Yr2_L) # ns 0.96
wilcox.test(Yr3_S, Yr3_L) # ns 0.92
wilcox.test(Yr4_S, Yr4_L) # ns 0.63
wilcox.test(Yr5_S, Yr5_L) # ns 0.33
wilcox.test(Yr6_S, Yr6_L) # ns 0.38
wilcox.test(Yr7_S, Yr7_L) # ns 0.37
wilcox.test(Yr8_S, Yr8_L) # ns 0.24
wilcox.test(Yr9_S, Yr9_L) # ns 0.30
wilcox.test(Yr10_S, Yr10_L) # ns 0.62
sig_tbl$`S.L` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_XS, Yr1_L) # ns 0.63
wilcox.test(Yr2_XS, Yr2_L) # ns 0.63
wilcox.test(Yr3_XS, Yr3_L) # ns 1
wilcox.test(Yr4_XS, Yr4_L) # ns 0.51
wilcox.test(Yr5_XS, Yr5_L) # ns 0.59
wilcox.test(Yr6_XS, Yr6_L) # ns 0.59
wilcox.test(Yr7_XS, Yr7_L) # ns 0.86
wilcox.test(Yr8_XS, Yr8_L) # ns 1
wilcox.test(Yr9_XS, Yr9_L) # ns 0.68
wilcox.test(Yr10_XS, Yr10_L) # ns 0.80
sig_tbl$`XS.L` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

# wilcox.test(c(Yr1_XS, Yr1_S), c(Yr1_M, Yr1_L)) # ns 0.59
# wilcox.test(c(Yr2_XS, Yr2_S), c(Yr2_M, Yr2_L)) # ns 0.75
# wilcox.test(c(Yr3_XS, Yr3_S), c(Yr3_M, Yr3_L)) # ns 0.72
# wilcox.test(c(Yr4_XS, Yr4_S), c(Yr4_M, Yr4_L)) # ns 0.75
# wilcox.test(c(Yr5_XS, Yr5_S), c(Yr5_M, Yr5_L)) # ns 0.41
# wilcox.test(c(Yr6_XS, Yr6_S), c(Yr6_M, Yr6_L)) # ns 0.53
# wilcox.test(c(Yr7_XS, Yr7_S), c(Yr7_M, Yr7_L)) # ns 0.76
# wilcox.test(c(Yr8_XS, Yr8_S), c(Yr8_M, Yr8_L)) # ns 0.77
# wilcox.test(c(Yr9_XS, Yr9_S), c(Yr9_M, Yr9_L)) # ns 0.94
# wilcox.test(c(Yr10_XS, Yr10_S), c(Yr10_M, Yr10_L)) #ns 0.56

kruskal.test(Recovery ~ Extent, data = Yr1) # ns 0.67
kruskal.test(Recovery ~ Extent, data = Yr2) # ns 0.71
kruskal.test(Recovery ~ Extent, data = Yr3) # ns 0.88
kruskal.test(Recovery ~ Extent, data = Yr4) # ns 0.87
kruskal.test(Recovery ~ Extent, data = Yr5) # ns 0.52
kruskal.test(Recovery ~ Extent, data = Yr6) # ns 0.60
kruskal.test(Recovery ~ Extent, data = Yr7) # ns 0.73
kruskal.test(Recovery ~ Extent, data = Yr8) # ns 0.35
kruskal.test(Recovery ~ Extent, data = Yr9) # ns 0.34
kruskal.test(Recovery ~ Extent, data = Yr10) # ns 0.33
sig_tbl$`K-W` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Burnedkm2_recovery_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_extent_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 15)
dev.off()

### Vs. Severity ###
# Recovery
# Scatterplot - PF1, PF5, PF10, PF15 vs. dNBR
#####
# x <- AllFires_tbl$dNBR # Change for each figure
# y <- AllFires_tbl$PF15 # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

PF1_severity_scatter = ggplot(data = NULL,
                              aes(x = AllFires_tbl$dNBR,
                                  y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
               #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 235, y = 41, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 270, y = 37, label = expression(italic(R^2) * "= 0.077"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_severity_scatter = ggplot(data = NULL,
                              aes(x = AllFires_tbl$dNBR,
                                  y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
               #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 235, y = 84.5, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 725, y = 30, label = expression(italic(R^2) * "= 0.18*"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_severity_scatter = ggplot(data = NULL,
                              aes(x = AllFires_tbl$dNBR,
                                  y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
               #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 240, y = 96.5, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 272, y = 91.5, label = expression(italic(R^2) * "= 0.44****"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_severity_scatter = ggplot(data = NULL,
                               aes(x = AllFires_tbl$dNBR,
                                   y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
               #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 240, y = 99, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 263, y = 96, label = expression(italic(R^2) * "= 0.50**"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_severity_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_severity_scatter, PF5_severity_scatter, 
                   PF10_severity_scatter, PF15_severity_scatter,
          ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("dNBR (x1000)", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

# Scatterplot - PF1, PF5, PF10, PF15 vs. Low Severity Burn
#####
# x <- AllFires_tbl$LowPer_Burn # Change for each figure
# y <- AllFires_tbl$PF15 # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

PF1_severity_scatter1 = ggplot(data = NULL,
                              aes(x = AllFires_tbl$LowPer_Burn,
                                  y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 83, y = 41, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 78, y = 37, label = expression(italic(R^2) * "= 0.079"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_severity_scatter1 = ggplot(data = NULL,
                              aes(x = AllFires_tbl$LowPer_Burn,
                                  y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 83, y = 84.5, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 80, y = 30, label = expression(italic(R^2) * "= 0.28**"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_severity_scatter1 = ggplot(data = NULL,
                               aes(x = AllFires_tbl$LowPer_Burn,
                                   y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 83, y = 91, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 78, y = 86, label = expression(italic(R^2) * "= 0.61****"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_severity_scatter1 = ggplot(data = NULL,
                               aes(x = AllFires_tbl$LowPer_Burn,
                                   y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 83, y = 94, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 80, y = 91, label = expression(italic(R^2) * "= 0.51**"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_severity_4xscatter_byfire1.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_severity_scatter1, PF5_severity_scatter1, 
                   PF10_severity_scatter1, PF15_severity_scatter1,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("Low Severity Burn (%)", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

# Timeseires plot
#####
recovery_severity_ts_byFire = ggplot(data = AllFires_recovery1, 
                                   aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Severity), alpha = 0.333)  +
  stat_summary(aes(group = Severity, color = Severity), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow2", "orange2", "orangered2", "darkred")) +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.87))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Severity == "L")
Yr1_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity == "ML")
Yr1_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity == "MH")
Yr1_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity == "H")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Severity == "L")
Yr2_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity == "ML")
Yr2_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity == "MH")
Yr2_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity == "H")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Severity == "L")
Yr3_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity == "ML")
Yr3_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity == "MH")
Yr3_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity == "H")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Severity == "L")
Yr4_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity == "ML")
Yr4_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity == "MH")
Yr4_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity == "H")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Severity == "L")
Yr5_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity == "ML")
Yr5_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity == "MH")
Yr5_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity == "H")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Severity == "L")
Yr6_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity == "ML")
Yr6_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity == "MH")
Yr6_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity == "H")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Severity == "L")
Yr7_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity == "ML")
Yr7_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity == "MH")
Yr7_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity == "H")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Severity == "L")
Yr8_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity == "ML")
Yr8_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity == "MH")
Yr8_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity == "H")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Severity == "L")
Yr9_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity == "ML")
Yr9_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity == "MH")
Yr9_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity == "H")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Severity == "L")
Yr10_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity == "ML")
Yr10_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity == "MH")
Yr10_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity == "H")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 8))
colnames(sig_tbl) = c("Year", "L.ML", "ML.MH","MH.H", "L.MH", "ML.H", "L.H", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_L, Yr1_ML) # ns 0.32
wilcox.test(Yr2_L, Yr2_ML) # ns 1
wilcox.test(Yr3_L, Yr3_ML) # ns 0.34
wilcox.test(Yr4_L, Yr4_ML) # ns 0.30
wilcox.test(Yr5_L, Yr5_ML) # ns 0.54
wilcox.test(Yr6_L, Yr6_ML) # ns 0.54
wilcox.test(Yr7_L, Yr7_ML) # ns 0.50
wilcox.test(Yr8_L, Yr8_ML) # ns 0.71
wilcox.test(Yr9_L, Yr9_ML) # ns 0.26
wilcox.test(Yr10_L, Yr10_ML) # ns 0.07
sig_tbl$`L.ML` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_ML, Yr1_MH) # ns 0.72
wilcox.test(Yr2_ML, Yr2_MH) # ns 0.65
wilcox.test(Yr3_ML, Yr3_MH) # ns 0.18
wilcox.test(Yr4_ML, Yr4_MH) # ns 0.37
wilcox.test(Yr5_ML, Yr5_MH) # ns 0.52
wilcox.test(Yr6_ML, Yr6_MH) # ns 0.58
wilcox.test(Yr7_ML, Yr7_MH) # ns 0.46
wilcox.test(Yr8_ML, Yr8_MH) # ns 0.23
wilcox.test(Yr9_ML, Yr9_MH) # ns 0.15
wilcox.test(Yr10_ML, Yr10_MH) # ns 0.28
sig_tbl$`ML.MH` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_MH, Yr1_H) # ns 0.26
wilcox.test(Yr2_MH, Yr2_H) # ns 0.95
wilcox.test(Yr3_MH, Yr3_H) # ns 0.98
wilcox.test(Yr4_MH, Yr4_H) # ns 0.87
wilcox.test(Yr5_MH, Yr5_H) # ns 1
wilcox.test(Yr6_MH, Yr6_H) # ns 0.85
wilcox.test(Yr7_MH, Yr7_H) # ns 0.73
wilcox.test(Yr8_MH, Yr8_H) # ns 0.60
wilcox.test(Yr9_MH, Yr9_H) # ns 0.44
wilcox.test(Yr10_MH, Yr10_H) # ns 0.27
sig_tbl$`MH.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_L, Yr1_MH) # ns 0.44
wilcox.test(Yr2_L, Yr2_MH) # ns 0.74
wilcox.test(Yr3_L, Yr3_MH) # ns 0.08
wilcox.test(Yr4_L, Yr4_MH) # ns 0.08
wilcox.test(Yr5_L, Yr5_MH) # ns 0.25
wilcox.test(Yr6_L, Yr6_MH) # ns 0.38
wilcox.test(Yr7_L, Yr7_MH) # ns 0.32
wilcox.test(Yr8_L, Yr8_MH) # ns 0.23
wilcox.test(Yr9_L, Yr9_MH) # ns 0.10
wilcox.test(Yr10_L, Yr10_MH) # * 0.04
sig_tbl$`L.MH` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*")

wilcox.test(Yr1_ML, Yr1_H) # ns 0.46
wilcox.test(Yr2_ML, Yr2_H) # ns 0.50
wilcox.test(Yr3_ML, Yr3_H) # ns 0.41
wilcox.test(Yr4_ML, Yr4_H) # ns 0.36
wilcox.test(Yr5_ML, Yr5_H) # ns 0.27
wilcox.test(Yr6_ML, Yr6_H) # ns 0.20
wilcox.test(Yr7_ML, Yr7_H) # * 0.05
wilcox.test(Yr8_ML, Yr8_H) # * 0.04
wilcox.test(Yr9_ML, Yr9_H) # * 0.03
wilcox.test(Yr10_ML, Yr10_H) # ** 0.006
sig_tbl$`ML.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "*", "*", "*", "**")

wilcox.test(Yr1_L, Yr1_H) # ns 0.30
wilcox.test(Yr2_L, Yr2_H) # ns 0.61
wilcox.test(Yr3_L, Yr3_H) # ns 0.15
wilcox.test(Yr4_L, Yr4_H) # ns 0.15
wilcox.test(Yr5_L, Yr5_H) # ns 0.15
wilcox.test(Yr6_L, Yr6_H) # ns 0.21
wilcox.test(Yr7_L, Yr7_H) # ns 0.21
wilcox.test(Yr8_L, Yr8_H) # ns 0.11
wilcox.test(Yr9_L, Yr9_H) # * 0.05
wilcox.test(Yr10_L, Yr10_H) # * 0.01
sig_tbl$`L.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*")

wilcox.test(c(Yr1_L, Yr1_ML), c(Yr1_MH, Yr1_H)) # ns 0.33
wilcox.test(c(Yr2_L, Yr2_ML), c(Yr2_MH, Yr2_H)) # ns 0.45
wilcox.test(c(Yr3_L, Yr3_ML), c(Yr3_MH, Yr3_H)) # ns 0.06
wilcox.test(c(Yr4_L, Yr4_ML), c(Yr4_MH, Yr4_H)) # ns 0.09
wilcox.test(c(Yr5_L, Yr5_ML), c(Yr5_MH, Yr5_H)) # ns 0.15
wilcox.test(c(Yr6_L, Yr6_ML), c(Yr6_MH, Yr6_H)) # ns 0.17
wilcox.test(c(Yr7_L, Yr7_ML), c(Yr7_MH, Yr7_H)) # ns 0.08
wilcox.test(c(Yr8_L, Yr8_ML), c(Yr8_MH, Yr8_H)) # * 0.02
wilcox.test(c(Yr9_L, Yr9_ML), c(Yr9_MH, Yr9_H)) # ** 0.009
wilcox.test(c(Yr10_L, Yr10_ML), c(Yr10_MH, Yr10_H)) # ** 0.004

kruskal.test(Recovery ~ Severity, data = Yr1) # ns 0.50
kruskal.test(Recovery ~ Severity, data = Yr2) # ns 0.89
kruskal.test(Recovery ~ Severity, data = Yr3) # ns 0.21
kruskal.test(Recovery ~ Severity, data = Yr4) # ns 0.25
kruskal.test(Recovery ~ Severity, data = Yr5) # ns 0.43
kruskal.test(Recovery ~ Severity, data = Yr6) # ns 0.48
kruskal.test(Recovery ~ Severity, data = Yr7) # ns 0.27
kruskal.test(Recovery ~ Severity, data = Yr8) # ns 0.13
kruskal.test(Recovery ~ Severity, data = Yr9) # * 0.05
kruskal.test(Recovery ~ Severity, data = Yr10) # * 0.01
sig_tbl$`K-W` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_severity_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_severity_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 15)
dev.off()

### vs, Landscape ###
# Extent
# Bar and box plot - Actual values
#####
extent_landscape_bar = ggplot(AllFires_extent, 
                    aes(x = reorder(EventID, ForestPer), y = Burned_km2, fill = Landscape)) +
  geom_bar(stat = "identity", color = "black", width = 0.75) +
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  ylab("Burned Area (km²)") + scale_y_continuous(expand = c(0, 1)) +
  coord_cartesian(ylim = c(0, 200)) +
  geom_text(x = 40, y = 191, label = "599.8", angle = 90, size = 3, fontface = "italic") +
  geom_text(x = 26, y = 191, label = "1285.2", angle = 90, size = 3, fontface = "italic") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.125, 0.85))

far_compare = list(c("Non-Forest","Forest"))
clo_compare = list(c("Non-Forest","Mixed"), c("Mixed","Forest"))

extent_landscape_box = ggplot(AllFires_extent, 
                                           aes(x = Landscape, y = Burned_km2)) + #dNBR_median
  geom_boxplot(aes(fill = Landscape), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = clo_compare, label = "p.signif",
                     label.y = 150, tip.length = 0.003) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = 175, tip.length = 0.003) +
  stat_compare_means(method = "kruskal.test", size = 4,
                     label.y = 190, label.x = 1.15) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  coord_cartesian(ylim = c(0, 200)) +
  scale_y_continuous(expand = c(0, 1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 60, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))

#####
pdf("Extent_Landscape_barbox_byFire.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(extent_landscape_bar, extent_landscape_box, 
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Bar and box plot - Ln values
#####
extentln_landscape_bar = ggplot(AllFires_extent, 
                              aes(x = reorder(EventID, ForestPer), y = Burned_km2_log, 
                                  fill = Landscape)) +
  geom_bar(stat = "identity", color = "black", width = 0.75) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  ylab("Ln(Burned Area (km²))") + scale_y_continuous(expand = c(0, 0.05)) +
  #coord_cartesian(ylim = c(0, 200)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.125, 0.85))

far_compare = list(c("Non-Forest","Forest"))
clo_compare = list(c("Non-Forest","Mixed"), c("Mixed","Forest"))

extentln_landscape_box = ggplot(AllFires_extent, 
                             aes(x = Landscape, y = Burned_km2_log)) + 
  geom_boxplot(aes(fill = Landscape), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = clo_compare, label = "p.signif",
                     label.y = 2, tip.length = 0.008) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = 2.25, tip.length = 0.008) +
  stat_compare_means(method = "kruskal.test", size = 3.5,
                     label.y = 2.6, label.x = 1.03) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  #coord_cartesian(ylim = c(0, 200)) +
  scale_y_continuous(expand = c(0, 0.05)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 60, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))

#####
pdf("Extentln_Landscape_barbox_byFire.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(extentln_landscape_bar, extentln_landscape_box, 
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Scatterplot - ForestPer
#####
# x <- AllFires_tbl$ForPer # Change for each figure
# y <- AllFires_tbl$Burned_km2_log # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

extentln_landscape_scatter = ggplot(data = NULL, 
                                   aes(x = AllFires_extent$ForestPer, 
                                       y = AllFires_extent$Burned_km2_log)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'Forest Coverage (%)', expand = c(0.01,0)) +
  scale_y_continuous(name = 'Ln(Burned Area (km²))', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 15, y = 2.9, label = expression(italic(R^2) * "= 0.17"^"**"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))

#####
pdf("Extentln_Landscape_Scatterr17.pdf", width = 11.599, height = 7) #16.57 # 10
extentln_landscape_scatter
dev.off()

# Severity
# Scatterplot - dNBR vs. ForestPer
#####
# x <- AllFires_tbl$ForPer # Change for each figure
# y <- AllFires_tbl$dNBR # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

severity_landscape_scatter = ggplot(data = NULL, 
                                    aes(x = AllFires_extent$ForestPer, 
                                        y = AllFires_severity1$dNBR_mean)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'Forest Coverage (%)', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 17, y = 810, label = expression(italic(R^2) * "= 0.27"^"***"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severity_Landscape_Scatterr27.pdf", width = 11.599, height = 7) #16.57 # 10
severity_landscape_scatter
dev.off()

# Scatterplot - LowPerBurn vs. ForestPer
#####
# x <- AllFires_tbl$ForPer # Change for each figure
# y <- AllFires_tbl$LowPer_Burn # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

severity_landscape_scatter1 = ggplot(data = NULL, 
                                    aes(x = AllFires_tbl$ForPer, 
                                        y = AllFires_tbl$LowPer_Burn)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'Forest Coverage (%)', expand = c(0.01,0)) +
  scale_y_continuous(name = 'Low Severity Burn (%)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 17, y = 85, label = expression(italic(R^2) * "= 0.30"^"****"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severity_Landscape_Scatterr30.pdf", width = 11.599, height = 7) #16.57 # 10
severity_landscape_scatter1
dev.off()

# Bar and Boxplot
#####
# All Fire events
severity_landscape_box = ggplot(AllFires_severity, 
                             aes(x = reorder(ID, ForestPer, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = Landscape), color = "black") +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) + 
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))

# All pixels
far_compare = list(c("Non-Forest","Forest"))
clo_compare = list(c("Non-Forest","Mixed"), c("Mixed","Forest"))

severity_landscape_box_allpixels = ggplot(AllFires_severity, aes(x = Landscape, y = dNBR)) +
  geom_violin(aes(fill = Landscape), color = "black", show.legend = FALSE, trim = FALSE) +#_boxplot
  geom_boxplot(width = 0.20, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 1) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = clo_compare, label = "p.signif", label.y = 1000) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med1_compare, label = "p.signif", label.y = 1080) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med2_compare, label = "p.signif", label.y = 1160) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = far_compare, label = "p.signif", label.y = 1240) +
  stat_compare_means(method = "kruskal.test", 
                     label= "p.format", label.y = max(AllFires_severity$dNBR) - 50,
                     label.x.npc = "left", size = 5) + 
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 60, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

# Means
severity_landscape_box_byfire = ggplot(AllFires_severity1, 
                                           aes(x = Landscape, y = dNBR_mean)) + #dNBR_median
  geom_boxplot(aes(fill = Landscape), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test",
                     comparisons = clo_compare, label = "p.signif", size = 5.5, 
                     label.y = max(AllFires_severity1$dNBR_mean) + 80) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 160) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 260, label.x = 1.2, size = 4) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 60, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
severity_landscape_box
severity_landscape_box_allpixels
severity_landscape_box_byfire

# Combine into one figure
pdf("Severity_Landscape_box_byfire.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(severity_landscape_box, severity_landscape_box_byfire, #severity_extent_box_allpixels
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Recovery
# Timeseries plot
#####
recovery_landscape_ts_byFire = ggplot(data = AllFires_recovery1, 
                                     aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Landscape), alpha = 0.333)  +
  stat_summary(aes(group = Landscape, color = Landscape), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow2", "yellowgreen", "green4")) +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.25, 0.15))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr1_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr1_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr2_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr2_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr3_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr3_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr4_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr4_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr5_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr5_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr6_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr6_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr7_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr7_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr8_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr8_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Landscape == "Non-Forest")
Yr9_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Landscape == "Mixed")
Yr9_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Landscape == "Forest")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_NF = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Landscape == "Non-Forest")
Yr10_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Landscape == "Mixed")
Yr10_F = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Landscape == "Forest")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "NF.M", "M.F","NF.F", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_NF, Yr1_M) # ns 0.90
wilcox.test(Yr2_NF, Yr2_M) # ns 0.46
wilcox.test(Yr3_NF, Yr3_M) # * 0.02
wilcox.test(Yr4_NF, Yr4_M) # ** 0.01
wilcox.test(Yr5_NF, Yr5_M) # * 0.02
wilcox.test(Yr6_NF, Yr6_M) # * 0.05
wilcox.test(Yr7_NF, Yr7_M) # ns 0.09
wilcox.test(Yr8_NF, Yr8_M) # ns 0.17
wilcox.test(Yr9_NF, Yr9_M) # ns 0.09
wilcox.test(Yr10_NF, Yr10_M) # ns 0.23
sig_tbl$`NF.M` = c("ns", "ns", "ns", "*", "**", "*", "*", "ns", "ns", "ns")

wilcox.test(Yr1_M, Yr1_F) # * 0.05
wilcox.test(Yr2_M, Yr2_F) # ** 0.003
wilcox.test(Yr3_M, Yr3_F) # *** 0.0005
wilcox.test(Yr4_M, Yr4_F) # ** 0.003
wilcox.test(Yr5_M, Yr5_F) # ** 0.002
wilcox.test(Yr6_M, Yr6_F) # ** 0.005
wilcox.test(Yr7_M, Yr7_F) # * 0.04
wilcox.test(Yr8_M, Yr8_F) # ns 0.14
wilcox.test(Yr9_M, Yr9_F) # ns 0.23
wilcox.test(Yr10_M, Yr10_F) # ns 0.64
sig_tbl$`M.F` = c("*", "**", "***", "**", "**", "**", "*", "ns", "ns", "ns")

wilcox.test(Yr1_NF, Yr1_F) # ns 0.11
wilcox.test(Yr2_NF, Yr2_F) # ns 0.10
wilcox.test(Yr3_NF, Yr3_F) # ns 0.76
wilcox.test(Yr4_NF, Yr4_F) # ns 0.87
wilcox.test(Yr5_NF, Yr5_F) # ns 0.79
wilcox.test(Yr6_NF, Yr6_F) # ns 0.80
wilcox.test(Yr7_NF, Yr7_F) # ns 0.90
wilcox.test(Yr8_NF, Yr8_F) # ns 0.87
wilcox.test(Yr9_NF, Yr9_F) # ns 0.67
wilcox.test(Yr10_NF, Yr10_F) # ns 0.42
sig_tbl$`NF.F` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(c(Yr1_NF, Yr1_M), Yr1_F) # * 0.03
wilcox.test(c(Yr2_NF, Yr2_M), Yr2_F) # ** 0.003
wilcox.test(c(Yr3_NF, Yr3_M), Yr3_F) # * 0.01
wilcox.test(c(Yr4_NF, Yr4_M), Yr4_F) # * 0.04
wilcox.test(c(Yr5_NF, Yr5_M), Yr5_F) # * 0.03
wilcox.test(c(Yr6_NF, Yr6_M), Yr6_F) # * 0.04
wilcox.test(c(Yr7_NF, Yr7_M), Yr7_F) # ns 0.15
wilcox.test(c(Yr8_NF, Yr8_M), Yr8_F) # ns 0.38
wilcox.test(c(Yr9_NF, Yr9_M), Yr9_F) # ns 0.59
wilcox.test(c(Yr10_NF, Yr10_M), Yr10_F) # ns 0.91

kruskal.test(Recovery ~ Landscape, data = Yr1) # ns 0.08
kruskal.test(Recovery ~ Landscape, data = Yr2) # ** 0.009
kruskal.test(Recovery ~ Landscape, data = Yr3) # ** 0.003
kruskal.test(Recovery ~ Landscape, data = Yr4) # ** 0.008
kruskal.test(Recovery ~ Landscape, data = Yr5) # ** 0.007
kruskal.test(Recovery ~ Landscape, data = Yr6) # * 0.02
kruskal.test(Recovery ~ Landscape, data = Yr7) # ns 0.08
kruskal.test(Recovery ~ Landscape, data = Yr8) # ns 0.23
kruskal.test(Recovery ~ Landscape, data = Yr9) # ns 0.23
kruskal.test(Recovery ~ Landscape, data = Yr10) # ns 0.45
sig_tbl$`K-W` = c("ns", "**", "**", "**", "**", "*", "ns", "ns", "ns", "ns")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_Landscape_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_landscape_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 60, xmin = 20)
dev.off()

# Scatterplot - PF1, PF5, PF10, PF15
#####
# x <- AllFires_tbl$ForPer # Change for each figure
# y <- AllFires_tbl$PF15 # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

PF1_landscape_scatter = ggplot(data = NULL,
                               aes(x = AllFires_tbl$ForPer,
                                   y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 18, y = 40.5, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 22, y = 36, label = expression(italic(R^2) * "= 0.07"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_landscape_scatter = ggplot(data = NULL,
                               aes(x = AllFires_tbl$ForPer,
                                   y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 18, y = 37.5, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 19, y = 31, label = expression(italic(R^2) * "= 0"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_landscape_scatter = ggplot(data = NULL,
                                aes(x = AllFires_tbl$ForPer,
                                    y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 19, y = 58, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 18, y = 52, label = expression(italic(R^2) * "= 0"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_landscape_scatter = ggplot(data = NULL,
                                aes(x = AllFires_tbl$ForPer,
                                    y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 18.5, y = 94, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 18, y = 91, label = expression(italic(R^2) * "= 0"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_Landscape_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_landscape_scatter, PF5_landscape_scatter, 
                   PF10_landscape_scatter, PF15_landscape_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("Forest Coverage (%)", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

### Vs. Short Term Fire History ###
# Severity
# Scatterplot - dNBR
#####
# x <- Forest_sincelastburn # Change for each figure
# y <- Forest_dNBR # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

Forest_sincelastburn = subset(AllFires_tbl$SinceLastBurn_NA70, AllFires_tbl$ForFire1 == "Forest")
Forest_dNBR = subset(AllFires_tbl$dNBR, AllFires_tbl$ForFire1 == "Forest")

severity_sincelastburn_scatter = ggplot(data = NULL, 
                                    aes(x = Forest_sincelastburn, # AllFires_tbl$SinceLastBurn_NA70
                                        y = Forest_dNBR)) + # AllFires_tbl$dNBR
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 12, y = 810, label = expression(italic(R^2) * "= 0"^"ns"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severityforest_sincelastburn_Scatterr0.pdf", width = 11.599, height = 7) #16.57 # 10
severity_sincelastburn_scatter
dev.off()

# Bar and Boxplot
#####
# All Fire events
severity_sincelastburn_box = ggplot(AllFires_severity, 
                                aes(x = reorder(ID, SinceLastBurn, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = SinceBurnCat), color = "black") +
  scale_fill_manual(values = c("red3", "orange3", "yellow3")) +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) + 
  scale_y_continuous(expand = c(0, 20)) +
  guides(fill=guide_legend(title="Years since Last Burn")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.16, 0.875))

# All pixels
far_compare = list(c("<30",">50"))
clo_compare = list(c("<30","30-50"), c("30-50",">50"))

severity_sincelastburn_box_allpixels = ggplot(AllFires_severity, aes(x = SinceBurnCat, y = dNBR)) +
  geom_violin(aes(fill = SinceBurnCat), color = "black", show.legend = FALSE, trim = FALSE) +#_boxplot
  geom_boxplot(width = 0.20, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("red3", "orange3", "yellow3")) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 1) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = clo_compare, label = "p.signif", label.y = 1000) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med1_compare, label = "p.signif", label.y = 1080) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med2_compare, label = "p.signif", label.y = 1160) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = far_compare, label = "p.signif", label.y = 1240) +
  stat_compare_means(method = "kruskal.test", 
                     label= "p.format", label.y = max(AllFires_severity$dNBR) - 50,
                     label.x.npc = "left", size = 3.5) + 
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

# Means
severity_sincelastburn_box_byfire = ggplot(AllFires_severity1, 
                                       aes(x = SinceBurnCat, y = dNBR_mean)) + #dNBR_median
  geom_boxplot(aes(fill = SinceBurnCat), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test",
                     comparisons = clo_compare, label = "p.signif", size = 5.5, 
                     label.y = max(AllFires_severity1$dNBR_mean) + 80) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 160) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 260, label.x = 1.2, size = 4) +
  scale_fill_manual(values = c("red3", "orange3", "yellow3")) +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
severity_sincelastburn_box
severity_sincelastburn_box_allpixels
severity_sincelastburn_box_byfire

# Combine into one figure
pdf("Severity_Landscape_box_allpixels.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(severity_sincelastburn_box, severity_sincelastburn_box_allpixels, #severity_extent_box_byfire
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Recovery
# Scatterplot - PF1, PF5, PF10, PF15 vs. NA70
#####
# x <- AllFires_tbl$SinceLastBurn_NArm # Change for each figure
# y <- AllFires_tbl$PF15 # Change for each figure
# 
# formula1 = y ~ x
# 
# lm1 <- lm(y ~ x)
# lm1_summary <- summary(lm1)
# 
# r2 <- lm1_summary$r.squared
# r2
# pval <- lm1_summary$coefficients[2,4]
# pval

PF1_sincelastburn_scatter = ggplot(data = NULL,
                               aes(x = AllFires_tbl$SinceLastBurn_NA70,
                                   y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 12.5, y = 40.5, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 15.5, y = 36, label = expression(italic(R^2) * "= 0.06"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_sincelastburn_scatter = ggplot(data = NULL,
                               aes(x = AllFires_tbl$SinceLastBurn_NA70,
                                   y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 12.5, y = 84, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 61, y = 31, label = expression(italic(R^2) * "= 0.20**"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_sincelastburn_scatter = ggplot(data = NULL,
                                aes(x = AllFires_tbl$SinceLastBurn_NA70,
                                    y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 13, y = 94, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 61, y = 52, label = expression(italic(R^2) * "= 0.12"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_sincelastburn_scatter = ggplot(data = NULL,
                                aes(x = AllFires_tbl$SinceLastBurn_NA70,
                                    y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 13, y = 95, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 15, y = 92.5, label = expression(italic(R^2) * "= 0.01"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_sincelastburn_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_sincelastburn_scatter, PF5_sincelastburn_scatter, 
                   PF10_sincelastburn_scatter, PF15_sincelastburn_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("Years since Last Burn", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

# Scatterplot - PF1, PF5, PF10, PF15 vs. NArm
#####
PF1_sincelastburn_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$SinceLastBurn_NArm,
                                       y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 12.5, y = 40.5, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 15, y = 36, label = expression(italic(R^2) * "= 0.16*"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_sincelastburn_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$SinceLastBurn_NArm,
                                       y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 12.5, y = 87, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 55, y = 32, label = expression(italic(R^2) * "= 0.40***"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_sincelastburn_scatter = ggplot(data = NULL,
                                    aes(x = AllFires_tbl$SinceLastBurn_NArm,
                                        y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 13, y = 97, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 55.5, y = 53, label = expression(italic(R^2) * "= 0.27**"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_sincelastburn_scatter = ggplot(data = NULL,
                                    aes(x = AllFires_tbl$SinceLastBurn_NArm,
                                        y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 13, y = 95, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 15, y = 92.5, label = expression(italic(R^2) * "= 0.02"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_sincelastburn_4xscatter_byfire1.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_sincelastburn_scatter, PF5_sincelastburn_scatter, 
                   PF10_sincelastburn_scatter, PF15_sincelastburn_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("Years since Last Burn", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

# Timeseries plot - NA70
#####
recovery_sincelastburn_ts_byFire = ggplot(data = AllFires_recovery1[!is.na(AllFires_recovery1$SinceBurnCat),], 
                                      aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = SinceBurnCat), alpha = 0.333)  +
  stat_summary(aes(group = SinceBurnCat, color = SinceBurnCat), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("red3", "orange3", "yellow3"),name = "Years since Last Burn") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.35, 0.15))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr1_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr1_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr2_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr2_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr3_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr3_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr4_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr4_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr5_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr5_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr6_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr6_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr7_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr7_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr8_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr8_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$SinceBurnCat == "<30")
Yr9_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$SinceBurnCat == "30-50")
Yr9_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$SinceBurnCat == ">50")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_30 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$SinceBurnCat == "<30")
Yr10_3050 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$SinceBurnCat == "30-50")
Yr10_50 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$SinceBurnCat == ">50")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "<30.30-50", "30-50.>50","<30.>50", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_30, Yr1_3050) # ns 0.40
wilcox.test(Yr2_30, Yr2_3050) # ns 0.13
wilcox.test(Yr3_30, Yr3_3050) # ns 0.17
wilcox.test(Yr4_30, Yr4_3050) # * 0.03
wilcox.test(Yr5_30, Yr5_3050) # ns 0.08
wilcox.test(Yr6_30, Yr6_3050) # ns 0.11
wilcox.test(Yr7_30, Yr7_3050) # ns 0.12
wilcox.test(Yr8_30, Yr8_3050) # ns 0.23
wilcox.test(Yr9_30, Yr9_3050) # ns 0.32
wilcox.test(Yr10_30, Yr10_3050) # ns 0.46
sig_tbl$`<30.30-50` = c("ns", "ns", "ns", "*", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_3050, Yr1_50) # ns 0.80
wilcox.test(Yr2_3050, Yr2_50) # ns 0.71
wilcox.test(Yr3_3050, Yr3_50) # ns 1
wilcox.test(Yr4_3050, Yr4_50) # ns 0.56
wilcox.test(Yr5_3050, Yr5_50) # ns 0.70
wilcox.test(Yr6_3050, Yr6_50) # ns 0.66
wilcox.test(Yr7_3050, Yr7_50) # ns 0.55
wilcox.test(Yr8_3050, Yr8_50) # ns 0.60
wilcox.test(Yr9_3050, Yr9_50) # ns 0.78
wilcox.test(Yr10_3050, Yr10_50) # ns 0.67
sig_tbl$`30-50.>50` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_30, Yr1_50) # ns 0.40
wilcox.test(Yr2_30, Yr2_50) # ns 0.09
wilcox.test(Yr3_30, Yr3_50) # ns 0.16
wilcox.test(Yr4_30, Yr4_50) # * 0.05
wilcox.test(Yr5_30, Yr5_50) # * 0.03
wilcox.test(Yr6_30, Yr6_50) # * 0.02
wilcox.test(Yr7_30, Yr7_50) # * 0.05
wilcox.test(Yr8_30, Yr8_50) # ns 0.08
wilcox.test(Yr9_30, Yr9_50) # ns 0.12
wilcox.test(Yr10_30, Yr10_50) # ns 0.43
sig_tbl$`<30.>50` = c("ns", "ns", "ns", "*", "*", "*", "*", "ns", "ns", "ns")

kruskal.test(Recovery ~ SinceBurnCat, data = Yr1) # ns 0.57
kruskal.test(Recovery ~ SinceBurnCat, data = Yr2) # ns 0.15
kruskal.test(Recovery ~ SinceBurnCat, data = Yr3) # ns 0.24
kruskal.test(Recovery ~ SinceBurnCat, data = Yr4) # * 0.04
kruskal.test(Recovery ~ SinceBurnCat, data = Yr5) # * 0.05
kruskal.test(Recovery ~ SinceBurnCat, data = Yr6) # * 0.05
kruskal.test(Recovery ~ SinceBurnCat, data = Yr7) # ns 0.09
kruskal.test(Recovery ~ SinceBurnCat, data = Yr8) # ns 0.16
kruskal.test(Recovery ~ SinceBurnCat, data = Yr9) # ns 0.26
kruskal.test(Recovery ~ SinceBurnCat, data = Yr10) # ns 0.59
sig_tbl$`K-W` = c("ns", "ns", "ns", "*", "*", "*", "ns", "ns", "ns", "ns")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_SinceLastBurn_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_sincelastburn_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 60, xmin = 16)
dev.off()

### Vs. Long Term Fire History ###
# Severity
# Scatterplot
#####
x <- AllFires_tbl$Burned_Last50 # Change for each figure
y <- AllFires_tbl$dNBR # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

severity_burnedlast50_scatter = ggplot(data = NULL, 
                                     aes(x = AllFires_tbl$Burned_Last50, 
                                         y = AllFires_tbl$dNBR)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'Times Burned in last 50 Years', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 0.35, y = 850, label = expression(italic(R^2) * "= 0.28"^"***"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severity_burnedlast50_Scatterr28.pdf", width = 11.599, height = 7) #16.57 # 10
severity_burnedlast50_scatter
dev.off()

# Bar and Boxplot
##### 
# All Fire events
severity_burnedlast50_box = ggplot(AllFires_severity[!is.na(AllFires_severity$BurnedLast50),], 
                                aes(x = reorder(ID, BurnedLast50, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = BurnedLast50Cat), color = "black") +
  scale_fill_manual(values = c("yellow3", "orange3", "red3"), name = "Times burned in last 50 Years") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) + 
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.225, 0.875))

# All pixels
far_compare = list(c("0",">1"))
clo_compare = list(c("0","<1"), c("<1",">1"))

severity_burnedlast50_box_allpixels = ggplot(AllFires_severity[!is.na(AllFires_severity$BurnedLast50),], 
                                             aes(x = BurnedLast50Cat, y = dNBR)) +
  geom_violin(aes(fill = BurnedLast50Cat), color = "black", show.legend = FALSE, trim = FALSE) +#_boxplot
  geom_boxplot(width = 0.20, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("yellow3", "orange3", "red3")) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 1) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = clo_compare, label = "p.signif", label.y = 1000) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med1_compare, label = "p.signif", label.y = 1080) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med2_compare, label = "p.signif", label.y = 1160) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = far_compare, label = "p.signif", label.y = 1240) +
  stat_compare_means(method = "kruskal.test", 
                     label= "p.format", label.y = max(AllFires_severity$dNBR) - 50,
                     label.x.npc = "left", size = 5) + 
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

# Means
severity_burnedlast50_box_byfire = ggplot(AllFires_severity1[!is.na(AllFires_severity1$BurnedLast50Cat),], 
                                       aes(x = BurnedLast50Cat, y = dNBR_mean)) + #dNBR_median
  geom_boxplot(aes(fill = BurnedLast50Cat), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test",
                     comparisons = clo_compare, label = "p.signif", size = 5.5, 
                     label.y = max(AllFires_severity1$dNBR_mean) + 80) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 160) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 260, label.x = 1.2, size = 4) +
  scale_fill_manual(values = c("yellow3", "orange3", "red3")) +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
severity_burnedlast50_box
severity_burnedlast50_box_allpixels
severity_burnedlast50_box_byfire

# Combine into one figure
pdf("Severity_burnedlast50_box_allpixels.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(severity_burnedlast50_box, severity_burnedlast50_box_allpixels, #byfire
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Recovery
# Scatterplot - PF1, PF5, PF10, PF15
#####
x <- AllFires_tbl$Burned_Last40 # Change for each figure
y <- AllFires_tbl$PF15 # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

PF1_burnedlast40_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$Burned_Last40,
                                       y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 3, y = 40.5, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2.9, y = 36, label = expression(italic(R^2) * "= 0.10*"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_burnedlast40_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$Burned_Last40,
                                       y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 3, y = 84, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2.9, y = 76.5, label = expression(italic(R^2) * "= 0.27**"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_burnedlast40_scatter = ggplot(data = NULL,
                                    aes(x = AllFires_tbl$Burned_Last40,
                                        y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 2.95, y = 91, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2.85, y = 85, label = expression(italic(R^2) * "= 0.25**"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_burnedlast40_scatter = ggplot(data = NULL,
                                    aes(x = AllFires_tbl$Burned_Last40,
                                        y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 2.95, y = 94, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 3, y = 91, label = expression(italic(R^2) * "= 0"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_burnedlast40_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_burnedlast40_scatter, PF5_burnedlast40_scatter, 
                   PF10_burnedlast40_scatter, PF15_burnedlast40_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("Times Burned in last 40 Years", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

# Timeseries plot
#####
recovery_burnedlast40_ts_byFire = ggplot(data = AllFires_recovery1[!is.na(AllFires_recovery1$BurnedLast40),], 
                                          aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = BurnedLast40), alpha = 0.333)  +
  stat_summary(aes(group = BurnedLast40, color = BurnedLast40), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow3", "orange3", "red3"),name = "Times Burned in last 40 Years") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.38, 0.15))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr1_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr1_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr2_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr2_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr3_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr3_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr4_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr4_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr5_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr5_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr6_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr6_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr7_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr7_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr8_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr8_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$BurnedLast40 == "0")
Yr9_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                    AllFires_recovery1$BurnedLast40 == "<1")
Yr9_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$BurnedLast40 == ">1")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$BurnedLast40 == "0")
Yr10_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                     AllFires_recovery1$BurnedLast40 == "<1")
Yr10_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$BurnedLast40 == ">1")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "0.<1", "<1.>1","0.>1", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_0, Yr1_1) # ns 0.41
wilcox.test(Yr2_0, Yr2_1) # ns 0.32
wilcox.test(Yr3_0, Yr3_1) # ns 0.49
wilcox.test(Yr4_0, Yr4_1) # ns 0.86
wilcox.test(Yr5_0, Yr5_1) # ns 0.39
wilcox.test(Yr6_0, Yr6_1) # ns 0.29
wilcox.test(Yr7_0, Yr7_1) # ns 0.21
wilcox.test(Yr8_0, Yr8_1) # ns 0.16
wilcox.test(Yr9_0, Yr9_1) # ns 0.16
wilcox.test(Yr10_0, Yr10_1) # ns 0.17
sig_tbl$`0.<1` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_1, Yr1_2) # ns 0.68
wilcox.test(Yr2_1, Yr2_2) # ns 0.37
wilcox.test(Yr3_1, Yr3_2) # ns 0.56
wilcox.test(Yr4_1, Yr4_2) # ns 0.21
wilcox.test(Yr5_1, Yr5_2) # ns 0.34
wilcox.test(Yr6_1, Yr6_2) # ns 0.39
wilcox.test(Yr7_1, Yr7_2) # ns 0.43
wilcox.test(Yr8_1, Yr8_2) # ns 0.55
wilcox.test(Yr9_1, Yr9_2) # ns 0.70
wilcox.test(Yr10_1, Yr10_2) # ns 1
sig_tbl$`<1.>1` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_0, Yr1_2) # ns 0.30
wilcox.test(Yr2_0, Yr2_2) # * 0.05
wilcox.test(Yr3_0, Yr3_2) # ns 0.11
wilcox.test(Yr4_0, Yr4_2) # * 0.03
wilcox.test(Yr5_0, Yr5_2) # * 0.03
wilcox.test(Yr6_0, Yr6_2) # * 0.02
wilcox.test(Yr7_0, Yr7_2) # * 0.04
wilcox.test(Yr8_0, Yr8_2) # ns 0.07
wilcox.test(Yr9_0, Yr9_2) # ns 0.09
wilcox.test(Yr10_0, Yr10_2) # ns 0.31
sig_tbl$`0.>1` = c("ns", "*", "ns", "*", "*", "*", "*", "ns", "ns", "ns")

kruskal.test(Recovery ~ BurnedLast40, data = Yr1) # ns 0.50
kruskal.test(Recovery ~ BurnedLast40, data = Yr2) # ns 0.15
kruskal.test(Recovery ~ BurnedLast40, data = Yr3) # ns 0.29
kruskal.test(Recovery ~ BurnedLast40, data = Yr4) # ns 0.10
kruskal.test(Recovery ~ BurnedLast40, data = Yr5) # ns 0.09
kruskal.test(Recovery ~ BurnedLast40, data = Yr6) # ns 0.08
kruskal.test(Recovery ~ BurnedLast40, data = Yr7) # ns 0.10
kruskal.test(Recovery ~ BurnedLast40, data = Yr8) # ns 0.13
kruskal.test(Recovery ~ BurnedLast40, data = Yr9) # ns 0.17
kruskal.test(Recovery ~ BurnedLast40, data = Yr10) # ns 0.36
sig_tbl$`K-W` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_Burnedlast40_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_burnedlast40_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 60, xmin = 16)
dev.off()

### Vs. Timing ###
# Severity
# Scatterplot
#####
x <- AllFires_tbl$SeasonEnd # Change for each figure
y <- AllFires_tbl$dNBR # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

severity_year_scatter = ggplot(data = NULL, 
                                       aes(x = AllFires_tbl$SeasonEnd, 
                                           y = AllFires_tbl$dNBR, color = AllFires_tbl$MissingData)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  scale_color_manual(values = c("black", "red2")) +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'Burn Year', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 1991.5, y = 825, label = expression(italic(R^2) * "= 0.04"^"ns"), 
           size = 6) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severity_year_Scatterr04.pdf", width = 11.599, height = 7) #16.57 # 10
severity_year_scatter
dev.off()

# Recovery
#####
x <- AllFires_tbl$SeasonEnd # Change for each figure
y <- AllFires_tbl$PF15 # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

PF1_year_scatter = ggplot(data = NULL,
                                  aes(x = AllFires_tbl$SeasonEnd,
                                      y = AllFires_tbl$PF1, color = AllFires_tbl$MissingData)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  scale_color_manual(values = c("black", "red2")) +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 2017, y = 40.5, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2017, y = 36, label = expression(italic(R^2) * "= 0"^"ns"), size = 6) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_year_scatter = ggplot(data = NULL,
                                  aes(x = AllFires_tbl$SeasonEnd,
                                      y = AllFires_tbl$PF5, color = AllFires_tbl$MissingData)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  scale_color_manual(values = c("black", "red2")) +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 2017, y = 84, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2015.5, y = 31, label = expression(italic(R^2) * "= 0.26**"), size = 6) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_year_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$SeasonEnd,
                                       y = AllFires_tbl$PF10, color = AllFires_tbl$MissingData)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  scale_color_manual(values = c("black", "red2")) +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 2016, y = 91, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2014, y = 85, label = expression(italic(R^2) * "= 0.59****"), size = 6) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_year_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$SeasonEnd,
                                       y = AllFires_tbl$PF15, color = AllFires_tbl$MissingData)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  scale_color_manual(values = c("black", "red2")) +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 2016, y = 95.5, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 2015.4, y = 92.5, label = expression(italic(R^2) * "= 0.37*"), size = 6) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_year_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_year_scatter, PF5_year_scatter, 
                   PF10_year_scatter, PF15_year_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("Burn Year", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

### Vs. Topography ###
# Severity
# Scatterplot
#####
x <- AllFires_tbl$TWI # Change for each figure
y <- AllFires_tbl$dNBR # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

severity_topo_scatter = ggplot(data = NULL, 
                                       aes(x = AllFires_tbl$TWI, 
                                           y = AllFires_tbl$dNBR)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'TWI', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 7, y = 825, label = expression(italic(R^2) * "= 0.02"^"ns"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severity_twi_Scatterr04.pdf", width = 11.599, height = 7) #16.57 # 10
severity_topo_scatter
dev.off()

# Recovery
x <- AllFires_tbl$TWI # Change for each figure
y <- AllFires_tbl$PF15 # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

PF1_topo_scatter = ggplot(data = NULL,
                                  aes(x = AllFires_tbl$TWI,
                                      y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 7.5, y = 40.5, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 7.5, y = 36, label = expression(italic(R^2) * "= 0.05"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_topo_scatter = ggplot(data = NULL,
                                  aes(x = AllFires_tbl$TWI,
                                      y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 8, y = 39, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 8, y = 32, label = expression(italic(R^2) * "= 0.01"^"ns"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_topo_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$TWI,
                                       y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 8, y = 58, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 8, y = 52, label = expression(italic(R^2) * "= 0.01"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_topo_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$TWI,
                                       y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 7.25, y = 94, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 7.25, y = 91, label = expression(italic(R^2) * "= 0.05"^"ns"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_twi_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_topo_scatter, PF5_topo_scatter, 
                   PF10_topo_scatter, PF15_topo_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("TWI", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

### Vs. Missing Data ###
# Severity
# Bar and Boxplot
##### 
# All Fire events
severity_missingdata_box = ggplot(AllFires_severity, 
                                   aes(x = reorder(ID, SeasonEnd), y = dNBR)) +
  geom_boxplot(aes(fill = MissingData), color = "black") +
  scale_fill_manual(values = c("white", "red3"), name = "Fires impacted by missing data") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) + 
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.225, 0.875))

# All pixels
far_compare = list(c("0","1"))

severity_missingdata_box_allpixels = ggplot(AllFires_severity, 
                                             aes(x = MissingData, y = dNBR)) +
  geom_violin(aes(fill = MissingData), color = "black", show.legend = FALSE, trim = FALSE) +#_boxplot
  geom_boxplot(width = 0.20, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("white", "red3")) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 1) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = clo_compare, label = "p.signif", label.y = 1000) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med1_compare, label = "p.signif", label.y = 1080) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med2_compare, label = "p.signif", label.y = 1160) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = far_compare, label = "p.signif", label.y = 1240) +
  stat_compare_means(method = "kruskal.test", 
                     label= "p.format", label.y = max(AllFires_severity$dNBR) - 50,
                     label.x.npc = "center", size = 5) + 
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

# Means
severity_missingdata_box_byfire = ggplot(AllFires_severity1, 
                                          aes(x = MissingData, y = dNBR_mean)) + #dNBR_median
  geom_boxplot(aes(fill = MissingData), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 80) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 200, label.x = 1, size = 4) +
  scale_fill_manual(values = c("white", "red3")) +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
severity_missingdata_box
severity_missingdata_box_allpixels
severity_missingdata_box_byfire

# Combine into one figure
pdf("Severity_missingdata_box_allpixels.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(severity_missingdata_box, severity_missingdata_box_allpixels, #byfire
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Recovery
#####
recovery_missingdata_ts_byFire = ggplot(data = AllFires_recovery1, 
                                         aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = MissingData), alpha = 0.333)  +
  stat_summary(aes(group = MissingData, color = MissingData), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("black", "red3"), name = "Fires impacted by missing data") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.38, 0.15))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$MissingData == "0")
Yr1_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$MissingData == "1")


Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$MissingData == "0")
Yr2_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$MissingData == "1")


Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$MissingData == "0")
Yr3_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$MissingData == "1")


Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$MissingData == "0")
Yr4_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$MissingData == "1")


Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$MissingData == "0")
Yr5_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$MissingData == "1")


Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$MissingData == "0")
Yr6_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$MissingData == "1")


Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$MissingData == "0")
Yr7_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$MissingData == "1")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$MissingData == "0")
Yr8_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$MissingData == "1")


Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$MissingData == "0")
Yr9_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$MissingData == "1")


Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$MissingData == "0")
Yr10_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$MissingData == "1")


sig_tbl = data.frame(matrix(nrow = 10, ncol = 2))
colnames(sig_tbl) = c("Year", "0.1")
sig_tbl$Year = 1:10

wilcox.test(Yr1_0, Yr1_1) # ns 0.74
wilcox.test(Yr2_0, Yr2_1) # ns 0.42
wilcox.test(Yr3_0, Yr3_1) # ns 0.20
wilcox.test(Yr4_0, Yr4_1) # * 0.05
wilcox.test(Yr5_0, Yr5_1) # * 0.02
wilcox.test(Yr6_0, Yr6_1) # * 0.02
wilcox.test(Yr7_0, Yr7_1) # * 0.01
wilcox.test(Yr8_0, Yr8_1) # ** 0.007
wilcox.test(Yr9_0, Yr9_1) # ** 0.003
wilcox.test(Yr10_0, Yr10_1) # *** 0.0003
sig_tbl$`0.1` = c("ns", "ns", "ns", "*", "*", "*", "*", "**", "**", "***")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_missingdata_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_missingdata_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 60, xmin = 16)
dev.off()

### Vs. Pre/PostFire NBR ###
# Severity vs. PreFireNBR
# Scatterplot
#####
x <- AllFires_tbl$PreFireNBR # Change for each figure
y <- AllFires_tbl$dNBR # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

severity_prefireNBR_scatter = ggplot(data = NULL, 
                                       aes(x = AllFires_tbl$PreFireNBR, 
                                           y = AllFires_tbl$dNBR)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'PreFireNBR (x1000)', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 135, y = 825, label = expression(italic(R^2) * "= 0.35****"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severity_prefireNBR_Scatterr35.pdf", width = 11.599, height = 7) #16.57 # 10
severity_prefireNBR_scatter
dev.off()

# Bar and Boxplot
##### 
# All Fire events
severity_prefirenbr_box = ggplot(AllFires_severity, 
                                   aes(x = reorder(ID, PreFireNBR, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = PreFireCat), color = "black") +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4"), name = "PreFireNBR (x1000)") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) + 
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.15, 0.875))

# All pixels
far_compare = list(c("<350",">500"))
clo_compare = list(c("<350","350-500"), c("350-500",">500"))

severity_prefirenbr_box_allpixels = ggplot(AllFires_severity, 
                                             aes(x = PreFireCat, y = dNBR)) +
  geom_violin(aes(fill = PreFireCat), color = "black", show.legend = FALSE, trim = FALSE) +#_boxplot
  geom_boxplot(width = 0.20, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 1) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = clo_compare, label = "p.signif", label.y = 1000) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med1_compare, label = "p.signif", label.y = 1080) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med2_compare, label = "p.signif", label.y = 1160) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = far_compare, label = "p.signif", label.y = 1240) +
  stat_compare_means(method = "kruskal.test", 
                     label= "p.format", label.y = max(AllFires_severity$dNBR) - 50,
                     label.x.npc = "left", size = 5) + 
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

# Means
severity_prefirenbr_box_byfire = ggplot(AllFires_severity1, 
                                          aes(x = PreFireNBRCat, y = dNBR_mean)) + #dNBR_median
  geom_boxplot(aes(fill = PreFireNBRCat), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test",
                     comparisons = clo_compare, label = "p.signif", size = 5.5, 
                     label.y = max(AllFires_severity1$dNBR_mean) + 80) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 160) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 260, label.x = 1.2, size = 4) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
severity_prefirenbr_box
severity_prefirenbr_box_allpixels
severity_prefirenbr_box_byfire

# Combine into one figure
pdf("Severity_prefireNBR_box_allpixels.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(severity_prefirenbr_box, severity_prefirenbr_box_allpixels, #byfire
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Recovery
# Scatterplot - PF1, PF5, PF10, PF15
#####
x <- AllFires_tbl$PostFireNBR # Change for each figure
y <- AllFires_tbl$PF15 # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

PF1_postfirenbr_scatter = ggplot(data = NULL,
                                  aes(x = AllFires_tbl$PostFireNBR,
                                      y = AllFires_tbl$PF1)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.9) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 150, y = 40.5, label = "Year 1", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 110, y = 36, label = expression(italic(R^2) * "= 0.42****"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0,0.125,0), "cm"))

PF5_postfirenbr_scatter = ggplot(data = NULL,
                                  aes(x = AllFires_tbl$PostFireNBR,
                                      y = AllFires_tbl$PF5)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 150, y = 84, label = "Year 5", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 135, y = 76.5, label = expression(italic(R^2) * "= 0.23**"), size = 6) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.25,0.3,0.125,0), "cm"))

PF10_postfirenbr_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$PostFireNBR,
                                       y = AllFires_tbl$PF10)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 145, y = 93, label = "Year 10", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 125, y = 87, label = expression(italic(R^2) * "= 0.34***"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0,0,0), "cm"))

PF15_postfirenbr_scatter = ggplot(data = NULL,
                                   aes(x = AllFires_tbl$PostFireNBR,
                                       y = AllFires_tbl$PF15)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = x ~ y, parse = TRUE, size = 5, label.y = 0.1) +
  scale_x_continuous(name = NULL, expand = c(0.02,0)) +
  scale_y_continuous(name = NULL, expand = c(0.02,0)) +
  #coord_cartesian(xlim = c(190, 800)) + # ylim = c(0, 100)
  annotate(geom = "text", x = 145, y = 94, label = "Year 15", size = 6, fontface = "bold") +
  annotate(geom = "text", x = 135, y = 91, label = expression(italic(R^2) * "= 0.33*"), size = 6) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), # element_blank()
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.125,0.3,0,0), "cm"))

#####
pdf("Recovery_postfireNBR_4xscatter_byfire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
figure = ggarrange(PF1_postfirenbr_scatter, PF5_postfirenbr_scatter, 
                   PF10_postfirenbr_scatter, PF15_postfirenbr_scatter,
                   ncol = 2, nrow = 2, align = "v")
annotate_figure(p = figure, bottom = text_grob("PostFireNBR (x1000)", size = 14), 
                left = text_grob("NBR Recovery to max vegetation (%)", size = 14, rot = 90))
dev.off()

# Timeseries plot
#####
recovery_postfireNBR_ts_byFire = ggplot(data = AllFires_recovery1, 
                                         aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = PostFireNBR), alpha = 0.333)  +
  stat_summary(aes(group = PostFireNBR, color = PostFireNBR), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("red3", "orange3", "yellow3"),name = "PostFireNBR (x1000)") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.38, 0.15))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr1_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr1_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr2_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr2_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr3_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr3_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr4_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr4_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr5_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr5_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr6_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr6_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr7_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr7_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr8_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr8_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$PostFireNBR == "<-200")
Yr9_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr9_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$PostFireNBR == ">0")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_0 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$PostFireNBR == "<-200")
Yr10_1 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$PostFireNBR == "-200 - 0")
Yr10_2 = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$PostFireNBR == ">0")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "<-200.-200-0", "-200-0.>0","<-200.>0", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_0, Yr1_1) # ns 0.11
wilcox.test(Yr2_0, Yr2_1) # ns 0.64
wilcox.test(Yr3_0, Yr3_1) # ns 0.72
wilcox.test(Yr4_0, Yr4_1) # ns 0.31
wilcox.test(Yr5_0, Yr5_1) # ns 0.37
wilcox.test(Yr6_0, Yr6_1) # ns 0.47
wilcox.test(Yr7_0, Yr7_1) # ns 0.34
wilcox.test(Yr8_0, Yr8_1) # ns 0.48
wilcox.test(Yr9_0, Yr9_1) # ns 0.41
wilcox.test(Yr10_0, Yr10_1) # ns 0.22
sig_tbl$`<-200.-200-0` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_1, Yr1_2) # ** 0.002
wilcox.test(Yr2_1, Yr2_2) # ** 0.006
wilcox.test(Yr3_1, Yr3_2) # * 0.04
wilcox.test(Yr4_1, Yr4_2) # ns 0.29
wilcox.test(Yr5_1, Yr5_2) # ns 0.29
wilcox.test(Yr6_1, Yr6_2) # ns 0.24
wilcox.test(Yr7_1, Yr7_2) # ns 0.12
wilcox.test(Yr8_1, Yr8_2) # ns 0.08
wilcox.test(Yr9_1, Yr9_2) # ns 0.07
wilcox.test(Yr10_1, Yr10_2) # ns 0.13
sig_tbl$`-200-0.>0` = c("**", "**", "*", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_0, Yr1_2) # *** 0.0001
wilcox.test(Yr2_0, Yr2_2) # ** 0.001
wilcox.test(Yr3_0, Yr3_2) # * 0.01
wilcox.test(Yr4_0, Yr4_2) # * 0.04
wilcox.test(Yr5_0, Yr5_2) # ns 0.06
wilcox.test(Yr6_0, Yr6_2) # ns 0.06
wilcox.test(Yr7_0, Yr7_2) # * 0.03
wilcox.test(Yr8_0, Yr8_2) # * 0.03
wilcox.test(Yr9_0, Yr9_2) # * 0.04
wilcox.test(Yr10_0, Yr10_2) # ns 0.07
sig_tbl$`<-200.>0` = c("***", "**", "*", "*", "ns", "ns", "*", "*", "*", "ns")

# <0 vs. > 0
wilcox.test(c(Yr1_0, Yr1_1), Yr1_2) # *** 0.0001
wilcox.test(c(Yr2_0, Yr2_1), Yr2_2) # *** 0.0007
wilcox.test(c(Yr3_0, Yr3_1), Yr3_2) # * 0.01
wilcox.test(c(Yr4_0, Yr4_1), Yr4_2) # ns 0.09
wilcox.test(c(Yr5_0, Yr5_1), Yr5_2) # ns 0.1
wilcox.test(c(Yr6_0, Yr6_1), Yr6_2) # ns 0.09
wilcox.test(c(Yr7_0, Yr7_1), Yr7_2) # * 0.04
wilcox.test(c(Yr8_0, Yr8_1), Yr8_2) # * 0.03
wilcox.test(c(Yr9_0, Yr9_1), Yr9_2) # * 0.03
wilcox.test(c(Yr10_0, Yr10_1), Yr10_2) # * 0.05

kruskal.test(Recovery ~ PostFireNBR, data = Yr1) # *** 0.0004
kruskal.test(Recovery ~ PostFireNBR, data = Yr2) # ** 0.004
kruskal.test(Recovery ~ PostFireNBR, data = Yr3) # * 0.04
kruskal.test(Recovery ~ PostFireNBR, data = Yr4) # ns 0.13
kruskal.test(Recovery ~ PostFireNBR, data = Yr5) # ns 0.16
kruskal.test(Recovery ~ PostFireNBR, data = Yr6) # ns 0.16
kruskal.test(Recovery ~ PostFireNBR, data = Yr7) # ns 0.07
kruskal.test(Recovery ~ PostFireNBR, data = Yr8) # ns 0.07
kruskal.test(Recovery ~ PostFireNBR, data = Yr9) # ns 0.07
kruskal.test(Recovery ~ PostFireNBR, data = Yr10) # ns 0.09
sig_tbl$`K-W` = c("***", "**", "*", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_postfireNBR_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_postfireNBR_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 60, xmin = 16)
dev.off()

# Severity1
# Recovery
# Timeseries plot
#####
recovery_severity1_ts_byFire = ggplot(data = AllFires_recovery1, 
                                     aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Severity1), alpha = 0.333)  +
  stat_summary(aes(group = Severity1, color = Severity1), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow2", "orange2", "orangered2", "darkred"), name = "Severity") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.87))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity1 == "L")
Yr1_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr1_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr1_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity1 == "H")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity1 == "L")
Yr2_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr2_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr2_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity1 == "H")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity1 == "L")
Yr3_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr3_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr3_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity1 == "H")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity1 == "L")
Yr4_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr4_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr4_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity1 == "H")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity1 == "L")
Yr5_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr5_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr5_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity1 == "H")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity1 == "L")
Yr6_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr6_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr6_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity1 == "H")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity1 == "L")
Yr7_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr7_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr7_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity1 == "H")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity1 == "L")
Yr8_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr8_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr8_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity1 == "H")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity1 == "L")
Yr9_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Severity1 == "ML")
Yr9_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Severity1 == "MH")
Yr9_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity1 == "H")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity1 == "L")
Yr10_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Severity1 == "ML")
Yr10_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Severity1 == "MH")
Yr10_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity1 == "H")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 8))
colnames(sig_tbl) = c("Year", "L.ML", "ML.MH","MH.H", "L.MH", "ML.H", "L.H", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_L, Yr1_ML) # ns 0.09
wilcox.test(Yr2_L, Yr2_ML) # ns 0.49
wilcox.test(Yr3_L, Yr3_ML) # ns 0.10
wilcox.test(Yr4_L, Yr4_ML) # ns 0.19
wilcox.test(Yr5_L, Yr5_ML) # ns 0.61
wilcox.test(Yr6_L, Yr6_ML) # ns 0.52
wilcox.test(Yr7_L, Yr7_ML) # ns 0.52
wilcox.test(Yr8_L, Yr8_ML) # ns 0.83
wilcox.test(Yr9_L, Yr9_ML) # ns 0.35
wilcox.test(Yr10_L, Yr10_ML) # ns 0.26
sig_tbl$`L.ML` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_ML, Yr1_MH) # ns 1
wilcox.test(Yr2_ML, Yr2_MH) # ns 0.89
wilcox.test(Yr3_ML, Yr3_MH) # ns 0.31
wilcox.test(Yr4_ML, Yr4_MH) # ns 0.52
wilcox.test(Yr5_ML, Yr5_MH) # ns 0.60
wilcox.test(Yr6_ML, Yr6_MH) # ns 0.70
wilcox.test(Yr7_ML, Yr7_MH) # ns 0.52
wilcox.test(Yr8_ML, Yr8_MH) # ns 0.27
wilcox.test(Yr9_ML, Yr9_MH) # ns 0.18
wilcox.test(Yr10_ML, Yr10_MH) # ns 0.26
sig_tbl$`ML.MH` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_MH, Yr1_H) # ns 0.26
wilcox.test(Yr2_MH, Yr2_H) # ns 0.95
wilcox.test(Yr3_MH, Yr3_H) # ns 0.98
wilcox.test(Yr4_MH, Yr4_H) # ns 0.87
wilcox.test(Yr5_MH, Yr5_H) # ns 1
wilcox.test(Yr6_MH, Yr6_H) # ns 0.85
wilcox.test(Yr7_MH, Yr7_H) # ns 0.73
wilcox.test(Yr8_MH, Yr8_H) # ns 0.60
wilcox.test(Yr9_MH, Yr9_H) # ns 0.44
wilcox.test(Yr10_MH, Yr10_H) # ns 0.27
sig_tbl$`MH.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_L, Yr1_MH) # ns 0.21
wilcox.test(Yr2_L, Yr2_MH) # ns 0.42
wilcox.test(Yr3_L, Yr3_MH) # * 0.03
wilcox.test(Yr4_L, Yr4_MH) # * 0.04
wilcox.test(Yr5_L, Yr5_MH) # ns 0.21
wilcox.test(Yr6_L, Yr6_MH) # ns 0.28
wilcox.test(Yr7_L, Yr7_MH) # ns 0.28
wilcox.test(Yr8_L, Yr8_MH) # ns 0.18
wilcox.test(Yr9_L, Yr9_MH) # ns 0.09
wilcox.test(Yr10_L, Yr10_MH) # ns 0.08
sig_tbl$`L.MH` = c("ns", "ns", "*", "*", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_ML, Yr1_H) # ns 0.66
wilcox.test(Yr2_ML, Yr2_H) # ns 0.72
wilcox.test(Yr3_ML, Yr3_H) # ns 0.54
wilcox.test(Yr4_ML, Yr4_H) # ns 0.54
wilcox.test(Yr5_ML, Yr5_H) # ns 0.37
wilcox.test(Yr6_ML, Yr6_H) # ns 0.32
wilcox.test(Yr7_ML, Yr7_H) # ns 0.08
wilcox.test(Yr8_ML, Yr8_H) # ns 0.06
wilcox.test(Yr9_ML, Yr9_H) # * 0.05
wilcox.test(Yr10_ML, Yr10_H) # * 0.01
sig_tbl$`ML.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*")

wilcox.test(Yr1_L, Yr1_H) # ns 0.14
wilcox.test(Yr2_L, Yr2_H) # ns 0.33
wilcox.test(Yr3_L, Yr3_H) # * 0.05
wilcox.test(Yr4_L, Yr4_H) # ns 0.06
wilcox.test(Yr5_L, Yr5_H) # ns 0.09
wilcox.test(Yr6_L, Yr6_H) # ns 0.09
wilcox.test(Yr7_L, Yr7_H) # ns 0.09
wilcox.test(Yr8_L, Yr8_H) # * 0.05
wilcox.test(Yr9_L, Yr9_H) # * 0.02
wilcox.test(Yr10_L, Yr10_H) # ** 0.004
sig_tbl$`L.H` = c("ns", "ns", "*", "ns", "ns", "ns", "ns", "*", "*", "**")

wilcox.test(c(Yr1_L, Yr1_ML), c(Yr1_MH, Yr1_H)) # ns 0.33
wilcox.test(c(Yr2_L, Yr2_ML), c(Yr2_MH, Yr2_H)) # ns 0.45
wilcox.test(c(Yr3_L, Yr3_ML), c(Yr3_MH, Yr3_H)) # * 0.05
wilcox.test(c(Yr4_L, Yr4_ML), c(Yr4_MH, Yr4_H)) # ns 0.09
wilcox.test(c(Yr5_L, Yr5_ML), c(Yr5_MH, Yr5_H)) # ns 0.14
wilcox.test(c(Yr6_L, Yr6_ML), c(Yr6_MH, Yr6_H)) # ns 0.17
wilcox.test(c(Yr7_L, Yr7_ML), c(Yr7_MH, Yr7_H)) # ns 0.07
wilcox.test(c(Yr8_L, Yr8_ML), c(Yr8_MH, Yr8_H)) # * 0.02
wilcox.test(c(Yr9_L, Yr9_ML), c(Yr9_MH, Yr9_H)) # ** 0.009
wilcox.test(c(Yr10_L, Yr10_ML), c(Yr10_MH, Yr10_H)) # ** 0.004

kruskal.test(Recovery ~ Severity1, data = Yr1) # ns 0.27
kruskal.test(Recovery ~ Severity1, data = Yr2) # ns 0.76
kruskal.test(Recovery ~ Severity1, data = Yr3) # ns 0.11
kruskal.test(Recovery ~ Severity1, data = Yr4) # ns 0.18
kruskal.test(Recovery ~ Severity1, data = Yr5) # ns 0.41
kruskal.test(Recovery ~ Severity1, data = Yr6) # ns 0.42
kruskal.test(Recovery ~ Severity1, data = Yr7) # ns 0.26
kruskal.test(Recovery ~ Severity1, data = Yr8) # ns 0.13
kruskal.test(Recovery ~ Severity1, data = Yr9) # * 0.05
kruskal.test(Recovery ~ Severity1, data = Yr10) # * 0.01
sig_tbl$`K-W` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_severity1_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_severity1_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 15)
dev.off()

# Severity2
# Recovery
# Timeseries plot
#####
recovery_severity2_ts_byFire = ggplot(data = AllFires_recovery1, 
                                      aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Severity2), alpha = 0.333)  +
  stat_summary(aes(group = Severity2, color = Severity2), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow2", "orange2", "orangered2", "darkred"), name = "Severity") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.87))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity2 == "L")
Yr1_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr1_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr1_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity2 == "H")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity2 == "L")
Yr2_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr2_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr2_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity2 == "H")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity2 == "L")
Yr3_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr3_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr3_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity2 == "H")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity2 == "L")
Yr4_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr4_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr4_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity2 == "H")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity2 == "L")
Yr5_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr5_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr5_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity2 == "H")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity2 == "L")
Yr6_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr6_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr6_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity2 == "H")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity2 == "L")
Yr7_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr7_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr7_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity2 == "H")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity2 == "L")
Yr8_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr8_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr8_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity2 == "H")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity2 == "L")
Yr9_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Severity2 == "ML")
Yr9_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Severity2 == "MH")
Yr9_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity2 == "H")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity2 == "L")
Yr10_ML = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Severity2 == "ML")
Yr10_MH = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Severity2 == "MH")
Yr10_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity2 == "H")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 8))
colnames(sig_tbl) = c("Year", "L.ML", "ML.MH","MH.H", "L.MH", "ML.H", "L.H", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_L, Yr1_ML) # ns 0.08
wilcox.test(Yr2_L, Yr2_ML) # ns 0.38
wilcox.test(Yr3_L, Yr3_ML) # ns 0.15
wilcox.test(Yr4_L, Yr4_ML) # ns 0.16
wilcox.test(Yr5_L, Yr5_ML) # ns 0.32
wilcox.test(Yr6_L, Yr6_ML) # ns 0.21
wilcox.test(Yr7_L, Yr7_ML) # ns 0.18
wilcox.test(Yr8_L, Yr8_ML) # ns 0.53
wilcox.test(Yr9_L, Yr9_ML) # ns 0.29
wilcox.test(Yr10_L, Yr10_ML) # ns 0.26
sig_tbl$`L.ML` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_ML, Yr1_MH) # ns 0.97
wilcox.test(Yr2_ML, Yr2_MH) # ns 0.83
wilcox.test(Yr3_ML, Yr3_MH) # ns 0.61
wilcox.test(Yr4_ML, Yr4_MH) # ns 0.69
wilcox.test(Yr5_ML, Yr5_MH) # ns 1
wilcox.test(Yr6_ML, Yr6_MH) # ns 1
wilcox.test(Yr7_ML, Yr7_MH) # ns 0.75
wilcox.test(Yr8_ML, Yr8_MH) # ns 0.52
wilcox.test(Yr9_ML, Yr9_MH) # ns 0.40
wilcox.test(Yr10_ML, Yr10_MH) # ns 0.49
sig_tbl$`ML.MH` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_MH, Yr1_H) # ns 0.26
wilcox.test(Yr2_MH, Yr2_H) # ns 0.95
wilcox.test(Yr3_MH, Yr3_H) # ns 0.98
wilcox.test(Yr4_MH, Yr4_H) # ns 0.87
wilcox.test(Yr5_MH, Yr5_H) # ns 1
wilcox.test(Yr6_MH, Yr6_H) # ns 0.85
wilcox.test(Yr7_MH, Yr7_H) # ns 0.73
wilcox.test(Yr8_MH, Yr8_H) # ns 0.60
wilcox.test(Yr9_MH, Yr9_H) # ns 0.44
wilcox.test(Yr10_MH, Yr10_H) # ns 0.27
sig_tbl$`MH.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_L, Yr1_MH) # ns 0.27
wilcox.test(Yr2_L, Yr2_MH) # ns 0.27
wilcox.test(Yr3_L, Yr3_MH) # ** 0.01
wilcox.test(Yr4_L, Yr4_MH) # * 0.04
wilcox.test(Yr5_L, Yr5_MH) # ns 0.11
wilcox.test(Yr6_L, Yr6_MH) # ns 0.17
wilcox.test(Yr7_L, Yr7_MH) # ns 0.20
wilcox.test(Yr8_L, Yr8_MH) # ns 0.09
wilcox.test(Yr9_L, Yr9_MH) # * 0.03
wilcox.test(Yr10_L, Yr10_MH) # * 0.05
sig_tbl$`L.MH` = c("ns", "ns", "**", "*", "ns", "ns", "ns", "ns", "*", "*")

wilcox.test(Yr1_ML, Yr1_H) # ns 0.74
wilcox.test(Yr2_ML, Yr2_H) # ns 0.74
wilcox.test(Yr3_ML, Yr3_H) # ns 0.89
wilcox.test(Yr4_ML, Yr4_H) # ns 1
wilcox.test(Yr5_ML, Yr5_H) # ns 0.87
wilcox.test(Yr6_ML, Yr6_H) # ns 0.78
wilcox.test(Yr7_ML, Yr7_H) # ns 0.28
wilcox.test(Yr8_ML, Yr8_H) # ns 0.23
wilcox.test(Yr9_ML, Yr9_H) # ns 0.18
wilcox.test(Yr10_ML, Yr10_H) # ns 0.07
sig_tbl$`ML.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_L, Yr1_H) # ns 0.17
wilcox.test(Yr2_L, Yr2_H) # ns 0.09
wilcox.test(Yr3_L, Yr3_H) # * 0.01
wilcox.test(Yr4_L, Yr4_H) # * 0.02
wilcox.test(Yr5_L, Yr5_H) # * 0.02
wilcox.test(Yr6_L, Yr6_H) # * 0.02
wilcox.test(Yr7_L, Yr7_H) # * 0.02
wilcox.test(Yr8_L, Yr8_H) # ** 0.009
wilcox.test(Yr9_L, Yr9_H) # ** 0.004
wilcox.test(Yr10_L, Yr10_H) # *** 0.0007
sig_tbl$`L.H` = c("ns", "ns", "*", "*", "*", "*", "*", "**", "**", "***")

wilcox.test(c(Yr1_L, Yr1_ML), c(Yr1_MH, Yr1_H)) # ns 0.33
wilcox.test(c(Yr2_L, Yr2_ML), c(Yr2_MH, Yr2_H)) # ns 0.45
wilcox.test(c(Yr3_L, Yr3_ML), c(Yr3_MH, Yr3_H)) # * 0.05
wilcox.test(c(Yr4_L, Yr4_ML), c(Yr4_MH, Yr4_H)) # ns 0.09
wilcox.test(c(Yr5_L, Yr5_ML), c(Yr5_MH, Yr5_H)) # ns 0.14
wilcox.test(c(Yr6_L, Yr6_ML), c(Yr6_MH, Yr6_H)) # ns 0.17
wilcox.test(c(Yr7_L, Yr7_ML), c(Yr7_MH, Yr7_H)) # ns 0.07
wilcox.test(c(Yr8_L, Yr8_ML), c(Yr8_MH, Yr8_H)) # * 0.02
wilcox.test(c(Yr9_L, Yr9_ML), c(Yr9_MH, Yr9_H)) # ** 0.009
wilcox.test(c(Yr10_L, Yr10_ML), c(Yr10_MH, Yr10_H)) # ** 0.004

kruskal.test(Recovery ~ Severity2, data = Yr1) # ns 0.30
kruskal.test(Recovery ~ Severity2, data = Yr2) # ns 0.50
kruskal.test(Recovery ~ Severity2, data = Yr3) # ns 0.06
kruskal.test(Recovery ~ Severity2, data = Yr4) # ns 0.13
kruskal.test(Recovery ~ Severity2, data = Yr5) # ns 0.24
kruskal.test(Recovery ~ Severity2, data = Yr6) # ns 0.23
kruskal.test(Recovery ~ Severity2, data = Yr7) # ns 0.17
kruskal.test(Recovery ~ Severity2, data = Yr8) # ns 0.10
kruskal.test(Recovery ~ Severity2, data = Yr9) # * 0.03
kruskal.test(Recovery ~ Severity2, data = Yr10) # * 0.01
sig_tbl$`K-W` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_severity2_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_severity2_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 15)
dev.off()

# Severity3
# Recovery
# Timeseries plot
#####
recovery_severity3_ts_byFire = ggplot(data = AllFires_recovery1, 
                                      aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Severity3), alpha = 0.333)  +
  stat_summary(aes(group = Severity3, color = Severity3), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow3", "orange3", "red3"), name = "Severity") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.87))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity3 == "L")
Yr1_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                  AllFires_recovery1$Severity3 == "M")
Yr1_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity3 == "H")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity3 == "L")
Yr2_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                  AllFires_recovery1$Severity3 == "M")
Yr2_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity3 == "H")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity3 == "L")
Yr3_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                  AllFires_recovery1$Severity3 == "M")
Yr3_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity3 == "H")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity3 == "L")
Yr4_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                  AllFires_recovery1$Severity3 == "M")
Yr4_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity3 == "H")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity3 == "L")
Yr5_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                  AllFires_recovery1$Severity3 == "M")
Yr5_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity3 == "H")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity3 == "L")
Yr6_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                  AllFires_recovery1$Severity3 == "M")
Yr6_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity3 == "H")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity3 == "L")
Yr7_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                  AllFires_recovery1$Severity3 == "M")
Yr7_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity3 == "H")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity3 == "L")
Yr8_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                  AllFires_recovery1$Severity3 == "M")
Yr8_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity3 == "H")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity3 == "L")
Yr9_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                  AllFires_recovery1$Severity3 == "M")
Yr9_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity3 == "H")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity3 == "L")
Yr10_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                   AllFires_recovery1$Severity3 == "M")
Yr10_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity3 == "H")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "L.M", "M.H", "L.H", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_L, Yr1_M) # ns 0.12
wilcox.test(Yr2_L, Yr2_M) # ns 0.24
wilcox.test(Yr3_L, Yr3_M) # * 0.02
wilcox.test(Yr4_L, Yr4_M) # * 0.04
wilcox.test(Yr5_L, Yr5_M) # ns 0.12
wilcox.test(Yr6_L, Yr6_M) # ns 0.12
wilcox.test(Yr7_L, Yr7_M) # ns 0.12
wilcox.test(Yr8_L, Yr8_M) # ns 0.13
wilcox.test(Yr9_L, Yr9_M) # * 0.05
wilcox.test(Yr10_L, Yr10_M) # * 0.04
sig_tbl$`L.M` = c("ns", "ns", "*", "*", "ns", "ns", "ns", "ns", "*", "*")

wilcox.test(Yr1_M, Yr1_H) # ns 0.34
wilcox.test(Yr2_M, Yr2_H) # ns 0.93
wilcox.test(Yr3_M, Yr3_H) # ns 0.92
wilcox.test(Yr4_M, Yr4_H) # ns 0.90
wilcox.test(Yr5_M, Yr5_H) # ns 0.94
wilcox.test(Yr6_M, Yr6_H) # ns 0.78
wilcox.test(Yr7_M, Yr7_H) # ns 0.46
wilcox.test(Yr8_M, Yr8_H) # ns 0.34
wilcox.test(Yr9_M, Yr9_H) # ns 0.24
wilcox.test(Yr10_M, Yr10_H) # ns 0.12
sig_tbl$`M.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_L, Yr1_H) # ns 0.17
wilcox.test(Yr2_L, Yr2_H) # ns 0.09
wilcox.test(Yr3_L, Yr3_H) # * 0.01
wilcox.test(Yr4_L, Yr4_H) # * 0.02
wilcox.test(Yr5_L, Yr5_H) # * 0.02
wilcox.test(Yr6_L, Yr6_H) # * 0.02
wilcox.test(Yr7_L, Yr7_H) # * 0.02
wilcox.test(Yr8_L, Yr8_H) # ** 0.009
wilcox.test(Yr9_L, Yr9_H) # ** 0.004
wilcox.test(Yr10_L, Yr10_H) # *** 0.0007
sig_tbl$`L.H` = c("ns", "ns", "*", "*", "*", "*", "*", "**", "**", "***")

kruskal.test(Recovery ~ Severity3, data = Yr1) # ns 0.17
kruskal.test(Recovery ~ Severity3, data = Yr2) # ns 0.31
kruskal.test(Recovery ~ Severity3, data = Yr3) # * 0.03
kruskal.test(Recovery ~ Severity3, data = Yr4) # ns 0.06
kruskal.test(Recovery ~ Severity3, data = Yr5) # ns 0.12
kruskal.test(Recovery ~ Severity3, data = Yr6) # ns 0.11
kruskal.test(Recovery ~ Severity3, data = Yr7) # ns 0.09
kruskal.test(Recovery ~ Severity3, data = Yr8) # * 0.05
kruskal.test(Recovery ~ Severity3, data = Yr9) # * 0.02
kruskal.test(Recovery ~ Severity3, data = Yr10) # ** 0.007
sig_tbl$`K-W` = c("ns", "ns", "*", "ns", "ns", "ns", "ns", "*", "*", "**")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_severity3_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_severity3_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 20)
dev.off()

# Severity4
# Recovery
# Timeseries plot
#####
recovery_severity4_ts_byFire = ggplot(data = AllFires_recovery1, 
                                      aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Severity4), alpha = 0.333)  +
  stat_summary(aes(group = Severity4, color = Severity4), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow3", "orange3", "red3"), name = "Severity") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.87))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity4 == "L")
Yr1_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity4 == "M")
Yr1_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity4 == "H")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity4 == "L")
Yr2_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity4 == "M")
Yr2_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity4 == "H")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity4 == "L")
Yr3_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity4 == "M")
Yr3_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity4 == "H")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity4 == "L")
Yr4_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity4 == "M")
Yr4_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity4 == "H")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity4 == "L")
Yr5_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity4 == "M")
Yr5_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity4 == "H")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity4 == "L")
Yr6_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity4 == "M")
Yr6_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity4 == "H")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity4 == "L")
Yr7_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity4 == "M")
Yr7_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity4 == "H")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity4 == "L")
Yr8_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity4 == "M")
Yr8_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity4 == "H")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity4 == "L")
Yr9_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity4 == "M")
Yr9_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity4 == "H")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity4 == "L")
Yr10_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity4 == "M")
Yr10_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity4 == "H")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "L.M", "M.H", "L.H", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_L, Yr1_M) # ns 0.33
wilcox.test(Yr2_L, Yr2_M) # ns 0.81
wilcox.test(Yr3_L, Yr3_M) # ns 0.12
wilcox.test(Yr4_L, Yr4_M) # ns 0.11
wilcox.test(Yr5_L, Yr5_M) # ns 0.30
wilcox.test(Yr6_L, Yr6_M) # ns 0.39
wilcox.test(Yr7_L, Yr7_M) # ns 0.33
wilcox.test(Yr8_L, Yr8_M) # ns 0.35
wilcox.test(Yr9_L, Yr9_M) # ns 0.11
wilcox.test(Yr10_L, Yr10_M) # * 0.02
sig_tbl$`L.M` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*")

wilcox.test(Yr1_M, Yr1_H) # ns 0.27
wilcox.test(Yr2_M, Yr2_H) # ns 0.70
wilcox.test(Yr3_M, Yr3_H) # ns 0.70
wilcox.test(Yr4_M, Yr4_H) # ns 0.75
wilcox.test(Yr5_M, Yr5_H) # ns 0.58
wilcox.test(Yr6_M, Yr6_H) # ns 0.42
wilcox.test(Yr7_M, Yr7_H) # ns 0.22
wilcox.test(Yr8_M, Yr8_H) # ns 0.15
wilcox.test(Yr9_M, Yr9_H) # ns 0.10
wilcox.test(Yr10_M, Yr10_H) # * 0.04
sig_tbl$`M.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*")

wilcox.test(Yr1_L, Yr1_H) # ns 0.30
wilcox.test(Yr2_L, Yr2_H) # ns 0.61
wilcox.test(Yr3_L, Yr3_H) # ns 0.15
wilcox.test(Yr4_L, Yr4_H) # ns 0.15
wilcox.test(Yr5_L, Yr5_H) # ns 0.15
wilcox.test(Yr6_L, Yr6_H) # ns 0.21
wilcox.test(Yr7_L, Yr7_H) # ns 0.21
wilcox.test(Yr8_L, Yr8_H) # 11 0.11
wilcox.test(Yr9_L, Yr9_H) # * 0.05
wilcox.test(Yr10_L, Yr10_H) # * 0.01
sig_tbl$`L.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*")

kruskal.test(Recovery ~ Severity4, data = Yr1) # ns 0.31
kruskal.test(Recovery ~ Severity4, data = Yr2) # ns 0.84
kruskal.test(Recovery ~ Severity4, data = Yr3) # ns 0.23
kruskal.test(Recovery ~ Severity4, data = Yr4) # ns 0.22
kruskal.test(Recovery ~ Severity4, data = Yr5) # ns 0.36
kruskal.test(Recovery ~ Severity4, data = Yr6) # ns 0.37
kruskal.test(Recovery ~ Severity4, data = Yr7) # ns 0.23
kruskal.test(Recovery ~ Severity4, data = Yr8) # ns 0.15
kruskal.test(Recovery ~ Severity4, data = Yr9) # * 0.05
kruskal.test(Recovery ~ Severity4, data = Yr10) # ** 0.008
sig_tbl$`K-W` = c("ns", "ns", "*", "ns", "ns", "ns", "ns", "ns", "*", "**")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_severity4_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_severity4_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 20)
dev.off()

# Severity5
# Recovery
# Timeseries plot
#####
recovery_severity5_ts_byFire = ggplot(data = AllFires_recovery1, 
                                      aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = Severity5), alpha = 0.333)  +
  stat_summary(aes(group = Severity5, color = Severity5), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow3", "orange3", "red3"), name = "Severity") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.87))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity5 == "L")
Yr1_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity5 == "M")
Yr1_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$Severity5 == "H")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity5 == "L")
Yr2_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity5 == "M")
Yr2_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$Severity5 == "H")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity5 == "L")
Yr3_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity5 == "M")
Yr3_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$Severity5 == "H")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity5 == "L")
Yr4_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity5 == "M")
Yr4_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$Severity5 == "H")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity5 == "L")
Yr5_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity5 == "M")
Yr5_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$Severity5 == "H")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity5 == "L")
Yr6_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity5 == "M")
Yr6_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$Severity5 == "H")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity5 == "L")
Yr7_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity5 == "M")
Yr7_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$Severity5 == "H")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity5 == "L")
Yr8_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity5 == "M")
Yr8_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$Severity5 == "H")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity5 == "L")
Yr9_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity5 == "M")
Yr9_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$Severity5 == "H")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_L = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity5 == "L")
Yr10_M = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity5 == "M")
Yr10_H = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$Severity5 == "H")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "L.M", "M.H", "L.H", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_L, Yr1_M) # ns 0.11
wilcox.test(Yr2_L, Yr2_M) # ns 0.39
wilcox.test(Yr3_L, Yr3_M) # * 0.03
wilcox.test(Yr4_L, Yr4_M) # * 0.05
wilcox.test(Yr5_L, Yr5_M) # ns 0.28
wilcox.test(Yr6_L, Yr6_M) # ns 0.31
wilcox.test(Yr7_L, Yr7_M) # ns 0.30
wilcox.test(Yr8_L, Yr8_M) # ns 0.33
wilcox.test(Yr9_L, Yr9_M) # ns 0.12
wilcox.test(Yr10_L, Yr10_M) # ns 0.08
sig_tbl$`L.M` = c("ns", "ns", "*", "*", "ns", "ns", "ns", "ns", "ns", "ns")

wilcox.test(Yr1_M, Yr1_H) # ns 0.34
wilcox.test(Yr2_M, Yr2_H) # ns 0.82
wilcox.test(Yr3_M, Yr3_H) # ns 0.83
wilcox.test(Yr4_M, Yr4_H) # ns 0.88
wilcox.test(Yr5_M, Yr5_H) # ns 0.66
wilcox.test(Yr6_M, Yr6_H) # ns 0.52
wilcox.test(Yr7_M, Yr7_H) # ns 0.28
wilcox.test(Yr8_M, Yr8_H) # ns 0.20
wilcox.test(Yr9_M, Yr9_H) # ns 0.13
wilcox.test(Yr10_M, Yr10_H) # * 0.05
sig_tbl$`M.H` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*")

wilcox.test(Yr1_L, Yr1_H) # ns 0.14
wilcox.test(Yr2_L, Yr2_H) # ns 0.33
wilcox.test(Yr3_L, Yr3_H) # * 0.05
wilcox.test(Yr4_L, Yr4_H) # ns 0.06
wilcox.test(Yr5_L, Yr5_H) # ns 0.09
wilcox.test(Yr6_L, Yr6_H) # ns 0.09
wilcox.test(Yr7_L, Yr7_H) # ns 0.09
wilcox.test(Yr8_L, Yr8_H) # * 0.05
wilcox.test(Yr9_L, Yr9_H) # * 0.02
wilcox.test(Yr10_L, Yr10_H) # ** 0.004
sig_tbl$`L.H` = c("ns", "ns", "*", "ns", "ns", "ns", "ns", "*", "*", "**")

kruskal.test(Recovery ~ Severity5, data = Yr1) # ns 0.15
kruskal.test(Recovery ~ Severity5, data = Yr2) # ns 0.57
kruskal.test(Recovery ~ Severity5, data = Yr3) # ns 0.07
kruskal.test(Recovery ~ Severity5, data = Yr4) # ns 0.11
kruskal.test(Recovery ~ Severity5, data = Yr5) # ns 0.31
kruskal.test(Recovery ~ Severity5, data = Yr6) # ns 0.29
kruskal.test(Recovery ~ Severity5, data = Yr7) # ns 0.20
kruskal.test(Recovery ~ Severity5, data = Yr8) # ns 0.13
kruskal.test(Recovery ~ Severity5, data = Yr9) # * 0.04
kruskal.test(Recovery ~ Severity5, data = Yr10) # * 0.01
sig_tbl$`K-W` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_severity5_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_severity5_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 20)
dev.off()

# Severity vs. maxNBR
# Scatterplot
#####
x <- AllFires_tbl$maxNBR # Change for each figure
y <- AllFires_tbl$dNBR # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- lm1_summary$r.squared
r2
pval <- lm1_summary$coefficients[2,4]
pval

severity_maxnbr_scatter = ggplot(data = NULL, 
                                     aes(x = AllFires_tbl$maxNBR, 
                                         y = AllFires_tbl$dNBR)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, color = "black") +
  #stat_poly_eq(aes(label = paste0("atop(",..eq.label.., ",", ..rr.label.., ")")),
  #formula = formula1, parse = TRUE, size = 6.5, label.y = 0.95) +
  scale_x_continuous(name = 'maxNBR (x1000)', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
  #coord_cartesian(ylim = c(150, 950)) +
  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
  annotate(geom = "text", x = 175, y = 825, label = expression(italic(R^2) * "= 0.32****"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
pdf("severity_maxnbr_Scatterr32.pdf", width = 11.599, height = 7) #16.57 # 10
severity_maxnbr_scatter
dev.off()

# Bar and Boxplot
##### 
# All Fire events
severity_maxnbr_box = ggplot(AllFires_severity, 
                                 aes(x = reorder(ID, maxNBR, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = maxCat), color = "black") +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4"), name = "maxNBR (x1000)") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1800)) + 
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.15, 0.875))

# All pixels
far_compare = list(c("<450",">600"))
clo_compare = list(c("<450","450-600"), c("450-600",">600"))

severity_maxnbr_box_allpixels = ggplot(AllFires_severity, 
                                           aes(x = maxCat, y = dNBR)) +
  geom_violin(aes(fill = maxCat), color = "black", show.legend = FALSE, trim = FALSE) +#_boxplot
  geom_boxplot(width = 0.20, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  stat_summary(fun = mean, geom = "point", shape = 1, size = 1) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = clo_compare, label = "p.signif", label.y = 1000) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med1_compare, label = "p.signif", label.y = 1080) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = med2_compare, label = "p.signif", label.y = 1160) +
  #stat_compare_means(method = "wilcox.test", #****
  #comparisons = far_compare, label = "p.signif", label.y = 1240) +
  stat_compare_means(method = "kruskal.test", 
                     label= "p.format", label.y = max(AllFires_severity$dNBR) - 50,
                     label.x.npc = "left", size = 5) + 
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

# Means
severity_maxnbr_box_byfire = ggplot(AllFires_severity1, 
                                        aes(x = maxCat, y = dNBR_mean)) + #dNBR_median
  geom_boxplot(aes(fill = maxCat), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test",
                     comparisons = clo_compare, label = "p.signif", size = 5.5, 
                     label.y = max(AllFires_severity1$dNBR_mean) + 80) +
  stat_compare_means(method = "wilcox.test", size = 5.5,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 160) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(AllFires_severity1$dNBR_mean) + 260, label.x = 1.2, size = 4) +
  scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  coord_cartesian(ylim = c(100, 1800)) +
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
#####
severity_maxnbr_box
severity_maxnbr_box_allpixels
severity_maxnbr_box_byfire

# Combine into one figure
pdf("Severity_maxNBR_box_byfire.pdf", width = 11.599, height = 7) #16.57 # 10
plot_grid(severity_maxnbr_box, severity_maxnbr_box_byfire, #allpixels
          nrow = 1, align = "h", rel_widths = c(4, 1))  #labels = "auto"
dev.off()

# Burn Year
# Recovery
# Timeseries plot
#####
recovery_burnyear_ts_byFire = ggplot(data = AllFires_recovery1, 
                                      aes(x = Year, y = Recovery, group = ID)) +
  geom_line(aes(color = BurnYear), alpha = 0.4)  +
  stat_summary(aes(group = BurnYear, color = BurnYear), fun = mean, geom = "line", size = 2.5) + 
  scale_color_manual(values = c("yellow3", "orange3", "red3"), name = "Burn Year") +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0)) +
  scale_y_continuous(name = 'NBR Recovery to max vegetation (%)', limits = c(0,100), expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.065, 0.87))

# Significance testing
Yr1 = subset(AllFires_recovery1, AllFires_recovery1$Year == 1)
Yr1_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr1_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr1_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 1 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr2 = subset(AllFires_recovery1, AllFires_recovery1$Year == 2)
Yr2_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr2_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr2_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 2 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr3 = subset(AllFires_recovery1, AllFires_recovery1$Year == 3)
Yr3_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr3_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr3_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 3 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr4 = subset(AllFires_recovery1, AllFires_recovery1$Year == 4)
Yr4_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr4_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr4_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 4 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr5 = subset(AllFires_recovery1, AllFires_recovery1$Year == 5)
Yr5_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr5_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr5_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 5 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr6 = subset(AllFires_recovery1, AllFires_recovery1$Year == 6)
Yr6_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr6_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr6_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 6 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr7 = subset(AllFires_recovery1, AllFires_recovery1$Year == 7)
Yr7_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr7_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr7_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 7 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr8 = subset(AllFires_recovery1, AllFires_recovery1$Year == 8)
Yr8_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr8_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr8_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 8 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr9 = subset(AllFires_recovery1, AllFires_recovery1$Year == 9)
Yr9_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$BurnYear == "1990s")
Yr9_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$BurnYear == "2000s")
Yr9_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 9 & 
                 AllFires_recovery1$BurnYear == "2010s")

Yr10 = subset(AllFires_recovery1, AllFires_recovery1$Year == 10)
Yr10_90s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$BurnYear == "1990s")
Yr10_00s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$BurnYear == "2000s")
Yr10_10s = subset(AllFires_recovery1$Recovery, AllFires_recovery1$Year == 10 & 
                  AllFires_recovery1$BurnYear == "2010s")

sig_tbl = data.frame(matrix(nrow = 10, ncol = 5))
colnames(sig_tbl) = c("Year", "90s.00s", "00s.10s", "90s.10s", "K-W")
sig_tbl$Year = 1:10

wilcox.test(Yr1_90s, Yr1_00s) # ns 0.27
wilcox.test(Yr2_90s, Yr2_00s) # ns 0.10
wilcox.test(Yr3_90s, Yr3_00s) # * 0.03
wilcox.test(Yr4_90s, Yr4_00s) # ** 0.007
wilcox.test(Yr5_90s, Yr5_00s) # *** 0.001
wilcox.test(Yr6_90s, Yr6_00s) # *** 0.0007
wilcox.test(Yr7_90s, Yr7_00s) # **** 9.248e-05
wilcox.test(Yr8_90s, Yr8_00s) # **** 5.089e-06
wilcox.test(Yr9_90s, Yr9_00s) # **** 2.453e-06
wilcox.test(Yr10_90s, Yr10_00s) # **** 6.99e-07
sig_tbl$`90s.00s` = c("ns", "ns", "*", "**", "***", "***", "****", "****", "****", "****")

wilcox.test(Yr1_00s, Yr1_10s) # ns 0.88
wilcox.test(Yr2_00s, Yr2_10s) # ns 0.95
wilcox.test(Yr3_00s, Yr3_10s) # ns 0.70
wilcox.test(Yr4_00s, Yr4_10s) # ns 0.93
wilcox.test(Yr5_00s, Yr5_10s) # ns 0.53
wilcox.test(Yr6_00s, Yr6_10s) # ns 0.14
wilcox.test(Yr7_00s, Yr7_10s) # ns 0.46
wilcox.test(Yr8_00s, Yr8_10s) # ns 0.09
wilcox.test(Yr9_00s, Yr9_10s) # ns 0.61
wilcox.test(Yr10_00s, Yr10_10s) # NA
sig_tbl$`00s.10s` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "")

wilcox.test(Yr1_90s, Yr1_10s) # ns 0.47
wilcox.test(Yr2_90s, Yr2_10s) # ns 0.39
wilcox.test(Yr3_90s, Yr3_10s) # ns 0.21
wilcox.test(Yr4_90s, Yr4_10s) # ns 0.22
wilcox.test(Yr5_90s, Yr5_10s) # ns 0.09
wilcox.test(Yr6_90s, Yr6_10s) # ns 0.08
wilcox.test(Yr7_90s, Yr7_10s) # ns 0.17
wilcox.test(Yr8_90s, Yr8_10s) # * 0.03
wilcox.test(Yr9_90s, Yr9_10s) # * 0.03
wilcox.test(Yr10_90s, Yr10_10s) # NA
sig_tbl$`90s.10s` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*", "")

kruskal.test(Recovery ~ BurnYear, data = Yr1) # ns 0.51
kruskal.test(Recovery ~ BurnYear, data = Yr2) # ns 0.29
kruskal.test(Recovery ~ BurnYear, data = Yr3) # ns 0.09
kruskal.test(Recovery ~ BurnYear, data = Yr4) # * 0.04
kruskal.test(Recovery ~ BurnYear, data = Yr5) # ** 0.007
kruskal.test(Recovery ~ BurnYear, data = Yr6) # ** 0.003
kruskal.test(Recovery ~ BurnYear, data = Yr7) # *** 0.001
kruskal.test(Recovery ~ BurnYear, data = Yr8) # **** 7.052e-05
kruskal.test(Recovery ~ BurnYear, data = Yr9) # **** 0.000101
kruskal.test(Recovery ~ BurnYear, data = Yr10) # **** 2.43e-05
sig_tbl$`K-W` = c("ns", "ns", "ns", "*", "**", "**", "***", "****", "****", "****")

sig_tbl1 = ggtexttable(sig_tbl, rows = NULL, theme = ttheme(base_size = 12))

#recovery_extent_ts_allPixels

#####
pdf("Recovery_burnyear_ts_byFire.pdf", width = 11.599, height = 7) #16.57 # 10 #allPixels
recovery_burnyear_ts_byFire + annotation_custom(ggplotGrob(sig_tbl1), ymax = 62, xmin = 20)
dev.off()