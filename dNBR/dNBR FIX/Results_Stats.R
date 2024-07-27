setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change/dNBR FIX")

tbl = read.csv('AllPixels_dnbr_fix.csv')

tbl_forest = subset(tbl, tbl$LT == "Forest")
mean(tbl_forest$dNBR)
sd(tbl_forest$dNBR)

tbl_nonforest = subset(tbl, tbl$LT == "Non-Forest")
mean(tbl_nonforest$dNBR)
sd(tbl_nonforest$dNBR)

tbl_2008fire = subset(tbl, tbl$ID == "P4_5")
mean(tbl_2008fire$dNBR)
sd(tbl_2008fire$dNBR)\

tbl_2020fire = subset(tbl, tbl$ID == "P6_3")
mean(tbl_2020fire$dNBR)
sd(tbl_2020fire$dNBR)

tbl_lowrecovfire = subset(tbl, tbl$ID == "P2_1")
mean(tbl_lowrecovfire$dNBR)
sd(tbl_lowrecovfire$dNBR)