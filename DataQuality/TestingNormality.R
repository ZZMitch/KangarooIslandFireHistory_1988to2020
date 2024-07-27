# Testing Normality #

#dNBR
dnbr_fix_tbl = read.csv("dNBR FIX/Fig7_tbl_fix.csv")

hist(dnbr_fix_tbl$dNBR) #Normal
shapiro.test(dnbr_fix_tbl$dNBR) #Normal

hist(dnbr_fix_tbl$mNBR) #Negatively Skewed
shapiro.test(dnbr_fix_tbl$mNBR) #Not Normal

hist(dnbr_fix_tbl$BL50) #Positively Skewed
shapiro.test(dnbr_fix_tbl$BL50) #Not Normal

hist(dnbr_fix_tbl$BExt) #Normal-ish
shapiro.test(dnbr_fix_tbl$BExt) #Not Normal

hist(dnbr_fix_tbl$LT) #Negatively Skewed
shapiro.test(dnbr_fix_tbl$LT) #Not Normal

hist(dnbr_fix_tbl$SLB) #Negatively Skewed or tri-modal
shapiro.test(dnbr_fix_tbl$SLB) #Not Normal

hist(dnbr_fix_tbl$Byr) #Negatively Skewed
shapiro.test(dnbr_fix_tbl$Byr) #Not Normal

dnbr_v_mnbr = lm(dNBR ~ mNBR, data = dnbr_fix_tbl)
par(mfrow = c(2,2))
plot(dnbr_v_mnbr) # Some heteroscedasticity but not a huge deal

dnbr_v_bl50 = lm(dNBR ~ BL50, data = dnbr_fix_tbl)
par(mfrow = c(2,2))
plot(dnbr_v_bl50) # Non-normal residual, some high leveage points

dnbr_v_bext = lm(dNBR ~ BExt, data = dnbr_fix_tbl)
par(mfrow = c(2,2))
plot(dnbr_v_bext) # Good

dnbr_v_LT = lm(dNBR ~ LT, data = dnbr_fix_tbl)
par(mfrow = c(2,2))
plot(dnbr_v_LT) # Good

dnbr_v_slb = lm(dNBR ~ SLB, data = dnbr_fix_tbl)
par(mfrow = c(2,2))
plot(dnbr_v_slb) # Good

dnbr_v_byr = lm(dNBR ~ Byr, data = dnbr_fix_tbl)
par(mfrow = c(2,2))
plot(dnbr_v_byr) # Good

# Test dNBR Sub-groups #
par(mfrow = c(1,1))
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$mNBR_grp == "<450")) # Normal
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$mNBR_grp == "<450")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$mNBR_grp == "450-600")) # Normal
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$mNBR_grp == "450-600")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$mNBR_grp == ">600")) # Normal
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$mNBR_grp == ">600")) # Normal

hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BL50_grp == "0")) # Positive Skew
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BL50_grp == "0")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BL50_grp == "<1")) # Normal-ish
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BL50_grp == "<1")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BL50_grp == ">1")) # Normal-ish
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BL50_grp == ">1")) # Norma

hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BExt_grp == "S")) # Normal-ish
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BExt_grp == "S")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BExt_grp == "M")) # Negative Skew
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BExt_grp == "M")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BExt_grp == "L")) # Normal
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$BExt_grp == "L")) # Normal

hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$SLB_grp == "<30")) # Negative Skew
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$SLB_grp == "<30")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$SLB_grp == "30-50")) # Positive Skew
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$SLB_grp == "30-50")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$SLB_grp == ">50")) # Positive Skew
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$SLB_grp == ">50")) # Normal

hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$Byr_grp == "1990s")) # Normal
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$Byr_grp == "1990s")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$Byr_grp == "2000s")) # Negative Skew
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$Byr_grp == "2000s")) # Normal
hist(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$Byr_grp == "2010s")) # Normal
shapiro.test(subset(dnbr_fix_tbl$dNBR, dnbr_fix_tbl$Byr_grp == "2010s")) # Normal
# Stay with MW/KW tests
# Although shapiro tests quantify normality, sample size < 30 for subsets so should also take
# into account visual inspection and some histograms are clearly non-normal visually

#%RtoMV
pf_tbl = read.csv("AllFires_recovery1.csv")

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 1)) #Normal
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 1)) #Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 2)) #Normal-ish
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 2)) #Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 3)) #Negatively Skewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 3)) #Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 4)) #Negatively Skewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 4)) #Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 5)) #Negatively Skewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 5)) #Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 6)) #Negatively SKewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 6)) #Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 7)) #Negatively SKewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 7)) #Not Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 8)) #Negatively SKewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 8)) #Not Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 9)) #Negatively SKewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 9)) #Not Normal

hist(subset(pf_tbl$Recovery, pf_tbl$Year == 10)) #Negatively SKewed
shapiro.test(subset(pf_tbl$Recovery, pf_tbl$Year == 10)) #Not Normal
# Full PF distributions in later years are non-normal. Would be the same for subsets as well. 
# Stick with MW/KW tests. 
