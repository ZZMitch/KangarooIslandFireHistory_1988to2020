### Creating multivariate models for Severity and Recovery ###

setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
#fire_tbl = read.csv("FireEvents_Stats3.csv") #Pre-dNBR fix, works for PF only
fire_tbl = read.csv("dNBR FIX/FireEventsStats_Fix.csv")

# Multiple linear regression #
# Assumes normality... random forest better

# Random Forest
library(randomForest)

# Set variables to categories
fire_tbl$LT = as.factor(fire_tbl$LT)
fire_tbl$MD = as.factor(fire_tbl$MD)
fire_tbl$Landuse = as.factor(fire_tbl$Landuse)
fire_tbl$VegGroup = as.factor(fire_tbl$VegGroup)
fire_tbl$Species = as.factor(fire_tbl$Species)

# Testing mean of squared residuals
# fire_tbl$dNBR1 = fire_tbl$dNBR / 1000 # Mean of squared residuals scales with scale of dNBR

ntree = 500

### Severity ###
#####
# Variables
####
# dnbr_vars = dNBR ~ Burned_km2_log + SeasonEnd + Burned_Last50 + Burned_Last40 + Burned_Last30 + 
#   Burned_Last20 + Burned_Last10 + SinceLastBurn_NArm + SinceLastBurn_NA70 + ForFire1 + ForPer + Elev + 
#   Elev_Slope + Aspect + TPI3 + TPI5 + TPI9 + TPI15 + TPI31 + TPI65 + TPI111 + TWI + MissingData + maxNBR +
#   PreFireNBR + Burned_Last50_sd + Burned_Last40_sd + Burned_Last30_sd + Burned_Last20_sd + 
#   Burned_Last10_sd + SinceLastBurn_NArm_sd + SinceLastBurn_NA70_sd + Elev_sd + Elev_Slope_sd + Aspect_sd +
#   TPI3_sd + TPI5_sd + TPI9_sd + TPI15_sd + TPI31_sd + TPI65_sd + TPI111_sd + TWI_sd + maxNBR_sd + 
#   PreFireNBR_sd # All relevent variables (45) 
# # Burned_Last50 and sd: 11 missing values
# # SinceLastBurn_NArm: 5 missing values
# # Burned_Last40 and sd: 1 missing value
# 
# dnbr_vars2 = dNBR ~ Burned_km2_log + SeasonEnd + Burned_Last50 + Burned_Last40 + Burned_Last30 + 
#   Burned_Last20 + Burned_Last10 + SinceLastBurn_NArm + SinceLastBurn_NA70 + ForFire1 + ForPer + Elev + 
#   Elev_Slope + Aspect + TPI3 + TPI5 + TPI9 + TPI15 + TPI31 + TPI65 + TPI111 + TWI + MissingData + maxNBR +
#   PreFireNBR + Burned_Last50_sd + Burned_Last40_sd + Burned_Last30_sd + Burned_Last20_sd + 
#   Burned_Last10_sd + Aspect_sd + maxNBR_sd + PreFireNBR_sd + Elev_sd # Most (34)
# # Remove some bad variables that hurt model
# 
# dnbr_vars3 = dNBR ~ maxNBR + Burned_km2_log + SeasonEnd + Burned_Last50 + SinceLastBurn_NA70 +
#   Burned_Last50_sd + MissingData + Elev + TPI5 + TPI111
# 
# dnbr_vars4 = dNBR ~ Burned_km2_log + ForFire1 + PreFireNBR + PreFireNBR_sd + Burned_Last50 + 
#   Burned_Last50_sd + SinceLastBurn_NA70 + SinceLastBurn_NArm_sd + Elev + Elev_sd + Elev_Slope + Aspect +
#   Aspect_sd + TPI5 + TPI111 + TWI + TWI_sd + SeasonEnd + MissingData # 19
# # Top R2 in each category (see Random Forest Helper doc)
# 
# dnbr_vars4.1 = dNBR ~ Burned_km2_log + ForFire1 + maxNBR + maxNBR_sd + Burned_Last50 + 
#   Burned_Last50_sd + SinceLastBurn_NA70 + SinceLastBurn_NArm_sd + Elev + Elev_sd + Elev_Slope + Aspect +
#   Aspect_sd + TPI5 + TPI111 + TWI + TWI_sd + SeasonEnd + MissingData # 19
# # Replace PreFireNBR with maxNBR
# 
# dnbr_vars4.2 = dNBR ~ Burned_km2_log + ForFire1 + maxNBR + maxNBR_sd + Burned_Last50 + 
#   Burned_Last50_sd + SinceLastBurn_NA70 + SinceLastBurn_NArm_sd + Elev + Elev_sd + Elev_Slope + Aspect +
#   Aspect_sd + TPI5 + TPI111 + TWI + TWI_sd + SeasonEnd + MissingData + PreFireNBR + PreFireNBR_sd # 21
# # Add maxNBR and maxNBR_sd to original dnbr_vars4
# 
# dnbr_vars5 = dNBR ~ Burned_km2_log + maxNBR + Burned_Last50 + SinceLastBurn_NA70 + ForFire1 + SeasonEnd +
#   MissingData
# # Key vars only 
# 
# dnbr_vars5.1 = dNBR ~ Burned_km2_log + maxNBR + Burned_Last50 + SinceLastBurn_NA70 + ForFire1 + SeasonEnd
# # Key vars only, remove MissingData
# 
# dnbr_vars5.2 = dNBR ~ Burned_km2_log + maxNBR + Burned_Last50 + SinceLastBurn_NA70 + ForFire1 + 
#   SeasonEnd + MissingData + TPI5
# # Key vars only + TPI5
# 
# dnbr_vars5.3 = dNBR ~ Burned_km2_log + maxNBR + Burned_Last50 + SinceLastBurn_NA70 + ForFire1 + 
#   SeasonEnd + MissingData + TPI31
# # Key vars only + TPI31
####

dnbr_vars = dNBR ~ Bext + mNBR + BL50 + SLB + Byr + LT + MD + TPI65 + NatConPer
# Key vars + TPI65
# NatConPer, MinUsePer, EucPer, MalPer, Landuse, VegGroup, Species

dnbr_vars = dNBR ~ mNBR + BL50 + NatConPer + Bext + Byr + TPI65 + MD


# Variable check
####
x <- fire_tbl$LT # Change for each figure
y <- fire_tbl$dNBR # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- round(lm1_summary$r.squared,2)
r2
#pval <- lm1_summary$coefficients[2,4]
#pval
plot(x, y)
####

# randomForest
rf_dnbr = randomForest(dnbr_vars, data = fire_tbl, importance = TRUE, ntree = 500,
                       na.action = na.roughfix) # na.omit, default mtry = vars / 3 rounded down
varImpPlot(rf_dnbr)
pve_dnbr = round(rf_dnbr$rsq[ntree] * 100,1) # % Variation explained: Psuedo R2
print(pve_dnbr)
rmse_dnbr = round(sqrt(rf_dnbr$mse[ntree]),1) # RMSE (scales based on size of response variable)
prmse_dnbr = round((rmse_dnbr/mean(fire_tbl$dNBR))*100,1) # Mean dNBR is 491
print(prmse_dnbr) # %RMSE - equalized for size of response variable
imp_dnbr = rf_dnbr$importance[, "%IncMSE"]
round(imp_dnbr/max(imp_dnbr), 2)
# See Bright's code 2017 veg recovery analysis 
#####

### Recovery ###
#####
fire_tbl_pf1 = subset(fire_tbl, fire_tbl$PF1 > 0)
fire_tbl_pf2 = subset(fire_tbl, fire_tbl$PF2 > 0)
fire_tbl_pf3 = subset(fire_tbl, fire_tbl$PF3 > 0)
fire_tbl_pf4 = subset(fire_tbl, fire_tbl$PF4 > 0)
fire_tbl_pf5 = subset(fire_tbl, fire_tbl$PF5 > 0)
fire_tbl_pf6 = subset(fire_tbl, fire_tbl$PF6 > 0)
fire_tbl_pf7 = subset(fire_tbl, fire_tbl$PF7 > 0)
fire_tbl_pf8 = subset(fire_tbl, fire_tbl$PF8 > 0)
fire_tbl_pf9 = subset(fire_tbl, fire_tbl$PF9 > 0)
fire_tbl_pf10 = subset(fire_tbl, fire_tbl$PF10 > 0)

# Variables
pf_vars = PF5 ~ Burned_km2_log + SeasonEnd + Burned_Last50 + Burned_Last40 + Burned_Last30 + 
  Burned_Last20 + Burned_Last10 + SinceLastBurn_NArm + SinceLastBurn_NA70 + ForFire1 + ForPer + Elev +
  Elev_Slope + Aspect + TPI3 + TPI5 + TPI9 + TPI15 + TPI31 + TPI65 + TPI111 + TWI + BurnedPer + 
  LowPer_Burn + ModPer_Burn + HiPer_Burn + dNBR_Length + MissingData + dNBR + maxNBR + PreFireNBR + 
  PostFireNBR + dNBR_fromMax + Burned_Last50_sd + Burned_Last40_sd + Burned_Last30_sd + Burned_Last20_sd +
  Burned_Last10_sd + SinceLastBurn_NArm_sd + SinceLastBurn_NA70_sd + Elev_sd + Elev_Slope_sd + Aspect_sd +
  TPI3_sd + TPI5_sd + TPI9_sd + TPI15_sd + TPI31_sd + TPI65_sd + TPI111_sd + TWI_sd + dNBR_sd + maxNBR_sd + 
  PreFireNBR_sd + PostFireNBR_sd + dNBR_fromMax_sd + PF1_sd + PF2_sd + PF3_sd + PF4_sd
# Burned_Last50 and sd: 11 missing values
# SinceLastBurn_NArm: 3 missing values
# Burned_Last40 and sd: 1 missing value

#pf_vars2 = PF5 ~ SinceLastBurn_NArm + dNBR + Burned_Last40 + ForFire1 + MissingData + PostFireNBR + 
#  SeasonEnd

pf_vars2 = PF5 ~ Burned_km2_log + ForFire1 + Burned_Last40 + SinceLastBurn_NArm + Elev + Elev_Slope +
  Aspect + TPI3 + TWI + SeasonEnd + MissingData + PostFireNBR + dNBR
# Best correlating variables by category

pf_vars3 = PF5 ~ Burned_km2_log + ForFire1 + Burned_Last40 + SinceLastBurn_NArm + Elev + TPI3 + 
  SeasonEnd + MissingData + PostFireNBR + dNBR
# Best correlating variables by category, remove bad topography vars

pf_vars3.1 = PF5 ~ Burned_km2_log + ForFire1 + Burned_Last40 + SinceLastBurn_NArm + SeasonEnd + 
  MissingData + PostFireNBR + dNBR
# Best correlating variables by category, remove all topography vars

pf_vars3.2 = PF5 ~ ForFire1 + Burned_Last40 + SinceLastBurn_NA70 + SeasonEnd + PostFireNBR + dNBR +
  MissingData
# Best correlating variables by category, remove all topography vars and extent

pf_vars3.3 = PF5 ~ ForFire1 + Burned_Last40 + SinceLastBurn_NA70 + SeasonEnd + PostFireNBR + dNBR
# Best correlating variables by category, remove all topography vars and extent, no MissingData

pf_vars = PF10 ~ Byr + postNBR + BL40 + SLB + dNBR + EucPer
# NatConPer, MinUsePer, EucPer, MalPer, Landuse, VegGroup, Species

# Variable check
####
x <- fire_tbl_pf5$postNBR # Change for each figure
y <- fire_tbl_pf5$dNBR # Change for each figure

formula1 = y ~ x

lm1 <- lm(y ~ x)
lm1_summary <- summary(lm1)

r2 <- round(lm1_summary$r.squared,2)
r2
#pval <- lm1_summary$coefficients[2,4]
#pval
plot(x, y)
####

# RandomForest
rf_pf = randomForest(pf_vars, data = fire_tbl_pf10, importance = TRUE, ntree = 500,
                       na.action = na.roughfix)  # na.omit, default mtry = vars / 3 rounded down
varImpPlot(rf_pf)
pve_pf = round(rf_pf$rsq[ntree] * 100,1) # % Variation explained: Psuedo R2
print(pve_pf)
rmse_pf = round(sqrt(rf_pf$mse[ntree]),1) # RMSE (scales based on size of response variable)
prmse_pf = round((rmse_pf/mean(fire_tbl_pf10$PF10))*100,1) # Mean PF5 is 67.42
print(prmse_pf) # %RMSE - equalized for size of response variable
imp_pf = rf_pf$importance[, "%IncMSE"]
round(imp_pf/max(imp_pf), 2)
# See Bright's code 2017 veg recovery analysis 
#####

### Create Figures ###
library(ggplot2)
library(cowplot) # plot_grid

# dNBR Output
dnbr_tbl  = read.csv("dNBR FIX/rfoutput_dnbr_fix.csv")
dnbr_main = subset(dnbr_tbl, type == "rf")
dnbr_main$vars = factor(dnbr_main$vars, levels = c("pve", "prmse"))
dnbr_mir = subset(dnbr_tbl, type == "mir")
dnbr_mir$vars = factor(dnbr_mir$vars, levels = c("BL50", "mNBR", "%NC", "BExt", "BYr",
                                                 "TPI65", "MD"))

dnbr_main_fig = ggplot(dnbr_main, aes(x = vars, y = avg)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = avg - sd, ymax = avg + sd), width = 0.5) +
  scale_y_continuous(name = '%', expand = c(0,1), breaks = seq(0,100,10)) +
  scale_x_discrete(labels = c("prmse" = "%RMSE", "pve" = "%VE")) +
  coord_cartesian(ylim = c(0,100)) +
  annotate(geom = "text", x = 1.1, y = 97.5, label = "dNBR", size = 6.5) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

dnbr_mir_fig = ggplot(dnbr_mir, aes(x = vars, y = avg)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = avg - sd, ymax = avg + sd), width = 0.5) +
  scale_y_continuous(name = 'MIR', expand = c(0,0), breaks = seq(0,1,0.1)) +
  coord_cartesian(ylim = c(0,1)) +
  #scale_x_discrete(labels = c("burned_km2_log" = "BExt", "forfire1" = "LT", "maxnbr" = "mNBR",
  #                            "sincelastburnna70" = "SLB", "burned_last50" = "BL50", "tpi65" = "TPI65",
  #                            "seasonend" = "BYr", "missingdata" = "MD")) +
  annotate(geom = "text", x = 2, y = 0.15, label = "dNBR \n %VE = 58.7 ± 1.3 \n %RMSE = 22.9 ± 0.4", 
           size = 6.5) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA)) 

#pdf("dnbr_randomforest.pdf", width = 8.5, height = 7) #16.57 # 10
#plot_grid(dnbr_main_fig, dnbr_mir_fig, 
#          nrow = 1, align = "h", rel_widths = c(1, 2.75), labels = c("A", "B"), label_size = 20) 
#dev.off()

tiff("dNBR_RF.tiff", units = "in", width = 6.5, height = 5, res = 300)
dnbr_mir_fig
dev.off()

# %RtoMV output
pf_tbl  = read.csv("rfoutput_pf1.csv")
pf_main = subset(pf_tbl, type == "rf")
pf_main$vars = factor(pf_main$vars, levels = c("pve", "prmse"))
pf_mir = subset(pf_tbl, type == "mir")
pf_mir$vars = factor(pf_mir$vars, levels = c("EucPer", "postNBR", "dNBR", "SLB", 
                                             "BL40", "BYr"))

pf_main_fig = ggplot(pf_main, aes(x = year, y = avg, group = vars)) + 
  geom_point(aes(color = vars), size = 3) +
  geom_line(aes(color = vars), size = 1) + 
  geom_errorbar(aes(ymin = avg - sd, ymax = avg + sd, color = vars), width = 0.25, size = 1) +
  scale_color_manual(values = c("black", "darkgray"), name = "%RtoMV",
                     labels = c("pve" = "%VE", "prmse" = "%RMSE")) + 
  scale_y_continuous(name = '%', expand = c(0,1), breaks = seq(0,100,10)) +
  scale_x_continuous(name = NULL, expand = c(0,0.05), breaks = seq(0,10,1)) +
  coord_cartesian(ylim = c(0,100), xlim = c(1,10)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.125, 0.85),
        plot.margin = unit(c(0.25,0.25,0,0.25), "cm")) 

pf_mir_fig = ggplot(pf_mir, aes(x = year, y = avg, group = vars)) + 
  geom_point(aes(color = vars), size = 3) +
  geom_line(aes(color = vars), size = 1) + 
  #geom_errorbar(aes(ymin = avg - sd, ymax = avg + sd, color = vars), width = 0.1, size = 1,
                #position = position_dodge(width = 0.5)) + # Too crowded
  scale_color_manual(values = c("red3", "blue3", "green3", "black", "orange3", "yellow3")) +
  scale_y_continuous(name = 'MIR', expand = c(0,0.01), breaks = seq(0,1,0.1)) +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0.05), breaks = seq(0,10,1)) +
  coord_cartesian(ylim = c(0,1), xlim = c(1,10)) +
  theme(axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = c(0.125, 0.7),
        plot.margin = unit(c(0.1,0.25,0,0.25), "cm"),
        legend.background = element_rect(fill = "transparent")) 

tiff("RtoMV_RF.tiff", units = "in", width = 6.5, height = 8, res = 300)
plot_grid(pf_main_fig, pf_mir_fig, 
          nrow = 2, align = "v", labels = c("A", "B"), label_size = 20)
dev.off()