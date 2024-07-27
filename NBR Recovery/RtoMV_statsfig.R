### Create multi-part figure of %RtoMV RF outputs ###

### Inputs ###
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

tbl = read.csv("Fig9_tbl1.csv")
tbl$postNBR = factor(tbl$postNBR, levels = c("<-200", "-200", ">0"))
tbl$dNBR = factor(tbl$dNBR, levels = c("L", "M", "H"))
tbl$BL40 = factor(tbl$BL40, levels = c("0", "<1", ">1"))
tbl$SLB = factor(tbl$SLB, levels = c("<30", "30-50", ">50"))
tbl$EucPer = factor(tbl$EucPer, levels = c("<10", "10-40", ">40"))
tbl$BYr = factor(tbl$BYr, levels = c("1990s", "2000s", "2010s"))

### A: %RtoMV vs. postNBR ###
#####
a1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = postNBR), alpha = 0.4)  +
  stat_summary(aes(group = postNBR, color = postNBR), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("<-200 (1)", "-200 - 0 (2)", ">0 (3)")) +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(0,10,1)) +
  scale_y_continuous(name = '%RtoMV', limits = c(0,100), expand = c(0,0),
                     breaks = seq(0,100,10)) +
  coord_cartesian(xlim = c(0,10)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.175, 0.85),
        legend.background = element_blank())
a1

a2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(a2) = c("", "1v2", "2v3","1v3", "KW")
a2[1] = 1:10
a2$`1v2` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # <-200 vs. -200 - 0
a2$`2v3` = c("**", "**", "*", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # -200 - 0 vs. >0
a2$`1v3` = c("***", "**", "*", "*", "ns", "ns", "*", "*", "*", "ns") # <-200 vs. >0
a2$`KW` = c("***", "**", "*", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # Overall KW test
a2_1 = ggtexttable(a2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
a_combined = plot_grid(a1,a2_1,nrow = 1, align = "h", rel_widths = c(3.5,1),
                       labels = c("A"), label_size = 20)
a_combined

### B: %RtoMV vs. dNBR ###
### New stats for dNBR ###
#####
Yr1 = subset(tbl, tbl$yr == 1)
Yr1_L = subset(tbl$rtomv, tbl$yr == 1 & tbl$dNBR == "L")
Yr1_M = subset(tbl$rtomv, tbl$yr == 1 & tbl$dNBR == "M")
Yr1_H = subset(tbl$rtomv, tbl$yr == 1 & tbl$dNBR == "H")

Yr2 = subset(tbl, tbl$yr == 2)
Yr2_L = subset(tbl$rtomv, tbl$yr == 2 & tbl$dNBR == "L")
Yr2_M = subset(tbl$rtomv, tbl$yr == 2 & tbl$dNBR == "M")
Yr2_H = subset(tbl$rtomv, tbl$yr == 2 & tbl$dNBR == "H")

Yr3 = subset(tbl, tbl$yr == 3)
Yr3_L = subset(tbl$rtomv, tbl$yr == 3 & tbl$dNBR == "L")
Yr3_M = subset(tbl$rtomv, tbl$yr == 3 & tbl$dNBR == "M")
Yr3_H = subset(tbl$rtomv, tbl$yr == 3 & tbl$dNBR == "H")

Yr4 = subset(tbl, tbl$yr == 4)
Yr4_L = subset(tbl$rtomv, tbl$yr == 4 & tbl$dNBR == "L")
Yr4_M = subset(tbl$rtomv, tbl$yr == 4 & tbl$dNBR == "M")
Yr4_H = subset(tbl$rtomv, tbl$yr == 4 & tbl$dNBR == "H")

Yr5 = subset(tbl, tbl$yr == 5)
Yr5_L = subset(tbl$rtomv, tbl$yr == 5 & tbl$dNBR == "L")
Yr5_M = subset(tbl$rtomv, tbl$yr == 5 & tbl$dNBR == "M")
Yr5_H = subset(tbl$rtomv, tbl$yr == 5 & tbl$dNBR == "H")

Yr6 = subset(tbl, tbl$yr == 6)
Yr6_L = subset(tbl$rtomv, tbl$yr == 6 & tbl$dNBR == "L")
Yr6_M = subset(tbl$rtomv, tbl$yr == 6 & tbl$dNBR == "M")
Yr6_H = subset(tbl$rtomv, tbl$yr == 6 & tbl$dNBR == "H")

Yr7 = subset(tbl, tbl$yr == 7)
Yr7_L = subset(tbl$rtomv, tbl$yr == 7 & tbl$dNBR == "L")
Yr7_M = subset(tbl$rtomv, tbl$yr == 7 & tbl$dNBR == "M")
Yr7_H = subset(tbl$rtomv, tbl$yr == 7 & tbl$dNBR == "H")

Yr8 = subset(tbl, tbl$yr == 8)
Yr8_L = subset(tbl$rtomv, tbl$yr == 8 & tbl$dNBR == "L")
Yr8_M = subset(tbl$rtomv, tbl$yr == 8 & tbl$dNBR == "M")
Yr8_H = subset(tbl$rtomv, tbl$yr == 8 & tbl$dNBR == "H")

Yr9 = subset(tbl, tbl$yr == 9)
Yr9_L = subset(tbl$rtomv, tbl$yr == 9 & tbl$dNBR == "L")
Yr9_M = subset(tbl$rtomv, tbl$yr == 9 & tbl$dNBR == "M")
Yr9_H = subset(tbl$rtomv, tbl$yr == 9 & tbl$dNBR == "H")

Yr10 = subset(tbl, tbl$yr == 10)
Yr10_L = subset(tbl$rtomv, tbl$yr == 10 & tbl$dNBR == "L")
Yr10_M = subset(tbl$rtomv, tbl$yr == 10 & tbl$dNBR == "M")
Yr10_H = subset(tbl$rtomv, tbl$yr == 10 & tbl$dNBR == "H")

wilcox.test(Yr1_L, Yr1_M) # ns 0.15
wilcox.test(Yr2_L, Yr2_M) # ns 0.25
wilcox.test(Yr3_L, Yr3_M) # ** 0.006
wilcox.test(Yr4_L, Yr4_M) # * 0.02
wilcox.test(Yr5_L, Yr5_M) # * 0.02
wilcox.test(Yr6_L, Yr6_M) # * 0.02
wilcox.test(Yr7_L, Yr7_M) # ns 0.07
wilcox.test(Yr8_L, Yr8_M) # * 0.02
wilcox.test(Yr9_L, Yr9_M) # * 0.01
wilcox.test(Yr10_L, Yr10_M) # ns 0.06

wilcox.test(Yr1_M, Yr1_H) # ns 0.88
wilcox.test(Yr2_M, Yr2_H) # ns 0.41
wilcox.test(Yr3_M, Yr3_H) # ns 0.37
wilcox.test(Yr4_M, Yr4_H) # ns 0.51
wilcox.test(Yr5_M, Yr5_H) # ns 0.56
wilcox.test(Yr6_M, Yr6_H) # ns 0.63
wilcox.test(Yr7_M, Yr7_H) # ns 1
wilcox.test(Yr8_M, Yr8_H) # ns 1
wilcox.test(Yr9_M, Yr9_H) # ns 0.97
wilcox.test(Yr10_M, Yr10_H) # ns 0.55

wilcox.test(Yr1_L, Yr1_H) # ns 0.13
wilcox.test(Yr2_L, Yr2_H) # ns 0.15
wilcox.test(Yr3_L, Yr3_H) # * 0.04
wilcox.test(Yr4_L, Yr4_H) # * 0.03
wilcox.test(Yr5_L, Yr5_H) # * 0.02
wilcox.test(Yr6_L, Yr6_H) # * 0.03
wilcox.test(Yr7_L, Yr7_H) # * 0.02
wilcox.test(Yr8_L, Yr8_H) # ** 0.007
wilcox.test(Yr9_L, Yr9_H) # ** 0.004
wilcox.test(Yr10_L, Yr10_H) # ** 0.003

kruskal.test(rtomv ~ dNBR, data = Yr1) # ns 0.21
kruskal.test(rtomv ~ dNBR, data = Yr2) # ns 0.28
kruskal.test(rtomv ~ dNBR, data = Yr3) # * 0.01
kruskal.test(rtomv ~ dNBR, data = Yr4) # * 0.03
kruskal.test(rtomv ~ dNBR, data = Yr5) # * 0.03
kruskal.test(rtomv ~ dNBR, data = Yr6) # * 0.03
kruskal.test(rtomv ~ dNBR, data = Yr7) # * 0.05
kruskal.test(rtomv ~ dNBR, data = Yr8) # * 0.02
kruskal.test(rtomv ~ dNBR, data = Yr9) # ** 0.009
kruskal.test(rtomv ~ dNBR, data = Yr10) # * 0.02
#####

#####
b1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = dNBR), alpha = 0.4)  +
  stat_summary(aes(group = dNBR, color = dNBR), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("<400 (1)", "400 - 600 (2)", ">600 (3)")) +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(0,10,1)) +
  scale_y_continuous(name = '', limits = c(0,100), expand = c(0,0),
                     breaks = seq(0,100,10)) +
  coord_cartesian(xlim = c(0,10)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.175, 0.85),
        legend.background = element_blank())
b1

b2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(b2) = c("", "1v2", "2v3","1v3", "KW")
b2[1] = 1:10
b2$`1v2` = c("ns", "ns", "**", "*", "*", "*", "ns", "*", "*", "ns") # Low vs. Moderate
b2$`2v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # Moderate vs. High
b2$`1v3` = c("ns", "ns", "*", "*", "*", "*", "*", "**", "**", "**") # Low vs. High
b2$`KW` = c("ns", "ns", "*", "*", "*", "*", "*", "*", "**", "*") # Overall KW test
b2_1 = ggtexttable(b2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
b_combined = plot_grid(b1,b2_1,nrow = 1, align = "h", rel_widths = c(3.5,1), 
                       labels = c("B"), label_size = 20)
b_combined

### C: %RtoMV vs.BL40 ###
#####
c1 = ggplot(data = tbl[!is.na(tbl$BL40),], aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = BL40), alpha = 0.4)  +
  stat_summary(aes(group = BL40, color = BL40), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("0 (1)", "<1 (2)", ">1 (3)")) +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(0,10,1)) +
  scale_y_continuous(name = '%RtoMV', limits = c(0,100), expand = c(0,0),
                     breaks = seq(0,100,10)) +
  coord_cartesian(xlim = c(0,10)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.13, 0.85),
        legend.background = element_blank())
c1

c2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(c2) = c("", "1v2", "2v3","1v3", "KW")
c2[1] = 1:10
c2$`1v2` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # 0 vs. <1
c2$`2v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # <1 vs. >1
c2$`1v3` = c("ns", "*", "ns", "*", "*", "*", "*", "ns", "ns", "ns") # 0 vs. >1
c2$`KW` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # Overall KW test
c2_1 = ggtexttable(c2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
c_combined = plot_grid(c1,c2_1,nrow = 1, align = "h", rel_widths = c(3.5,1),
                       labels = c("C"), label_size = 20)
c_combined

### D: %RtoMV vs. SLB ###
#####
d1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = SLB), alpha = 0.4)  +
  stat_summary(aes(group = SLB, color = SLB), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("<30 (1)", "30 - 50 (2)", ">50 (3)")) +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(0,10,1)) +
  scale_y_continuous(name = '', limits = c(0,100), expand = c(0,0),
                     breaks = seq(0,100,10)) +
  coord_cartesian(xlim = c(0,10)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.15, 0.85),
        legend.background = element_blank())
d1

d2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(d2) = c("", "1v2", "2v3","1v3", "KW")
d2[1] = 1:10
d2$`1v2` = c("ns", "ns", "ns", "*", "ns", "ns", "ns", "ns", "ns", "ns") # <30 vs. 30-50
d2$`2v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # 30-50 vs. >50
d2$`1v3` = c("ns", "ns", "ns", "*", "*", "*", "*", "ns", "ns", "ns") # <30 vs. >50
d2$`KW` = c("ns", "ns", "ns", "*", "*", "*", "ns", "ns", "ns", "ns") # Overall KW test
d2_1 = ggtexttable(d2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
d_combined = plot_grid(d1,d2_1,nrow = 1, align = "h", rel_widths = c(3.5,1), 
                       labels = c("D"), label_size = 20)
d_combined

### E: %RtoMV vs. EucPer
### New stats for EucPer ###
#####
Yr1 = subset(tbl, tbl$yr == 1)
Yr1_L = subset(tbl$rtomv, tbl$yr == 1 & tbl$EucPer == "<10")
Yr1_M = subset(tbl$rtomv, tbl$yr == 1 & tbl$EucPer == "10-40")
Yr1_H = subset(tbl$rtomv, tbl$yr == 1 & tbl$EucPer == ">40")

Yr2 = subset(tbl, tbl$yr == 2)
Yr2_L = subset(tbl$rtomv, tbl$yr == 2 & tbl$EucPer == "<10")
Yr2_M = subset(tbl$rtomv, tbl$yr == 2 & tbl$EucPer == "10-40")
Yr2_H = subset(tbl$rtomv, tbl$yr == 2 & tbl$EucPer == ">40")

Yr3 = subset(tbl, tbl$yr == 3)
Yr3_L = subset(tbl$rtomv, tbl$yr == 3 & tbl$EucPer == "<10")
Yr3_M = subset(tbl$rtomv, tbl$yr == 3 & tbl$EucPer == "10-40")
Yr3_H = subset(tbl$rtomv, tbl$yr == 3 & tbl$EucPer == ">40")

Yr4 = subset(tbl, tbl$yr == 4)
Yr4_L = subset(tbl$rtomv, tbl$yr == 4 & tbl$EucPer == "<10")
Yr4_M = subset(tbl$rtomv, tbl$yr == 4 & tbl$EucPer == "10-40")
Yr4_H = subset(tbl$rtomv, tbl$yr == 4 & tbl$EucPer == ">40")

Yr5 = subset(tbl, tbl$yr == 5)
Yr5_L = subset(tbl$rtomv, tbl$yr == 5 & tbl$EucPer == "<10")
Yr5_M = subset(tbl$rtomv, tbl$yr == 5 & tbl$EucPer == "10-40")
Yr5_H = subset(tbl$rtomv, tbl$yr == 5 & tbl$EucPer == ">40")

Yr6 = subset(tbl, tbl$yr == 6)
Yr6_L = subset(tbl$rtomv, tbl$yr == 6 & tbl$EucPer == "<10")
Yr6_M = subset(tbl$rtomv, tbl$yr == 6 & tbl$EucPer == "10-40")
Yr6_H = subset(tbl$rtomv, tbl$yr == 6 & tbl$EucPer == ">40")

Yr7 = subset(tbl, tbl$yr == 7)
Yr7_L = subset(tbl$rtomv, tbl$yr == 7 & tbl$EucPer == "<10")
Yr7_M = subset(tbl$rtomv, tbl$yr == 7 & tbl$EucPer == "10-40")
Yr7_H = subset(tbl$rtomv, tbl$yr == 7 & tbl$EucPer == ">40")

Yr8 = subset(tbl, tbl$yr == 8)
Yr8_L = subset(tbl$rtomv, tbl$yr == 8 & tbl$EucPer == "<10")
Yr8_M = subset(tbl$rtomv, tbl$yr == 8 & tbl$EucPer == "10-40")
Yr8_H = subset(tbl$rtomv, tbl$yr == 8 & tbl$EucPer == ">40")

Yr9 = subset(tbl, tbl$yr == 9)
Yr9_L = subset(tbl$rtomv, tbl$yr == 9 & tbl$EucPer == "<10")
Yr9_M = subset(tbl$rtomv, tbl$yr == 9 & tbl$EucPer == "10-40")
Yr9_H = subset(tbl$rtomv, tbl$yr == 9 & tbl$EucPer == ">40")

Yr10 = subset(tbl, tbl$yr == 10)
Yr10_L = subset(tbl$rtomv, tbl$yr == 10 & tbl$EucPer == "<10")
Yr10_M = subset(tbl$rtomv, tbl$yr == 10 & tbl$EucPer == "10-40")
Yr10_H = subset(tbl$rtomv, tbl$yr == 10 & tbl$EucPer == ">40")

wilcox.test(Yr1_L, Yr1_M) # ns 0.28
wilcox.test(Yr2_L, Yr2_M) # ns 0.35
wilcox.test(Yr3_L, Yr3_M) # ns 0.13
wilcox.test(Yr4_L, Yr4_M) # * 0.03
wilcox.test(Yr5_L, Yr5_M) # ns 0.07
wilcox.test(Yr6_L, Yr6_M) # ns 0.13
wilcox.test(Yr7_L, Yr7_M) # ns 0.25
wilcox.test(Yr8_L, Yr8_M) # ns 0.36
wilcox.test(Yr9_L, Yr9_M) # ns 0.32
wilcox.test(Yr10_L, Yr10_M) # ns 0.19

wilcox.test(Yr1_M, Yr1_H) # ns 0.61
wilcox.test(Yr2_M, Yr2_H) # ns 0.73
wilcox.test(Yr3_M, Yr3_H) # ns 0.15
wilcox.test(Yr4_M, Yr4_H) # ns 0.15
wilcox.test(Yr5_M, Yr5_H) # ns 0.07
wilcox.test(Yr6_M, Yr6_H) # ns 0.15
wilcox.test(Yr7_M, Yr7_H) # ns 0.08
wilcox.test(Yr8_M, Yr8_H) # ns 0.12
wilcox.test(Yr9_M, Yr9_H) # ns 0.17
wilcox.test(Yr10_M, Yr10_H) # ns 0.34

wilcox.test(Yr1_L, Yr1_H) # ns 0.36
wilcox.test(Yr2_L, Yr2_H) # ns 0.08
wilcox.test(Yr3_L, Yr3_H) # ** 0.003
wilcox.test(Yr4_L, Yr4_H) # **** 7.772e-05
wilcox.test(Yr5_L, Yr5_H) # *** 0.0009
wilcox.test(Yr6_L, Yr6_H) # ** 0.005
wilcox.test(Yr7_L, Yr7_H) # ** 0.005
wilcox.test(Yr8_L, Yr8_H) # ** 0.006
wilcox.test(Yr9_L, Yr9_H) # ** 0.007
wilcox.test(Yr10_L, Yr10_H) # ** 0.004

kruskal.test(rtomv ~ EucPer, data = Yr1) # ns 0.44
kruskal.test(rtomv ~ EucPer, data = Yr2) # ns 0.23
kruskal.test(rtomv ~ EucPer, data = Yr3) # * 0.01
kruskal.test(rtomv ~ EucPer, data = Yr4) # ** 0.001
kruskal.test(rtomv ~ EucPer, data = Yr5) # ** 0.004
kruskal.test(rtomv ~ EucPer, data = Yr6) # * 0.02
kruskal.test(rtomv ~ EucPer, data = Yr7) # * 0.01
kruskal.test(rtomv ~ EucPer, data = Yr8) # * 0.02
kruskal.test(rtomv ~ EucPer, data = Yr9) # * 0.03
kruskal.test(rtomv ~ EucPer, data = Yr10) # * 0.03
#####

#####
e1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = EucPer), alpha = 0.4)  +
  stat_summary(aes(group = EucPer, color = EucPer), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("<10 (1)", "10 - 40 (2)", ">40 (3)")) +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0), breaks = seq(0,10,1)) +
  scale_y_continuous(name = '%RtoMV', limits = c(0,100), expand = c(0,0),
                     breaks = seq(0,100,10)) +
  coord_cartesian(xlim = c(0,10)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.15, 0.85),
        legend.background = element_blank())
e1

e2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(e2) = c("", "1v2", "2v3","1v3", "KW")
e2[1] = 1:10
e2$`1v2` = c("ns", "ns", "ns", "*", "ns", "ns", "ns", "ns", "ns", "ns") # <10 vs. 10-40
e2$`2v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # 10-40 vs. >40
e2$`1v3` = c("ns", "ns", "**", "****", "***", "**", "**", "**", "**", "**") # <10 vs. >40
e2$`KW` = c("ns", "ns", "*", "**", "**", "*", "*", "*", "*", "*") # Overall KW test
e2_1 = ggtexttable(e2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
e_combined = plot_grid(e1,e2_1,nrow = 1, align = "h", rel_widths = c(3.5,1), 
                       labels = c("E"), label_size = 20)
e_combined

### F: %RtoMV vs. Byr ###
#####
f1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = BYr), alpha = 0.4)  +
  stat_summary(aes(group = BYr, color = BYr), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("1990s (1)", "2000s (2)", "2010s (3)")) +
  scale_x_continuous(name = 'Years Post Fire', expand = c(0,0), breaks = seq(0,10,1)) +
  scale_y_continuous(name = '', limits = c(0,100), expand = c(0,0),
                     breaks = seq(0,100,10)) +
  coord_cartesian(xlim = c(0,10)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.position = c(0.15, 0.85),
        legend.background = element_blank())
f1

f2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(f2) = c("", "1v2", "2v3","1v3", "KW")
f2[1] = 1:10
f2$`1v2` = c("ns", "ns", "*", "**", "***", "***", "****", "****", "****", "****") # 1990s vs 2000s
f2$`2v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "") # 2000s vs 2010s
f2$`1v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "*", "*", "") # 1990s vs 2010s
f2$`KW` = c("ns", "ns", "ns", "*", "**", "**", "***", "****", "****", "****") # Overall KW test
f2_1 = ggtexttable(f2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
f_combined = plot_grid(f1,f2_1,nrow = 1, align = "h", rel_widths = c(3.5,1), 
                       labels = c("F"), label_size = 20)
f_combined

### Full Combined Plot ###
tiff("RtoMV_RF_stats.tiff", units = "in", width = 16.5, height = 16.5, res = 300)
plot_grid(a_combined, b_combined, c_combined, d_combined, e_combined, f_combined, ncol = 2)
dev.off()