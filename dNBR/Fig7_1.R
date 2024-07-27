### Create multi-part figure of dNBR RF outputs ###

### Inputs ###
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# By Fire
tbl = read.csv("dNBR FIX/Fig7_tbl_fix.csv")
tbl$mNBR_grp = factor(tbl$mNBR_grp, levels = c("<450", "450-600", ">600"))
tbl$BL50_grp = factor(tbl$BL50_grp, levels = c("0", "<1", ">1"))
tbl$BExt_grp = factor(tbl$BExt_grp, levels = c("S", "M", "L"))
tbl$LT = factor(tbl$LT, levels = c("0", "0.5", "1"))
tbl$SLB_grp = factor(tbl$SLB_grp, levels = c("<30", "30-50", ">50"))
tbl$Byr_grp = factor(tbl$Byr_grp, levels = c("1990s", "2000s", "2010s"))
tbl$MD = factor(tbl$MD, levels = c("0", "1"))

# All Pixels (LT and MD only)
tbl_ap = read.csv("dNBR FIX/Allpixels_dnbr_fix.csv")
tbl_ap$LT = factor(tbl_ap$LT, levels = c("Non-Forest", "Mixed", "Forest"))
#tbl_ap$MissingData = factor(tbl_ap$MissingData, levels = c("0", "1"))

### A: dNBR vs. mNBR ###
#####
# A1
a1 = ggplot(data = tbl, aes(x = mNBR, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = mNBR_grp), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  geom_smooth(method = lm, color = "black") +
  scale_x_continuous(name = 'mNBR', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR', expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = 205, y = 950, label = expression(italic(R^2) * "= 0.36****"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
a1

# A2
a2c = list(c("<450","450-600"), c("450-600",">600"))
a2f = list(c("<450",">600"))

a2 = ggplot(tbl, aes(x = mNBR_grp, y = dNBR)) +
  geom_boxplot(aes(fill = mNBR_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = a2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = a2f, label = "p.signif", 
                     label.y = max(tbl$dNBR) + 100, tip.length = 0.015) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = min(tbl$dNBR) - 100, label.x = 1.875, size = 12, label = "p.signif") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
a2
#####
a_combined = plot_grid(a1, a2, nrow = 1, align = "h", rel_widths = c(3, 1), 
                       labels = c("A"), label_size = 20)
a_combined

### B: dNBR vs. BL50 ###
#####
# B1
b1 = ggplot(data = tbl, aes(x = BL50, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = BL50_grp), show.legend = FALSE) +
  geom_smooth(method = lm, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(name = 'BL50', expand = c(0.01,0)) +
  scale_y_continuous(name = "", expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = 0.5, y = 950, label = expression(italic(R^2) * "= 0.30***"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
b1

# B2
b2c = list(c("0","<1"), c("<1",">1"))
b2f = list(c("0",">1"))

b2 = ggplot(tbl[!is.na(tbl$BL50_grp),], aes(x = BL50_grp, y = dNBR)) + # Some  fires are NA
  geom_boxplot(aes(fill = BL50_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = b2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = b2f, label = "p.signif", 
                     label.y = max(tbl$dNBR) + 100, tip.length = 0.015) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = min(tbl$dNBR) - 100, label.x = 1.75, size = 12, label = "p.signif") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
b2
#####
b_combined = plot_grid(b1, b2, nrow = 1, align = "h", rel_widths = c(3, 1),
                       labels = c("B"), label_size = 20)  
b_combined

### C: dNBR vs. BExt (ln) ###
#####
# C1
c1 = ggplot(data = tbl, aes(x = BExt, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = BExt_grp), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  geom_smooth(method = lm, color = "black") +
  scale_x_continuous(name = 'BExt', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR', expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = -0.25, y = 950, label = expression(italic(R^2) * "= 0.35****"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
c1

# C2
#c2c = list(c("XS","S"), c("S","M"), c("M","L"))
#c2m1 = list(c("XS","M"))
#c2m2 = list(c("S","L"))
#c2f = list(c("XS","L"))
c2c = list(c("S","M"), c("M","L"))
c2f = list(c("S","L"))

c2 = ggplot(tbl, aes(x = BExt_grp, y = dNBR)) +
  geom_boxplot(aes(fill = BExt_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
#  stat_compare_means(method = "wilcox.test", comparisons = c2c, label = "p.signif", size = 5.5, 
#                     label.y = max(tbl$dNBR) - 80, tip.length = 0.01) +
#  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = c2m1, label = "p.signif", 
#                     label.y = max(tbl$dNBR) - 20, tip.length = 0.01) +
#  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = c2m2, label = "p.signif", 
#                     label.y = max(tbl$dNBR) + 40, tip.length = 0.01) +
#  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = c2f, label = "p.signif", 
#                     label.y = max(tbl$dNBR) + 100, tip.length = 0.01) +
#  stat_compare_means(method = "kruskal.test", 
#                     label.y = min(tbl$dNBR) - 80, label.x = 2.25, size = 12, label = "p.signif") +
  stat_compare_means(method = "wilcox.test", comparisons = c2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = c2f, label = "p.signif", 
                     label.y = max(tbl$dNBR) + 100, tip.length = 0.015) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = min(tbl$dNBR) - 100, label.x = 1.875, size = 12, label = "p.signif") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
c2
#####
c_combined = plot_grid(c1, c2, nrow = 1, align = "h", rel_widths = c(3, 1),
                       labels = c("C"), label_size = 20)  
c_combined

### D: dNBR vs. LT ###
#####
# D1
d1 = ggplot(tbl_ap, aes(x = reorder(ID, ForestPer, FUN = median), y = dNBR)) +
  geom_boxplot(aes(fill = LT), color = "black", show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("")  + xlab("LT") +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0, 20)) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
d1

# D2
d2c = list(c("0","0.5"), c("0.5","1"))
d2f = list(c("0","1"))

d2 = ggplot(tbl, aes(x = LT, y = dNBR)) +
  geom_boxplot(aes(fill = LT), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = d2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 80) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = d2f, label = "p.signif", 
                     label.y = max(tbl$dNBR) + 160) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = min(tbl$dNBR) - 120, label.x = 1.75, size = 12, label = "p.signif") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(100, 1300)) +
  scale_y_continuous(expand = c(0, 20)) +
  scale_x_discrete(labels = c("0" = "NF", "0.5" = "M", "1" = "F")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
d2
#####
d_combined = plot_grid(d1, d2, nrow = 1, align = "h", rel_widths = c(3, 1),
                       labels = c("D"), label_size = 20)  
d_combined

### E: dNBR vs. SLB (NA70) ###
#####
# E1
e1 = ggplot(data = tbl, aes(x = SLB, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = SLB_grp), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  geom_smooth(method = lm, color = "black") +
  scale_x_continuous(name = 'SLB', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR', expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = 14, y = 950, label = expression(italic(R^2) * "= 0.15**"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
e1

# E2
e2c = list(c("<30","30-50"), c("30-50",">50"))
e2f = list(c("<30",">50"))

e2 = ggplot(tbl, aes(x = SLB_grp, y = dNBR)) + 
  geom_boxplot(aes(fill = SLB_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = e2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = e2f, label = "p.signif", 
                     label.y = max(tbl$dNBR) + 100, tip.length = 0.015) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = min(tbl$dNBR) - 100, label.x = 1.875, size = 12, label = "p.signif") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
e2
#####
e_combined = plot_grid(e1, e2, nrow = 1, align = "h", rel_widths = c(3, 1),
                       labels = c("E"), label_size = 20)  
e_combined

### F: dNBR vs. BYr ###
#####
# F1
f1 = ggplot(data = tbl, aes(x = Byr, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = Byr_grp), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  geom_smooth(method = lm, color = "black") +
  scale_x_continuous(name = 'Byr', expand = c(0.01,0)) +
  scale_y_continuous(name = '', expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000), xlim = c(1988,2021)) +
  annotate(geom = "text", x = 1992, y = 950, label = expression(italic(R^2) * "= 0.02"^"ns"), 
           size = 6) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
f1

# F2
f2c = list(c("1990s","2000s"), c("2000s","2010s"))
f2f = list(c("1990s","2010s"))

f2 = ggplot(tbl, aes(x = Byr_grp, y = dNBR)) + 
  geom_boxplot(aes(fill = Byr_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = f2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = f2f, label = "p.signif", 
                     label.y = max(tbl$dNBR) + 100, tip.length = 0.015) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = min(tbl$dNBR) - 75, label.x = 1.875, size = 7, label = "p.signif") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
f2
#####
f_combined = plot_grid(f1, f2, nrow = 1, align = "h", rel_widths = c(3, 1),
                       labels = c("F"), label_size = 20)  
f_combined

### G: dNBR vs. TPI65 ### - Move G1 to Supplemental Figures
#####
# G1
#g1 = ggplot(data = tbl, aes(x = TPI65, y = dNBR)) +
#  geom_point(size = 2) +
#  geom_smooth(method = lm, color = "black") +
#  scale_x_continuous(name = 'TPI65 (m)', expand = c(0.01,0)) +
#  scale_y_continuous(name = 'dNBR (x1000)', expand = c(0.01,0)) +
#  #coord_cartesian(ylim = c(150, 950)) +
#  #coord_cartesian(xlim = c(1989, 2008)) + # Adjusts visible range for x axis manually
#  annotate(geom = "text", x = -5, y = 825, label = expression(italic(R^2) * "= 0.02"^"ns"), 
#           size = 6) +
#  theme(axis.title.x = element_text(size = 18),
#        axis.text.x = element_text(size = 14),
#        axis.title.y = element_text(size = 18),
#        axis.text.y = element_text(size = 14),
#        panel.background = element_blank(),
#        panel.border = element_rect(color = 'black', fill = NA))
#g1

# G2 - Not Needed

#####

### H: dNBR vs. MD ### - Move H1 to Supplemental Figures
#####
# H1
#h1 = ggplot(tbl_ap, aes(x = reorder(ID, SeasonEnd), y = dNBR)) +
#  geom_boxplot(aes(fill = MissingData), color = "black", show.legend = FALSE) +
#  scale_fill_manual(values = c("white", "red3")) +
#  stat_summary(fun = mean, geom = "point", shape = 1) +
#  ylab("dNBR (x1000)")  +
#  coord_cartesian(ylim = c(100, 1300)) + 
#  scale_y_continuous(expand = c(0, 20)) +
#  theme(axis.title.x = element_blank(),
#        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
#        axis.title.y = element_text(size = 18),
#        axis.text.y = element_text(size = 14),
#        panel.background = element_blank(),
#        panel.border = element_rect(color = 'black', fill = NA))
#h1

# H2 - Not needed
#h2f = list(c("0","1"))

#h2 = ggplot(tbl, aes(x = MD, y = dNBR)) + 
#  geom_boxplot(aes(fill = MD), color = "black", show.legend = FALSE) +
#  geom_jitter(width = 0) +
#  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = h2f, label = "p.signif", 
#                     label.y = max(tbl$dNBR) + 80) +
#  stat_compare_means(method = "kruskal.test", 
#                     label.y = max(tbl$dNBR) + 200, label.x = 1, size = 4) +
#  scale_fill_manual(values = c("white", "red3")) +
#  coord_cartesian(ylim = c(100, 1800)) +
#  scale_y_continuous(expand = c(0, 20)) +
#  theme(axis.title.x = element_blank(),
#        axis.text.x = element_text(size = 14),
#        axis.title.y = element_blank(),
#        axis.text.y = element_blank(),
#        panel.background = element_blank(),
#        panel.border = element_rect(color = 'black', fill = NA))
#h2

#####

### Full Combined Plot ###
pdf("Figure7.pdf", width = 16.5, height = 16.5)
plot_grid(a_combined, b_combined, c_combined, d_combined, e_combined, f_combined, ncol = 2, align = "hv")
dev.off()