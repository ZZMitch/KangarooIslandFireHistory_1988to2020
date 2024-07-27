### Create multi-part figure of %RtoMV RF outputs ###

### Inputs ###
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

tbl = read.csv("Fig9_tbl.csv")
tbl$postNBR = factor(tbl$postNBR, levels = c("<-200", "-200", ">0"))
tbl$dNBR = factor(tbl$dNBR, levels = c("L", "M", "H"))
tbl$LT = factor(tbl$LT, levels = c("Non-Forest", "Mixed", "Forest"))
tbl$SLB = factor(tbl$SLB, levels = c("<30", "30-50", ">50"))
tbl$BL40 = factor(tbl$BL40, levels = c("0", "<1", ">1"))
tbl$Byr = factor(tbl$Byr, levels = c("1990s", "2000s", "2010s"))

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
#####
b1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = dNBR), alpha = 0.4)  +
  stat_summary(aes(group = dNBR, color = dNBR), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("Low (1)", "Moderate (2)", "High (3)")) +
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
b2$`1v2` = c("ns", "ns", "*", "*", "ns", "ns", "ns", "ns", "*", "*") # Low vs. Moderate
b2$`2v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # Moderate vs. High
b2$`1v3` = c("ns", "ns", "*", "*", "*", "*", "*", "**", "**", "***") # Low vs. High
b2$`KW` = c("ns", "ns", "*", "ns", "ns", "ns", "ns", "*", "*", "**") # Overall KW test
b2_1 = ggtexttable(b2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
b_combined = plot_grid(b1,b2_1,nrow = 1, align = "h", rel_widths = c(3.5,1), 
                       labels = c("B"), label_size = 20)
b_combined

### C: %RtoMV vs. LT ###
#####
c1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = LT), alpha = 0.4)  +
  stat_summary(aes(group = LT, color = LT), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("Non-Forest (1)", "Mixed (2)", "Forest (3)")) +
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
        legend.position = c(0.19, 0.85),
        legend.background = element_blank())
c1

c2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(c2) = c("", "1v2", "2v3","1v3", "KW")
c2[1] = 1:10
c2$`1v2` = c("ns", "ns", "ns", "*", "**", "*", "*", "ns", "ns", "ns") # Non-Forest vs. Mixed
c2$`2v3` = c("*", "**", "***", "**", "**", "**", "*", "ns", "ns", "ns") # Mixed vs. Forest
c2$`1v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # Non-Forest vs. Forest
c2$`KW` = c("ns", "**", "**", "**", "**", "*", "ns", "ns", "ns", "ns") # Overall KW test
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

### E: %RtoMV vs.BL40 ###
#####
e1 = ggplot(data = tbl[!is.na(tbl$BL40),], aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = BL40), alpha = 0.4)  +
  stat_summary(aes(group = BL40, color = BL40), fun = mean, geom = "line", size = 2.5) + 
  scale_color_brewer(palette = "Set1", labels = c("0 (1)", "<1 (2)", ">1 (3)")) +
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
        legend.position = c(0.13, 0.85),
        legend.background = element_blank())
e1

e2 = data.frame(matrix(nrow = 10, ncol = 5))
colnames(e2) = c("", "1v2", "2v3","1v3", "KW")
e2[1] = 1:10
e2$`1v2` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # 0 vs. <1
e2$`2v3` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # <1 vs. >1
e2$`1v3` = c("ns", "*", "ns", "*", "*", "*", "*", "ns", "ns", "ns") # 0 vs. >1
e2$`KW` = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns", "ns") # Overall KW test
e2_1 = ggtexttable(e2, rows = NULL, theme = ttheme(base_size = 12, padding = unit(c(2,4), "mm")))
#####
e_combined = plot_grid(e1,e2_1,nrow = 1, align = "h", rel_widths = c(3.5,1),
                       labels = c("E"), label_size = 20)
e_combined

### F: %RtoMV vs. Byr ###
#####
f1 = ggplot(data = tbl, aes(x = yr, y = rtomv, group = ID)) +
  geom_line(aes(color = Byr), alpha = 0.4)  +
  stat_summary(aes(group = Byr, color = Byr), fun = mean, geom = "line", size = 2.5) + 
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
pdf("Figure9.pdf", width = 16.5, height = 16.5)
plot_grid(a_combined, b_combined, c_combined, d_combined, e_combined, f_combined, ncol = 2)
dev.off()