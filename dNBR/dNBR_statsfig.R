### Create multi-part figure of dNBR RF outputs ###

### Inputs ###
library(ggplot2)
library(ggpubr)
library(cowplot)

setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

# By Fire
tbl = read.csv("dNBR FIX/Fig7_tbl_fix1.csv")
tbl$BL50_grp = factor(tbl$BL50_grp, levels = c("0", "<1", ">1"))
tbl$mNBR_grp = factor(tbl$mNBR_grp, levels = c("<450", "450-600", ">600"))
tbl$NC_grp = factor(tbl$NC_grp, levels = c("<5", "5-75", ">75"))
tbl$BExt_grp = factor(tbl$BExt_grp, levels = c("S", "M", "L"))

### A: dNBR vs. BL50 ###
#####
# A1
a1 = ggplot(data = tbl, aes(x = BL50, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = BL50_grp), show.legend = FALSE) +
  geom_smooth(method = lm, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(name = 'BL50', expand = c(0.01,0)) +
  scale_y_continuous(name = "dNBR", expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = 0.5, y = 950, label = expression(italic(R^2) * "= 0.30***"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
a1

# A2
a2c = list(c("0","<1"), c("<1",">1"))
a2f = list(c("0",">1"))

a2 = ggplot(tbl[!is.na(tbl$BL50_grp),], aes(x = BL50_grp, y = dNBR)) + # Some  fires are NA
  geom_boxplot(aes(fill = BL50_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = a2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = a2f, label = "p.signif", 
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
a2
#####
a_combined = plot_grid(a1, a2, nrow = 1, align = "h", rel_widths = c(3, 1),
                       labels = c("A"), label_size = 20)  
a_combined

### B: dNBR vs. mNBR ###
#####
# B1
b1 = ggplot(data = tbl, aes(x = mNBR, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = mNBR_grp), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  geom_smooth(method = lm, color = "black") +
  scale_x_continuous(name = 'mNBR', expand = c(0.01,0)) +
  scale_y_continuous(name = '', expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = 205, y = 950, label = expression(italic(R^2) * "= 0.36****"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
b1

# B2
b2c = list(c("<450","450-600"), c("450-600",">600"))
b2f = list(c("<450",">600"))

b2 = ggplot(tbl, aes(x = mNBR_grp, y = dNBR)) +
  geom_boxplot(aes(fill = mNBR_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = b2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = b2f, label = "p.signif", 
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
b2
#####
b_combined = plot_grid(b1, b2, nrow = 1, align = "h", rel_widths = c(3, 1), 
                       labels = c("B"), label_size = 20)
b_combined

### C: dNBR vs. %NC ###
#####
# C1
c1 = ggplot(data = tbl, aes(x = NC, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = NC_grp), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  geom_smooth(method = lm, color = "black") +
  scale_x_continuous(name = '%NC', expand = c(0.01,0)) +
  scale_y_continuous(name = 'dNBR', expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = 13, y = 950, label = expression(italic(R^2) * "= 0.23***"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
c1


# C2
c2c = list(c("<5","5-75"), c("5-75",">75"))
c2f = list(c("<5",">75"))

c2 = ggplot(tbl, aes(x = NC_grp, y = dNBR)) +
  geom_boxplot(aes(fill = NC_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
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
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
c2
#####
c_combined = plot_grid(c1, c2, nrow = 1, align = "h", rel_widths = c(3, 1), 
                       labels = c("C"), label_size = 20)
c_combined

### D: dNBR vs. BExt (ln) ###
#####
# D1
d1 = ggplot(data = tbl, aes(x = BExt, y = dNBR)) +
  geom_point(size = 4, shape = 21, aes(fill = BExt_grp), show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  geom_smooth(method = lm, color = "black") +
  scale_x_continuous(name = 'BExt', expand = c(0.01,0)) +
  scale_y_continuous(name = '', expand = c(0,0), breaks = seq(100,1000,100)) +
  coord_cartesian(ylim = c(100, 1000)) +
  annotate(geom = "text", x = -0.25, y = 950, label = expression(italic(R^2) * "= 0.35****"), 
           size = 6) +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
d1

# D2
d2c = list(c("S","M"), c("M","L"))
d2f = list(c("S","L"))

d2 = ggplot(tbl, aes(x = BExt_grp, y = dNBR)) +
  geom_boxplot(aes(fill = BExt_grp), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", comparisons = d2c, label = "p.signif", size = 5.5, 
                     label.y = max(tbl$dNBR) + 40, tip.length = 0.015) +
  stat_compare_means(method = "wilcox.test", size = 5.5, comparisons = d2f, label = "p.signif", 
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
d2
#####
d_combined = plot_grid(d1, d2, nrow = 1, align = "h", rel_widths = c(3, 1),
                       labels = c("D"), label_size = 20)  
d_combined
#####

### Full Combined Plot ###
tiff("dNBR_RF_stats.tiff", units = "in", width = 16.5, height = 12, res = 300)
plot_grid(a_combined, b_combined, c_combined, d_combined, ncol = 2, align = "hv")
dev.off()