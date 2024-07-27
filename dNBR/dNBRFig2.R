### dNBR Violin Plot split by period ###

### Inputs ###
#setwd('C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')
setwd('C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change')

library(ggplot2) # Figure creation
library(ggpubr)
library(cowplot)

tbl = read.csv("dNBR FIX/Allpixels_dnbr_fix.csv")

tbl_P1 = subset(tbl, tbl$ID %in% c("P1_0", "P1_1", "P1_2", "P1_3", "P1_4", "P1_5"))
tbl_P2 = subset(tbl, tbl$ID %in% c("P2_0", "P2_1", "P2_2", "P2_3", "P2_4"))
tbl_P3 = subset(tbl, tbl$ID %in% c("P3-0", "P3_1", "P3_2", "P3_3", "P3_4", "P3_5", "P3_6",
                                 "P3_7", "P3_8", "P3_9", "P3_10", "P3_11", "P3_12"))
tbl_P4 = subset(tbl, tbl$ID %in% c("P4_0", "P4_1", "P4_2", "P4_3", "P4_4", "P4_5"))
tbl_P5 = subset(tbl, tbl$ID %in% c("P5-0", "P5_1", "P5_2", "P5_3", "P5_4", "P5_5", "P5_6",
                                  "P5_7", "P5_8", "P5_9", "P5_10", "P5_11", "P5_12"))
tbl_P6 = subset(tbl, tbl$ID %in% c("P6_0", "P6_1", "P6_2", "P6_3"))

# All Together
All_violin = ggplot(tbl, aes(x = reorder(ID, SeasonEnd, FUN = median), y = dNBR)) +
  geom_violin(aes(fill = dNBR), color = "black") +
  #scale_fill_manual(values = c("yellow2", "yellowgreen", "green4")) +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))
All_violin

#P1
P1_violin = ggplot(tbl_P1, aes(x = reorder(ID, SeasonEnd, FUN = median), y = dNBR)) +
  geom_violin(aes(fill = dNBR), color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1.25, y = 1200, label = "1989 - 1992", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))
P1_violin

#P2
P2_violin = ggplot(tbl_P2, aes(x = reorder(ID, SeasonEnd, FUN = median), y = dNBR)) +
  geom_violin(aes(fill = dNBR), color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1, y = 1200, label = "1993 - 1997", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))
P2_violin

#P3
P3_violin = ggplot(tbl_P3, aes(x = reorder(ID, SeasonEnd, FUN = median), y = dNBR)) +
  geom_violin(aes(fill = dNBR), color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 2, y = 1245, label = "1998 - 2007", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))
P3_violin

#P4
P4_violin = ggplot(tbl_P4, aes(x = reorder(ID, SeasonEnd, FUN = median), y = dNBR)) +
  geom_violin(aes(fill = dNBR), color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1, y = 1200, label = "2008", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))
P4_violin

#P5
P5_violin = ggplot(tbl_P5, aes(x = reorder(ID, SeasonEnd, FUN = median), y = dNBR)) +
  geom_violin(aes(fill = dNBR), color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 2, y = 1200, label = "2009 - 2019", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))
P5_violin

#P6
P6_violin = ggplot(tbl_P6, aes(x = reorder(ID, SeasonEnd, FUN = median), y = dNBR)) +
  geom_violin(aes(fill = dNBR), color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 1) +
  ylab("dNBR (x1000)")  +
  coord_cartesian(ylim = c(100, 1300)) + 
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", x = 1, y = 1200, label = "2020", size = 6) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.position = c(0.125, 0.875))
P6_violin

### Full Combined Plot ###
pdf("FigureS3.pdf", width = 16.5, height = 16.5)
plot_grid(P1_violin, P2_violin, P3_violin, P4_violin, P5_violin, P6_violin, ncol = 2, align = "hv")
dev.off()