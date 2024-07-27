library(ggplot2)
library(ggpubr)
library(patchwork)

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Kangeroo Island Veg Change")

pixel = read.csv("MissingData.csv")
allpixels = read.csv("dNBR FIX/AllPixels_dnbr_fix.csv")
firemeans = read.csv("dNBR FIX/FireEventsStats_Fix.csv")

allpixels$MissingData = ifelse(allpixels$ID == "P1_0" | allpixels$ID == "P1_1" |
                               allpixels$ID == "P1_2" | allpixels$ID == "P1_3" | 
                               allpixels$ID == "P1_4" | allpixels$ID == "P2_0" | 
                               allpixels$ID == "P2_1" | allpixels$ID == "P2_3" |
                               allpixels$ID == "P2_4" | allpixels$ID == "P5_2" | 
                               allpixels$ID == "P5_10", 1, 0)
allpixels$MissingData = factor(allpixels$MissingData, 
                                  levels = c("0", "1"))

firemeans$MD = factor(firemeans$MD, 
                               levels = c("0", "1"))
firemeans$Date = factor(firemeans$Date, 
                      levels = c("1", "2", "3"))
firemeans$Date1 = factor(firemeans$Date1, 
                      levels = c("0", "1"))
firemeans$MD_Date = factor(firemeans$MD_Date, 
                         levels = c("0", "1"))

# Example Pixel
ExPixel = ggplot(pixel) +
  geom_vline(xintercept = 1990, lwd = 1.5, lty = 2) +
  geom_vline(xintercept = 1997, lwd = 1.5, lty = 2) +
  geom_line(aes(x = Year, y = Fitted), color = "red3", lwd = 1.5) +
  geom_point(aes(x = Year, y = Reflectance), size = 2) + 
  scale_x_continuous(expand = c(0,0)) +
  ylab("NBR (x1000)") +
  theme(panel.background = element_blank(),
       panel.border = element_rect(color = 'black', fill = NA),
       axis.title.y = element_text(size = 18),
       axis.text.y = element_text(size = 14),
       axis.title.x = element_text(size = 18),
       axis.text.x = element_text(size = 14),
       axis.ticks.length = unit(0.25, "cm"),
       plot.margin = unit(c(0, 0.75, 0, 0), "cm"))

tiff("expixel_dNBR_2yr.tiff", units = "in", width = 6.5, height = 4, res = 300)
ExPixel
dev.off()

# Severity Plot B1
# MD_severity = ggplot(allpixels, 
#                      aes(x = reorder(ID, SeasonEnd), y = dNBR)) +
#   geom_boxplot(aes(fill = MissingData), color = "black") +
#   scale_fill_manual(values = c("white", "red3"), name = "Fires impacted by missing data") +
#   stat_summary(fun = mean, geom = "point", shape = 1) +
#   ylab("dNBR (x1000)")  +
#   coord_cartesian(ylim = c(100, 1300)) + 
#   scale_y_continuous(expand = c(0, 20)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
#         axis.title.y = element_text(size = 18),
#         axis.text.y = element_text(size = 14),
#         panel.background = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 16),
#         legend.position = c(0.225, 0.875))
# MD_severity

# MD Influence: Do fires with missing data in dNBR years have sig. lower dNBR?
far_compare = list(c("0","1"))

MD_severity2 = ggplot(firemeans, aes(x = MD, y = dNBR)) + 
  geom_boxplot(aes(fill = MD), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", size = 4,
                     comparisons = far_compare, label = "p.signif", 
                     label.y = max(firemeans$dNBR) + 40) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(firemeans$dNBR) + 125, label.x = 0.9, size = 3) +
  scale_fill_manual(values = c("white", "red3")) +
  ylab("dNBR (x1000)") +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
MD_severity2

# Date Influence
# far_compare = list(c("0","1"))
# 
# Date_Severity = ggplot(firemeans, aes(x = Date1, y = dNBR)) + 
#   geom_boxplot(aes(fill = Date1), color = "black", show.legend = FALSE) +
#   geom_jitter(width = 0) +
#   stat_compare_means(method = "wilcox.test", size = 5.5,
#                      comparisons = far_compare, label = "p.signif", 
#                      label.y = max(firemeans$dNBR) + 80) +
#   stat_compare_means(method = "kruskal.test", 
#                      label.y = max(firemeans$dNBR) + 200, label.x = 1, size = 4) +
#   scale_fill_manual(values = c("white", "red3")) +
#   coord_cartesian(ylim = c(100, 1300)) +
#   scale_y_continuous(expand = c(0, 20)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 14),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA))
# Date_Severity

# Date Influence 2: Do fires that burn after image collection (i.e., have longer wait to 
# get to post-fire image have sig. lower dNBR?)
far_compare1 = list(c("1", "3"))
clo_compare = list(c("1", "2"), c("2", "3"))

Date_Severity = ggplot(firemeans, aes(x = Date, y = dNBR)) + 
  geom_boxplot(aes(fill = Date), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", size = 4,
                     comparisons = far_compare1, label = "p.signif", 
                     label.y = max(firemeans$dNBR) + 100) +
  stat_compare_means(method = "wilcox.test", size = 4,
                     comparisons = clo_compare, label = "p.signif",
                     label.y = max(firemeans$dNBR) + 40) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = min(firemeans$dNBR - 65), label.x = 1.1, size = 3) +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  scale_x_discrete(labels = c("1" = "< Dec 25", "2" = "Between", "3" = "> Mar 25")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
Date_Severity

#plot(firemeans$DaysFrom1231, firemeans$dNBR)

# MD and Date: Do fires with missing data in one dNBR year or that burned after March 25
# have sig. lower dNBR?
far_compare2 = list(c("0","1"))

MD_Date = ggplot(firemeans, aes(x = MD_Date, y = dNBR)) + 
  geom_boxplot(aes(fill = MD_Date), color = "black", show.legend = FALSE) +
  geom_jitter(width = 0) +
  stat_compare_means(method = "wilcox.test", size = 4,
                     comparisons = far_compare2, label = "p.signif", 
                     label.y = max(firemeans$dNBR) + 40) +
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(firemeans$dNBR) + 125, label.x = 0.9, size = 3) +
  scale_fill_manual(values = c("white", "red3")) +
  coord_cartesian(ylim = c(100, 1000)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(100,1000,100)) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA))
MD_Date

tiff("MD_Dates_dNBR.tiff", units = "in", width = 6.5, height = 4, res = 300)
(MD_severity2 | Date_Severity | MD_Date) + plot_annotation(tag_levels = "A")
dev.off()

# Stats
mean(subset(firemeans$dNBR, firemeans$MD == 0))
mean(subset(firemeans$dNBR, firemeans$MD == 1))
sd(subset(firemeans$dNBR, firemeans$MD == 0))
sd(subset(firemeans$dNBR, firemeans$MD == 1))

mean(subset(firemeans$dNBR, firemeans$Date == 1))
mean(subset(firemeans$dNBR, firemeans$Date == 2))
mean(subset(firemeans$dNBR, firemeans$Date == 3))
sd(subset(firemeans$dNBR, firemeans$Date == 1))
sd(subset(firemeans$dNBR, firemeans$Date == 2))
sd(subset(firemeans$dNBR, firemeans$Date == 3))

mean(subset(firemeans$dNBR, firemeans$MD_Date == 0))
mean(subset(firemeans$dNBR, firemeans$MD_Date == 1))
sd(subset(firemeans$dNBR, firemeans$MD_Date == 0))
sd(subset(firemeans$dNBR, firemeans$MD_Date == 1))
