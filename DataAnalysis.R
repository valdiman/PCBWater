# PCB water analysis


# Install packages
install.packages("readxl")
install.packages("ggpubr")
install.packages("ggpmisc")
install.packages("tidyverse")
install.packages("reshape2")

# Load libraries
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(scales)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(stringr)

# Read data.xlsx
# Data in pg/L
w <- read_excel("compiledListV02.xlsx", sheet = "Sheet1",
                  col_names = TRUE, col_types = NULL, na = "NA")
head(w)

# remove unnecessary columns
w.1 <- subset(w, select = -c(ID:AroclorCongener))
w.1 <- subset(w.1, select = -c(AroclorA1016:AroclorA1260))
#w.1[w.1 == 0] <- NA

# remove samples (rows) with total PCBs  = 0
w.2 <- w[!(rowSums(w[, c(12:115)], na.rm = TRUE)==0),] # sum of PCB1 to PCB209
w.2 <- subset(w.2, select = -c(ID:AroclorCongener))
w.2 <- subset(w.2, select = -c(AroclorA1016:AroclorA1260))

# Total PCBs
ggplot(w.2, aes(x = "", y = rowSums(w.2, na.rm = T))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_boxplot(width = 0.7, outlier.colour = "white") +
  theme_classic() +
  theme(aspect.ratio = 14/2) +
  xlab(expression(bold(Sigma*"PCB")))+
  ylab(expression(bold("Water Concentration (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10,
                                    angle = 60, hjust = 0.5)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  geom_jitter(position = position_jitter(0.3), cex = 1.8,
              shape = 1, col = "black") +
  annotation_logticks(sides = "l")

# Congener
# select PCB
# Remove samples with = 0 and NA

w.3 <- subset(w, w$PCB1 != 0 & w$PCB1 != "NA")

ggplot(w.3, aes(x = "", y = PCB3)) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_boxplot(width = 0.7, outlier.colour = "white") +
  theme_classic() +
  theme(aspect.ratio = 14/2) +
  xlab(expression(bold("PCB 1")))+
  ylab(expression(bold("Water Concentration (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10,
                                    angle = 60, hjust = 0.5)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  geom_jitter(position = position_jitter(0.3), cex = 1.8,
              shape = 1, col = "black") +
  annotation_logticks(sides = "l")

#plots
#spatial
#change x-axis
sites <- c("BlueRiver", "ClarkForkRiver", "FoxRiver",
           "KalamazooRiver", "Richmond", "TitanMissileComplex1A")

#need w.2
w.4 <- w[!(rowSums(w[, c(12:115)], na.rm = TRUE)==0),] # sum of PCB1 to PCB209

r <- 5/7

# Total PCBs
ggplot(w.4, aes(x = factor(SiteName, levels = sites),
                          y = rowSums(w.2,  na.rm = T))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_boxplot(width = 0.6, outlier.colour = "white") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 8/20) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 7)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l") +
  geom_jitter(position = position_jitter(0.3), cex = 1,
              shape = 1, col = "black")

# Per congeners
# using w.3
ggplot(w.3, aes(x = factor(SiteName, levels = sites),
                y = PCB3)) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_boxplot(width = 0.6, outlier.colour = "white") +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 8/20) +
  ylab(expression(bold(Sigma*"PCB (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 7)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l") +
  geom_jitter(position = position_jitter(0.3), cex = 1,
              shape = 1, col = "black")

# temporal log box-plots
# points
w$SampleDate <- as.Date(w$SampleDate)

# Total PCBs
# using w.2 and w.4

ggplot(w.4, aes(y=rowSums(w.2, na.rm = T),
                x=reorder(format(SampleDate,'%Y-%m'),
                          SampleDate))) +
  geom_boxplot(width = 0.3, outlier.colour = "white") +
  xlab("") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(aspect.ratio = 5/15) +
  ylab(expression(bold(Sigma*"PCB concentration (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   angle = 45, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 10)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l") +
  geom_jitter(position = position_jitter(0.3), cex = 1,
              shape = 1, col = "black")

# Congeners
# using w.3

ggplot(w.3, aes(y=PCB1,
                x=reorder(format(SampleDate,'%Y-%m'),
                          SampleDate))) +
  geom_boxplot(width = 0.3, outlier.colour = "white") +
  xlab("") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(aspect.ratio = 5/15) +
  ylab(expression(bold("PCB 3 concentration (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   angle = 45, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 10)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l") +
  geom_jitter(position = position_jitter(0.3), cex = 1,
              shape = 1, col = "black")

# Plot individual site

w.5 <- w.4[str_detect(w.4$SiteName, 'FoxRiver'),] 
# remove samples (rows) with total PCBs  = 0
w.6 <- w.5[!(rowSums(w.5[, c(12:115)], na.rm = TRUE)==0),] # sum of PCB1 to PCB209
w.6 <- subset(w.6, select = -c(ID:AroclorCongener))
w.6 <- subset(w.6, select = -c(AroclorA1016:AroclorA1260))

ggplot(w.5, aes(y=rowSums(w.6, na.rm = T),
                x=reorder(format(SampleDate,'%Y-%m'),
                          SampleDate))) +
  geom_boxplot(width = 0.3, outlier.colour = "white") +
  xlab("") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(aspect.ratio = 5/15) +
  ylab(expression(bold("Fox River "*Sigma*"PCB concentration (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 7,
                                   angle = 45, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 10)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l") +
  geom_jitter(position = position_jitter(0.3), cex = 1,
              shape = 1, col = "black")

