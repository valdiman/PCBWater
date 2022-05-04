## Water PCB concentrations analysis.
# Data were obtained from EPA and contractors from PCB Superfund
# sites in USA

# Install packages
install.packages("ggpubr")
install.packages("ggpmisc")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("ggplot2")

# Load libraries
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(scales) # function trans_breaks
library(gridExtra)
library(tidyverse)
library(reshape2)
library(stringr)

# Data in pg/L
d.cong <- read.csv("WaterDataCongener050322.csv")
d.aroc <- read.csv("WaterDataCongenerAroclor050322.csv")

# Summary plots -----------------------------------------------------------

# Box plots with Aroclor dataset
# Remove samples (rows) with total PCBs  = 0
d.aroc.2 <- d.aroc[!(rowSums(d.aroc[, c(12:115)], na.rm = TRUE)==0),]
# Remove metadata
d.aroc.2 <- subset(d.aroc.2, select = -c(ID:AroclorCongener))
# Remove Aroclor data
d.aroc.2 <- subset(d.aroc.2, select = -c(A1016:A1260))

# Total PCBs in 1 box plot
ggplot(d.aroc.2, aes(x = "", y = rowSums(d.aroc.2, na.rm = T))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic() +
  theme(aspect.ratio = 14/2) +
  xlab(expression(bold(Sigma*"PCB (n = 45,146)")))+
  ylab(expression(bold("Water Concentration 1990 - 2020 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10,
                                    angle = 45, hjust = 1.8,
                                    vjust = 2)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  geom_jitter(position = position_jitter(0.3), cex = 1.2,
              shape = 1, col = "lightblue") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
  annotation_logticks(sides = "l")

# Prepare data for plots with congener dataset
# Remove samples (rows) with total PCBs  = 0
d.cong.2 <- d.cong[!(rowSums(d.cong[, c(12:115)], na.rm = TRUE)==0),]
# Remove metadata
d.cong.2 <- subset(d.cong.2, select = -c(ID:AroclorCongener))
# Remove Aroclor data
d.cong.2 <- subset(d.cong.2, select = -c(A1016:A1260))
# Create a frequency detection plot
d.cong.freq <- colSums(! is.na(d.cong.2) & (d.cong.2 !=0))/nrow(d.cong.2)
d.cong.freq <- data.frame(d.cong.freq)
colnames(d.cong.freq) <- c("PCB.frequency")
congener <- row.names(d.cong.freq)
d.cong.freq <- cbind(congener, d.cong.freq$PCB.frequency)
colnames(d.cong.freq) <- c("congener", "PCB.frequency")
d.cong.freq <- data.frame(d.cong.freq)
d.cong.freq$congener <- as.character(d.cong.freq$congener)


d.cong.freq$congener <- str_replace(d.cong.freq$congener,".","+")

d.cong.freq$PCB.frequency <- as.numeric(as.character(d.cong.freq$PCB.frequency))
d.cong.freq$congener <- factor(d.cong.freq$congener,
                            levels = unique(d.cong.freq$congener))

d.cong.freq$congener <- replace(d.cong.freq$congener,
                                d.cong.freq$congener == ".", "+")

# Frequency detection plot
ggplot(d.cong.freq, aes(x = congener, y = PCB.frequency)) +
  geom_bar(stat = "identity", fill = "black") +
  xlab("") +
  theme_bw() +
  theme(aspect.ratio = 4/12) +
  ylab(expression(bold("Frequnecy detection"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Total PCBs in 1 box plot
ggplot(d.cong.2, aes(x = "", y = rowSums(d.cong.2, na.rm = T))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic() +
  theme(aspect.ratio = 14/2) +
  xlab(expression(bold(Sigma*"PCB (n = 2093)")))+
  ylab(expression(bold("Water Concentration 1990 - 2019 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10,
                                    angle = 45, hjust = 1.8,
                                    vjust = 2)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  geom_jitter(position = position_jitter(0.3), cex = 1.2,
              shape = 1, col = "lightblue") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
  annotation_logticks(sides = "l")

# Individual congeners
# Remove samples with = 0 and NA
d.cong.PCB5.8 <- subset(d.cong,
                        d.cong$PCB5.8 != 0 & d.cong$PCB5.8 != "NA")



d.cong.3 <- colMeans(d.cong.2 > 0, na.rm = TRUE)

ggplot(d.cong.PCB5.8, aes(x = "", y = PCB5.8)) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic() +
  theme(aspect.ratio = 14/2) +
  xlab(expression(bold("PCBs 5+8 (n = 1434)")))+
  ylab(expression(bold("Water Concentration 1990 - 2019 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12)) +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold", size = 10,
                                    angle = 45, hjust = 1.8,
                                    vjust = 2)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  geom_jitter(position = position_jitter(0.3), cex = 1.2,
              shape = 1, col = "lightblue") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
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

