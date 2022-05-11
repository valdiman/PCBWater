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
#library(ggpubr)
#library(ggpmisc)
library(scales) # function trans_breaks
#library(gridExtra)
#library(tidyverse)
#library(reshape2)
#library(stringr)

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
              shape = 1, col = "#66ccff") +
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
d.cong.freq$congener <- gsub('\\.', '+', d.cong.freq$congener) # replace dot for +
d.cong.freq$PCB.frequency <- as.numeric(as.character(d.cong.freq$PCB.frequency))
d.cong.freq$congener <- factor(d.cong.freq$congener,
                            levels = rev(d.cong.freq$congener)) # change the order to be plotted.

# Summary statistic of frequency of detection
summary(d.cong.freq$PCB.frequency)

# Frequency detection plot
ggplot(d.cong.freq, aes(x = 100*PCB.frequency, y = congener)) +
  geom_bar(stat = "identity", fill = "#66ccff") +
  ylab("") +
  theme_bw() +
  xlim(c(0,100)) +
  theme(aspect.ratio = 20/5) +
  xlab(expression(bold("Frequency detection (%)"))) +
  theme(axis.text.x = element_text(face = "bold", size = 9),
        axis.title.x = element_text(face = "bold", size = 10)) +
  theme(axis.text.y = element_text(face = "bold", size = 5))

# Summary statistic of total PCBs
summary(rowSums(d.cong.2, na.rm = T))

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
              shape = 1, col = "#66ccff") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
  annotation_logticks(sides = "l")

# Individual congeners
# Summary statistic of individual congeners
summary(d.cong.2, na.rm = T, zero = T)
# Obtain the median for each individual congener
cong.median <- as.numeric(sub('.*:', '', summary(d.cong.2,
                                                 na.rm = T, zero = T)[3,]))

# Individual PCB boxplot
ggplot(stack(d.cong.2), aes(x = ind, y = values)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_boxplot(width = 0.6, outlier.colour = "#66ccff", col = "#66ccff",
               outlier.shape = 1) +
  scale_x_discrete(labels = d.cong.freq$congener) + # use to change the "." to "+"
  theme_bw() +
  theme(aspect.ratio = 25/135) +
  xlab(expression("")) +
  ylab(expression(bold("PCB congener concentration (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 8,
                                   color = "black"),
        axis.title.y = element_text(face = "bold", size = 8,
                                    color = "black")) +
  theme(axis.text.x = element_text(face = "bold", size = 6,
                                   angle = 60, hjust = 1,
                                   color = "black"),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(size = 0.6, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l",
                      short = unit(0.5, "mm"),
                      mid = unit(1.5, "mm"),
                      long = unit(2, "mm"))

# Spatial plots and analysis ----------------------------------------------
# Modify x-axis
sites <- c("CA", "DE", "ID", "IN", "MA", "MI", "MO",
           "MT", "NM", "NY", "OR", "TX", "WA", "WI")

# Total PCBs
ggplot(d.cong, aes(x = factor(StateSampled, levels = sites),
                y = rowSums(d.cong[, c(12:115)],  na.rm = T))) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 5/20) +
  ylab(expression(bold("Water Conncetration " *Sigma*"PCB 1990 - 2019 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l") +
  geom_jitter(position = position_jitter(0.3), cex = 1.2,
              shape = 1, col = "#66ccff") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
  geom_hline(yintercept = 5600, color = "#cc0000")

# Selected PCB congeners
# Format data for selected PCBs
# Remove samples with = 0 and NA
d.cong.pcb.sp <- subset(d.cong, d.cong$PCB4.10 != 0 & d.cong$PCB4.10 != "NA")
d.cong.pcb.sp <- subset(d.cong.pcb.sp, select = -c(ID))
d.cong.pcb.sp <- subset(d.cong.pcb.sp, select = -c(SiteName:AroclorCongener))
d.cong.pcb.sp <- subset(d.cong.pcb.sp, select = -c(A1016:A1260))

# Plot
ggplot(d.cong.pcb.sp, aes(x = factor(StateSampled, levels = sites),
                   y = PCB4.10)) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  xlab(expression("")) +
  theme(aspect.ratio = 5/20) +
  ylab(expression(bold("Water Conncetration PCB 4+10 1990 - 2019 (pg/L)"))) +
  theme(axis.text.y = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  theme(axis.text.x = element_text(face = "bold", size = 8,
                                   angle = 60, hjust = 1),
        axis.title.x = element_text(face = "bold", size = 8)) +
  theme(axis.ticks = element_line(size = 0.8, color = "black"), 
        axis.ticks.length = unit(0.2, "cm")) +
  annotation_logticks(sides = "l") +
  geom_jitter(position = position_jitter(0.3), cex = 1.2,
              shape = 1, col = "#66ccff") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
  geom_hline(yintercept = 374.15, color = "#cc0000")

# Temporal plots
# Change date format
d.cong$SampleDate <- strptime(x = as.character(d.cong$SampleDate),
                              format = "%m/%d/%Y")

# Total PCBs
ggplot(d.cong, aes(y = rowSums(d.cong[, c(12:115)],  na.rm = T),
                x = format(SampleDate,'%Y'))) +
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
  geom_jitter(position = position_jitter(0.3), cex = 1.2,
              shape = 1, col = "#66ccff") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
  annotation_logticks(sides = "l")

# Congeners
# Format data for selected PCBs
# the first line needs to be changed for each congener
# Remove samples with = 0 and NA
d.cong.pcb.tp <- subset(d.cong, d.cong$PCB4.10 != 0 & d.cong$PCB4.10 != "NA")
d.cong.pcb.tp <- subset(d.cong.pcb.tp, select = -c(ID:SiteSampled))
d.cong.pcb.tp <- subset(d.cong.pcb.tp, select = -c(Latitude:AroclorCongener))
d.cong.pcb.tp <- subset(d.cong.pcb.tp, select = -c(A1016:A1260))

ggplot(d.cong.pcb.tp, aes(y = PCB4.10,
                   x = format(SampleDate,'%Y'))) +
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
  geom_jitter(position = position_jitter(0.3), cex = 1.2,
              shape = 1, col = "#66ccff") +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0) +
  annotation_logticks(sides = "l")


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

