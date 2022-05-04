## Water PCB concentrations mapping.
# Data were obtained from EPA and contractors from PCB Superfund
# sites in USA

# Install packages
install.packages("ggplot2")
install.packages("devtools")
install.packages("dplyr")
install.packages("stringr")
install.packages("maps")
install.packages("mapdata")
install.packages("ggmap")
install.packages("usethis")
install.packages("GISTools")
install.packages("rgeos")
install.packages("ggsn")
install.packages("ggrepel")
install.packages("ggpp")

# Load libraries
library(dplyr)
library(usethis)
library(devtools)
library(ggmap) # function map_data
library(maps)
library(ggplot2)
library(leaflet)
library(raster)
library(GISTools)
library(rgeos)
library(ggsn)
library(ggrepel)
library(reshape2)
library(ggpmisc)

# Data in pg/L
d.cong <- read.csv("WaterDataCongener050322.csv")
d.aroc <- read.csv("WaterDataCongenerAroclor050322.csv")
us <- map_data("usa")
states <- map_data("state")

# Map locations -----------------------------------------------------------

# Find number of samples per state to be included as table in maps
d.cong.n <- d.cong %>%
  group_by(StateSampled) %>%
  summarise(n = n())
colnames(d.cong.n) <- c("State", "# samples")

d.aroc.n <- d.aroc %>%
  group_by(StateSampled) %>%
  summarise(n = n())
colnames(d.aroc.n) <- c("State", "# samples")

# Creates map of US with locations from congener data
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group),
               color = "black", fill = "lightblue") +
  coord_fixed(1.3) +
  theme_nothing() +
  geom_path(data = states, aes(x = long, y = lat, group = group),
             colour = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_point(data = d.cong, aes(x = Longitude, y = Latitude), color = "black",
             size = 1.2, shape = 20) +
  annotate(geom = 'table', x = -57, y = 28, label = list(d.cong.n), size = 1.5) # add table with info

# Creates map of US with locations from Aroclor data
ggplot() +
  geom_polygon(data = us, aes(x = long, y = lat, group = group),
               color = "black", fill = "lightblue") +
  coord_fixed(1.3) +
  theme_nothing() +
  geom_path(data = states, aes(x = long, y = lat, group = group),
            colour = "white") +
  geom_polygon(color = "black", fill = NA) +
  geom_point(data = d.aroc, aes(x = Longitude, y = Latitude), color = "black",
             size = 1.2, shape = 20) +
  annotate(geom = 'table', x = -57, y = 27, label = list(d.aroc.n), size = 1.5) # add table with info


# Map total PCB concentrations --------------------------------------------
# Determine a time range (e.g., 2010 - 2019) and few locations

# Prepare data
# (i) Remove samples (rows) with total PCBs  = 0
d.cong.2 <- d.cong[!(rowSums(d.cong[, c(12:115)], na.rm = TRUE)==0),]
# (ii) Remove metadata
d.cong.2 <- subset(d.cong.2, select = -c(ID:AroclorCongener))
# (iii) Remove Aroclor data
d.cong.2 <- subset(d.cong.2, select = -c(A1016:A1260))
# (iv) Get 

# Map with ggmap
WI.box <- make_bbox(lon = w.WI$Long, lat = w.WI$Lat, f = 0.6)
wi.map <- get_stamenmap(bbox = WI.box, zoom = 11)


# Remove samples (rows) with total PCBs  = 0
w.WI.t <- w.WI[!(rowSums(w.WI[,
                              c(12:115)],
                         na.rm = TRUE)==0),] # sum of PCB1 to PCB209
site.sampled <- w.WI.t$SiteSampled
w.WI.t <- subset(w.WI.t, select = -c(ID:AroclorCongener))
w.WI.t <- subset(w.WI.t, select = -c(AroclorA1016:AroclorA1260))
# Get mean congener per site, excluding zeros
tPCB <- rowSums(w.WI.t, na.rm = TRUE)
tPCB <- data.frame(cbind(site.sampled, tPCB))
tPCB$tPCB <- as.numeric(as.character(tPCB$tPCB))
tPCB.mean <- aggregate(tPCB ~ site.sampled, data = tPCB, mean)
# add coordinates
tPCB.mean <- data.frame(c(tPCB.mean, wi.coord))

# (3) Plot map + tPCB
ggmap(wi.map) +
  geom_point(data = tPCB.mean, aes(x = Long, y = Lat,
                                   size = tPCB), alpha = 0.5) +
  scale_size_area(breaks = c(250, 500, 750, 1000, 1500),
                  labels = c(250, 500, 750, 1000, 1500),
                  name = "PCBs ng/L") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title = "Fox River PCBs water concentration (pg/L) 2010-2018")



# select only WI
w.WI <- subset(w, w$StateSampled == "WI")

# Map with ggmap
WI.box <- make_bbox(lon = w.WI$Long, lat = w.WI$Lat, f = 0.6)
wi.map <- get_stamenmap(bbox = WI.box, zoom = 11)

# (1) Plot map
ggmap(wi.map)

# (2) Plot map + sampling locations
ggmap(wi.map) +
  geom_point(data = w.WI, aes(x = Long, y = Lat), shape = 21,
  color = "red",
  fill = "white", size = 1.75, stroke = 0.75)

# Prepare congener data for plotting
# Get coordinates per site
LW <- subset(w.WI, w.WI$SiteSampled == 'LakeWinnebago')
LW <- data.frame(c(LW[1,6], LW[1,7]))
OU1 <- subset(w.WI, w.WI$SiteSampled == 'OperableUnit1')
OU1 <- data.frame(c(OU1[1,6], OU1[1,7]))
OU2A <- subset(w.WI, w.WI$SiteSampled == 'OperableUnit2A')
OU2A <- data.frame(c(OU2A[1,6], OU2A[1,7]))
OU2B <- subset(w.WI, w.WI$SiteSampled == 'OperableUnit2B')
OU2B <- data.frame(c(OU2B[1,6], OU2B[1,7]))
OU2C <- subset(w.WI, w.WI$SiteSampled == 'OperableUnit2C')
OU2C <- data.frame(c(OU2C[1,6], OU2C[1,7]))
OU3 <- subset(w.WI, w.WI$SiteSampled == 'OperableUnit3')
OU3 <- data.frame(c(OU3[1,6], OU3[1,7]))
wi.coord <- rbind(LW, OU1, OU2A, OU2B, OU2C, OU3)

# Total PCBs
# # remove samples (rows) with total PCBs  = 0
w.WI.t <- w.WI[!(rowSums(w.WI[,
                           c(12:115)],
                         na.rm = TRUE)==0),] # sum of PCB1 to PCB209
site.sampled <- w.WI.t$SiteSampled
w.WI.t <- subset(w.WI.t, select = -c(ID:AroclorCongener))
w.WI.t <- subset(w.WI.t, select = -c(AroclorA1016:AroclorA1260))
# Get mean congener per site, excluding zeros
tPCB <- rowSums(w.WI.t, na.rm = TRUE)
tPCB <- data.frame(cbind(site.sampled, tPCB))
tPCB$tPCB <- as.numeric(as.character(tPCB$tPCB))
tPCB.mean <- aggregate(tPCB ~ site.sampled, data = tPCB, mean)
# add coordinates
tPCB.mean <- data.frame(c(tPCB.mean, wi.coord))

# (3) Plot map + tPCB
ggmap(wi.map) +
  geom_point(data = tPCB.mean, aes(x = Long, y = Lat,
                              size = tPCB), alpha = 0.5) +
  scale_size_area(breaks = c(250, 500, 750, 1000, 1500),
                  labels = c(250, 500, 750, 1000, 1500),
                  name = "PCBs ng/L") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title = "Fox River PCBs water concentration (pg/L) 2010-2018")

# Congener maps
# Select congener and remove samples with = 0 and NA for selected congener
w.WI.2 <- subset(w.WI, w.WI$PCB1 != 0 & w.WI$PCB1 != "NA")
# Get mean congener per site, excluding zeros
PCB1 <- aggregate(PCB1 ~ SiteSampled, data = w.WI.2, mean)
PCB1 <- data.frame(c(PCB1, wi.coord))

# (4) Plot map + congener
ggmap(wi.map) +
  geom_point(data = PCB1, aes(x = Long, y = Lat,
                              size = PCB1), alpha = 0.5) +
  scale_size_area(breaks = c(0.1, 1, 2, 4, 6),
                 labels = c(0.1, 1, 2, 4, 6),
                 name = "PCB 1 ng/L") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title = "Fox River PCB 1 water concentration (pg/L) 2010-2018")
  #geom_label_repel(data = PCB1, aes(x = Long, y = Lat, label = SiteSampled),
  #                 fill = "white", box.padding = unit(0.3, "lines"),
  #                 label.padding = unit(0.15, "lines"),
  #                 segment.color = "black", segment.size = 1)
                   
                   
