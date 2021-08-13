
library(tidyverse)

# Shape files
states <- map_data("state")
NAmerica <- map_data("world")
FlounderStates <- subset(states, region %in% c("north carolina", "virginia", "delaware", "new jersey", 
                                               "new york", "connecticut",  "rhode island",
                                               "massachusetts", "maryland"))
AdjacentStates <- subset(states, region %in% c("pennsylvania", "west virginia", "south carolina",
                                               "georgia", "ohio", "florida", "tennessee", "kentucky",
                                               "indiana", "michigan", "maine", "new hampshire", "vermont"))



# Coastline decision area
ggplot() +
  geom_polygon(data = FlounderStates,
               aes(x = long, y = lat, group = group), 
               fill = "royalblue3", 
               color = "black") + 
  geom_polygon(data = AdjacentStates,
               aes(x = long, y = lat, group = group), 
               fill = "gray87", 
               color = "black") + 
  coord_fixed(xlim = c(-81.6, -65),  ylim = c(33.5, 47), ratio = 1.2) +
  theme_void()

# 5 area option
ggplot() +
  geom_polygon(data = subset(states, region %in% "massachusetts"), 
               aes(x = long, y = lat, group = group), 
               fill = c("royalblue4"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "rhode island"), 
               aes(x = long, y = lat, group = group), 
               fill = c("royalblue3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "connecticut"), 
               aes(x = long, y = lat, group = group), 
               fill = c("lightsteelblue1"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "new york"), 
               aes(x = long, y = lat, group = group), 
               fill = c("lightsteelblue1"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "new jersey"), 
               aes(x = long, y = lat, group = group), 
               fill = c("lightsteelblue1"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "delaware"), 
               aes(x = long, y = lat, group = group), 
               fill = c("sienna3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "maryland"), 
               aes(x = long, y = lat, group = group), 
               fill = c("sienna3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "virginia"), 
               aes(x = long, y = lat, group = group), 
               fill = c("sienna3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "north carolina"), 
               aes(x = long, y = lat, group = group), 
               fill = "#542f04",
               color = "black") + 
  geom_polygon(data = AdjacentStates,
               aes(x = long, y = lat, group = group), 
               fill = "gray87", 
               color = "black") + 
  coord_fixed(xlim = c(-81.6, -65),  ylim = c(33.5, 47), ratio = 1.2) +
  theme_void()

# 6 area option
ggplot() +
  geom_polygon(data = subset(states, region %in% "massachusetts"), 
               aes(x = long, y = lat, group = group), 
               fill = c("royalblue4"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "rhode island"), 
               aes(x = long, y = lat, group = group), 
               fill = c("royalblue3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "connecticut"), 
               aes(x = long, y = lat, group = group), 
               fill = c("lightsteelblue1"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "new york"), 
               aes(x = long, y = lat, group = group), 
               fill = c("lightsteelblue1"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "new jersey"), 
               aes(x = long, y = lat, group = group), 
               fill = c("#ffdab9"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "delaware"), 
               aes(x = long, y = lat, group = group), 
               fill = c("sienna3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "maryland"), 
               aes(x = long, y = lat, group = group), 
               fill = c("sienna3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "virginia"), 
               aes(x = long, y = lat, group = group), 
               fill = c("sienna3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "north carolina"), 
               aes(x = long, y = lat, group = group), 
               fill = "#542f04",
               color = "black") + 
  geom_polygon(data = AdjacentStates,
               aes(x = long, y = lat, group = group), 
               fill = "gray87", 
               color = "black") + 
  coord_fixed(xlim = c(-81.6, -65),  ylim = c(33.5, 47), ratio = 1.2) +
  theme_void()

ggplot() +
  geom_polygon(data = subset(states, region %in% "massachusetts"), 
               aes(x = long, y = lat, group = group), 
               fill = c("royalblue4"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "rhode island"), 
               aes(x = long, y = lat, group = group), 
               fill = c("royalblue3"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "connecticut"), 
               aes(x = long, y = lat, group = group), 
               fill = c("#7fc0ed"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "new york"), 
               aes(x = long, y = lat, group = group), 
               fill = c("lightsteelblue1"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "new jersey"), 
               aes(x = long, y = lat, group = group), 
               fill = c("#ffdab9"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "delaware"), 
               aes(x = long, y = lat, group = group), 
               fill = c("#ff8147"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "maryland"), 
               aes(x = long, y = lat, group = group), 
               fill = c("#cd6739"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "virginia"), 
               aes(x = long, y = lat, group = group), 
               fill = c("#8b4627"), 
               color = "black") + 
  geom_polygon(data = subset(states, region %in% "north carolina"), 
               aes(x = long, y = lat, group = group), 
               fill = "#542f04",
               color = "black") + 
  geom_polygon(data = AdjacentStates,
               aes(x = long, y = lat, group = group), 
               fill = "gray87", 
               color = "black") + 
  coord_fixed(xlim = c(-81.6, -65),  ylim = c(33.5, 47), ratio = 1.2) +
  theme_void()

