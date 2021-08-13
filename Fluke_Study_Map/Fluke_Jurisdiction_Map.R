# Try mapping states, 12nm territorial sea, 24nm Coastal zone

# Definition of maratime areas: https://www.gc.noaa.gov/gcil_maritime.html
# - 3nm previously defined territorial sea baseline, retained for some federal laws, not examined/plotted here
# - 12nm territorial sea for each state
# - 24nm contiguous zone may be claimed for each state
# - 200nm exculusive economic zone (EEZ)

# To find shapefiles: https://hub.arcgis.com/datasets/44f58c599b1e4f7192df9d4d10b7ddcf_1/data?geometry=172.090%2C-12.805%2C-172.090%2C73.355
  # Can download shapefiles

# Cool shapefile example: https://rstudio-pubs-static.s3.amazonaws.com/199150_7becf121a2ac45c7ada9bc26d7cde985.html

# Maps by country/county: https://gadm.org/

# Map of canada tutorial https://www.r-bloggers.com/2018/12/canada-map/

# Stat areas: https://www.northeastoceandata.org/data-download/

# NOAA EDAB: https://noaa-edab.github.io/tech-doc/survdat.html
  # https://github.com/NOAA-EDAB/tech-doc
  # Shared code to generate report & plots

library(sf)
library(ggplot2)
library(mapdata)

# Plot all together from NE Ocean Data Portal
maratime <- st_read(dsn = paste(here::here(), "Fluke_Study_Map","USMaritimeLimitsAndBoundariesSHP/USMaritimeLimitsNBoundaries.shp",sep="/"))

# Transform coordinates to projected coordinates
maratime_trans <- st_transform(maratime, "+proj=longlat +ellps=WGS84 +datum=WGS84")

# ggplot() +
#   geom_sf(data = maratime_trans, color="black") +
#   theme_minimal() + 
#   coord_sf(xlim = c(-80, -60),  ylim = c(25, 50))
# 
# ggplot() +
#   geom_sf(data = EEZ, color="red") +
#   theme_minimal() + 
#   coord_sf(xlim = c(-80, -60),  ylim = c(25, 50))

##### 24nm Contiguous Zone
contiguous <- st_read(dsn = paste(here::here(), "Fluke_Study_Map","Contiguous_Zone/US_Maritime_Limits_Boundaries_Map_Service_Layer.shp", sep="/"))
# Transform coordinates to projected coordinates
contiguous_trans <- st_transform(contiguous, "+proj=longlat +ellps=WGS84 +datum=WGS84")

##### 12nm Territorial Sea
territory <- st_read(dsn = paste(here::here(), "Fluke_Study_Map","Territorial_Sea/US_Maritime_Limits_Boundaries_Map_Service_Layer.shp", sep="/"))
# Transform coordinates to projected coordinates
territory_trans <- st_transform(territory, "+proj=longlat +ellps=WGS84 +datum=WGS84")

##### 200nm EEZ
# Plot all together from NE Ocean Data Portal
maratime <- st_read(dsn = paste(here::here(), "Fluke_Study_Map","USMaritimeLimitsAndBoundariesSHP/USMaritimeLimitsNBoundaries.shp", sep="/"))
# Transform coordinates to projected coordinates
maratime_trans <- st_transform(maratime, "+proj=longlat +ellps=WGS84 +datum=WGS84")
# Filter out EEZ line
EEZ <- maratime_trans[which(maratime_trans$EEZ==1),]

##### US States
US <- st_read(dsn = paste(here::here(), "Fluke_Study_Map","US_States/cb_2018_us_state_20m.shp", sep="/"))
# Transform coordinates to projected coordinates
US_trans <- st_transform(US, "+proj=longlat +ellps=WGS84 +datum=WGS84")

##### Canada
library(mapdata)
Canada <- map_data("world", "Canada")

##### Survey strata
strata <- st_read(dsn = paste(here::here(), "Fluke_Study_Map","strata.shp", sep="/"))
# Transform coordinates to projected coordinates
strata_trans <- st_transform(strata, "+proj=longlat +ellps=WGS84 +datum=WGS84")

# ggplot() +
#   geom_sf(data = strata, color=alpha("blue",0.3)) + # Group = group keeps stuff together https://gis.stackexchange.com/questions/37204/what-is-the-cause-of-tearing-of-polygons-artifacts-using-r-ggplot-and-geom
#   coord_sf(xlim = c(-80, -60),  ylim = c(25, 50)) +
#   theme_minimal()
# 
# ggplot() +
#   geom_polygon(data = strata, color = "blue") + # Group = group keeps stuff together https://gis.stackexchange.com/questions/37204/what-is-the-cause-of-tearing-of-polygons-artifacts-using-r-ggplot-and-geom
#   coord_sf(xlim = c(-80, -60),  ylim = c(25, 50)) +
#   theme_minimal()

# Finalish Version
FlukeMap <- ggplot() +
  geom_sf(data = strata_trans, color=alpha("blue",0.3), fill = "white") + # Causes error
  geom_sf(data = contiguous_trans, color="black", linetype = "dashed") +
  geom_sf(data = territory_trans, color = "black") + 
  geom_sf(data = EEZ, color = "black", linetype = "dotdash") +
  geom_sf(data = US_trans, color="gray50", fill = "gray") +
  geom_polygon(data = Canada, color = "gray50", fill = "gray", aes(x=long, y=lat, group=group)) + 
  coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46)) +
  theme_minimal() +
  annotate("path", x=c(-70.5,-68.5), y=c(36.25,36.25), linetype="dotdash") +
  annotate("text", x = -68, y = 36.25, label = "200nm EEZ", hjust=0) +
  annotate("path", x=c(-70.5,-68.5), y=c(35.25,35.25), linetype="dashed") +
  annotate("text", x = -68, y = 35.25, label = "24nm CZ", hjust=0) + 
  annotate("path", x=c(-70.5,-68.5), y=c(34.25,34.25)) +
  annotate("text", x = -68, y = 34.25, label = "12nm TS", hjust=0) +
  annotate("path", x=c(-70.5,-68.5), y=c(33.25,33.25), color=alpha("blue", 0.3)) +
  annotate("text", x = -68, y = 33.25, label = "Survey strata", hjust=0) 
# CZ = contiguous zone
# EEZ = Exclusive economic zone
# TS = Territorial Seas
# Survey strata = NEFSC bottom trawl survey strata
FlukeMap

##### Try to ID overlapping points to define new geometries for state waters
# http://postgis.net/workshops/postgis-intro/spatial_relationships.html


# library(plotly)
# FlukeMap %>% ggplotly() %>%
#   highlight(.,"plotly_hover")
# 
# overlap <- st_intersects(strata_trans, contiguous_trans, sparse = FALSE)
# # plot(strata_trans[st_intersects(strata_trans, contiguous_trans, sparse = FALSE) %>% lengths > 0,]) # lengths filter doesn't work
# 
# strata_trans %>%
#   filter(st_intersects(., contiguous_trans, sparse = FALSE))
# 
# ncol(overlap) #54 contiguous-trans polygons
# length(contiguous_trans[[1]])
# colnames(overlap) <- c(paste("CZ", 1:54, sep=""))
# nrow(overlap) #310 strata-trans polygons
# length(strata_trans[[1]])
# rownames(overlap) <- c(paste("Strat", 1:310, sep=""))
# which(overlap == TRUE)
# overlap[636]
# 
# # Pick out polygons that have overlapping points
# ptCZ <- NULL
# ptStrata <- NULL
# for(icol in 1:ncol(overlap)){
#   for(irow in 1:nrow(overlap)){
#     if(overlap[irow,icol] == TRUE){
#       ptCZ <- append(ptCZ, colnames(overlap)[icol])
#       ptStrata <- append(ptStrata, rownames(overlap)[irow])
#     }
#   }
# }
# 
# # Plot confirms that first 2 strata in list overlap
# plot(strata_trans[[1]][[16]]); plot(contiguous_trans[[13]][[3]], add=T)
# # Maybe defined points don't match exactly
# which(strata_trans[[1]][[16]][[1]][,1] %in% contiguous_trans[[13]][[3]][[1]][,1] == TRUE)
# strata_trans[[1]][[16]][[1]][,2] == contiguous_trans[[13]][[3]][[1]][,2]
# # This successfully plots points of overlap
# #try2 <- st_intersection(strata_trans[[1]][[16]],contiguous_trans[[13]][[3]])
# plot(strata_trans[[1]][[16]], col="blue"); plot(contiguous_trans[[13]][[3]], add=T); plot(try, add=T, col="red")
# # This highlights line within polygon, just need to take end
# try3 <- st_intersection(contiguous_trans[[13]][[3]],strata_trans[[1]][[16]])
# plot(strata_trans[[1]][[16]], col="blue"); plot(contiguous_trans[[13]][[3]], add=T); plot(try2, add=T, col="red")

# Success!!!!
strata_bound <- st_boundary(strata_trans)
CZ <- st_intersection(strata_bound,contiguous_trans, sparse=FALSE)
plot(strata_trans, col="blue"); plot(contiguous_trans, add=T); plot(CZ, add=T, col="red")

TS <- st_intersection(strata_bound,territory_trans, sparse=FALSE)
plot(strata_trans); plot(territory_trans, col = "blue", add=T); plot(TS, add=T, col="red"); plot(contiguous_trans, col = "blue", add=T); plot(CZ, add=T, col="orange"); plot(US_trans, add=T, col="gray")

#######################################################################################################################################
##### Generate new state-waters shapes
# Plot intersection points with TS and CZ
plot(strata_trans); plot(territory_trans, col = "blue", add=T); plot(TS, add=T, col="red"); plot(contiguous_trans, col = "blue", add=T); plot(CZ, add=T, col="orange"); plot(US_trans, add=T, col="gray"); plot(TS[[13]][[61]], add=T, col="green") # Change [[3]] to look at other features
# Plot only CZ
plot(strata_trans); plot(territory_trans, col = "blue", add=T); plot(contiguous_trans, col = "blue", add=T); plot(CZ, add=T, col="orange"); plot(US_trans, add=T, col="gray"); plot(CZ[[13]][[1]], add=T, col="green") # Change [[3]] to look at other features
# Plot individual points
FlukeMap + geom_point(aes(x=-70.73481,y= 40.85227), col="red", size=0.1)

# Combine territory into single multipoint geometry
newTerritory <- st_combine(territory_trans)
TerritoryMatrix <- as.matrix(newTerritory[[1]], ncol=2)
 # Filter TerritoryMatrix so only dealing with NEUS
TerritoryMatrix <- TerritoryMatrix[which(TerritoryMatrix[,2] <= 44 & TerritoryMatrix[,2] >= 32),]
TerritoryMatrix <- TerritoryMatrix[which(TerritoryMatrix[,1] <= -67.84834 & TerritoryMatrix[,1] >= -80),]
# Combine contiguous into single multipoint geometry
newContiguous<- st_combine(contiguous_trans)
ContiguousMatrix <- as.matrix(newContiguous[[1]], ncol=2)
 # Filter ContiguousMatrix so only dealing with NEUS
ContiguousMatrix <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 44 & ContiguousMatrix[,2] >= 32),]
ContiguousMatrix <- ContiguousMatrix[which(ContiguousMatrix[,1] <= -67.84834 & ContiguousMatrix[,1] >= -80),]

##### Massachusetts waters shape
# MA TS N POINT -70.35 42.88677 # approximate y=max MA lat
#    TS S POINT (-70.80276 41.04882)
#    CZ N POINT -70.03 42.88677 # approximate y=max MA lat
#    CZ S POINT -70.73481 40.85227

test <- TerritoryMatrix[which(TerritoryMatrix[,2] < 43 & TerritoryMatrix[,1] > -71),]
plot(test); plot(territory_trans, col = "blue", add=T); plot(contiguous_trans, col = "blue", add=T); plot(US_trans, add=T, col="gray")

# Check that correct subset of points selected
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 42.88677 & TerritoryMatrix[,1] >= -70.80276),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 42.88677 & ContiguousMatrix[,1] >= -70.73481),])

maTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 42.88677 & TerritoryMatrix[,1] >= -70.80276),]
# Find break points
for(irow in 2:nrow(maTStemp)){
  if(abs(maTStemp[irow,2] - maTStemp[irow-1, 2]) > 0.2){
    print(irow)
    print(maTStemp[irow-1,])
    print(maTStemp[irow,])
  }
}
maCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 42.88677 & ContiguousMatrix[,1] >= -70.73481),]
# Find break points
for(irow in 2:nrow(maCZtemp)){
  if(abs(maCZtemp[irow,2] - maCZtemp[irow-1, 2]) > 0.2){
    print(irow)
    #print(maCZtemp[irow-1,])
    #print(maCZtemp[irow,])
  }
}

MA1 <- maTStemp[1:460,] %>% st_linestring()
MA2 <- maTStemp[566:696,] %>% st_linestring()
MA3 <- maTStemp[697:1035,] %>% st_linestring()
MA4 <- maTStemp[461:565,] %>% st_linestring()
  MA5 <-  maCZtemp[1:9,] %>% st_linestring()
  MA6 <-  maCZtemp[10:48,] %>% st_linestring()
  MA7 <-  maCZtemp[49:299,] %>% st_linestring()
  MA8 <-  maCZtemp[300:362,] %>% st_linestring()
  MA9 <-  maCZtemp[363:591,] %>% st_linestring()
plot(MAwater); plot(MA2, col="blue", add=T); plot(MA3, col="purple", add=T); plot(MA1, col="pink", add=T); plot(MA4, col="red", add=T); # Confirm breakpoints IDed above = wonkiness
plot(MAwater); plot(MA5, col="blue", add=T); plot(MA6, col="purple", add=T); plot(MA7, col="pink", add=T); plot(MA8, col="red", add=T);plot(MA9, col="green", add=T); # Confirm breakpoints IDed above = wonkiness

MApts <- rbind(c(-70.35, 42.88677), apply(maTStemp[1:460,],2,rev), apply(maTStemp[566:696,], 2, rev),  apply(maTStemp[697:1035,],2,rev), apply(maTStemp[461:565,],2,rev), c(-70.80276, 41.04882), # MA territorial waters
               c(-70.73481, 40.85227), maCZtemp[10:48,], maCZtemp[363:591,], maCZtemp[300:362,], maCZtemp[49:299,], maCZtemp[1:9,], c(-70.03, 42.88677), # Contiguous waters
               c(-70.35, 42.88677)) # Close polygon
MAwater <- st_polygon(list(MApts)) #st_linestring(MApts)
plot(MAwater)
customMAwater <- st_sfc(MAwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customMAwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))
# This picks out the point and adds the MA TS N/S points

##### Rhode Island waters shape
# RI TS N POINT (-70.80276 41.04882)
#    TS S POINT (-71.68259 40.9142)
#    CZ N POINT -70.73481 40.85227
#    CZ S POINT -71.68259, 40.6822 # approximate
# Check that correct subset of points selected
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 42 & TerritoryMatrix[,1] >= -71.68259 & TerritoryMatrix[,1] <= -70.80276),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 42& ContiguousMatrix[,1] >= -71.68259 & ContiguousMatrix[,1] <= -70.73481),])

riTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 42 & TerritoryMatrix[,1] >= -71.68259 & TerritoryMatrix[,1] <= -70.80276),]
# Find break points
for(irow in 2:nrow(riTStemp)){
  if(abs(riTStemp[irow,2] - riTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
riCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 42 & ContiguousMatrix[,1] >= -71.68259 & ContiguousMatrix[,1] <= -70.73481),]
# Find break points
for(irow in 2:nrow(riCZtemp)){
  if(abs(riCZtemp[irow,2] - riCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

# TS check order
RI1 <- riTStemp[1:114,] %>% st_linestring()
RI2 <- riTStemp[115:280,] %>% st_linestring()
plot(RIwater); plot(RI1, col="blue", add=T); plot(RI2, col="purple", add=T);  # Confirm breakpoints IDed above = wonkiness
#CZ check order
RI3 <- riCZtemp[1:50,] %>% st_linestring()
RI4 <- riCZtemp[51:88,] %>% st_linestring()
plot(RIwater); plot(RI3, col="blue", add=T); plot(RI4, col="purple", add=T);  # Confirm breakpoints IDed above = wonkiness

RIpts <- rbind(c(-70.80276, 41.04882), apply(riTStemp[1:114,],2,rev), apply(riTStemp[115:280,],2,rev),  c(-71.68259, 40.9142), # RI territorial waters
               c(-71.68259, 40.6822), riCZtemp[51:88,], riCZtemp[1:50,], c(-70.73481, 40.85227), # Contiguous waters
               c(-70.80276, 41.04882)) # Close polygon
RIwater <- st_polygon(list(RIpts)) #st_linestring(MApts)
plot(RIwater)
customRIwater <- st_sfc(RIwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customRIwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))


##### Connecticut waters shape
# CT TS N POINT (-71.68259 40.9142)
#    TS S POINT (-72.40097 40.64665)
#    CZ N POINT -71.68259, 40.6822 # approximate
#    CZ S POINT -72.24101 40.48009
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 40.9142 & TerritoryMatrix[,1] >= -72.40097 & TerritoryMatrix[,1] <= -71.68259),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 40.6822 & ContiguousMatrix[,1] >= -72.24101 & ContiguousMatrix[,1] <= -71.68259),])

ctTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 40.9142 & TerritoryMatrix[,1] >= -72.40097 & TerritoryMatrix[,1] <= -71.68259),]
# Find break points
for(irow in 2:nrow(ctTStemp)){
  if(abs(ctTStemp[irow,2] - ctTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
ctCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 40.6822 & ContiguousMatrix[,1] >= -72.24101 & ContiguousMatrix[,1] <= -71.68259),]
# Find break points
for(irow in 2:nrow(ctCZtemp)){
  if(abs(ctCZtemp[irow,2] - ctCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

CTpts <- rbind(c(-71.68259, 40.9142), apply(ctTStemp,2,rev),  c(-72.40097, 40.64665), # Territorial waters
               c(-72.24101, 40.48009), ctCZtemp, c(-71.68259, 40.6822), # Contiguous waters
               c(-71.68259, 40.9142)) # Close polygon
CTwater <- st_polygon(list(CTpts)) #st_linestring(MApts)
plot(CTwater)
customCTwater <- st_sfc(CTwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customCTwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))

##### New York waters shape
# NY TS N POINT (-72.40097 40.64665)
#    TS S POINT -73.80669, 39.94716
#    CZ N POINT -72.24101 40.48009
#    CZ S POINT -73.56949 39.7994
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 40.64665 & TerritoryMatrix[,1] >= -73.80669 & TerritoryMatrix[,1] <= -72.40097),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 40.64665 & ContiguousMatrix[,1] >= -73.56949 & ContiguousMatrix[,1] <= -72.24101),])

nyTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 40.64665 & TerritoryMatrix[,1] >= -73.80669 & TerritoryMatrix[,1] <= -72.40097),]
# Find break points
for(irow in 2:nrow(nyTStemp)){
  if(abs(nyTStemp[irow,2] - nyTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
nyCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 40.64665 & ContiguousMatrix[,1] >= -73.56949 & ContiguousMatrix[,1] <= -72.24101),]
# Find break points
for(irow in 2:nrow(nyCZtemp)){
  if(abs(nyCZtemp[irow,2] - nyCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

#TS check order
NY1 <- nyTStemp[1:442,] %>% st_linestring()
NY2 <- nyTStemp[443:605,] %>% st_linestring()
plot(NYwater); plot(NY1, col="blue", add=T); plot(NY2, col="purple", add=T);  # Confirm breakpoints IDed above = wonkiness
#CZ check order
NY3 <- nyCZtemp[1:220,] %>% st_linestring()
NY4 <- nyCZtemp[221:308,] %>% st_linestring()
plot(NYwater); plot(NY3, col="blue", add=T); plot(NY4, col="purple", add=T);  # Confirm breakpoints IDed above = wonkiness

NYpts <- rbind(c(-72.40097, 40.64665), apply(nyTStemp[1:442,],2,rev), apply(nyTStemp[443:605,],2,rev), c(-73.80669, 39.94716), # Territorial waters
               c(-73.56949, 39.7994), nyCZtemp[221:308,], nyCZtemp[1:220,], c(-72.24101, 40.48009), # Contiguous waters
               c(-72.40097, 40.64665)) # Close polygon
NYwater <- st_polygon(list(NYpts)) #st_linestring(MApts)
plot(NYwater)
customNYwater <- st_sfc(NYwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customNYwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))


##### New Jersey waters shape
# NJ TS N POINT -73.80669, 39.94716
#    TS S POINT -74.4583 39.04239
#    CZ N POINT -73.56949 39.7994
#    CZ S POINT -74.2347 38.93898
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 39.94716 & TerritoryMatrix[,1] >= -74.4583 & TerritoryMatrix[,1] <= -73.80669),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 39.7994 & ContiguousMatrix[,1] >= -74.2347 & ContiguousMatrix[,1] <= -73.56949),])

njTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 39.94716 & TerritoryMatrix[,1] >= -74.4583 & TerritoryMatrix[,1] <= -73.80669),]
# Find break points
for(irow in 2:nrow(njTStemp)){
  if(abs(njTStemp[irow,2] - njTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
njCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 39.7994 & ContiguousMatrix[,1] >= -74.2347 & ContiguousMatrix[,1] <= -73.56949),]
# Find break points
for(irow in 2:nrow(njCZtemp)){
  if(abs(njCZtemp[irow,2] - njCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

NJpts <- rbind(c(-73.80669, 39.94716), apply(njTStemp,2,rev), c(-74.4583, 39.04239), # Territorial waters
               c(-74.2347, 38.93898), njCZtemp, c(-73.56949, 39.7994), # Contiguous waters
               c(-73.80669, 39.94716)) # Close polygon
NJwater <- st_polygon(list(NJpts)) #st_linestring(MApts)
plot(NJwater)
customNJwater <- st_sfc(NJwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customNJwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))


##### Delaware waters shape
# DE TS N POINT -74.4583 39.04239
#    TS S POINT -74.8023 38.49 # approximate
#    CZ N POINT -74.2347 38.93898
#    CZ S POINT -74.5413 38.49 # approximate
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 39.04239 & TerritoryMatrix[,2] >= 38.49 & TerritoryMatrix[,1] >= -74.8023 & TerritoryMatrix[,1] <= -74.4583),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 38.93898 & ContiguousMatrix[,2] >= 38.49 & ContiguousMatrix[,1] >= -74.5413 & ContiguousMatrix[,1] <= -74.2347),])

deTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 39.04239 & TerritoryMatrix[,2] >= 38.49 & TerritoryMatrix[,1] >= -74.8023 & TerritoryMatrix[,1] <= -74.4583),]
# Find break points
for(irow in 2:nrow(deTStemp)){
  if(abs(deTStemp[irow,2] - deTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
deCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 38.93898 &  ContiguousMatrix[,2] >= 38.49 &  ContiguousMatrix[,1] >= -74.5413 & ContiguousMatrix[,1] <= -74.2347),]
# Find break points
for(irow in 2:nrow(deCZtemp)){
  if(abs(deCZtemp[irow,2] - deCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

DEpts <- rbind(c(-74.4583, 39.04239), apply(deTStemp,2,rev), c(-74.8023, 38.49), # Territorial waters
               c(-74.5413, 38.49), deCZtemp, c(-74.2347, 38.93898), # Contiguous waters
               c(-74.4583, 39.04239)) # Close polygon
DEwater <- st_polygon(list(DEpts)) #st_linestring(MApts)
plot(DEwater)
customDEwater <- st_sfc(DEwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customDEwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))


##### Maryland waters shape
# MD TS N POINT -74.8023 38.49 # approximate
#    TS S POINT -75.03552, 37.9111 (-74.96366 38.01223)
#    CZ N POINT -74.5413 38.49 # approximate
#    CZ S POINT -74.82215 37.80391
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 38.49 & TerritoryMatrix[,2] >= 37.9111 & TerritoryMatrix[,1] >= -75.03552 & TerritoryMatrix[,1] <= -74.8023),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 38.49 & ContiguousMatrix[,2] >= 37.80391 & ContiguousMatrix[,1] >= -74.82215 & ContiguousMatrix[,1] <= -74.5413),])

mdTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 38.49 & TerritoryMatrix[,2] >= 37.9111 & TerritoryMatrix[,1] >= -75.03552 & TerritoryMatrix[,1] <= -74.8023),]
# Find break points
for(irow in 2:nrow(mdTStemp)){
  if(abs(mdTStemp[irow,2] - mdTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
mdCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 38.49 & ContiguousMatrix[,2] >= 37.80391 & ContiguousMatrix[,1] >= -74.82215 & ContiguousMatrix[,1] <= -74.5413),]
# Find break points
for(irow in 2:nrow(deCZtemp)){
  if(abs(mdCZtemp[irow,2] - mdCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

#TS check order
NY1 <- nyTStemp[1:442,] %>% st_linestring()
NY2 <- nyTStemp[443:605,] %>% st_linestring()
plot(NYwater); plot(NY1, col="blue", add=T); plot(NY2, col="purple", add=T);  # Confirm breakpoints IDed above = wonkiness
#CZ check order
MD1 <- mdCZtemp[1:41,] %>% st_linestring()
MD2 <- mdCZtemp[42:138,] %>% st_linestring()
plot(MDwater); plot(MD1, col="blue", add=T); plot(MD2, col="purple", add=T);  # Confirm breakpoints IDed above = wonkiness

MDpts <- rbind(c(-74.8023, 38.49), apply(mdTStemp,2,rev), c(-75.03552, 37.9111), # Territorial waters
               c(-74.82215, 37.80391), mdCZtemp[42:138,], mdCZtemp[1:41,], c(-74.5413, 38.49), # Contiguous waters
               c(-74.8023, 38.49)) # Close polygon
MDwater <- st_polygon(list(MDpts)) #st_linestring(MApts)
plot(MDwater)
customMDwater <- st_sfc(MDwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customMDwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))


##### Virginia waters shape
# VA TS N POINT -75.03552, 37.9111   (-74.96366 38.01223)
#    TS S POINT (-75.60452 36.50592) # May need to remove trailing 2 after 9
#    CZ N POINT -74.82215 37.80391
#    CZ S POINT -75.34971 36.505)
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 37.9111 & TerritoryMatrix[,2] >= 36.50592 & TerritoryMatrix[,1] <= -75.03552),])
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 37.80391 & ContiguousMatrix[,2] >= 36.505  & ContiguousMatrix[,1] <= -74.82215),])

vaTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 37.9111 & TerritoryMatrix[,2] >= 36.50592 & TerritoryMatrix[,1] <= -75.03552),]
# Find break points
for(irow in 2:nrow(vaTStemp)){
  if(abs(vaTStemp[irow,2] - vaTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
vaCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 37.80391 & ContiguousMatrix[,2] >= 36.505  & ContiguousMatrix[,1] <= -74.82215),]
# Find break points
for(irow in 2:nrow(vaCZtemp)){
  if(abs(vaCZtemp[irow,2] - vaCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

VApts <- rbind(c(-75.03552, 37.9111), apply(vaTStemp,2,rev), c(-75.60452, 36.50592), # Territorial waters
               c(-75.34971, 36.505), vaCZtemp, c(-74.82215, 37.80391), # Contiguous waters
               c(-75.03552, 37.9111)) # Close polygon
VAwater <- st_polygon(list(VApts)) #st_linestring(MApts)
plot(VAwater)
customVAwater <- st_sfc(VAwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customVAwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))


##### North Carolina waters shape
# NC TS N POINT (-75.60452 36.50592) # May need to remove trailing 2 after 9
#    TS S POINT -78.43447 33.66457
#    CZ N POINT -75.34971 36.505)
#    CZ S POINT -78.3252, 33.4863
plot(TerritoryMatrix[which(TerritoryMatrix[,2] <= 36.50592 & TerritoryMatrix[,2] >= 33 & TerritoryMatrix[,1] >= -78.43447 ),]) # & TerritoryMatrix[,1] <= -75.60452
plot(ContiguousMatrix[which(ContiguousMatrix[,2] <= 36.505 & ContiguousMatrix[,2] >= 33 & ContiguousMatrix[,1] >= -78.762 ),]) # & ContiguousMatrix[,1] <= -75.34971

ncTStemp <-  TerritoryMatrix[which(TerritoryMatrix[,2] <= 36.50592 & TerritoryMatrix[,2] >= 33 & TerritoryMatrix[,1] >= -78.43447 ),]
# Find break points
for(irow in 2:nrow(ncTStemp)){
  if(abs(ncTStemp[irow,2] - ncTStemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}
ncCZtemp <- ContiguousMatrix[which(ContiguousMatrix[,2] <= 36.505 & ContiguousMatrix[,2] >= 33 & ContiguousMatrix[,1] >= -78.3252),]
# Find break points
for(irow in 2:nrow(ncZtemp)){
  if(abs(ncCZtemp[irow,2] - ncCZtemp[irow-1, 2]) > 0.1){
    print(irow)
  }
}

NCpts <- rbind(c(-75.60452, 36.50592), apply(ncTStemp,2,rev), c(-78.43447, 33.66457), # Territorial waters
               c(-78.3252, 33.4863), ncCZtemp, c(-75.34971, 36.505), # Contiguous waters
               c(-75.60452, 36.50592)) # Close polygon
NCwater <- st_polygon(list(NCpts)) #st_linestring(MApts)
plot(NCwater)
customNCwater <- st_sfc(NCwater, crs = 4269)
#customMAwater <- st_sf(object = 1, geometry=st_sfc(MAwater), crs=st_set_crs(MAwater, "+proj=longlat +ellps=WGS84 +datum=WGS84")) # 4269 match CRS from source shape
FlukeMap + geom_sf(data=customNCwater, col="orange") +   coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46))





# Final Version with state waters areas
FlukeMap <- ggplot() +
  geom_sf(data = strata_trans, color=alpha("blue",0.3), fill = "white") + # Causes error
  geom_sf(data=customMAwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data=customRIwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data=customCTwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data=customNYwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data=customNJwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data=customDEwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data=customMDwater, col="orange", fill = "orange", alpha = 0.4) +
  geom_sf(data=customVAwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data=customNCwater, col="orange", fill = "orange", alpha = 0.4) + 
  geom_sf(data = contiguous_trans, color="black", linetype = "dashed") +
  geom_sf(data = territory_trans, color = "black") + 
  geom_sf(data = EEZ, color = "black", linetype = "dotdash") +
  geom_sf(data = US_trans, color="gray50", fill = "gray") +
  geom_polygon(data = Canada, color = "gray50", fill = "gray", aes(x=long, y=lat, group=group)) + 
  coord_sf(xlim = c(-80, -65),  ylim = c(32.5, 46)) +
  theme_minimal() +
  annotate("path", x=c(-70.5,-68.5), y=c(36.25,36.25), linetype="dotdash") +
  annotate("text", x = -68, y = 36.25, label = "200nm EEZ", hjust=0) +
  annotate("path", x=c(-70.5,-68.5), y=c(35.25,35.25), linetype="dashed") +
  annotate("text", x = -68, y = 35.25, label = "24nm CZ", hjust=0) + 
  annotate("path", x=c(-70.5,-68.5), y=c(34.25,34.25)) +
  annotate("text", x = -68, y = 34.25, label = "12nm TS", hjust=0) +
  annotate("path", x=c(-70.5,-68.5), y=c(33.25,33.25), color=alpha("blue", 0.3)) +
  annotate("text", x = -68, y = 33.25, label = "Survey strata", hjust=0) +
  annotate("rect", xmin = -70.5, xmax = -68.5, ymin = 32.36, ymax = 32.12, col="orange", fill = "orange", alpha=0.4) +
  annotate("text", x = -68, y = 32.25, label = "State waters", hjust=0)
# CZ = contiguous zone
# EEZ = Exclusive economic zone
# TS = Territorial Seas
# Survey strata = NEFSC bottom trawl survey strata
FlukeMap
ggsave(filename = "FlukeJurisdictionMap_LowRes.png",  height = 4)
ggsave(filename = "FlukeJurisdictionMap_HighRes.png",  height = 6.5)





# Notes for me:
# - breaking points into st_linestring() meant they could be plotted in different colors using base R as a check
# - this coloring helped reorder the groups of points that were MULTIPOINT or polygon sub geometries in the original territorial seas and contiguous zone objects
# - apply(DATA, 2, rev) reverses the order of points so connections between segmens are smooth rather than adding spider web lines

# Helpful to define state areas: https://geocompr.robinlovelace.net/geometric-operations.html
  # https://cran.r-project.org/web/packages/sf/vignettes/sf5.html
