# Summer Flounder Project Planning Plots
  # See also: FlounderMaps.R, SummerFlounder_DataFormat.R, TimeseriesPlotData.R, EffortCatchVsMngmtPlotData.R for previous data exploration attempts


# Library packages
library(tidyverse)
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(scatterpie)
library(plotly)
library(ggridges) # https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

# NOTES:
  # If the data returns an error that an invalid 'type' (character) of an argument was used in a calculation 
    # this means the variable you are manipulating has a "*"  value somewhere and you can't sum over this character
    # remove the rows with "*" will fix the problem when you run as.numeric() and then sum() or other manipulating function

############################################ Data ########################################################################################
# See also: "/Users/ahart2/Research/Summer_Flounder_MSE/Data_FormattedMatrices" for Catch/Effort vs. Bag/SeasonLength/MinSize datasets by state & whole coastline
# See also: "/Users/ahart2/Research/Summer_Flounder_MSE/Data_Files" for raw source materials 

setwd("/Users/ahart2/Research/Summer_Flounder_MSE/Data/Data_Files_PreliminaryPlots")

StateNames <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
FullStateNames <- c("MASSACHUSETTS", "RHODE ISLAND", "CONNECTICUT", "NEW YORK", "NEW JERSEY", "DELAWARE", "MARYLAND", "VIRGINIA", "NORTH CAROLINA")
DataYrs <- c(1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
             2011, 2012, 2013, 2014, 2015, 2016, 2017)
PlotColors <- c("#01665e", "#35978f", "#80cdc1", "#c7eae5", 
                "#dfc27d", "#bf812d", "#8c510a", "#543005", "coral2")

##### Summer Flounder discards by state & year #####
Discards <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/SourceFiles/Discards.csv")

Discards %>%
  as_tibble() %>%  # Convert data.frame to a tibble format for viewing
  select(everything(), LiveDisc = Released.Alive..B2.) %>% # Rename Released.Alive..B2. column to LiveDisc & keep other information
  filter(LiveDisc != "*") %>% # Remove 1 weird observation from VA where no value for discards provided
  mutate(LiveDisc = as.numeric(as.character(LiveDisc))) %>% # By setting LiveDisc to calculated value, this overwrites the existing column
  arrange(StateNames[State]) %>% # Change order of state data to reflect geography
  group_by(State, Year) %>% 
  dplyr::summarise(YrSums = sum(LiveDisc)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
  mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
ggplot() + 
  geom_line(aes(x=Year, y = YrSums, group=State, color = StateFact), size = 1) + 
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.line = element_line()) + # Remove grid from theme
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +  # Add in axes again and force to be at zero
  ylab("Live discards") +
  labs(title = "Live discards by state") +
  scale_color_manual(values = c("#01665e", "#35978f", "#80cdc1", "#c7eae5", 
                                "#dfc27d", "#bf812d", "#8c510a", "#543005", "coral2"))

# Color <- c("#003c30", "#01665e", "#35978f", "#80cdc1", "#c7eae5", 
#            "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a", "#543005")


# Total Recreational Landings
Catch <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/SourceFiles/CatchAtLength_NoClosed.csv")

Catch %>%
  as_tibble() %>%
  select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., MinSz = Min.Size..inches., SeasonLength_days = Season..days.) %>% # Change column names
  group_by(State, Year) %>% 
  dplyr::summarise(TotCat = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
  mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
  ggplot() + 
  geom_line(aes(x=Year, y = TotCat, group=State, color = StateFact)) + 
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.line = element_line()) + # Remove grid from theme
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +  # Add in axes again and force to be at zero
  ylab("Total catch ") +
  labs(title = "Total catch by state") +
  scale_color_manual(values = c("#01665e", "#35978f", "#80cdc1", "#c7eae5", 
                                "#dfc27d", "#bf812d", "#8c510a", "#543005", "coral2"))


##### Recreational Summer Flounder Catch at Length #####
# Single state & year histogram
Catch %>%
  as_tibble() %>%
  select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., MinSz = Min.Size..inches., SeasonLength_days = Season..days.) %>% # Change column names
  group_by(State, Year,ForkLength_Inch) %>% 
  dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
  mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
  filter(StateFact == "MA", Year == 1993) %>%
ggplot() + # Single year & state histogram
  geom_col(aes(x = ForkLength_Inch, y = CatchAtLength), fill = PlotColors[1]) +  
  ylab("Catch-at-Length (inches) ") +
  labs(title = "MA Catch-at-Length") + 
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.line.x = element_line()) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))   # Add in axes again and force to be at zero
  
# Multi-year plot
for(iplot in 1:length(StateNames)){ # !!! Why doesn't this loop work?
  print(iplot)
  DoPlot <- Catch %>%
    as_tibble() %>%
    select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., MinSz = Min.Size..inches., SeasonLength_days = Season..days.) %>% # Change column names
    group_by(State, Year,ForkLength_Inch) %>% 
    dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
    mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
    filter(StateFact == StateNames[iplot]) %>%
  ggplot() +
    geom_density_ridges2(aes(x = ForkLength_Inch, y = Year, group = Year), fill = PlotColors[iplot]) +
    labs(title = paste(StateNames[iplot],"Catch-at-Length", sep = " ")) 
  DoPlot
}


# 3D version
FormattedCatch <- Catch %>%
  as_tibble() %>%
  select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., MinSz = Min.Size..inches., SeasonLength_days = Season..days.) %>% # Change column names
  group_by(State, Year,ForkLength_Inch) %>% 
  dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
  mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
  filter(StateFact == "MA")
Plot3DLine <- plot_ly( 
  FormattedCatch, 
  y = ~ForkLength_Inch, 
  x = ~-Year, # Making this negative reverses the year order
  z = ~CatchAtLength,
  type = 'scatter3d', mode = 'lines'
) %>%
  layout(showlegend = FALSE) 
  layout(
    scene = list(xaxis = list(title = 'Weight'),
                 yaxis = list(title = 'Gross horsepower'),
                 zaxis = list(title = '1/4 mile time'))
  )
Plot3DLine 
# # Try later, requires XQuartz download
# library(rgl)
# persp3d(x=FormattedCatch$ForkLength_Inch, 
#         y = FormattedCatch$Year, 
#         z = FormattedCatch$CatchAtLength, col="skyblue")

# # The following won't work because: https://community.plot.ly/t/3d-surface-plot-in-r/676/3
# Plot3DSurface <- plot_ly(  
#   FormattedCatch,
#   y = ~ForkLength_Inch, 
#   x = ~-as.numeric(Year),
#   z = ~as.matrix(cbind(as.numeric(FormattedCatch$Year), as.numeric(FormattedCatch$ForkLength_Inch), as.numeric(FormattedCatch$CatchAtLength)))
# ) %>%
#   add_surface()
# Plot3DSurface


# Mean catch at length over time 
palette(PlotColors)
legend_title <- "State"
annualMeanLength <- Catch %>% # Annual coast-wide mean length of landed summer flounder
  as_tibble() %>%
  select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., MinSz = Min.Size..inches., SeasonLength_days = Season..days.) %>% # Change column names
  group_by(Year,ForkLength_Inch) %>% 
  dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
  mutate(Coast = sum(ForkLength_Inch*CatchAtLength)/sum(CatchAtLength))

Catch %>%
  as_tibble() %>%
  select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., MinSz = Min.Size..inches., SeasonLength_days = Season..days.) %>% # Change column names
  group_by(State, Year,ForkLength_Inch) %>% 
  dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
  mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
  # filter(StateFact == "MA") %>%
  mutate(MeanLength = sum(ForkLength_Inch*CatchAtLength)/sum(CatchAtLength)) %>%
  select(State, Year, MeanLength, StateFact) %>%
  distinct() %>%
ggplot() +
  geom_line(aes(x=Year, y=MeanLength, colour = StateFact)) +
  scale_color_manual(legend_title, values = c("#01665e", "#35978f", "#80cdc1", "#c7eae5", "#dfc27d", "#bf812d", "#8c510a", "#543005", "coral2")) +
  geom_line(data = annualMeanLength, aes(x=Year, y=Coast), color = "black", lwd=1) +
  ylab("Mean Length (In.)") +
  labs(title = "Mean Length of Landed Summer Flounder") +
  theme_classic()
# Maybe get NEFSC bottom trawl survey data here: https://noaa-edab.github.io/ecodata/reference/nefsc_survey_disaggregated.html
ggsave(filename = "MeanLandedFlukeLength.png", width = 5, height = 4, units = c("in"), dpi = 700)



ggplot() + # Single year & state histogram
  geom_col(aes(x = ForkLength_Inch, y = CatchAtLength), fill = PlotColors[1]) +  
  ylab("Catch-at-Length (inches)") +
  labs(title = "MA Catch-at-Length") + 
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.line.x = element_line()) + 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))   # Add in axes again and force to be at zero


##### Total Summer Flounder Recreational Effort #####
Effort <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/SourceFiles/Effort_NoClosed.csv")

Effort %>%
  as_tibble() %>%
  select(everything(), Effort_NumbTrips = X..Trips, MinSz = Min.Size..inches., SeasonLength_days = Season..days.) %>% # Change column names
  group_by(State, Year) %>% 
  dplyr::summarise(TotEffort = sum(Effort_NumbTrips)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups
  mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
ggplot() + 
  geom_line(aes(x=Year, y = TotEffort, group=State, color = StateFact)) + 
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.line = element_line()) + # Remove grid from theme
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +  # Add in axes again and force to be at zero
  ylab("Effort (number of trips)") +
  labs(title = "Total effort by state") +
  scale_color_manual(values = c("#01665e", "#35978f", "#80cdc1", "#c7eae5", 
                                "#dfc27d", "#bf812d", "#8c510a", "#543005", "coral2"))


##### Catch time-series by area for each state Added in May,2019 #####################################################
# Data downloaded from mrip_database (no preprocessing by co-authors)
CatDat <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/mrip_SUMMER_FLOUNDER_catch_series_April18_2019.csv", skip = 38, stringsAsFactors = FALSE) # !!! problem, doesn't have NC data =( search StateNames)-1 and replace with StateNames once fixed, fix also lat/long/cities
NCDat <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data/mrip_SUMMER_FLOUNDER_catch_series_NC_June5_2019.csv", skip = 28, stringsAsFactors = FALSE)
NCState <- rep("NORTH CAROLINA", nrow(NCDat))
NCDat <- cbind(NCDat[,1:3], NCState, NCDat[,4:8])
colnames(NCDat) <- colnames(CatDat[,1:9])
AllCatDat <- rbind(CatDat[,1:9], NCDat)


istate = 1
for(istate in 1:length(FullStateNames)){ #loop still doesn't work
  InlandDat <- rep(NA,length(DataYrs))
  Less3MIDat <- rep(NA, length(DataYrs))
  Greater3MIDat <- rep(NA, length(DataYrs))
  for(iyear in 1:length(DataYrs)){
    StateData <- AllCatDat[which(AllCatDat$State==FullStateNames[istate]),2:ncol(AllCatDat)]
    colnames(StateData) <- c("Year", "Wave", "State", "Common_Name", "Fishing_Mode", "Fishing_Area", "TotalHarvest", "PSE")
    YearDat <- StateData[which(StateData$Year==DataYrs[iyear]),]
    YearDat <- YearDat[which(YearDat$TotalHarvest!="*"),] 
    InlandDat[iyear] <- sum(as.numeric(YearDat[which(YearDat$Fishing_Area=="INLAND"),"TotalHarvest"]))
    Less3MIDat[iyear] <- sum(as.numeric(YearDat[which(YearDat$Fishing_Area=="OCEAN (<= 3 MI)"),"TotalHarvest"]))
    Greater3MIDat[iyear] <- sum(as.numeric(YearDat[which(YearDat$Fishing_Area=="OCEAN (> 3 MI)"),"TotalHarvest"]))
  }
  
  TempPlot <- ggplot() + 
    geom_line(aes(x=DataYrs, y = InlandDat), linetype="solid", color = PlotColors[istate], show.legend = TRUE) + # Inland catch
    geom_line(aes(x=DataYrs, y = Less3MIDat), linetype="longdash", color = PlotColors[istate], show.legend = TRUE) + # Ocean less 3MI catch
    geom_line(aes(x=DataYrs, y = Greater3MIDat), linetype="dotdash", color = PlotColors[istate]) + # Ocean greater 3MI catch
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.line = element_line()) + # Remove grid from theme
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +  # Add in axes again and force to be at zero
    ylab("Catch (number of fish)") +
    xlab("Year") +
    labs(title = paste("Total",StateNames[istate],"catch")) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "longdash", "dotdash"))))
  assign(paste(StateNames[istate], "plot", sep="_"), TempPlot)
  TempPlot
  
}

# Multi-year plot of catch distribution by state & area #Added 6/24/19
  # preprocessed Catch dataset lack areas
  # mrip direct AllCatDat dataset lack catch-at-length
Catch624 <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data_Files/mrip_SUMMER_FLOUNDER_length_freq_6-24-19.csv", skip = 24, stringsAsFactors = FALSE)
#for(iplot in 1:length(StateNames)){ # !!! Why doesn't this loop work?
  InlandPlot <- Catch624 %>%
    as_tibble() %>%
    select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area) %>% # Change column names
    group_by(State, Year,Fishing_Area, ForkLength_Inch) %>% 
    dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups, this just picks the colums desired based on grouping
    filter(State %in% FullStateNames) %>%
    #mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
    filter(State == FullStateNames[iplot]) %>%
    filter(Fishing_Area == "INLAND") %>% # Filter only INLAND fishing
    ggplot() +
    geom_density_ridges2(aes(x = ForkLength_Inch, y = Year, group = Year), fill = PlotColors[iplot]) +
    labs(title = paste(StateNames[iplot],"Inland Catch-at-Length", sep = " ")) +
    xlim(0,40)
  StatePlot <- Catch624 %>%
    as_tibble() %>%
    select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area) %>% # Change column names
    group_by(State, Year,Fishing_Area, ForkLength_Inch) %>% 
    dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups, this just picks the colums desired based on grouping
    filter(State %in% FullStateNames) %>%
    #mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
    filter(State == FullStateNames[iplot]) %>%
    filter(Fishing_Area == "OCEAN (<= 3 MI)") %>% # Filter only INLAND fishing
    ggplot() +
    geom_density_ridges2(aes(x = ForkLength_Inch, y = Year, group = Year), fill = PlotColors[iplot]) +
    labs(title = paste(StateNames[iplot],"State Waters Catch-at-Length", sep = " ")) +
    xlim(0,40)
  FederalPlot <- Catch624 %>%
    as_tibble() %>%
    select(everything(), ForkLength_Inch = Straight.Fork.Length..in., CatchAtLength = Landings..No..at.Length., Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area) %>% # Change column names
    group_by(State, Year,Fishing_Area, ForkLength_Inch) %>% 
    dplyr::summarise(CatchAtLength = sum(CatchAtLength)) %>% # there are multiple summarise()/summarize() functions in the world, specify dplyr:: as source to ensure it summarizes by groups, this just picks the colums desired based on grouping
    filter(State %in% FullStateNames) %>%
    #mutate(StateFact = factor(State, levels = StateNames)) %>% # Change order of state factor levels so plot color correspond to geography
    filter(State == FullStateNames[iplot]) %>%
    filter(Fishing_Area == "OCEAN (> 3 MI)") %>% # Filter only INLAND fishing
    ggplot() +
    geom_density_ridges2(aes(x = ForkLength_Inch, y = Year, group = Year), fill = PlotColors[iplot]) +
    labs(title = paste(StateNames[iplot],"Federal Waters Catch-at-Length", sep = " ")) +
    xlim(0,40)
  InlandPlot
  StatePlot
  FederalPlot
  
#}













# SavePlot <- grid.arrange(MA_plot, RI_plot, CT_plot, NY_plot,
#                          NJ_plot, DE_plot, MD_plot, VA_plot,
#                          NC_plot, ncol=1)
# ggsave("/Users/ahart2/Research/Summer_Flounder_MSE/CatchByStateArea.png", plot=SavePlot, width = 8, height = 11, units="in")

# ##### Doesn't work "*" in harvest data causes problems, the above is equivalent
# InlandCatch <- AllCatDat %>%
#   as_tibble() %>%
#   dplyr::select(everything(), Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., PropStdError = PSE) %>% # Change column names
#   mutate(Wave = factor(Wave, labels = c("January_February", "July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
#   mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>% # Change factor names
#   mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>% # Change factor names
#   group_by(State, Year, Fishing_Area) %>%
#   dplyr::summarise(TotCatTrips = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
#   #mutate(PropModeTrips = TotModeTrips/sum(Total_Harvest)) %>%
#   filter(Fishing_Area == "Inland", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
#   arrange(match(State, FullStateNames))
# OceanLess3MICatch <- AllCatDat %>%
#   as_tibble() %>%
#   dplyr::select(everything(), Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., PropStdError = PSE) %>% # Change column names
#   mutate(Wave = factor(Wave, labels = c("January_February", "July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
#   mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>% # Change factor names
#   mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>% # Change factor names
#   group_by(State, Year, Fishing_Area) %>%
#   dplyr::summarise(TotCatTrips = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
#   #mutate(PropModeTrips = TotModeTrips/sum(Total_Harvest)) %>%
#   filter(Fishing_Area == "Ocean_Less3MI", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
#   arrange(match(State, FullStateNames))
# OceanGreater3MICatch <- AllCatDat %>%
#   as_tibble() %>%
#   dplyr::select(everything(), Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., PropStdError = PSE) %>% # Change column names
#   mutate(Wave = factor(Wave, labels = c("January_February", "July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
#   mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>% # Change factor names
#   mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>% # Change factor names
#   group_by(State, Year, Fishing_Area) %>%
#   dplyr::summarise(TotCatTrips = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
#   #mutate(PropModeTrips = TotModeTrips/sum(Total_Harvest)) %>%
#   filter(Fishing_Area == "Ocean_Greater3MI", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
#   arrange(match(State, FullStateNames))
# 
# # Plot For each state
# for(ist in 1:length(FullStateNames)){ #This loop may not work
#   istate <- ist
#   print(istate)
# TempPlot <- ggplot() + 
#     geom_line(aes(x=as.numeric(unlist(InlandCatch[which(InlandCatch$State==FullStateNames[istate]),"Year"])), y = as.numeric(unlist(InlandCatch[which(InlandCatch$State==FullStateNames[istate]),"TotCatTrips"])), linetype="solid"), color = PlotColors[istate]) + # Inland catch
#     geom_line(aes(x=as.numeric(unlist(OceanLess3MICatch[which(OceanLess3MICatch$State==FullStateNames[istate]),"Year"])), y = as.numeric(unlist(OceanLess3MICatch[which(OceanLess3MICatch$State==FullStateNames[istate]),"TotCatTrips"])), linetype="longdash"), color = PlotColors[istate]) + # Ocean less 3MI catch
#     geom_line(aes(x=as.numeric(unlist(OceanGreater3MICatch[which(OceanGreater3MICatch$State==FullStateNames[istate]),"Year"])), y = as.numeric(unlist(OceanGreater3MICatch[which(OceanGreater3MICatch$State==FullStateNames[istate]),"TotCatTrips"])), linetype="dotdash"), color = PlotColors[istate]) + # Ocean greater 3MI catch
#     theme_minimal() +
#     theme(panel.grid = element_blank(), axis.line = element_line()) + # Remove grid from theme
#     scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +  # Add in axes again and force to be at zero
#     ylab("Catch (number of fish)") +
#     xlab("Year") +
#     labs(title = paste("Total",StateNames[istate],"catch")) +
#     theme(legend.title = element_blank()) +
#     scale_linetype_manual(values = c("solid", "dashed", "dotdash"), labels = c("Inland", "Ocean < 3MI", "Ocean >= 3MI"))
# assign(paste(StateNames[istate], "plot", sep="_"), TempPlot)
# TempPlot
# }
# 
# library(gridExtra)
# library(ggplotify)
# library(grid)
# 
# SavePlot <- grid.arrange(MA_plot, RI_plot, CT_plot, NY_plot,
#              NJ_plot, DE_plot, MD_plot, VA_plot,
#              NC_plot, ncol=1)
# ggsave("/Users/ahart2/Research/Summer_Flounder_MSE/CatchByStateArea.png", plot=SavePlot, width = 8, height = 11, units="in")




# Commercial Landings

# Commercial Quota




######################################## Maps ################################################################################

##### Map with portion of effort by type & state plotted for all recreational fisheries ##### !!! currently missing effort data, points show locations of pie graphs !!! coloring is inconsistent
# Shape files
states <- map_data("state")
NAmerica <- map_data("world")
FlounderStates <- subset(states, region %in% c("north carolina", "virginia", "delaware", "new jersey", 
                                               "new york", "connecticut",  "rhode island",
                                               "massachusetts", "maryland"))
AdjacentStates <- subset(states, region %in% c("pennsylvania", "west virginia", "south carolina",
                                               "georgia", "ohio", "florida", "tennessee", "kentucky",
                                               "indiana", "michigan", "maine", "new hampshire", "vermont"))

# Data
TripDat <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Preliminary_Work/Data_Files/mrip_effort_series_April18_2019.csv", skip = 24) # !!! problem, doesn't have NC data =( search StateNames)-1 and replace with StateNames once fixed, fix also lat/long/cities

for(iyr in DataYrs){
  iyr <- iyr
  UseColors <- NULL
  # All states, 1 year
  PartyCharterBoat <- TripDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Angler_Trips = Angler.Trips, PropStdError = PSE) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI", "Unknown"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeTrips = sum(Angler_Trips)) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeTrips = TotModeTrips/sum(TotModeTrips)) %>%
    filter(Year == iyr, Fishing_Mode == "Party_Charter_Boat", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
    arrange(match(State, FullStateNames)) # Reorder based on geography
  partycharter <- PartyCharterBoat$PropModeTrips 
  UseColors <- c(UseColors, "dodgerblue") 
  if(empty(PartyCharterBoat)==TRUE){ # if no observations set prop=0
    partycharter <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  CharterBoat <- TripDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Angler_Trips = Angler.Trips, PropStdError = PSE) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI", "Unknown"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeTrips = sum(Angler_Trips)) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeTrips = TotModeTrips/sum(TotModeTrips)) %>%
    filter(Year == iyr, Fishing_Mode == "Charter_Boat", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
    arrange(match(State, FullStateNames)) # Reorder based on geography
  UseColors <- c(UseColors, "chocolate2") 
  charterboat <- CharterBoat$PropModeTrips
  if(empty(CharterBoat)==TRUE){ # if no observations set prop=0
    charterboat <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  PartyBoat <- TripDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Angler_Trips = Angler.Trips, PropStdError = PSE) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI", "Unknown"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeTrips = sum(Angler_Trips)) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeTrips = TotModeTrips/sum(TotModeTrips)) %>%
    filter(Year == iyr, Fishing_Mode == "Party_Boat", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
    arrange(match(State, FullStateNames)) # Reorder based on geography
  UseColors <- c(UseColors, "darkorchid2") 
  party <- PartyBoat$PropModeTrips
  if(empty(PartyBoat)==TRUE){ # if no observations set prop=0
    party <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  PrivateRentBoat <- TripDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Angler_Trips = Angler.Trips, PropStdError = PSE) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI", "Unknown"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeTrips = sum(Angler_Trips)) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeTrips = TotModeTrips/sum(TotModeTrips)) %>%
    filter(Year == iyr, Fishing_Mode == "Private_Rental_Boat", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
    arrange(match(State, FullStateNames)) # Reorder based on geography
  privaterent <- PrivateRentBoat$PropModeTrips
  UseColors <- c(UseColors, "darkolivegreen3") 
  if(empty(PrivateRentBoat)==TRUE){ # if no observations set prop=0
    privaterent <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  Shore <- TripDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Angler_Trips = Angler.Trips, PropStdError = PSE) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI", "Unknown"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeTrips = sum(Angler_Trips)) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeTrips = TotModeTrips/sum(TotModeTrips)) %>%
    filter(Year == iyr, Fishing_Mode == "Shore", State != "NEW HAMPSHIRE", State != "MAINE")  %>%
    arrange(match(State, FullStateNames)) # Reorder based on geography
  shore <- Shore$PropModeTrips
  UseColors <- c(UseColors, "goldenrod1")
  if(empty(Shore)==TRUE){ # if no observations set prop=0
    shore <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  effortType_dat <- data.frame(
    partycharter = partycharter, 
    party = party, 
    charter = charterboat,
    privaterent = privaterent,
    shore = shore,
    longpt = c(-69 , -69, -71, -75, -72.8, -72.4, -74.5, -78.5),#, -78.5),
    latpt = c(42.5, 40, 39.6, 43, 39.5, 37.7, 37, 37.5),#, 35.4), 
    cities = StateNames[1:8]
  )
  
  Flounder_map <- ggplot() +
    geom_polygon(data = NAmerica, 
                 aes(x=long, y = lat, group = group), 
                 fill = "grey87", # Change polygone fill color
                 color="black") + 
    theme_minimal() + # Change theme (there are other options)   
    geom_polygon(data = FlounderStates,
                 aes(x = long, y = lat, group = group), 
                 fill = "grey54", 
                 color = "black") + 
    geom_polygon(data = AdjacentStates,
                 aes(x = long, y = lat, group = group), 
                 fill = "gray87", 
                 color = "black") + 
    coord_fixed(xlim = c(-84.2, -65),  ylim = c(33.5, 47), ratio = 1.2) + 
    labs (title = "Effort Type For All Recreational Fisheries", 
          fill = NULL)
  # geom_point(data = effortType_dat, aes(x=longpt, y = latpt), color = "blue", size = 3) # To test plot location only
  Flounder_pie <- Flounder_map + 
    geom_scatterpie(mapping=aes(x=longpt, y = latpt, group = cities, r = 0.7), # r determines radius of plots
                    data = effortType_dat, 
                    cols = c("partycharter", "party",        "charter",      "privaterent",  "shore"),
                    color = "black",
                    size = 0.25) + # Line thickness of pie chart
    scale_fill_manual(breaks = c("partycharter", "party", "charter", "privaterent", "shore"),
                      labels = c("Party/Charter", "Party", "Charter", "Private/Rented", "Shore"),
                      values = UseColors) + 
    labs (title = iyr, 
          fill = NULL) + 
    #coord_fixed() + 
    theme_minimal() + 
    theme(legend.position = c(0.96, 0.02),
          legend.justification = c(1, 0),
          panel.grid = element_blank()) + 
    geom_segment(aes(x = -69.9, y = 42.5, xend = -71.5, yend = 42.4), size=1) + # MA
    geom_segment(aes(x = -69.75, y = 40.5, xend = -71.5, yend = 41.39), size=1) + # RI
    geom_segment(aes(x = -71.6, y = 40.3, xend = -72.6, yend = 41.65), size=1) + # CT
    geom_segment(aes(x = -73.6, y = 39.5, xend = -74.8, yend = 39.9), size=1) + # NJ
    geom_segment(aes(x = -73.2, y = 38, xend = -75.5, yend = 38.9), size=1) + # DE
    geom_segment(aes(x = -75.3, y = 37.2, xend = -77, yend = 39.3), size=1) # MD
  Flounder_pie
  
  # Save file
  ggsave(paste("/Users/ahart2/Research/Summer_Flounder_MSE/Preliminary_Work/TotalRecEffortGIFPlots/",iyr, "EffortTypePlot.png", sep = "_"))
}

library(purrr)
library(magick)
# https://ryanpeek.github.io/2016-10-19-animated-gif_maps_in_R/
setwd("/Users/ahart2/Research/Summer_Flounder_MSE/Preliminary_Work/EffortGIFPlots")
list.files(path = "/Users/ahart2/Research/Summer_Flounder_MSE/EffortGIFPlots", pattern = "*.png", full.names = T) %>% # use pattern = "MA_19*" instead to look at all 1900s data
  purrr::map(.f = image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write(paste("EffortType.gif")) # write to current dir (very slow) (looks like PDF with multiple pages)

# I need this package installed to render gif on computer: http://www.imagemagick.org/script/index.php OR you just put in powerpoint full screen mode













# !!! This has problems when more than one state has missing data
# !!! Also has issues when data is missing but (NA means total harvest/prop. total harvest not calculated correctly)
##### Map with portion of effort by type & state plotted for only flounder recreational fisheries ##### !!! currently missing effort data, points show locations of pie graphs !!! coloring is inconsistent

TripModeDat <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Data_Files/mrip_SUMMER_FLOUNDER_catch_series_April18_2019.csv", skip = 37) # Query info at top of CSV file

# Still doesn't have NC data
for(iyr in DataYrs){
  iyr <- iyr
  UseColors <- NULL
  # All states, 1 year
  PartyCharterBoat <- NULL
  PartyCharterBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping, as.numeric(as.character()) gives warnings that NAs may be introduced by coercion
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Party_Charter_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(PartyCharterBoat$State)) < length(FullStateNames)-1  && length(unique(PartyCharterBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    PartyCharterBoat <- as.data.frame(PartyCharterBoat)
    MissingData <- cbind(FullStateNames[which(PartyCharterBoat$State != FullStateNames)[1]], as.integer(iyr), "Party_Charter_Boat", 0, 0)
    colnames(MissingData) <- colnames(PartyCharterBoat)
    PartyCharterBoat <- rbind(PartyCharterBoat[1:which(PartyCharterBoat$State!=FullStateNames)[1]-1,], MissingData, PartyCharterBoat[which(PartyCharterBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  partycharter <- PartyCharterBoat$PropModeHarvest
  UseColors <- c(UseColors, "dodgerblue") 
  if(empty(PartyCharterBoat)==TRUE){ # if no observations set prop=0
    partycharter <- rep(0, length(FullStateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  CharterBoat <- NULL
  CharterBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Charter_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH  CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(CharterBoat$State)) < length(FullStateNames)-1 & length(unique(CharterBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    CharterBoat <- as.data.frame(CharterBoat)
    MissingData <- cbind(FullStateNames[which(CharterBoat$State != FullStateNames)[1]], as.integer(iyr), "Charter_Boat", 0, 0)
    colnames(MissingData) <- colnames(CharterBoat)
    CharterBoat <- rbind(CharterBoat[1:which(CharterBoat$State!=FullStateNames)[1]-1,], MissingData, CharterBoat[which(CharterBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  charterboat <- CharterBoat$PropModeHarvest 
  UseColors <- c(UseColors, "chocolate2")
  if(empty(CharterBoat)==TRUE){ # if no observations set prop=0
    charterboat <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  PartyBoat <- NULL
  PartyBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Party_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  PartyBoat[is.na(PartyBoat)]<-0 # Replace NA measurements 
  if(length(unique(PartyBoat$State)) < length(FullStateNames)-1 & length(unique(PartyBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    PartyBoat <- as.data.frame(PartyBoat)
    MissingData <- cbind(FullStateNames[which(PartyBoat$State != FullStateNames)[1]], as.integer(iyr), "Party_Boat", 0, 0)
    colnames(MissingData) <- colnames(PartyBoat)
    PartyBoat <- rbind(PartyBoat[1:which(PartyBoat$State!=FullStateNames)[1]-1,], MissingData, PartyBoat[which(PartyBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  party <- PartyBoat$PropModeHarvest 
  UseColors <- c(UseColors, "darkorchid2")
  if(empty(PartyBoat)==TRUE){ # if no observations set prop=0
    party <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  PrivateRentBoat <- NULL
  PrivateRentBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Private_Rental_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(PrivateRentBoat$State)) < length(FullStateNames)-1 & length(unique(PrivateRentBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    PrivateRentBoat <- as.data.frame(PrivateRentBoat)
    MissingData <- cbind(FullStateNames[which(PrivateRentBoat$State != FullStateNames)[1]], as.integer(iyr), "Private_Rental_Boat", 0, 0)
    colnames(MissingData) <- colnames(PrivateRentBoat)
    PrivateRentBoat <- rbind(PrivateRentBoat[1:which(PrivateRentBoat$State!=FullStateNames)[1]-1,], MissingData, PrivateRentBoat[which(PrivateRentBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  privaterent <- PrivateRentBoat$PropModeHarvest 
  UseColors <- c(UseColors, "darkolivegreen3") 
  if(empty(PrivateRentBoat)==TRUE){ # if no observations set prop=0
    privaterent <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  Shore <- NULL
  Shore <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Shore", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(Shore$State)) < length(FullStateNames)-1 & length(unique(Shore$State)) != 0){ # If a state has missing data fill with zeros for that year
    Shore <- as.data.frame(Shore)
    MissingData <- cbind(FullStateNames[which(Shore$State != FullStateNames)[1]], as.integer(iyr), "Shore", 0, 0)
    colnames(MissingData) <- colnames(Shore)
    Shore <- rbind(Shore[1:which(Shore$State!=FullStateNames)[1]-1,], MissingData, Shore[which(Shore$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  shore <- Shore$PropModeHarvest 
  UseColors <- c(UseColors, "goldenrod1")
  if(empty(Shore)==TRUE){ # if no observations set prop=0
    shore <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  # city_long <- c(-69 , -69, -71, -75, -72.8, -72.4, -74.5, -78.5, -78.5)
  # city_lat <- c(42.5, 40, 39.6, 43, 39.5, 37.7, 37, 37.5, 35.4)
  # cities = StateNames[1:9]
  effortType_flounder_dat <- data.frame(
    partycharter = as.numeric(as.character(partycharter[!is.na(partycharter)])), 
    party = as.numeric(as.character(party[!is.na(party)])), 
    charter = as.numeric(as.character(charterboat[!is.na(charterboat)])),
    privaterent = as.numeric(as.character(privaterent[!is.na(privaterent)])),
    shore = as.numeric(as.character(shore[!is.na(shore)])),
    longpt = c(-69 , -69, -71, -75, -72.8, -72.4, -74.5, -78.5), #, -78.5), # fix so it excludes when state data not available (e.g. CT not available for i=1 but available later)
    latpt = c(42.5, 40, 39.6, 43, 39.5, 37.7, 37, 37.5), #, 35.4), 
    cities = StateNames[1:8]
  )
  Flounder_map <- ggplot() +
    geom_polygon(data = NAmerica, 
                 aes(x=long, y = lat, group = group), 
                 fill = "grey87", # Change polygone fill color
                 color="black") + 
    theme_minimal() + # Change theme (there are other options)   
    geom_polygon(data = FlounderStates,
                 aes(x = long, y = lat, group = group), 
                 fill = "grey54", 
                 color = "black") + 
    geom_polygon(data = AdjacentStates,
                 aes(x = long, y = lat, group = group), 
                 fill = "gray87", 
                 color = "black") + 
    coord_fixed(xlim = c(-84.2, -65),  ylim = c(33.5, 47), ratio = 1.2) + 
    labs (title = "Effort Type For All Recreational Fisheries", 
          fill = NULL)
  # geom_point(data = effortType_dat, aes(x=longpt, y = latpt), color = "blue", size = 3) # To test plot location only
  Flounder_pie <- Flounder_map + 
    geom_scatterpie(mapping=aes(x=longpt, y = latpt, group = cities, r = 0.7), # r determines radius of plots
                    data = effortType_flounder_dat, 
                    cols = c("partycharter", "party",        "charter",      "privaterent",  "shore"),
                    color = "black",
                    size = 0.25) + # Line thickness of pie chart
    scale_fill_manual(breaks = c("partycharter", "party", "charter", "privaterent", "shore"),
                      labels = c("Party/Charter", "Party", "Charter", "Private/Rented", "Shore"),
                      values = UseColors) + 
    labs (title = iyr, 
          fill = NULL) + 
    #coord_fixed() + 
    theme_minimal() + 
    theme(legend.position = c(0.96, 0.02),
          legend.justification = c(1, 0),
          panel.grid = element_blank()) + 
    geom_segment(aes(x = -69.9, y = 42.5, xend = -71.5, yend = 42.4), size=1) + # MA
    geom_segment(aes(x = -69.75, y = 40.5, xend = -71.5, yend = 41.39), size=1) + # RI
    geom_segment(aes(x = -71.6, y = 40.3, xend = -72.6, yend = 41.65), size=1) + # CT
    geom_segment(aes(x = -73.6, y = 39.5, xend = -74.8, yend = 39.9), size=1) + # NJ
    geom_segment(aes(x = -73.2, y = 38, xend = -75.5, yend = 38.9), size=1) + # DE
    geom_segment(aes(x = -75.3, y = 37.2, xend = -77, yend = 39.3), size=1) # MD
  Flounder_pie
  
  # Save file
  ggsave(paste("/Users/ahart2/Research/Summer_Flounder_MSE/FlounderRecEffortGIFPlots/",iyr, "FlounderEffortTypePlot.png", sep = "_"))
}

library(purrr)
library(magick)
# https://ryanpeek.github.io/2016-10-19-animated-gif_maps_in_R/
setwd("/Users/ahart2/Research/Summer_Flounder_MSE/EffortGIFPlots")
list.files(path = "/Users/ahart2/Research/Summer_Flounder_MSE/EffortGIFPlots", pattern = "*.png", full.names = T) %>% # use pattern = "MA_19*" instead to look at all 1900s data
  purrr::map(.f = image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write(paste("FlounderEffortType.gif"))





##### Map with portion of area type fished by state plotted for summer flounder recreational fisheries ##### !!! currently missing effort data, points show locations of pie graphs !!! coloring is inconsistent
for(iyr in DataYrs){
  iyr <- iyr
  UseColors <- NULL
  # All states, 1 year
  PartyCharterBoat <- NULL
  PartyCharterBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping, as.numeric(as.character()) gives warnings that NAs may be introduced by coercion
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Party_Charter_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(PartyCharterBoat$State)) < length(FullStateNames)-1  && length(unique(PartyCharterBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    PartyCharterBoat <- as.data.frame(PartyCharterBoat)
    MissingData <- cbind(FullStateNames[which(PartyCharterBoat$State != FullStateNames)[1]], as.integer(iyr), "Party_Charter_Boat", 0, 0)
    colnames(MissingData) <- colnames(PartyCharterBoat)
    PartyCharterBoat <- rbind(PartyCharterBoat[1:which(PartyCharterBoat$State!=FullStateNames)[1]-1,], MissingData, PartyCharterBoat[which(PartyCharterBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  partycharter <- PartyCharterBoat$PropModeHarvest
  UseColors <- c(UseColors, "dodgerblue") 
  if(empty(PartyCharterBoat)==TRUE){ # if no observations set prop=0
    partycharter <- rep(0, length(FullStateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  CharterBoat <- NULL
  CharterBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Charter_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH  CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(CharterBoat$State)) < length(FullStateNames)-1 & length(unique(CharterBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    CharterBoat <- as.data.frame(CharterBoat)
    MissingData <- cbind(FullStateNames[which(CharterBoat$State != FullStateNames)[1]], as.integer(iyr), "Charter_Boat", 0, 0)
    colnames(MissingData) <- colnames(CharterBoat)
    CharterBoat <- rbind(CharterBoat[1:which(CharterBoat$State!=FullStateNames)[1]-1,], MissingData, CharterBoat[which(CharterBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  charterboat <- CharterBoat$PropModeHarvest 
  UseColors <- c(UseColors, "chocolate2")
  if(empty(CharterBoat)==TRUE){ # if no observations set prop=0
    charterboat <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  PartyBoat <- NULL
  PartyBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Party_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(PartyBoat$State)) < length(FullStateNames)-1 & length(unique(PartyBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    PartyBoat <- as.data.frame(PartyBoat)
    MissingData <- cbind(FullStateNames[which(PartyBoat$State != FullStateNames)[1]], as.integer(iyr), "Party_Boat", 0, 0)
    colnames(MissingData) <- colnames(PartyBoat)
    PartyBoat <- rbind(PartyCharterBoat[1:which(PartyBoat$State!=FullStateNames)[1]-1,], MissingData, PartyBoat[which(PartyBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  party <- PartyBoat$PropModeHarvest 
  UseColors <- c(UseColors, "darkorchid2")
  if(empty(PartyBoat)==TRUE){ # if no observations set prop=0
    party <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  PrivateRentBoat <- NULL
  PrivateRentBoat <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Private_Rental_Boat", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(PrivateRentBoat$State)) < length(FullStateNames)-1 & length(unique(PrivateRentBoat$State)) != 0){ # If a state has missing data fill with zeros for that year
    PrivateRentBoat <- as.data.frame(PrivateRentBoat)
    MissingData <- cbind(FullStateNames[which(PrivateRentBoat$State != FullStateNames)[1]], as.integer(iyr), "Private_Rental_Boat", 0, 0)
    colnames(MissingData) <- colnames(PrivateRentBoat)
    PrivateRentBoat <- rbind(PrivateRentBoat[1:which(PrivateRentBoat$State!=FullStateNames)[1]-1,], MissingData, PrivateRentBoat[which(PrivateRentBoat$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  privaterent <- PrivateRentBoat$PropModeHarvest 
  UseColors <- c(UseColors, "darkolivegreen3") 
  if(empty(PrivateRentBoat)==TRUE){ # if no observations set prop=0
    privaterent <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  Shore <- NULL
  Shore <- TripModeDat %>%
    as_tibble() %>%
    dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
    mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
    mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
    mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
    group_by(State, Year, Fishing_Mode) %>%
    dplyr::summarise(TotModeHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>% # Each summary peels off 1 layer of grouping
    mutate(PropModeHarvest = TotModeHarvest/sum(TotModeHarvest)) %>%
    filter(Year == iyr, Fishing_Mode == "Shore", State != "NEW HAMPSHIRE", State != "MAINE", State != "NORTH CAROLINA")  %>%
    arrange(match(State, FullStateNames))
  if(length(unique(Shore$State)) < length(FullStateNames)-1 & length(unique(Shore$State)) != 0){ # If a state has missing data fill with zeros for that year
    Shore <- as.data.frame(Shore)
    MissingData <- cbind(FullStateNames[which(Shore$State != FullStateNames)[1]], as.integer(iyr), "Shore", 0, 0)
    colnames(MissingData) <- colnames(Shore)
    Shore <- rbind(Shore[1:which(Shore$State!=FullStateNames)[1]-1,], MissingData, Shore[which(Shore$State!=FullStateNames)[1]:length(FullStateNames),])
  }
  shore <- Shore$PropModeHarvest 
  UseColors <- c(UseColors, "goldenrod1")
  if(empty(Shore)==TRUE){ # if no observations set prop=0
    shore <- rep(0, length(StateNames)-1)
    UseColors <- UseColors[-length(UseColors)] # Don't include this color
  }
  # city_long <- c(-69 , -69, -71, -75, -72.8, -72.4, -74.5, -78.5, -78.5)
  # city_lat <- c(42.5, 40, 39.6, 43, 39.5, 37.7, 37, 37.5, 35.4)
  # cities = StateNames[1:9]
  effortType_flounder_dat <- data.frame(
    partycharter = as.numeric(as.character(partycharter[!is.na(partycharter)])), 
    party = as.numeric(as.character(party[!is.na(party)])), 
    charter = as.numeric(as.character(charterboat[!is.na(charterboat)])),
    privaterent = as.numeric(as.character(privaterent[!is.na(privaterent)])),
    shore = as.numeric(as.character(shore[!is.na(shore)])),
    longpt = c(-69 , -69, -71, -75, -72.8, -72.4, -74.5, -78.5), #, -78.5), # fix so it excludes when state data not available (e.g. CT not available for i=1 but available later)
    latpt = c(42.5, 40, 39.6, 43, 39.5, 37.7, 37, 37.5), #, 35.4), 
    cities = StateNames[1:8]
  )
  Flounder_map <- ggplot() +
    geom_polygon(data = NAmerica, 
                 aes(x=long, y = lat, group = group), 
                 fill = "grey87", # Change polygone fill color
                 color="black") + 
    theme_minimal() + # Change theme (there are other options)   
    geom_polygon(data = FlounderStates,
                 aes(x = long, y = lat, group = group), 
                 fill = "grey54", 
                 color = "black") + 
    geom_polygon(data = AdjacentStates,
                 aes(x = long, y = lat, group = group), 
                 fill = "gray87", 
                 color = "black") + 
    coord_fixed(xlim = c(-84.2, -65),  ylim = c(33.5, 47), ratio = 1.2) + 
    labs (title = "Effort Type For All Recreational Fisheries", 
          fill = NULL)
  # geom_point(data = effortType_dat, aes(x=longpt, y = latpt), color = "blue", size = 3) # To test plot location only
  Flounder_pie <- Flounder_map + 
    geom_scatterpie(mapping=aes(x=longpt, y = latpt, group = cities, r = 0.7), # r determines radius of plots
                    data = effortType_flounder_dat, 
                    cols = c("partycharter", "party",        "charter",      "privaterent",  "shore"),
                    color = "black",
                    size = 0.25) + # Line thickness of pie chart
    scale_fill_manual(breaks = c("partycharter", "party", "charter", "privaterent", "shore"),
                      labels = c("Party/Charter", "Party", "Charter", "Private/Rented", "Shore"),
                      values = UseColors) + 
    labs (title = iyr, 
          fill = NULL) + 
    #coord_fixed() + 
    theme_minimal() + 
    theme(legend.position = c(0.96, 0.02),
          legend.justification = c(1, 0),
          panel.grid = element_blank()) + 
    geom_segment(aes(x = -69.9, y = 42.5, xend = -71.5, yend = 42.4), size=1) + # MA
    geom_segment(aes(x = -69.75, y = 40.5, xend = -71.5, yend = 41.39), size=1) + # RI
    geom_segment(aes(x = -71.6, y = 40.3, xend = -72.6, yend = 41.65), size=1) + # CT
    geom_segment(aes(x = -73.6, y = 39.5, xend = -74.8, yend = 39.9), size=1) + # NJ
    geom_segment(aes(x = -73.2, y = 38, xend = -75.5, yend = 38.9), size=1) + # DE
    geom_segment(aes(x = -75.3, y = 37.2, xend = -77, yend = 39.3), size=1) # MD
  Flounder_pie
  
  # Save file
  ggsave(paste("/Users/ahart2/Research/Summer_Flounder_MSE/FlounderEffortGIFPlots/",iyr, "FlounderEffortTypePlot.png", sep = "_"))
}






# Plot trip area proportion spatially

for(iyr in DataYrs){
  iyr <- iyr
  UseColors <- NULL
}

  
  
Inland <- TripModeDat %>%
  as_tibble() %>%
  dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
  mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
  mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
  mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
  filter(Total_Harvest != "*") %>% # Remove rows which do not contain harvest/landings/PSE info, contain "*" instead
  group_by(State, Year, Fishing_Area) %>%
  dplyr::summarise(TotAreaHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>%
  mutate(PropAreaHarvest = TotAreaHarvest/sum(TotAreaHarvest)) %>%
  filter(Year == iyr, Fishing_Area == "Inland", State != "MAINE", State != "NEW HAMPSHIRE") %>%
  arrange(match(State, FullStateNames))
  UseColors <- c(UseColors, "goldenrod1")
Ocean_Less3MI <- TripModeDat %>%
  as_tibble() %>%
  dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
  mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
  mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
  mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
  filter(Total_Harvest != "*") %>% # Remove rows which do not contain harvest/landings/PSE info, contain "*" instead
  group_by(State, Year, Fishing_Area) %>%
  dplyr::summarise(TotAreaHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>%
  mutate(PropAreaHarvest = TotAreaHarvest/sum(TotAreaHarvest)) %>%
  filter(Year == iyr, Fishing_Area == "Ocean_Less3MI", State != "MAINE", State != "NEW HAMPSHIRE") %>%
  arrange(match(State, FullStateNames))
  UseColors <- c(UseColors, "darkolivegreen3")
Ocean_Greater3MI <- TripModeDat %>%
  as_tibble() %>%
  dplyr::select(everything(), Estimate_Status = Estimate.Status, Common_Name = Common.Name, Fishing_Mode = Fishing.Mode, Fishing_Area = Fishing.Area, Total_Harvest = Total.Harvest..A.B1., Landings = Landings..no...without.Size.Information, Harvest_Weight = Harvest..A.B1..Total.Weight..lb., PropStdError = PSE.1) %>% # Change column names
  mutate(Wave = factor(Wave, labels = c("July_August", "March_April", "May_June", "November_December", "September_October"))) %>% # Change factor names
  mutate(Fishing_Mode = factor(Fishing_Mode, labels = c("Charter_Boat", "Party_Boat", "Party_Charter_Boat", "Private_Rental_Boat", "Shore"))) %>%
  mutate(Fishing_Area = factor(Fishing_Area, labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"))) %>%
  filter(Total_Harvest != "*") %>% # Remove rows which do not contain harvest/landings/PSE info, contain "*" instead
  group_by(State, Year, Fishing_Area) %>%
  dplyr::summarise(TotAreaHarvest = sum(as.numeric(as.character(Total_Harvest)))) %>%
  mutate(PropAreaHarvest = TotAreaHarvest/sum(TotAreaHarvest)) %>%
  filter(Year == iyr, Fishing_Area == "Ocean_Greater3MI", State != "MAINE", State != "NEW HAMPSHIRE") %>%
  arrange(match(State, FullStateNames))
  UseColors <- c(UseColors, "dodgerblue")

FishingArea_flounder <- matrix(rep(0, length(FullStateNames)*6), ncol = 6, nrow = length(FullStateNames))
rownames(FishingArea_flounder) <- FullStateNames
colnames(FishingArea_flounder) <- c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI", "longpt", "latpt", "states")

# Fill matrix & turn into data.frame
FishingArea_flounder[match(Inland$State, rownames(FishingArea_flounder)), "Inland"] <- Inland$PropAreaHarvest
FishingArea_flounder[match(Ocean_Less3MI$State, rownames(FishingArea_flounder)), "Ocean_Less3MI"] <- Ocean_Less3MI$PropAreaHarvest
FishingArea_flounder[match(Ocean_Greater3MI$State, rownames(FishingArea_flounder)), "Ocean_Greater3MI"] <- Ocean_Greater3MI$PropAreaHarvest
FishingArea_flounder[,"longpt"] <- c(-69 , -69, -71, -75, -72.8, -72.4, -74.5, -78.5, -78.5)
FishingArea_flounder[,"latpt"] <- c(42.5, 40, 39.6, 43, 39.5, 37.7, 37, 37.5, 35.4)
FishingArea_flounder[,"states"] <- StateNames
FishingArea_flounder <- as.data.frame(FishingArea_flounder) # Maka a data.frame
FishingArea_flounder$Inland <- as.numeric(as.character(FishingArea_flounder$Inland))
FishingArea_flounder$Ocean_Less3MI <- as.numeric(as.character(FishingArea_flounder$Ocean_Less3MI))
FishingArea_flounder$Ocean_Greater3MI <- as.numeric(as.character(FishingArea_flounder$Ocean_Greater3MI))
FishingArea_flounder$latpt <- as.numeric(as.character(FishingArea_flounder$latpt))
FishingArea_flounder$longpt <- as.numeric(as.character(FishingArea_flounder$longpt))

Flounder_map <- ggplot() +
  geom_polygon(data = NAmerica, 
               aes(x=long, y = lat, group = group), 
               fill = "grey87", # Change polygone fill color
               color="black") + 
  theme_minimal() + # Change theme (there are other options)   
  geom_polygon(data = FlounderStates,
               aes(x = long, y = lat, group = group), 
               fill = "grey54", 
               color = "black") + 
  geom_polygon(data = AdjacentStates,
               aes(x = long, y = lat, group = group), 
               fill = "gray87", 
               color = "black") + 
  coord_fixed(xlim = c(-84.2, -65),  ylim = c(33.5, 47), ratio = 1.2) + 
  labs (title = "Effort Type For All Recreational Fisheries", 
        fill = NULL)
Flounder_pie <- Flounder_map + 
  geom_scatterpie(mapping=aes(x=longpt, y = latpt, group = states, r = 0.7), # r determines radius of plots
                  data = FishingArea_flounder, 
                  cols = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"),
                  color = "black",
                  size = 0.25) + # Line thickness of pie chart
  scale_fill_manual(breaks = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"),
                    labels = c("Inland", "Ocean_Less3MI", "Ocean_Greater3MI"),
                    values = UseColors) + 
  labs (title = iyr, 
        fill = NULL) + 
  #coord_fixed() + 
  theme_minimal() + 
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        panel.grid = element_blank()) + 
  geom_segment(aes(x = -69.9, y = 42.5, xend = -71.5, yend = 42.4), size=1) + # MA
  geom_segment(aes(x = -69.75, y = 40.5, xend = -71.5, yend = 41.39), size=1) + # RI
  geom_segment(aes(x = -71.6, y = 40.3, xend = -72.6, yend = 41.65), size=1) + # CT
  geom_segment(aes(x = -73.6, y = 39.5, xend = -74.8, yend = 39.9), size=1) + # NJ
  geom_segment(aes(x = -73.2, y = 38, xend = -75.5, yend = 38.9), size=1) + # DE
  geom_segment(aes(x = -75.3, y = 37.2, xend = -77, yend = 39.3), size=1) # MD
Flounder_pie

# Save file
ggsave(paste("/Users/ahart2/Research/Summer_Flounder_MSE/FlounderEffortGIFPlots/",iyr, "FlounderEffortTypePlot.png", sep = "_"))


