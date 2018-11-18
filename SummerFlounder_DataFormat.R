# Format and save data for summer flounder model fitting

FileLocation <- c("/Users/ahart2/Research/Summer_Flounder_MSE")

setwd(FileLocation)

# Read in data here
effortData <- read.csv(paste(FileLocation, "Data_Files/Effort_NoClosed.csv",sep="/"))
# effortData <- effortData[,1:length(effortData)-1]
colnames(effortData) <- c("ID",	"Year",	"Wave",	"State",	"NumTrips",	"BagSz", "MinSzIn", "SeasonDz")

lengthData <- read.csv(paste(FileLocation, "Data_Files/CatchAtLength_NoClosed.csv",sep="/"))
colnames(lengthData) <- c("ID",	"Year",	"Wave",	"State",	"ForkLengthIn",	"LandingsNum", "BagSz",	"MinSzIn", "SeasonDz", "TimePeriod")

StateList <- c("VA", "CT", "DE", "MD", "MA", "NJ", "NY", "NC", "RI")
StateNames <- c("Virgina", "Connecticut", "Delaware", "Maryland", "Massachusetts", "New Jersey", "New York", "North Carolina", "Rhode Island")

FileLocation <- c("/Users/ahart2/Research/Summer_Flounder_MSE/Data_FormattedMatrices")
setwd(FileLocation)

# Produce state-specific data sets
for(istate in 1:length(StateList)){
  # Total landings at size for year
  TotalLandings <- rep(0, length(unique(lengthData$Year)))
  for(iyr in 1993:2017){
    TotalLandings[iyr-1992] <- sum(lengthData[which(lengthData$Year==iyr & lengthData$State==StateList[istate]),"LandingsNum"]) # sum across all fork length and waves
  }
  
  # Effort data
  Years <- c(1993:2017)
  EffortVec <- rep(NA, length(1993:2017))
  for(iyr in 1993:2017){
    EffortVec[iyr-1992] <- sum(effortData[which(effortData[,"State"]==StateList[istate] & effortData[,"Year"]==iyr),"NumTrips"])
  }
  
  ####################################################################################################################################
  ##### Effort vs. Bag size #####
  ####################################################################################################################################
  # Bag size
  Wave2 <- rep(NA, length(unique(effortData$Year)))
  Wave3 <- rep(NA, length(unique(effortData$Year)))
  Wave4 <- rep(NA, length(unique(effortData$Year)))
  Wave5 <- rep(NA, length(unique(effortData$Year)))
  Wave6 <- rep(NA, length(unique(effortData$Year)))
  for(iyr in 1993:2017){ # 
    Wave2[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==2),"BagSz"][1]
    Wave3[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==3),"BagSz"][1]
    Wave4[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==4),"BagSz"][1]
    Wave5[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==5),"BagSz"][1]
    Wave6[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==6),"BagSz"][1]
  }
  
  EffortBagData <- cbind(Years, EffortVec, Wave2, Wave3, Wave4, Wave5, Wave6)
  colnames(EffortBagData) <- c("Years", paste(StateList[istate],"Effort", sep="_"), "Wave2Bag", "Wave3Bag", "Wave4Bag", "Wave5Bag", "Wave6Bag")
  
  write.csv(EffortBagData, file=paste(FileLocation, paste(StateList[istate], "Effort_vs_Bag_Data", sep="_"), sep="/"))
  
  
  ############################################################################################################
  ##### Effort vs. Season Length #####
  ############################################################################################################
  seasonDataLength <- rep(NA, length(unique(lengthData$Year)))
  for(iyr in 1993:2017){
    seasonDataLength[iyr-1992] <- sum(unique(lengthData[which(lengthData[,"State"]==StateList[istate] & lengthData$Year==iyr),"SeasonDz"]))
  }
  
  EffortSeasonData <- cbind(Years, EffortVec, seasonDataLength)
  colnames(EffortSeasonData) <- c("Years", paste(StateList[istate],"Effort", sep="_"), "Season_Length")
  
  write.csv(EffortSeasonData, file=paste(FileLocation, paste(StateList[istate], "Effort_vs_Season_Data", sep="_"), sep="/"))
  
  
  ############################################################################################################
  ##### Effort vs. Minimum size #####
  ############################################################################################################
  Wave2 <- rep(0, length(unique(effortData$Year)))
  Wave3 <- rep(0, length(unique(effortData$Year)))
  Wave4 <- rep(0, length(unique(effortData$Year)))
  Wave5 <- rep(0, length(unique(effortData$Year)))
  Wave6 <- rep(0, length(unique(effortData$Year)))
  for(iyr in 1993:2017){ # 
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==2),"MinSzIn"])==0){
      Wave2[iyr-1992] <- NA
    } else{
      Wave2[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==2),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==3),"MinSzIn"])==0){
      Wave3[iyr-1992] <- NA
    } else{
      Wave3[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==3),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==4),"MinSzIn"])==0){
      Wave4[iyr-1992] <- NA
    } else{
      Wave4[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==4),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==5),"MinSzIn"])==0){
      Wave5[iyr-1992] <- NA
    } else{
      Wave5[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==5),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==6),"MinSzIn"])==0){
      Wave6[iyr-1992] <- NA
    } else{
      Wave6[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==6),"MinSzIn"]
    }
  }
  
  EffortMinSzData <- cbind(Years, EffortVec, Wave2, Wave3, Wave4, Wave5, Wave6)
  colnames(EffortMinSzData) <- c("Years", paste(StateList[istate],"Effort", sep="_"), "Wave2MinSz", "Wave3MinSz", "Wave4MinSz", "Wave5MinSz", "Wave6MinSz")
  
  write.csv(EffortMinSzData, file=paste(FileLocation, paste(StateList[istate], "Effort_vs_MinSz_Data", sep="_"), sep="/"))
  
  
  ############################################################################################################
  ##### Catch vs. Bag size #####
  ############################################################################################################
  # Bag size
  Wave2 <- rep(NA, length(unique(lengthData$Year)))
  Wave3 <- rep(NA, length(unique(lengthData$Year)))
  Wave4 <- rep(NA, length(unique(lengthData$Year)))
  Wave5 <- rep(NA, length(unique(lengthData$Year)))
  Wave6 <- rep(NA, length(unique(lengthData$Year)))
  for(iyr in 1993:2017){ # 
    Wave2[iyr-1992] <- lengthData[which(lengthData$State==StateList[istate] & lengthData$Year==iyr & lengthData$Wave==2),"BagSz"][1]
    Wave3[iyr-1992] <- lengthData[which(lengthData$State==StateList[istate] & lengthData$Year==iyr & lengthData$Wave==3),"BagSz"][1]
    Wave4[iyr-1992] <- lengthData[which(lengthData$State==StateList[istate] & lengthData$Year==iyr & lengthData$Wave==4),"BagSz"][1]
    Wave5[iyr-1992] <- lengthData[which(lengthData$State==StateList[istate] & lengthData$Year==iyr & lengthData$Wave==5),"BagSz"][1]
    Wave6[iyr-1992] <- lengthData[which(lengthData$State==StateList[istate] & lengthData$Year==iyr & lengthData$Wave==6),"BagSz"][1]
  }
  
  CatchBagData <- cbind(Years, TotalLandings, Wave2, Wave3, Wave4, Wave5, Wave6)
  colnames(CatchBagData) <- c("Years", paste(StateList[istate],"Catch", sep="_"), "Wave2Bag", "Wave3Bag", "Wave4Bag", "Wave5Bag", "Wave6Bag")
  
  write.csv(CatchBagData, file=paste(FileLocation, paste(StateList[istate], "Catch_vs_Bag_Data", sep="_"), sep="/"))
  
  
  ############################################################################################################
  ##### Landings vs. Season Length #####
  ############################################################################################################
  seasonDataLength <- rep(NA, length(unique(lengthData$Year)))
  for(iyr in 1993:2017){
    seasonDataLength[iyr-1992] <- sum(unique(lengthData[which(lengthData[,"State"]==StateList[istate] & lengthData$Year==iyr),"SeasonDz"]))
  }
  
  CatchSeasonData <- cbind(Years, TotalLandings, seasonDataLength)
  colnames(CatchSeasonData) <- c("Years", paste(StateList[istate],"Catch", sep="_"), "Season_Length")
  
  write.csv(CatchSeasonData, file=paste(FileLocation, paste(StateList[istate], "Catch_vs_Season_Data", sep="_"), sep="/"))
  
  
  ############################################################################################################
  ##### Landings vs. Minimum size #####
  ############################################################################################################
  Wave2 <- rep(0, length(unique(effortData$Year)))
  Wave3 <- rep(0, length(unique(effortData$Year)))
  Wave4 <- rep(0, length(unique(effortData$Year)))
  Wave5 <- rep(0, length(unique(effortData$Year)))
  Wave6 <- rep(0, length(unique(effortData$Year)))
  for(iyr in 1993:2017){ # 
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==2),"MinSzIn"])==0){
      Wave2[iyr-1992] <- NA
    } else{
      Wave2[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==2),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==3),"MinSzIn"])==0){
      Wave3[iyr-1992] <- NA
    } else{
      Wave3[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==3),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==4),"MinSzIn"])==0){
      Wave4[iyr-1992] <- NA
    } else{
      Wave4[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==4),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==5),"MinSzIn"])==0){
      Wave5[iyr-1992] <- NA
    } else{
      Wave5[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==5),"MinSzIn"]
    }
    if(length(effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==6),"MinSzIn"])==0){
      Wave6[iyr-1992] <- NA
    } else{
      Wave6[iyr-1992] <- effortData[which(effortData$State==StateList[istate] & effortData$Year==iyr & effortData$Wave==6),"MinSzIn"]
    }
  }
  
  CatchMinSzData <- cbind(Years, TotalLandings, Wave2, Wave3, Wave4, Wave5, Wave6)
  colnames(CatchMinSzData) <- c("Years", paste(StateList[istate],"Catch", sep="_"), "Wave2MinSz", "Wave3MinSz", "Wave4MinSz", "Wave5MinSz", "Wave6MinSz")
  
  write.csv(CatchMinSzData, file=paste(FileLocation, paste(StateList[istate], "Catch_vs_MinSz_Data", sep="_"), sep="/"))
}



#############################################################################
# Combine state-specific datasets into a region-wide dataset
#############################################################################

RegionData_Catch_vs_Bag <- NULL
RegionData_Effort_vs_Bag <- NULL
RegionData_Catch_vs_MinSz <- NULL
RegionData_Effort_vs_MinSz <- NULL
RegionData_Catch_vs_Season <- NULL
RegionData_Effort_vs_Season <- NULL

for(istate in 1:length(StateList)){
  # Catch vs Bag
  tempStateData <- read.csv(paste(FileLocation, paste(StateList[istate], "Catch_vs_Bag_Data", sep="_"), sep="/"))
  tempStateData <- tempStateData[,-1]
  tempStateData <- cbind(tempStateData, StateList[istate])
  colnames(tempStateData)[which(colnames(tempStateData) == paste(StateList[istate], "Catch", sep="_"))] <- "Catch"
  colnames(tempStateData)[ncol(tempStateData)] <- "StateID"
  RegionData_Catch_vs_Bag <- rbind(RegionData_Catch_vs_Bag, tempStateData)
  
  # Effort vs Bag
  tempStateData <- read.csv(paste(FileLocation, paste(StateList[istate], "Effort_vs_Bag_Data", sep="_"), sep="/"))
  tempStateData <- tempStateData[,-1]
  tempStateData <- cbind(tempStateData, StateList[istate])
  colnames(tempStateData)[which(colnames(tempStateData) == paste(StateList[istate], "Effort", sep="_"))] <- "Effort"
  colnames(tempStateData)[ncol(tempStateData)] <- "StateID"
  RegionData_Effort_vs_Bag <- rbind(RegionData_Effort_vs_Bag, tempStateData)
  
  # Catch vs MinSz
  tempStateData <- read.csv(paste(FileLocation, paste(StateList[istate], "Catch_vs_MinSz_Data", sep="_"), sep="/"))
  tempStateData <- tempStateData[,-1]
  tempStateData <- cbind(tempStateData, StateList[istate])
  colnames(tempStateData)[which(colnames(tempStateData) == paste(StateList[istate], "Catch", sep="_"))] <- "Catch"
  colnames(tempStateData)[ncol(tempStateData)] <- "StateID"
  RegionData_Catch_vs_MinSz <- rbind(RegionData_Catch_vs_MinSz, tempStateData)
  
  # Effort vs MinSz
  tempStateData <- read.csv(paste(FileLocation, paste(StateList[istate], "Effort_vs_MinSz_Data", sep="_"), sep="/"))
  tempStateData <- tempStateData[,-1]
  tempStateData <- cbind(tempStateData, StateList[istate])
  colnames(tempStateData)[which(colnames(tempStateData) == paste(StateList[istate], "Effort", sep="_"))] <- "Effort"
  colnames(tempStateData)[ncol(tempStateData)] <- "StateID"
  RegionData_Effort_vs_MinSz <- rbind(RegionData_Effort_vs_MinSz, tempStateData)
  
  # Catch vs Season
  tempStateData <- read.csv(paste(FileLocation, paste(StateList[istate], "Catch_vs_Season_Data", sep="_"), sep="/"))
  tempStateData <- tempStateData[,-1]
  tempStateData <- cbind(tempStateData, StateList[istate])
  colnames(tempStateData)[which(colnames(tempStateData) == paste(StateList[istate], "Catch", sep="_"))] <- "Catch"
  colnames(tempStateData)[ncol(tempStateData)] <- "StateID"
  RegionData_Catch_vs_Season <- rbind(RegionData_Catch_vs_Season, tempStateData)
  
  # Effort vs Season
  tempStateData <- read.csv(paste(FileLocation, paste(StateList[istate], "Effort_vs_Season_Data", sep="_"), sep="/"))
  tempStateData <- tempStateData[,-1]
  tempStateData <- cbind(tempStateData, StateList[istate])
  colnames(tempStateData)[which(colnames(tempStateData) == paste(StateList[istate], "Effort", sep="_"))] <- "Effort"
  colnames(tempStateData)[ncol(tempStateData)] <- "StateID"
  RegionData_Effort_vs_Season <- rbind(RegionData_Effort_vs_Season, tempStateData)
  
}

# write regional data to files !!!!!!!!!!!!!
colnames(RegionData_Catch_vs_Bag)[which(colnames(RegionData_Catch_vs_Bag)=="Catch")] <- "Region_Catch"
write.csv(RegionData_Catch_vs_Bag, file=paste(FileLocation, "Region_Catch_vs_Bag_Data", sep="/"))

colnames(RegionData_Effort_vs_Bag)[which(colnames(RegionData_Effort_vs_Bag)=="Effort")] <- "Region_Effort"
write.csv(RegionData_Effort_vs_Bag, file=paste(FileLocation, "Region_Effort_vs_Bag_Data", sep="/"))

colnames(RegionData_Catch_vs_MinSz)[which(colnames(RegionData_Catch_vs_MinSz)=="Catch")] <- "Region_Catch"
write.csv(RegionData_Catch_vs_MinSz, file=paste(FileLocation, "Region_Catch_vs_MinSz_Data", sep="/"))

colnames(RegionData_Effort_vs_MinSz)[which(colnames(RegionData_Effort_vs_MinSz)=="Effort")] <- "Region_Effort"
write.csv(RegionData_Effort_vs_MinSz, file=paste(FileLocation, "Region_Effort_vs_MinSz_Data", sep="/"))

colnames(RegionData_Catch_vs_Season)[which(colnames(RegionData_Catch_vs_Season)=="Catch")] <- "Region_Catch"
write.csv(RegionData_Catch_vs_Season, file=paste(FileLocation, "Region_Catch_vs_Season_Data", sep="/"))

colnames(RegionData_Effort_vs_Season)[which(colnames(RegionData_Effort_vs_Season)=="Effort")] <- "Region_Effort"
write.csv(RegionData_Effort_vs_Season, file=paste(FileLocation, "Region_Effort_vs_Season_Data", sep="/"))




