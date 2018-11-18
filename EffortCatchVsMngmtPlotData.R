##### Plot catch & effort data against management for Summer Flounder Project 2nd Attempt #####

FileLocation <- c("/Users/ahart2/Research/Summer_Flounder_MSE")

effortData <- read.csv(paste(FileLocation, "Effort.csv",sep="/"))
effortData <- effortData[,1:length(effortData)-1]
colnames(effortData) <- c("ID",	"Year",	"Wave",	"State",	"NumTrips",	"BagSz", "MinSzIn", "SeasonDz")

lengthData <- read.csv(paste(FileLocation, "CatchAtLength.csv",sep="/"))
colnames(lengthData) <- c("ID",	"Year",	"Wave",	"State",	"ForkLengthIn",	"LandingsNum", "BagSz",	"MinSzIn", "SeasonDz")

setwd(paste(FileLocation, "Data_Plots", sep="/"))

StateList <- c("VA", "CT", "DE", "MD", "MA", "NJ", "NY", "NC", "RI")
StateNames <- c("Virgina", "Connecticut", "Delaware", "Maryland", "Massachusetts", "New Jersey", "New York", "North Carolina", "Rhode Island")

for(istate in 1:length(StateList)){
  
  ############################################################################################################
  #################################### Effort vs. Management #################################################
  ############################################################################################################
  
  ############################################################################################################
  ##### Effort vs. Bag size #####
  ############################################################################################################
  # Effort data
  EffortVec <- rep(NA, length(1993:2017))
  for(iyr in 1993:2017){
    EffortVec[iyr-1992] <- sum(effortData[which(effortData[,"State"]==StateList[istate] & effortData[,"Year"]==iyr),"NumTrips"])
  }
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
  
  png(filename = paste(StateList[istate], "TotalEffort_vs_BagSize_Wave2.png", sep="_"))
  plot(x= Wave2, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Bag Size Wave 2", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_BagSize_Wave3.png", sep="_"))
  plot(x= Wave3, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Bag Size Wave 3", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_BagSize_Wave4.png", sep="_"))
  plot(x= Wave4, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Bag Size Wave 4", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_BagSize_Wave5.png", sep="_"))
  plot(x= Wave5, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Bag Size Wave 5", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_BagSize_Wave6.png", sep="_"))
  plot(x= Wave6, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Bag Size Wave 6", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Bag Size")
  dev.off()
  
  
  ############################################################################################################
  ##### Effort vs. Season Length #####
  ############################################################################################################
  seasonDataLength <- rep(NA, length(unique(lengthData$Year)))
  for(iyr in 1993:2017){
    seasonDataLength[iyr-1992] <- sum(unique(lengthData[which(lengthData[,"State"]==StateList[istate] & lengthData$Year==iyr),"SeasonDz"]))
  }
  
  png(filename = paste(StateList[istate], "TotalEffort_vs_SeasonLength.png", sep="_"))
  plot(x=seasonDataLength,
       y=EffortVec,
       main=paste(StateNames[istate], "Effort vs. Season Length", sep=" "),
       ylab="Effort (Number of Trips)",
       xlab="Minimum Size")
  dev.off()
  
  
  
  
  
  ############################################################################################################
  ##### Effort vs. Minimum size #####
  ############################################################################################################
  # Minimum size
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
  
  png(filename = paste(StateList[istate], "TotalEffort_vs_MinSize_Wave2.png", sep="_"))
  plot(x= Wave2, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Minimum Size Wave 2", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Minimum Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_MinSize_Wave3.png", sep="_"))
  plot(x= Wave3, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Minimum Size Wave 3", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Minimum Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_MinSize_Wave4.png", sep="_"))
  plot(x= Wave4, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Minimum Size Wave 4", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Minimum Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_MinSize_Wave5.png", sep="_"))
  plot(x= Wave5, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Minimum Size Wave 5", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Minimum Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalEffort_vs_MinSize_Wave6.png", sep="_"))
  plot(x= Wave6, 
       y= EffortVec,
       main=paste(StateNames[istate], "Effort vs. Minimum Size Wave 6", sep=" "),
       ylab = "Effort (Number of Trips)",
       xlab= "Minimum Size")
  dev.off()
  
  
  
  ############################################################################################################
  #################################### Catch vs. Management #################################################
  ############################################################################################################
  TotalLandings <- rep(0, length(unique(lengthData$Year)))
  for(iyr in 1993:2017){
    TotalLandings[iyr-1992] <- sum(lengthData[which(lengthData$Year==iyr & lengthData$State==StateList[istate]),"LandingsNum"]) # sum across all fork length and waves
  }
  
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
  
  png(filename = paste(StateList[istate], "TotalLandings_vs_BagSize_Wave2.png", sep="_"))
  plot(x= Wave2, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Bag Size Wave 2", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalLandings_vs_BagSize_Wave3.png", sep="_"))
  plot(x= Wave3, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Bag Size Wave 3", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalLandings_vs_BagSize_Wave4.png", sep="_"))
  plot(x= Wave4, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Bag Size Wave 4", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalLandings_vs_BagSize_Wave5.png", sep="_"))
  plot(x= Wave5, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Bag Size Wave 5", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalLandings_vs_BagSize_Wave6.png", sep="_"))
  plot(x= Wave6, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Bag Size Wave 6", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  
  
  ############################################################################################################
  ##### Landings vs. Season Length #####
  ############################################################################################################
  seasonDataLength <- rep(NA, length(unique(lengthData$Year)))
  for(iyr in 1993:2017){
    seasonDataLength[iyr-1992] <- sum(unique(lengthData[which(lengthData[,"State"]==StateList[istate] & lengthData$Year==iyr),"SeasonDz"]))
  }
  
  png(filename = paste(StateList[istate], "TotalLandings_vs_SeasonLength.png", sep="_"))
  plot(x=seasonDataLength,
       y=TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Season Length", sep=" "),
       ylab="Total Landings",
       xlab="Minimum Size")
  dev.off()
  
  ############################################################################################################
  ##### Landings vs. Minimum size #####
  ############################################################################################################
  # Minimum size
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
  
  png(filename = paste(StateList[istate], "TotalLandings_vs_MinSize_Wave2.png", sep="_"))
  plot(x= Wave2, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Minimum Size Wave 2", sep=" "),
       ylab = "Total Landings",
       xlab= "Minimum Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalLandings_vs_MinSize_Wave3.png", sep="_"))
  plot(x= Wave3, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Minimum Size Wave 3", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalLandings_vs_MinSize_Wave4.png", sep="_"))
  plot(x= Wave4, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Minimum Size Wave 4", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "TotalLandings_vs_MinSize_Wave5.png", sep="_"))
  plot(x= Wave5, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Minimum Size Wave 5", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  png(filename = paste(StateList[istate], "MA_TotalLandings_vs_MinSize_Wave6.png", sep="_"))
  plot(x= Wave6, 
       y= TotalLandings,
       main=paste(StateNames[istate], "Landings vs. Minimum Size Wave 6", sep=" "),
       ylab = "Total Landings",
       xlab= "Bag Size")
  dev.off()
  
  
}






