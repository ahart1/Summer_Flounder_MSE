##### Plot data for Summer Flounder Project #####

effortData <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/Effort.csv",sep=",")
effortData <- effortData[,1:length(effortData)-1]
colnames(effortData) <- c("ID",	"Year",	"Wave",	"State",	"NumTrips",	"BagSz", "MinSzIn", "SeasonDz")

lengthData <- read.csv("/Users/ahart2/Research/Summer_Flounder_MSE/CatchAtLength.csv",sep=",")
colnames(lengthData) <- c("ID",	"Year",	"Wave",	"State",	"ForkLengthIn",	"LandingsNum", "BagSz",	"MinSzIn", "SeasonDz")


# For each state I want to look at:
  ##########################################################################################################
  ##### Total landings over years #####
  ##########################################################################################################
    # sum landings at size for year
   TotalLandings <- rep(0, length(unique(lengthData$Year)))
   for(iyr in 1993:2017){
     TotalLandings[iyr-1992] <- sum(lengthData[which(lengthData$Year==iyr & lengthData$State=="MA"),"LandingsNum"]) # sum across all fork length and waves
   }
plot(x= unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]), 
     y= TotalLandings,
     main="Massachusetts Total Landings",
     ylab = "Total landings",
     xlab= "Year",
     type="l")

plot(x= unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]), 
     y= TotalLandings,
     main="Massachusetts Total Landings",
     ylab = "Total landings",
     xlab= "Year",
     col=unique(lengthData$MinSzIn), pch=16)
lines(x=unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
      y=TotalLandings)
legend("topright",
       legend=unique(lengthData$MinSzIn),
       fill=unique(lengthData$MinSzIn), ncol=2)
   
EffortVec <- rep(0, length(1993:2017))
for(iyr in 1993:2017){
  EffortVec[iyr-1992] <- sum(effortData[which(effortData[,"State"]=="MA" & effortData[,"Year"]==iyr),"NumTrips"])
}
plot(x= unique(effortData[which(effortData[,"State"]=="MA"),"Year"]), 
     y= EffortVec,
     main="Massachusetts Effort",
     ylab = "Number of Trips",
     xlab= "Year",
     type="l")
  ##########################################################################################################
  ##### Total landings by wave over years #####
  ##########################################################################################################
   TotalLandings_Wave2 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Wave3 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Wave4 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Wave5 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Wave6 <- rep(0, length(unique(lengthData$Year)))
   for(iyr in 1993:2017){ # 
     TotalLandings_Wave2[iyr-1992] <- sum(lengthData[ which(lengthData$Wave==2 & lengthData$Year==iyr & lengthData$State=="MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Wave3[iyr-1992] <- sum(lengthData[ which(lengthData$Wave==3 & lengthData$Year==iyr & lengthData$State=="MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Wave4[iyr-1992] <- sum(lengthData[ which(lengthData$Wave==4 & lengthData$Year==iyr & lengthData$State=="MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Wave5[iyr-1992] <- sum(lengthData[ which(lengthData$Wave==5 & lengthData$Year==iyr & lengthData$State=="MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Wave6[iyr-1992] <- sum(lengthData[ which(lengthData$Wave==6 & lengthData$Year==iyr & lengthData$State=="MA"),"LandingsNum"]) # sum across all fork length and waves
   }
   maxY <- max(c(TotalLandings_Wave2, TotalLandings_Wave3, TotalLandings_Wave4, TotalLandings_Wave5, TotalLandings_Wave6))
   plot(x= unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]), 
        y= TotalLandings_Wave2,
        main="Massachusetts Landings by Wave",
        ylab = "Total landings",
        xlab= "Year",
        type="l",
        col="lightblue",
        ylim = c(0,maxY))
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Wave3, col="red")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Wave4, col="darkgreen")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Wave5, col="blue")   
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Wave6, col="black")
   legend("topright",
          legend=c("Mar-Apr", "May-Jun", "July-August", "Sept-Oct", "Nov-Dec"),
          fill=c("lightblue", "red", "darkgreen", "blue", "black"))
   
  ##########################################################################################################
  ##### Total landings by bag size over years #####
  ##########################################################################################################
   TotalLandings_Bag0 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag2 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag3 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag4 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag5 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag6 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag7 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag8 <- rep(0, length(unique(lengthData$Year)))
   TotalLandings_Bag10 <- rep(0, length(unique(lengthData$Year)))
   for(iyr in 1993:2017){
     TotalLandings_Bag0[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==0 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag2[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==2 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag3[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==3 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag4[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==4 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag5[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==5 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag6[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==6 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag7[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==7 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag8[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==8 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
     TotalLandings_Bag10[iyr-1992] <-  sum(lengthData[which(lengthData$BagSz==10 & lengthData$Year==iyr & lengthData$State == "MA"),"LandingsNum"]) # sum across all fork length and waves
   }
   maxY <- max(c(TotalLandings_Bag0, TotalLandings_Bag2, TotalLandings_Bag3, TotalLandings_Bag4, TotalLandings_Bag5, TotalLandings_Bag6, TotalLandings_Bag7, TotalLandings_Bag8, TotalLandings_Bag10))
   plot(x= unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]), 
        y= TotalLandings_Bag0,
        main="Massachusetts Landings by Bag Size",
        ylab = "Total landings",
        xlab= "Year",
        type="l",
        col="lightblue",
        ylim = c(0,maxY))
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag2, col="red")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag3, col="darkgreen")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag4, col="blue")   
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag5, col="black")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag6, col="purple")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag7, col="lightgreen")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag8, col="orange")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_Bag10, col="pink")
   legend("topright",
          legend=c("0 Bag size", "2 Bag size", "3 Bag size", "4 Bag size", "5 Bag size", "6 Bag size", "7 Bag size", "8 Bag size", "10 Bag size"),
          fill=c("lightblue", "red", "darkgreen", "blue", "black", "purple", "lightgreen", "orange", "pink"))
   
  ##########################################################################################################
  ##### Total landings by min size over years #####
  ##########################################################################################################
   for(isz in unique(lengthData$MinSzIn)){
     LandingsVec <- rep(0, length(unique(lengthData$Year)))
     
     for(iyr in 1993:2017){
       LandingsVec[iyr-1992] <- sum(lengthData[which(lengthData$MinSzIn==isz & lengthData$Year==iyr & lengthData$State == "CT"),"LandingsNum"]) 
     }
     assign(paste("TotalLandings_MinSz", isz, sep=""), LandingsVec)
   }
   
   maxY <- max(c(TotalLandings_MinSz0, TotalLandings_MinSz14, TotalLandings_MinSz14.5, TotalLandings_MinSz15, TotalLandings_MinSz15.5,
                 TotalLandings_MinSz16, TotalLandings_MinSz16.5, TotalLandings_MinSz17, TotalLandings_MinSz17.5, TotalLandings_MinSz17.5,
                 TotalLandings_MinSz18, TotalLandings_MinSz18.5, TotalLandings_MinSz19, TotalLandings_MinSz19.5, TotalLandings_MinSz20,
                 TotalLandings_MinSz20.5, TotalLandings_MinSz21))
   plot(x= unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]), 
        y= TotalLandings_MinSz0,
        main="Massachusetts Landings by Minimum Size",
        ylab = "Total landings",
        xlab= "Year",
        #type="l",
        col="lightblue",
        ylim = c(0,maxY))
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz14, col="red")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz14.5, col="aquamarine")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz15, col="aquamarine4")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz15.5, col="blue")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz16, col="chocolate3")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz16.5, col="coral1")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz17, col="blueviolet")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz17.5, col="darkmagenta")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz18, col="darkorange")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz18.5, col="firebrick1")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz19, col="darkgreen")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz19.5, col="darkolivegreen2")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz20, col="deeppink")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz20.5, col="goldenrod1")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
         y = TotalLandings_MinSz21, col="black")
   legend("topright",
          legend=c("0 inches", "14 inches", "14.5 inches", "15 inches", "15.5 inches", "16 inches", "16.5 inches",
                   "17 inches", "17.5 inches", "18 inches", "18.5 inches", "19 inches", "19.5 inches", "20 inches",
                   "20.5 inches", "21 inches"),
          ncol = 2,
          fill=c("lightblue", "red", "aquamarine", "aquamarine4",
                 "blue", "chocolate3", "coral1", "blueviolet", "darkmagenta",
                 "darkorange", "firebrick1", "darkgreen", "darkolivegreen2",
                 "deeppink", "goldenrod1",  "black"))
   
   
  ##########################################################################################################
  ##### Total landings by Season length over years ##### !!!!!!!!!!!!! Broken
  ##########################################################################################################
   season <- c(0,7,14,21,28,35,42,49,56,63,70)
   for(iday in 1:length(season)){ # season length in 7 day intervals 
     LandingsVec <- rep(0, length(unique(lengthData$Year)))
     
     for(iyr in 1993:2017){
       if(iday == 0){
         LandingsVec[iyr-1992] <- sum(lengthData[which(lengthData$SeasonDz <= season[iday] & lengthData$Year==iyr & lengthData$State == "CT"),"LandingsNum"]) 
       } else{
         LandingsVec[iyr-1992] <- sum(lengthData[which(lengthData$SeasonDz <= season[iday] & lengthData$SeasonDz > season[iday-1] & lengthData$Year==iyr & lengthData$State == "CT"),"LandingsNum"]) 
       }
    }
     assign(paste("TotalLandings_Season", iday, sep=""), LandingsVec)
   }
   
   maxY <- max(c(TotalLandings_Season0, TotalLandings_Season7, TotalLandings_Season14,
                 TotalLandings_Season21, TotalLandings_Season28, TotalLandings_Season35,
                 TotalLandings_Season42, TotalLandings_Season49, TotalLandings_Season56,
                 TotalLandings_Season63, TotalLandings_Season70))
   plot(x= unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]), 
        y= TotalLandings_Season0,
        main="Connecticut Landings by Season Length",
        ylab = "Total landings",
        xlab= "Year",
        type="l",
        col="lightblue",
        ylim = c(0,maxY))
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season7, col="red")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season14, col="aquamarine")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season21, col="aquamarine4")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season28, col="blue")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season35, col="chocolate3")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season42, col="coral1")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season49, col="blueviolet")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season56, col="darkmagenta")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season63, col="darkorange")
   lines(x = unique(lengthData[which(lengthData[,"State"]=="CT"),"Year"]),
         y = TotalLandings_Season70, col="black")
   legend("topright",
          legend=c("0 days", "1-7 days", "8-14 days", "15-21 days", "22-28 days",
                   "29-35 days", "36-42 days", "43-49 days", "50-56 days", "57 to 63 days",
                   "64 to 70 days"),
          fill=c("lightblue", "red", "aquamarine", "aquamarine4",
                 "blue", "chocolate3", "coral1", "blueviolet", "darkmagenta",
                 "darkorange",   "black"))
   
# this isn't working correctly
   
   
   ##########################################################################################################
   ##### Season length over years #####
   ##########################################################################################################
   seasonData <- rep(NA, length(unique(lengthData$Year)))
   for(iyr in 1993:2017){
     seasonData[iyr-1992] <- sum(unique(lengthData[which(lengthData[,"State"]=="MA" & lengthData$Year==iyr),"SeasonDz"]))
   }
   
   plot(x=unique(lengthData[which(lengthData[,"State"]=="MA"),"Year"]),
        y=seasonData,
        ylab="Season length (days)",
        xlab="Year",
        main="Massachusetts season length", 
        type="l")
   
   
   
  ##########################################################################################################
  ##### Bag size over years #####
  ##########################################################################################################
    # Points overlap so coloring is only broadly representative
    # Years with multiple bag sizes have changes by wave
  plot(x= effortData[which(effortData[,"State"]=="MA"),"Year"], 
       y=effortData[which(effortData[,"State"]=="MA"),"BagSz"], 
       col=effortData$Wave,
       main="Massachusetts Bag Size",
       ylab = "Bag Size",
       xlab="Year")
  legend("topright",
         legend=unique(effortData$Wave),
         fill=effortData$Wave)
  
  # ##### Total season length over years #####
  #  TotalSeason <- rep(0, length(unique(effortData$Year)))
  #  for(iyr in 1993:2017){
  #    TotalSeason[iyr-1992] <- unique(effortData[which(effortData$Year==iyr & effortData$State=="MA"),"SeasonDz"])
  #  }
  #  plot(x= unique(effortData[which(effortData[,"State"]=="MA"),"Year"]), 
  #       y= TotalSeason,
  #       main="Massachusetts Season Length",
  #       ylab = "Season length (days)", 
  #       xlab= "Year",
  #       type="l")
  #  
   
  ##########################################################################################################
  ##### NumTrips by wave over years #####
  ##########################################################################################################
    # plot line for each year and colorcode by decade?
   
  ##########################################################################################################
  ##### Landings at size over years #####
  ##########################################################################################################
   
  ##########################################################################################################
  ##### Landings at size by bagsize over years #####
  ##########################################################################################################
   
  ##########################################################################################################
  ##### Landings at size by min size over years #####
  ##########################################################################################################
   
  ##########################################################################################################
  ##### Landings at size by season length over years #####
  ##########################################################################################################
   
  ##########################################################################################################
  ##### Landings at size by wave over years #####
  ##########################################################################################################
                       
                          
                          