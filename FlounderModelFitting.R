##### Summer Flounder Model Fitting #####

FileLocation <- c("/Users/ahart2/Research/Summer_Flounder_MSE")

setwd(FileLocation)

################################################################################################
################################################################################################
# To Do:
    # Get stock biomass info to explain Effort/Catch
    # See notes from class presentation, there is still something I am missing
    # Fix logistic model
    # search literature for other methods of characterizing this relationship
    # Make sure parameters are in the correct parameter space
################################################################################################
################################################################################################
source(paste(FileLocation, "FitFlukeModels.R", sep="/"))


ResponseDataList <- c("Effort", "Catch") # Currently there are effort and catch data 
StateList <- c("VA", "CT", "DE", "MD", "MA", "NJ", "NY", "NC", "RI", "Region")
StateNames <- c("Virgina", "Connecticut", "Delaware", "Maryland", "Massachusetts", "New Jersey", "New York", "North Carolina", "Rhode Island", "Region")


AllResults <- NULL # Initialize storage for fitted parameter values
for(iresponsedata in ResponseDataList){
  for(istate in 1:length(StateList)){
    ##### Read in data vs. bag size matrix #####
    data <- read.csv(paste(FileLocation, "Data_FormattedMatrices", paste(StateList[istate], iresponsedata, "vs_Bag_Data", sep="_"), sep="/"))
    data <- data[,2:ncol(data)]
    data
    # Partition data
    # Fishing <- data[,paste(StateList[istate],iresponsedata, sep="_")]
    # Years <- data[,"Years"]
    # Mngmt <- data[,"Wave4Bag"]
    Fishing <-  paste(StateList[istate],iresponsedata, sep="_")
    Mngmt <- "Wave4Bag"
    
    # Fit fluke models to mngmt & fishing data 
    Results <- FitFlukeModels(DataMatrix = data, State=StateList[istate], DataType = iresponsedata, MngmtType = "Bag", DataMngmt_name = Mngmt, DataFishing_name = Fishing, PlotModels = TRUE)
    AllResults <- rbind(AllResults, Results)
    
    
    ##### Read in data vs. minimum size matrix #####
    data <- read.csv(paste(FileLocation, "Data_FormattedMatrices", paste(StateList[istate], iresponsedata, "vs_MinSz_Data", sep="_"), sep="/"))
    data <- data[,2:ncol(data)]
    data
    # Partition data
    # Fishing <- data[,paste(StateList[istate],iresponsedata, sep="_")]
    # Years <- data[,"Years"]
    # Mngmt <- data[,"Wave4MinSz"]
    Fishing <- paste(StateList[istate],iresponsedata, sep="_")
    Mngmt <- "Wave4MinSz"
    
    # Fit fluke models to mngmt & fishing data 
    Results <- FitFlukeModels(DataMatrix = data, State=StateList[istate], DataType = iresponsedata, MngmtType = "MinSz", DataMngmt_name = Mngmt, DataFishing_name = Fishing, PlotModels = TRUE)
    AllResults <- rbind(AllResults, Results)
    
    
    ##### Read in data vs. season length matrix #####
    data <- read.csv(paste(FileLocation, "Data_FormattedMatrices", paste(StateList[istate], iresponsedata, "vs_Season_Data", sep="_"), sep="/"))
    data <- data[,2:ncol(data)]
    data
    # Partition data
    # Fishing <- data[,paste(StateList[istate],iresponsedata, sep="_")]
    # Years <- data[,"Years"]
    # Mngmt <- data[,"Season_Length"]
    Fishing <- paste(StateList[istate],iresponsedata, sep="_")
    Mngmt <- "Season_Length"
    
    # Fit fluke models to mngmt & fishing data 
    Results <- FitFlukeModels(DataMatrix = data, State=StateList[istate], DataType = iresponsedata, MngmtType = "Season", DataMngmt_name = Mngmt, DataFishing_name = Fishing, PlotModels = TRUE)
    AllResults <- rbind(AllResults, Results)
  }
}























