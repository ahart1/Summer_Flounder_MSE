# This script contains the R function which fits fluke data to different models

FitFlukeModels <- function(DataMatrix = NULL,
                           State=NULL,
                           DataType = NULL,
                           MngmtType = NULL,
                           DataMngmt_name = NULL,
                           DataFishing_name = NULL,
                           PlotModels = TRUE,
                           covar1_name = NA,
                           covar2_name = NA,
                           covar3_name = NA,
                           par_covar1 = 0, # Covariate parameters
                           par_covar2 = 0,
                           par_covar3 = 0,
                           par_constant = 1, # Model-specific parameters
                           par_slope = 1,
                           par_intercept = 1,
                           par_expa = 1,
                           par_expb = 1,
                           par_alog = 1,
                           par_blog = 1,
                           par_clog = 1,
                           par_sata = 1,
                           par_satb = 1){
  # Args:
    # DataMatrix: should contain columns with the name "Years" and names passed to DataMngmt_name, DataFishing_name, no default
    # State: String specifying which state or region data is from
    # DataType: String specifying "Effort", or "Catch" data corresponds to datafile name
    # MngmtType: String specifying "Bag", "MinSz", or "Season" data corresponds to datafile name
    # DataMngmt_name: String specifying name of column of management settings
    # DataFishing_name: String specifying name of column of effort or catch under Mngmt settings
    # PlotModels: Boolean, if true, plots the fitted equations to data, default=TRUE
    # To fit models with linear covariates, specify column names for covariates (up to three), default for covar1_name, covar2_name, covar3_name = NA
      # covar1_name: string, default = NA
      # covar2_name: string, default = NA
      # covar3_name: string, default = NA
    # Initial parameter settings for covariates, default used unless provided
      # par_covar1: Number, initial value for covariate, default = 0
      # par_covar2: Number, initial value for covariate, default = 0
      # par_covar3: Number, initial value for covariate, default = 0 
    # Initial parameter settings for model-specific parameters, default used unless provided
      # par_constant: Number, initial value for constant model, default = 1
      # par_slope: Number, initial value for linear model slope parameter, default = 1
      # par_intercept: Number, initial value for linear model intercept parameter, default = 1
      # par_expa: Number, initial value for exponential model (y=a*b^x) parameter, default = 1
      # par_expb: Number, initial value for exponential model parameter, default = 1
      # par_alog: Number, initial value for logistic model parameter, default = 1
      # par_blog: Number, initial value for logistic model parameter, default = 1
      # par-clog: Number, initial value for logistic model parameter, default = 1
      # par_sata: Number, initial value for saturation model parameter, default = 1
      # par_satb: Number, initial value for saturation model parameter, default = 1
  
  # Returns: Matrix containing AICc estimate and parameter estimates for models fit to given mngmt setting, fishing data, and state
  
  
  allPars <- c("par_constant", 
               "par_slope", "par_intercept", 
               "par_expa", "par_expb", 
               "par_alog", "par_blog", "par_clog",
               "par_sata", "par_satb")
  ModelList <- c("constant", "linear", "exponential", "logistic", "saturated")
  names(ModelList) <- ModelList # allows indexing by name to store results
  Covariate_names <- c(covar1_name, covar2_name, covar3_name)
  Covariate_names <- Covariate_names[which(is.na(Covariate_names) == FALSE)]
  Results <- matrix(NA,length(ModelList), (4+length(allPars)+length(Covariate_names)))
  colnames(Results) <- c("State", "DataType", "ManagementType", "AICc", allPars, Covariate_names)
  
  # Store management and data settings
  Results[,"State"] <- State
  Results[,"DataType"] <- DataType
  Results[,"ManagementType"] <- MngmtType
  
  # Partition data
  Fishing <- DataMatrix[,DataFishing_name]
  Years <- DataMatrix[,"Years"]
  Mngmt <- DataMatrix[,DataMngmt_name]
  
  ##### For covariates #####
  # Settings for ModelData based on covariates, if covariates not wanted, a vector of zeros is passed in for covariate data
  if(is.na(covar1_name)){
    covar1 <- c(rep(0, length(Years)))
  } 
  if(is.na(covar2_name)){
    covar2 = c(rep(0, length(Years)))
  }
  if(is.na(covar3_name)){
    covar3 = c(rep(0, length(Years)))
  }
  
  
  ##### For all models #####
  require(TMB)
  
  # Compile Cpp code
  compile("FlounderModelFitting.cpp") # file must be in working directory or provide full file path name
  dyn.load(dynlib("FlounderModelFitting"))
  
  # Create list of parameters and provide initial values (may include parameter vector, e.g. param_vec = rep(0,5))
  ModelParameters <- list(dummy=0, 
                          par_covar1 = par_covar1, par_covar2 = par_covar2, par_covar3 = par_covar3, # Linear covariates, set to zero if not provided
                          par_constant=par_constant, # Constant model parameters
                          par_slope=par_slope, par_intercept=par_intercept, # Linear model parameters
                          par_expa=par_expa, par_expb=par_expb, # Exponential model parameters
                          par_alog=par_alog, par_blog=par_blog, par_clog=par_clog, # Logistic model parameters
                          par_sata=par_sata, par_satb=par_satb) # Saturated model parameters
  
  ##### Model-specific settings #####
  # Loop over available model options (automatically fit all models, may set initial parameters or result to default)
  for(imodel in ModelList){
    # Specify parameters to estimate based on model options
    if(imodel == "constant"){
      ModelMap <- list(dummy=factor(NA), par_slope=factor(NA), par_intercept=factor(NA), par_expa=factor(NA), par_expb=factor(NA), par_alog=factor(NA), par_blog=factor(NA), par_clog=factor(NA), par_sata=factor(NA), par_satb=factor(NA))
      UseParameters <- c("par_constant")
      ModelOption <- 1
      ModelData <- list(FishingData = Fishing, Years = Years, MngmtData = Mngmt, ModelOption = ModelOption, covar1 = covar1, covar2 = covar2, covar3 = covar3)
    } else if(imodel == "linear"){
      ModelMap <- list(dummy=factor(NA), par_constant=factor(NA), par_expa=factor(NA), par_expb=factor(NA), par_alog=factor(NA), par_blog=factor(NA), par_clog=factor(NA), par_sata=factor(NA), par_satb=factor(NA))
      UseParameters <- c("par_slope", "par_intercept")
      ModelOption <- 2
      ModelData <- list(FishingData = Fishing, Years = Years, MngmtData = Mngmt, ModelOption = ModelOption, covar1 = covar1, covar2 = covar2, covar3 = covar3)
    } else if(imodel == "exponential"){
      ModelMap <- list(dummy=factor(NA), par_constant=factor(NA), par_slope=factor(NA), par_intercept=factor(NA), par_alog=factor(NA), par_blog=factor(NA), par_clog=factor(NA), par_sata=factor(NA), par_satb=factor(NA))
      UseParameters <- c("par_expa", "par_expb")
      ModelOption <- 3
      ModelData <- list(FishingData = Fishing, Years = Years, MngmtData = Mngmt, ModelOption = ModelOption, covar1 = covar1, covar2 = covar2, covar3 = covar3)
    } else if(imodel == "logistic"){
      ModelMap <- list(dummy=factor(NA), par_constant=factor(NA), par_slope=factor(NA), par_intercept=factor(NA), par_expa=factor(NA), par_expb=factor(NA), par_sata=factor(NA), par_satb=factor(NA))
      UseParameters <- c("par_alog", "par_blog", "par_clog")
      ModelOption <- 4
      ModelData <- list(FishingData = Fishing, Years = Years, MngmtData = Mngmt, ModelOption = ModelOption, covar1 = covar1, covar2 = covar2, covar3 = covar3)
    } else if(imodel == "saturated"){
      ModelMap <- list(dummy=factor(NA), par_constant=factor(NA), par_slope=factor(NA), par_intercept=factor(NA), par_expa=factor(NA), par_expb=factor(NA), par_alog=factor(NA), par_blog=factor(NA), par_clog=factor(NA))
      UseParameters <- c("par_sata", "par_satb")
      ModelOption <- 5
      ModelData <- list(FishingData = Fishing, Years = Years, MngmtData = Mngmt, ModelOption = ModelOption, covar1 = covar1, covar2 = covar2, covar3 = covar3)
    } else {
      print("There is no model with that name")
    }
    
    # Construct objective function to optimize based on data, parameters, and Cpp code
    Model <- MakeADFun(data = ModelData, parameters = ModelParameters, DLL="FlounderModelFitting",silent=T,map = ModelMap)
    
    # Fit model to data using structure provided by MakeADFun() function call
    fit <- nlminb(Model$par, Model$fn, Model$gr, control=list(rel.tol=1e-12,eval.max=100000,iter.max=1000))
    
    # Best parameter estimates
    best <- Model$env$last.par.best # order par_constant, slope, intercept, a, b
    # print(best)
    # print(sdreport(Model))
    
    
    #########################################################################################################
    # breaks here since no results provided
    
    ##### Store Results #####
    if(imodel == "constant"){
      Results[which(ModelList == imodel),"par_constant"] <- best["par_constant"]
    } else if(imodel == "linear"){
      Results[which(ModelList == imodel),"par_slope"] <- best["par_slope"]
      Results[which(ModelList == imodel),"par_intercept"] <- best["par_intercept"]
    } else if(imodel == "exponential"){
      Results[which(ModelList == imodel),"par_expa"] <- best["par_expa"]
      Results[which(ModelList == imodel),"par_expb"] <- best["par_expb"]
    } else if(imodel == "logistic"){
      Results[which(ModelList == imodel),"par_alog"] <- best["par_alog"]
      Results[which(ModelList == imodel),"par_blog"] <- best["par_blog"]
      Results[which(ModelList == imodel),"par_clog"] <- best["par_clog"]
    } else if(imodel == "saturated"){
      Results[which(ModelList == imodel), "par_sata"] <- best["par_sata"]
      Results[which(ModelList == imodel), "par_satb"] <- best["par_satb"]
    }
    
    # Calculate AICc here ????????? may also need to change row indexing to correspond with model
    AICc <- 2*fit$objective+2*length(UseParameters) + 2*length(UseParameters)*(length(UseParameters)+1)/(length(Mngmt) - length(UseParameters) - 1)
    Results[which(ModelList == imodel),"AICc"] <- AICc
  }
  
  Results 
  
  if(PlotModels==TRUE){
    # DataType: String specifying "Effort", or "Catch" data corresponds to datafile name
    # MngmtType: String specifying "Bag", "MinSz", or "Season" data corresponds to datafile name
    # Mngmt: Vector of management settings
    # Fishing: Vector of effort or catch under Mngmt settings
    
    # Estimated function parameters
    par_constant <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_constant")] != "NA"),which(colnames(Results)=="par_constant")])
    par_slope <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_slope")] != "NA"),which(colnames(Results)=="par_slope")])
    par_intercept <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_intercept")] != "NA"),which(colnames(Results)=="par_intercept")])
    par_expa <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_expa")] != "NA"),which(colnames(Results)=="par_expa")])
    par_expb <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_expb")] != "NA"),which(colnames(Results)=="par_expb")])
    par_alog <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_alog")] != "NA"),which(colnames(Results)=="par_alog")])
    par_blog <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_blog")] != "NA"),which(colnames(Results)=="par_blog")])
    par_clog <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_clog")] != "NA"),which(colnames(Results)=="par_clog")])
    par_sata <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_sata")] != "NA"),which(colnames(Results)=="par_sata")])
    par_satb <- as.numeric(Results[which(Results[,which(colnames(Results)=="par_satb")] != "NA"),which(colnames(Results)=="par_satb")])
    
    # Functions to plot
    linear <- function(x){
      par_slope*x + par_intercept
    } 
    exponential <- function(x){
      par_expa*par_expb^x
    } 
    logistic <- function(x){
      #par_clog/(1+par_alog*(par_blog^x))
      #par_clog/(1 + exp(-par_alog*(x - par_blog)))
      # par_clog/(1 + par_alog*exp(-par_blog*x))
      1/(1 + (1/par_alog - 1)*exp(-par_blog*x)) +par_clog
    }
    saturated <- function(x){
      #par_sata*x/(x + par_satb)
      par_sata + par_satb/x
    }
    
    plot(x = Mngmt,
         y = Fishing,
         main = paste(State, DataType, "vs.", MngmtType, sep=" "),
         ylab = DataType,
         xlab = MngmtType) # plot data 
    abline(h = par_constant, col="blue") # plot constant function
    # curve(expr = (par_slope*x + par_intercept), from = min(MngmtData), to = max(MngmtData))
    lines(x=seq(min(Mngmt), max(Mngmt)), y=linear(x=seq(min(Mngmt), max(Mngmt))), col="red") # plot linear function
    lines(x=seq(min(Mngmt), max(Mngmt)), y=exponential(x=seq(min(Mngmt), max(Mngmt))), col="chartreuse3") # plot exponential function
    lines(x=seq(min(Mngmt), max(Mngmt)), y=logistic(x=seq(min(Mngmt), max(Mngmt))), col="darkorange") # plot logistic function
    #print(logistic(x=seq(min(Mngmt), max(Mngmt))))
    print(par_alog)
    print("logistic")
    lines(x=seq(min(Mngmt), max(Mngmt)), y=saturated(x=seq(min(Mngmt), max(Mngmt))), col="darkorchid") # plot saturated function
    print(saturated(x=seq(min(Mngmt), max(Mngmt))))
    print("saturated")
    legend("topright", legend = c("Constant", "Linear", "Exponential", "Logistic", "Saturated"), fill = c("blue", "red", "chartreuse3", "darkorange", "darkorchid"))
  } 
  
  return(Results)
}