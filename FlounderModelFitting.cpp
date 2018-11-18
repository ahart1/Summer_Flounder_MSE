// C++ template for TMB code

#include <TMB.hpp>

// Optional function which square the value provided
template <class Type> Type square(Type x){return x*x;}

// Objective function which returns the value to minimize (often the negative log likelihood)
template<class Type>
Type objective_function<Type>::operator() ()
{
  ///// Data section /////
  DATA_VECTOR(FishingData); // Vector of fishing data (catch or effort)
  DATA_VECTOR(MngmtData); // Vector of management data (min size, bag size, season length, SSB from assessment, RHL/quota )
  DATA_VECTOR(Years); // Vector of years of same length as fishing and management data vectors
  DATA_INTEGER(ModelOption); // Specifies which model to use (1 = "constant", 2 = "linear", 3 = "exponential")
  DATA_VECTOR(covar1); // Covariate 1, same length as Years, if no covariate wanted a vector of zeros can be passed in
  DATA_VECTOR(covar2); // Covariate 2, same length as Years, if no covariate wanted a vector of zeros can be passed in
  DATA_VECTOR(covar3); // Covariate 3, same length as Years, if no covariate wanted a vector of zeros can be passed in
  
  //DATA_INTEGER(data1); 
  //DATA_IVECTOR(data2); // Vector of integers ??
  //DATA_MATRIX(data3); 
  // DATA_VECTOR(data_object_name); // Vector of data
  
  
  ///// Parameter section /////
  PARAMETER(dummy); // Include dummy variable to debug code without full estimation of likelihood
  PARAMETER(par_covar1); // Linear  covariate parameters
  PARAMETER(par_covar2);
  PARAMETER(par_covar3);
  PARAMETER(par_constant); // Estimated model-specific parameters
  PARAMETER(par_slope);
  PARAMETER(par_intercept);
  PARAMETER(par_expa);
  PARAMETER(par_expb);
  PARAMETER(par_alog);
  PARAMETER(par_blog);
  PARAMETER(par_clog);
  PARAMETER(par_sata);
  PARAMETER(par_satb);
  //PARAMETER_VECTOR(parameter_vector_name); // Vector of estimated parameters
  

  
  // Retransform variables so not in log space 
  // Type local_variable1 = exp(log_variable1); 
  
  // Local variables
  //Type local_variable2; // single value variable which is NOT an integer
  //local_variable2 = 5; 
  vector<Type> Prediction(Years.size()); // Vector of predicted value (catch or effort)
  
  //vector<Type> local_vector(5); // vector of length 5
  //matrix<Type> local_matrix(3,4); // 3X4col matrix
  
  Type obj_fun; // Always need objective function object
  
  
  ///// Calculate parameters (could also do in R and pass in as DATA) /////
  
  
  ///// Initialize objective function at zero /////
  obj_fun = 0; // sum of squares
  
  ///// Code which contributes to objective function /////
  // Generally make a prediction and compare that prediction to data (likelihood)
  // minimize comparison so you pick parameter values to best predict data
  // obj_fun += likelihood_component1 + likelihood_component2...
  //obj_fun += dummy*dummy; // dummy objective function used to debug (make sure code compiles) without estimating all parameters
  
  if(ModelOption == 1){ // Constant model
    for(int iyear=0; iyear<Years.size(); iyear++){
      Prediction(iyear) = par_constant + par_covar1*covar1(iyear) + par_covar2*covar2(iyear) + par_covar3*covar3(iyear);
      obj_fun += square(FishingData(iyear) - Prediction(iyear)); // This calculates the sum of squares 
    }
  } else if(ModelOption == 2){ // Linear model
    for(int iyear=0; iyear<Years.size(); iyear++){
      Prediction(iyear) = par_slope*MngmtData(iyear) + par_intercept + par_covar1*covar1(iyear) + par_covar2*covar2(iyear) + par_covar3*covar3(iyear);
      obj_fun += square(FishingData(iyear) - Prediction(iyear));
    }
  } else if(ModelOption == 3){ // Exponential model
    for(int iyear=0; iyear<Years.size(); iyear++){
      Prediction(iyear) = par_expa*pow(par_expb, MngmtData(iyear)) + par_covar1*covar1(iyear) + par_covar2*covar2(iyear) + par_covar3*covar3(iyear);
      obj_fun += square(FishingData(iyear) - Prediction(iyear));
    }
  } else if(ModelOption == 4){ // Logistic model # This is broken and keeps estimating a constant value, which is probably wrong !!!!!!!!!!!
    for(int iyear=0; iyear<Years.size(); iyear++){
      // Prediction(iyear) = par_clog/(1 + par_alog*pow(par_blog, MngmtData(iyear)));
      // Prediction(iyear) = par_clog/(1 + exp(-par_alog*(MngmtData(iyear) - par_blog)));
      // Prediction(iyear) = par_clog/(1 + par_alog*exp(-par_blog*MngmtData(iyear)));
      Prediction(iyear) = 1/(1 + (1/par_alog - 1)*exp(-par_blog*MngmtData(iyear))) + par_clog + par_covar1*covar1(iyear) + par_covar2*covar2(iyear) + par_covar3*covar3(iyear); 
      obj_fun += square(FishingData(iyear) - Prediction(iyear));
    }
  } else if(ModelOption == 5){ // Saturation ("saturated") model
    for(int iyear=0; iyear<Years.size(); iyear++){
      // Prediction(iyear) = par_sata*MngmtData(iyear)/(MngmtData(iyear) + par_satb);
      Prediction(iyear) = par_sata + par_satb/MngmtData(iyear) + par_covar1*covar1(iyear) + par_covar2*covar2(iyear) + par_covar3*covar3(iyear);
      obj_fun += square(FishingData(iyear) - Prediction(iyear));
    }
  }
  
  
  
  ///// ADReport reports deviation /////
  ADREPORT(par_covar1); // Linear  covariate parameters 
  ADREPORT(par_covar2);
  ADREPORT(par_covar3);
  ADREPORT(par_constant); // Estimated model-specific parameters
  ADREPORT(par_slope);
  ADREPORT(par_intercept);
  ADREPORT(par_expa);
  ADREPORT(par_expb);
  ADREPORT(par_alog);
  ADREPORT(par_blog);
  ADREPORT(par_clog);
  ADREPORT(par_sata);
  ADREPORT(par_satb);
  
  ///// Report /////
  // REPORT(variable_or_parameter_name_here); // Report variable or parameter value
  REPORT(Prediction);
  
  ///// Return objective function /////
  return(obj_fun);
  
  ///// Advice if not compiling /////
  // Check that all lines end in ;
  // Check that * between multiplied objects in equations
  // Check indexing on objects in equations
  // Check indexing of storage objects (e.g. StorageVector = DataVector1[i]*DataVector2[i] won't work since this produces a single value but there is no indexing of StorageVector[i])
  // Check indexing of for() loops (indexing starts at 0)
  // Check spelling of objects in equations
  // Try commenting out parts of the line causing the error and gradually add them back in until it causes an error again to ID the source
}







