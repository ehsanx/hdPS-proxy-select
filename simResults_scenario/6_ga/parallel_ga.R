# try parallel
library(parallel)
library(autoCovariateSelection)
library(dplyr)
library(cobalt)
library(WeightIt)
library(ggplot2)
library(lmtest)
library(MASS)
library(Boruta)
library(GA)
library(mclust)
library(penalizedSVM)
library(xgboost)
library(caret)
library(randomForest)
library(glmnet)

# ---------------------------------------------------------------

### global information
exposure <- "obese"
outcome <- "diabetes" 
investigator.specified.covariates <- 
  c(# Demographic
    "age.cat", "sex", "education", "race", 
    "marital", "income", "born", "year",
    
    # health history related variables/access
    "diabetes.family.history", "medical.access",
    
    # behavioral
    "smoking", "diet.healthy", "physical.activity", "sleep",
    
    # Laboratory 
    "uric.acid", "protein.total", "bilirubin.total", "phosphorus",
    "sodium", "potassium", "globulin", "calcium.total", 
    "systolicBP", "diastolicBP", "high.cholesterol"
  )
covform <- paste0(investigator.specified.covariates, collapse = "+")
out.formula <- as.formula(paste0("outcome", "~", "exposure"))
path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_1.rds")
data <- readRDS(path)
proxy.list <- names(data[, c(grep("^rec", names(data), value = TRUE))])
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(c("outcome~ exposure", 
                              covarsTfull), collapse = "+"))
gaOpt <- function(vars, IV.train, DV.train) {
  varNames <- colnames(IV.train) #getting names of all variables
  selectedVarNames <- varNames[vars == "1"] # getting names of selected vars from GA
  gaSolutionData <- IV.train[,selectedVarNames] # keeping only those selected vars
  
  gaDat <- cbind(gaSolutionData,DV.train) # combining selected variables with outcome variable
  gaMod <- glm(DV.train ~ ., family = "binomial", data = gaDat) #build model
  gaProb <- predict(gaMod, IV.train, type = "response") # get probabilities
  gaPred <- ifelse(gaProb >= .8, 1, 0) # get predicted 0s and 1s
  
  ari <- adjustedRandIndex(gaPred, DV.train)
  return(ari)
}
# ---------------------------------------------------------------

n_cores <- 12  # Match this to the --ntasks value in your Slurm script

proxy_select <- function(i) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  covar.mat <- model.matrix(Y.form, data = data)[,-1]
  
  ga.fit <- ga(fitness = function(vars)
    gaOpt(vars = vars, 
          IV.train = data.frame(covar.mat),
          DV.train = data$outcome),
    type = "binary", 
    nBits = ncol(covar.mat),
    names = colnames(covar.mat), 
    seed = 42,
    run=5)
  
  sel.variables <- proxy.list[ga.fit@solution[1,]==1]
  proxy_ga <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_ga, collapse = "+")
  proxy_ga.data.i <- paste0("proxy_ga.data.", i)
  assign(proxy_ga.data.i, proxyform)
  
  # Save the result to an .rds file
  save_path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/6_ga/proxy_ga.data.", i, ".RData")
  save(list = proxy_ga.data.i, file = save_path)
  
  rm(data, 
     covar.mat, 
     ga.fit, 
     sel.variables, 
     proxy_ga, 
     proxyform,
     proxy_ga.data.i)
  gc()
}

results <- mclapply(1:1000, proxy_select, mc.cores = n_cores)
