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
path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_1.rds")
data <- readRDS(path)
proxy.list <- names(data[, c(grep("^rec", names(data), value = TRUE))])
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(c("outcome~ exposure", 
                              covarsTfull), collapse = "+"))

# ---------------------------------------------------------------

n_cores <- 12  # Match this to the --ntasks value in your Slurm script

proxy_select <- function(i) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  boruta.fit <- Boruta(Y.form, data = data, doTrace = 1)
  sel.variables <- names(boruta.fit$finalDecision)[which(boruta.fit$finalDecision != "Rejected")]
  proxy_boruta <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_boruta, collapse = "+")
  proxy_boruta.data.i <- paste0("proxy_boruta.data.", i)
  assign(proxy_boruta.data.i, proxyform)
  
  # Save the result to an .rds file
  save_path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/10.1_boruta/proxy_boruta.data.", i, ".RData")
  save(list = proxy_boruta.data.i, file = save_path)
  
  rm(data, 
     boruta.fit, 
     sel.variables, 
     proxy_boruta, 
     proxyform,
     proxy_boruta.data.i)
  gc()
}

results <- mclapply(1:1000, proxy_select, mc.cores = n_cores)
