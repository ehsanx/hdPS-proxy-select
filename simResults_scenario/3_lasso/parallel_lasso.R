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
data1 <- readRDS(path)
proxy.list <- names(data1[, c(grep("^rec", names(data1), value = TRUE))])
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(c("outcome~ exposure", 
                              covarsTfull), collapse = "+") )
# ---------------------------------------------------------------

n_cores <- 12  # Match this to the --ntasks value in your Slurm script

proxy_select <- function(i) {
  
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  covar.mat <- model.matrix(Y.form, data = data)[,-1]
  
  lasso.fit <- glmnet::cv.glmnet(y = data$outcome, 
                                 x = covar.mat, 
                                 type.measure='mse',
                                 family="binomial",
                                 alpha = 1, 
                                 nfolds = 5)
  coef.fit <- coef(lasso.fit,s='lambda.min',exact=TRUE)
  sel.variables <- row.names(coef.fit)[which(as.numeric(coef.fit)!=0)]
  
  proxy_lasso <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_lasso, collapse = "+")
  proxy_lasso.data.i <- paste0("proxy_lasso.data.", i)
  assign(proxy_lasso.data.i, proxyform)
  
  # Save the result to an .rds file
  save_path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/3_lasso/proxy_lasso.data.", i, ".RData")
  save(list = proxy_lasso.data.i, file = save_path)

  rm(data, 
     covar.mat, 
     lasso.fit, 
     coef.fit, 
     sel.variables, 
     proxy_lasso, 
     proxyform,
     proxy_lasso.data.i)
  gc()
}

results <- mclapply(1:1000, proxy_select, mc.cores = n_cores)
