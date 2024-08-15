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
data1 <- readRDS(path)
proxy.list <- names(data1[, c(grep("^rec", names(data1), value = TRUE))])
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(c("outcome~ exposure", 
                              covarsTfull), collapse = "+"))
initial.formula <- as.formula(paste0("outcome~exposure+",
                                     covform,
                                     collapse = "+"))
full.formula <- as.formula(paste0("outcome~exposure+",
                                  paste0(covarsTfull, collapse = "+"),
                                  collapse = "+"))
# ---------------------------------------------------------------

n_cores <- 12  # Match this to the --ntasks value in your Slurm script

proxy_select <- function(i) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  data <- as.data.frame(data)
  
  initial.model <- glm(initial.formula, data = data, family = binomial)
  full.model <- glm(full.formula, data = data, family = binomial)
  stepwise_backward <- stepAIC(full.model, 
                              scope = list(lower = initial.model, 
                                           upper = full.model), 
                              direction = "backward")
  sel.variables <- all.vars(formula(stepwise_backward))[-1]
  proxy_backward <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_backward, collapse = "+")
  proxy_backward.data.i <- paste0("proxy_backward.data.", i)
  assign(proxy_backward.data.i, proxyform)
  
  # Save the result to an .rds file
  save_path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/10.2_backward/proxy_backward.data.", i, ".RData")
  save(list = proxy_backward.data.i, file = save_path)
  
  rm(data, 
     initial.model, 
     full.model, 
     stepwise_backward, 
     sel.variables, 
     proxy_backward, 
     proxyform,
     proxy_backward.data.i)
  gc()
}

results <- mclapply(1:1000, proxy_select, mc.cores = n_cores)

