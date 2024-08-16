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
  browser()
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  ds <<- readRDS(path)
  
  # found some id != idx
  ds$idx <<- ds$id
  ds$idx <- ds$id

  initial.model <- glm(initial.formula, data = ds, family = binomial)
  full.model <- glm(full.formula, data = ds, family = binomial)
  stepwise_both <- stepAIC(initial.model, 
                           scope = list(lower = initial.model, 
                                        upper = full.model), 
                           direction = "both")
  sel.variables <- all.vars(formula(stepwise_both))[-1]
  proxy_both <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_both, collapse = "+")
  proxy_both.data.i <- paste0("proxy_both.data.", i)
  assign(proxy_both.data.i, proxyform)
  return(proxyform)
  
  # Save the result to an .rds file
  save_path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/10.3_both/proxy_both.data.", i, ".RData")
  save(list = proxy_both.data.i, file = save_path)
}

results <- mclapply(1:1000, proxy_select, mc.cores = n_cores)
