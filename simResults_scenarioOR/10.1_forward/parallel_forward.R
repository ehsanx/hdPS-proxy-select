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
path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioOR/data_1.rds")
data <- readRDS(path)
proxy.list <- names(data[, c(grep("^rec", names(data), value = TRUE))])
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
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioOR/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  initial.model <- glm(initial.formula, data = data, family = binomial)
  full.model <- glm(full.formula, data = data, family = binomial)
  stepwise_forward <- stepAIC(initial.model, 
                              scope = list(lower = initial.model, 
                                           upper = full.model), 
                              direction = "forward")
  sel.variables <- all.vars(formula(stepwise_forward))[-1]
  proxy_forward <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_forward, collapse = "+")
  proxy_forward.data.i <- paste0("proxy_forward.data.", i)
  assign(proxy_forward.data.i, proxyform)
  
  # Save the result to an .rds file
  save_path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/10.1_forward/proxy_forward.data.", i, ".RData")
  save(list = proxy_forward.data.i, file = save_path)
  
  rm(data, 
     initial.model, 
     full.model, 
     stepwise_forward, 
     sel.variables, 
     proxy_forward, 
     proxyform,
     proxy_forward.data.i)
  gc()
}

results <- mclapply(1:1000, proxy_select, mc.cores = n_cores)
