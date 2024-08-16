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
library(leaps)
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
                              covarsTfull), collapse = "+") )
full.formula <- as.formula(paste0("outcome~exposure+",
                                  paste0(covarsTfull, collapse = "+"),
                                  collapse = "+"))
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 1000
b <- 4

RD_forward <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_forward <- c()
# ---------------------------------------------------------------

### scenarioER: for-loop generating RD & SE results
set.seed(42)

# Folder "scenarioER"
for (i in (b+1):a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  stepwise_forward <- regsubsets(full.formula, data = data, method = "forward", nvmax = length(proxy.list), nbest = 10)
  summary_stepwise <- summary(stepwise_forward)
  best_model <- which.max(summary_stepwise$adjr2)
  sel.variables <- names(summary_stepwise$which[best_model,])[summary_stepwise$which[best_model,]]
  proxy_forward <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_forward, collapse = "+")
  rhsform <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsform))
  
  W.out_forward <- weightit(ps.formula,
                          data = data, 
                          estimand = "ATE",
                          method = "ps")
  fit.OR_forward <- glm(out.formula,
                      data = data,
                      weights = W.out_forward$weights,
                      family= binomial(link = "logit"))
  fit.RD_forward <- glm(out.formula,
                      data= data,
                      weights= W.out_forward$weights,
                      family=gaussian(link= "identity"))
  sum.RD_forward <- c(length(proxy_forward), 
                    summary(fit.RD_forward)$coef["exposure", c("Estimate")], 
                    sqrt(sandwich::sandwich(fit.RD_forward)[2,2]))
  
  RD_forward[i-b,] <- sum.RD_forward
  rownames(RD_forward)[i-b] <- paste0("data.", i, "_forward")
  num.proxy_forward <- c(num.proxy_forward, length(proxy_forward))
  names(num.proxy_forward)[i-b] <- paste0("data.", i, "_forward")
  
  results_forward <- c(i, sum.RD_forward)
  names(results_forward) <- c("iteration", "numProxy", "RD", "SE")
  results_forward.i <- paste0("results_forward.", i)
  assign(results_forward.i, results_forward)
  
  save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/10.1_forward/"
  save_path <- paste0(save_dir, results_forward.i, ".RData")
  
  save(list = results_forward.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_forward <- mean(num.proxy_forward[complete.cases(num.proxy_forward)])

# ---------------------------------------------------------------

save(RD_forward, num.proxy_forward, avg.num.proxy_forward,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/10.1_forward/forward.RData")

