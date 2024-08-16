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
path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioOR/data_1.rds")
data <- readRDS(path)
proxy.list <- names(data[, c(grep("^rec", names(data), value = TRUE))])
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(c("outcome~ exposure", 
                              covarsTfull), collapse = "+") )
initial.formula <- as.formula(paste0("outcome~exposure+",
                                     covform,
                                     collapse = "+"))
full.formula <- as.formula(paste0("outcome~exposure+",
                                  paste0(covarsTfull, collapse = "+"),
                                  collapse = "+"))
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 1000
b <- 0

RD_backward <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_backward <- c()
# ---------------------------------------------------------------

### scenarioOR: for-loop generating RD & SE results
set.seed(42)

# Folder "scenarioOR"
for (i in (b+1):a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioOR/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id

  stepwise_backward <- regsubsets(full.formula, data = data, method = "backward", nvmax = length(proxy.list), nbest = 10)
  summary_stepwise <- summary(stepwise_backward)
  best_model <- which.max(summary_stepwise$adjr2)
  sel.variables <- names(summary_stepwise$which[best_model,])[summary_stepwise$which[best_model,]]
  proxy_backward <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_backward, collapse = "+")
  rhsform <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsform))
  
  W.out_backward <- weightit(ps.formula,
                          data = data, 
                          estimand = "ATE",
                          method = "ps")
  fit.OR_backward <- glm(out.formula,
                      data = data,
                      weights = W.out_backward$weights,
                      family= binomial(link = "logit"))
  fit.RD_backward <- glm(out.formula,
                      data= data,
                      weights= W.out_backward$weights,
                      family=gaussian(link= "identity"))
  sum.RD_backward <- c(length(proxy_backward), 
                    summary(fit.RD_backward)$coef["exposure", c("Estimate")], 
                    sqrt(sandwich::sandwich(fit.RD_backward)[2,2]))
  
  RD_backward[i-b,] <- sum.RD_backward
  rownames(RD_backward)[i-b] <- paste0("data.", i, "_backward")
  num.proxy_backward <- c(num.proxy_backward, length(proxy_backward))
  names(num.proxy_backward)[i-b] <- paste0("data.", i, "_backward")
  
  results_backward <- c(i, sum.RD_backward)
  names(results_backward) <- c("iteration", "numProxy", "RD", "SE")
  results_backward.i <- paste0("results_backward.", i)
  assign(results_backward.i, results_backward)
  
  save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/10.2_backward/"
  save_path <- paste0(save_dir, results_backward.i, ".RData")
  
  save(list = results_backward.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_backward <- mean(num.proxy_backward[complete.cases(num.proxy_backward)])

# ---------------------------------------------------------------

save(RD_backward, num.proxy_backward, avg.num.proxy_backward,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/10.2_backward/backward.RData")

