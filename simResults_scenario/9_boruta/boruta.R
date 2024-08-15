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
                              covarsTfull), collapse = "+") )
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 1000
b <- 4

RD_boruta <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_boruta <- c()
# ---------------------------------------------------------------

### scenario: for-loop generating RD & SE results
set.seed(42)

# Folder "scenario"
for (i in (b+1):a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  boruta.fit <- Boruta(Y.form, data = data, doTrace = 1)
  sel.variables <- names(boruta.fit$finalDecision)[which(boruta.fit$finalDecision != "Rejected")]
  proxy_boruta <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_boruta, collapse = "+")
  rhsform <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsform))
  
  W.out_boruta <- weightit(ps.formula,
                          data = data, 
                          estimand = "ATE",
                          method = "ps")
  fit.OR_boruta <- glm(out.formula,
                      data = data,
                      weights = W.out_boruta$weights,
                      family= binomial(link = "logit"))
  fit.RD_boruta <- glm(out.formula,
                      data= data,
                      weights= W.out_boruta$weights,
                      family=gaussian(link= "identity"))
  sum.RD_boruta <- c(length(proxy_boruta), 
                    summary(fit.RD_boruta)$coef["exposure", c("Estimate")], 
                    sqrt(sandwich::sandwich(fit.RD_boruta)[2,2]))
  
  RD_boruta[i-b,] <- sum.RD_boruta
  rownames(RD_boruta)[i-b] <- paste0("data.", i, "_boruta")
  num.proxy_boruta <- c(num.proxy_boruta, length(proxy_boruta))
  names(num.proxy_boruta)[i-b] <- paste0("data.", i, "_boruta")
  
  results_boruta <- c(i, sum.RD_boruta)
  names(results_boruta) <- c("iteration", "numProxy", "RD", "SE")
  results_boruta.i <- paste0("results_boruta.", i)
  assign(results_boruta.i, results_boruta)
  
  save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/9_boruta/"
  save_path <- paste0(save_dir, results_boruta.i, ".RData")
  
  save(list = results_boruta.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_boruta <- mean(num.proxy_boruta[complete.cases(num.proxy_boruta)])

# ---------------------------------------------------------------

save(RD_boruta, num.proxy_boruta, avg.num.proxy_boruta,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/9_boruta/boruta.RData")
