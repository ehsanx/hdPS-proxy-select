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

RD_enet <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_enet <- c()
# ---------------------------------------------------------------

### scenario: for-loop generating RD & SE results
set.seed(42)

# Folder "scenario"
for (i in 1:a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  covar.mat <- model.matrix(Y.form, data = data)[,-1]
  
  enet.fit <- glmnet::cv.glmnet(y = data$outcome, 
                                 x = covar.mat, 
                                 type.measure='mse',
                                 family="binomial",
                                 alpha = 0.5, 
                                 nfolds = 5)
  coef.fit <- coef(enet.fit,s='lambda.min',exact=TRUE)
  sel.variables <- row.names(coef.fit)[which(as.numeric(coef.fit)!=0)]
  proxy_enet <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_enet, collapse = "+")
  rhsform <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsform))
    
  W.out_enet <- weightit(ps.formula,
                          data = data, 
                          estimand = "ATE",
                          method = "ps")
  fit.OR_enet <- glm(out.formula,
                      data = data,
                      weights = W.out_enet$weights,
                      family= binomial(link = "logit"))
  fit.RD_enet <- glm(out.formula,
                      data= data,
                      weights= W.out_enet$weights,
                      family=gaussian(link= "identity"))
  sum.RD_enet <- c(length(proxy_enet), 
                    summary(fit.RD_enet)$coef["exposure", c("Estimate")], 
                    sqrt(sandwich::sandwich(fit.RD_enet)[2,2]))
    
  RD_enet[i,] <- sum.RD_enet
  rownames(RD_enet)[i] <- paste0("data.", i, "_enet")
  num.proxy_enet <- c(num.proxy_enet, length(proxy_enet))
  names(num.proxy_enet)[i] <- paste0("data.", i, "_enet")
    
  results_enet <- c(i, sum.RD_enet)
  names(results_enet) <- c("iteration", "numProxy", "RD", "SE")
  results_enet.i <- paste0("results_enet.", i)
  assign(results_enet.i, results_enet)
    
  save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/4_enet/"
  save_path <- paste0(save_dir, results_enet.i, ".RData")
    
  save(list = results_enet.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_enet <- mean(num.proxy_enet[complete.cases(num.proxy_enet)])

# ---------------------------------------------------------------

save(RD_enet, num.proxy_enet, avg.num.proxy_enet,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/4_enet/enet.RData")


