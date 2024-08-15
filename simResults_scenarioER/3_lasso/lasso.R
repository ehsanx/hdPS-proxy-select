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
path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_1.rds")
data <- readRDS(path)
proxy.list <- names(data[, c(grep("^rec", names(data), value = TRUE))])
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(c("outcome~ exposure", 
                              covarsTfull), collapse = "+") )
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 1000

RD_lasso_ER <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_lasso <- c()
# ---------------------------------------------------------------

### scenarioER: for-loop generating RD & SE results
set.seed(42)

# Folder "scenarioER"
for (i in 1:a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_", i, ".rds")
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
  rhsform <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsform))
    
  W.out_lasso <- weightit(ps.formula,
                          data = data, 
                          estimand = "ATE",
                          method = "ps")
  fit.OR_lasso <- glm(out.formula,
                      data = data,
                      weights = W.out_lasso$weights,
                      family= binomial(link = "logit"))
  fit.RD_lasso <- glm(out.formula,
                      data= data,
                      weights= W.out_lasso$weights,
                      family=gaussian(link= "identity"))
  sum.RD_lasso <- c(length(proxy_lasso), 
                    summary(fit.RD_lasso)$coef["exposure", c("Estimate")], 
                    sqrt(sandwich::sandwich(fit.RD_lasso)[2,2]))
    
  RD_lasso_ER[i,] <- sum.RD_lasso
  rownames(RD_lasso_ER)[i] <- paste0("data.", i, "_lasso")
  num.proxy_lasso <- c(num.proxy_lasso, length(proxy_lasso))
  names(num.proxy_lasso)[i] <- paste0("data.", i, "_lasso")
    
  results_lasso <- c(i, sum.RD_lasso)
  names(results_lasso) <- c("iteration", "numProxy", "RD", "SE")
  results_lasso.i <- paste0("results_lasso.", i)
  assign(results_lasso.i, results_lasso)
    
  save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/3_lasso/"
  save_path <- paste0(save_dir, results_lasso.i, ".RData")
    
  save(list = results_lasso.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_lasso <- mean(num.proxy_lasso[complete.cases(num.proxy_lasso)])

# ---------------------------------------------------------------

save(RD_lasso_ER, num.proxy_lasso, avg.num.proxy_lasso,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/3_lasso/lasso.RData")
