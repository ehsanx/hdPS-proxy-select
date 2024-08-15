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
b <- 3

RD_both <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_both <- c()
# ---------------------------------------------------------------

### scenario: for-loop generating RD & SE results
set.seed(42)

# Folder "scenario"
for (i in (b+1):a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  initial.model <- glm(initial.formula, data = data, family = binomial)
  full.model <- glm(full.formula, data = data, family = binomial)
  stepwise_both <- stepAIC(initial.model, 
                           scope = list(lower = initial.model, 
                                        upper = full.model), 
                           direction = "both")
  
  sel.variables <- all.vars(formula(stepwise_both))[-1]
  proxy_both <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_both, collapse = "+")
  rhsform <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsform))
  
  W.out_both <- weightit(ps.formula,
                          data = data, 
                          estimand = "ATE",
                          method = "ps")
  fit.OR_both <- glm(out.formula,
                      data = data,
                      weights = W.out_both$weights,
                      family= binomial(link = "logit"))
  fit.RD_both <- glm(out.formula,
                      data= data,
                      weights= W.out_both$weights,
                      family=gaussian(link= "identity"))
  sum.RD_both <- c(length(proxy_both), 
                    summary(fit.RD_both)$coef["exposure", c("Estimate")], 
                    sqrt(sandwich::sandwich(fit.RD_both)[2,2]))
  
  RD_both[i-b,] <- sum.RD_both
  rownames(RD_both)[i-b] <- paste0("data.", i, "_both")
  num.proxy_both <- c(num.proxy_both, length(proxy_both))
  names(num.proxy_both)[i-b] <- paste0("data.", i, "_both")
  
  results_both <- c(i, sum.RD_both)
  names(results_both) <- c("iteration", "numProxy", "RD", "SE")
  results_both.i <- paste0("results_both.", i)
  assign(results_both.i, results_both)
  
  save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/10.3_both/"
  save_path <- paste0(save_dir, results_both.i, ".RData")
  
  save(list = results_both.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_both <- mean(num.proxy_both[complete.cases(num.proxy_both)])

# ---------------------------------------------------------------

save(RD_both, num.proxy_both, avg.num.proxy_both,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/10.3_both/both.RData")
