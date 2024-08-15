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
path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioOR/data_1.rds")
data <- readRDS(path)
proxy.list <- names(data[, c(grep("^rec", names(data), value = TRUE))])
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(c("outcome~ exposure", 
                              covarsTfull), collapse = "+") )

# function for GA
gaOpt <- function(vars, IV.train, DV.train) {
  varNames <- colnames(IV.train) #getting names of all variables
  selectedVarNames <- varNames[vars == "1"] # getting names of selected vars from GA
  gaSolutionData <- IV.train[,selectedVarNames] # keeping only those selected vars
  
  gaDat <- cbind(gaSolutionData,DV.train) # combining selected variables with outcome variable
  gaMod <- glm(DV.train ~ ., family = "binomial", data = gaDat) #build model
  gaProb <- predict(gaMod, IV.train, type = "response") # get probabilities
  gaPred <- ifelse(gaProb >= .8, 1, 0) # get predicted 0s and 1s
  
  ari <- adjustedRandIndex(gaPred, DV.train)
  return(ari)
}
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 1000

RD_ga_OR <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_ga <- c()
# ---------------------------------------------------------------

### scenarioOR: for-loop generating RD & SE results
set.seed(42)

# Folder "scenarioOR"
for (i in 1:a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioOR/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  covar.mat <- model.matrix(Y.form, data = data)[,-1]
  
  ga.fit <- ga(fitness = function(vars)
    gaOpt(vars = vars, 
          IV.train = data.frame(covar.mat),
          DV.train = data$outcome),
    type = "binary", 
    nBits = ncol(covar.mat),
    names = colnames(covar.mat), 
    seed = 42,
    run=5)
  
  sel.variables <- proxy.list[ga.fit@solution[1,]==1]
  proxy_ga <- proxy.list[proxy.list %in% sel.variables]
  proxyform <- paste0(proxy_ga, collapse = "+")
  rhsform <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsform))
  
  W.out_ga <- weightit(ps.formula,
                          data = data, 
                          estimand = "ATE",
                          method = "ps")
  fit.OR_ga <- glm(out.formula,
                      data = data,
                      weights = W.out_ga$weights,
                      family= binomial(link = "logit"))
  fit.RD_ga <- glm(out.formula,
                      data= data,
                      weights= W.out_ga$weights,
                      family=gaussian(link= "identity"))
  sum.RD_ga <- c(length(proxy_ga), 
                    summary(fit.RD_ga)$coef["exposure", c("Estimate")], 
                    sqrt(sandwich::sandwich(fit.RD_ga)[2,2]))
  
  RD_ga_OR[i,] <- sum.RD_ga
  rownames(RD_ga_OR)[i] <- paste0("data.", i, "_ga")
  num.proxy_ga <- c(num.proxy_ga, length(proxy_ga))
  names(num.proxy_ga)[i] <- paste0("data.", i, "_ga")
  
  results_ga <- c(i, sum.RD_ga)
  names(results_ga) <- c("iteration", "numProxy", "RD", "SE")
  results_ga.i <- paste0("results_ga.", i)
  assign(results_ga.i, results_ga)
  
  save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/6_ga/"
  save_path <- paste0(save_dir, results_ga.i, ".RData")
  
  save(list = results_ga.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_ga <- mean(num.proxy_ga[complete.cases(num.proxy_ga)])

# ---------------------------------------------------------------

save(RD_ga_OR, num.proxy_ga, avg.num.proxy_ga,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/6_ga/ga.RData")
