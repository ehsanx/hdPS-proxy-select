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

a <- 1000
b <- 0

RD_both_ER <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_both_ER <- c()
# ---------------------------------------------------------------

### scenarioER: for-loop generating RD & SE results
set.seed(42)

for (i in (b+1):a) {
  path_proxy <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/10.3_both/proxy_both.data.", i, ".RData")
  
  if (file.exists(path_proxy)) {
    proxyform_name <- load(path_proxy)
    proxyform <- get(proxyform_name)
    proxy_both <- strsplit(proxyform, split = "\\+")[[1]]
    
    path_data <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_", i, ".rds")
    data <- readRDS(path_data)
    data$idx <- data$id
    
    if (length(proxy_both) == 0) {
      rhsform <- covform
    } else {
      rhsform <- paste0(c(covform, proxyform), collapse = "+")
    }
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
    
    RD_both_ER[i-b,] <- sum.RD_both
    rownames(RD_both_ER)[i-b] <- paste0("data.", i, "_both")
    num.proxy_both_ER <- c(num.proxy_both_ER, length(proxy_both))
    names(num.proxy_both_ER)[i-b] <- paste0("data.", i, "_both")
    
    results_both <- c(i, sum.RD_both)
    names(results_both) <- c("iteration", "numProxy", "RD", "SE")
    results_both.i <- paste0("results_both.", i)
    assign(results_both.i, results_both)
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/10.3_both/"
    save_path <- paste0(save_dir, results_both.i, ".RData")
    
    save(list = results_both.i, file = save_path)
  } else {
    RD_both_ER[i-b,] <- c(NA, NA, NA)
    rownames(RD_both_ER)[i-b] <- paste0("data.", i, "_both")
    num.proxy_both_ER <- c(num.proxy_both_ER, NA)
    names(num.proxy_both_ER)[i-b] <- paste0("data.", i, "_both")
  }
}

avg.num.proxy_both_ER <- mean(num.proxy_both_ER[complete.cases(num.proxy_both_ER)])
RD_both_ER_complete <- RD_both_ER[complete.cases(RD_both_ER),]
complete_ratio_both_ER <- nrow(RD_both_ER_complete)/nrow(RD_both_ER)
incomplete_indices_both_ER <- which(!complete.cases(RD_both_ER))

# ---------------------------------------------------------------

save(RD_both_ER, num.proxy_both_ER, avg.num.proxy_both_ER, complete_ratio_both_ER, incomplete_indices_both_ER,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/10.3_both/both.RData")

