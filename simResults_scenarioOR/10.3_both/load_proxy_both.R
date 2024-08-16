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

a <- 1000
b <- 0

RD_both_OR <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_both_OR <- c()
# ---------------------------------------------------------------

### scenarioOR: for-loop generating RD & SE results
set.seed(42)

for (i in (b+1):a) {
  path_proxy <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/10.3_both/proxy_both.data.", i, ".RData")
  
  if (file.exists(path_proxy)) {
    proxyform_name <- load(path_proxy)
    proxyform <- get(proxyform_name)
    proxy_both <- strsplit(proxyform, split = "\\+")[[1]]
    
    path_data <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioOR/data_", i, ".rds")
    data <- readRDS(path_data)
    data$idx <- data$id
    
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
    
    RD_both_OR[i-b,] <- sum.RD_both
    rownames(RD_both_OR)[i-b] <- paste0("data.", i, "_both")
    num.proxy_both_OR <- c(num.proxy_both_OR, length(proxy_both))
    names(num.proxy_both_OR)[i-b] <- paste0("data.", i, "_both")
    
    results_both <- c(i, sum.RD_both)
    names(results_both) <- c("iteration", "numProxy", "RD", "SE")
    results_both.i <- paste0("results_both.", i)
    assign(results_both.i, results_both)
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/10.3_both/"
    save_path <- paste0(save_dir, results_both.i, ".RData")
    
    save(list = results_both.i, file = save_path)
  } else {
    RD_both_OR[i-b,] <- c(NA, NA, NA)
    rownames(RD_both_OR)[i-b] <- paste0("data.", i, "_both")
    num.proxy_both_OR <- c(num.proxy_both_OR, NA)
    names(num.proxy_both_OR)[i-b] <- paste0("data.", i, "_both")
  }
}

avg.num.proxy_both_OR <- mean(num.proxy_both_OR[complete.cases(num.proxy_both_OR)])
RD_both_OR_complete <- RD_both_OR[complete.cases(RD_both_OR),]
complete_ratio_both_OR <- nrow(RD_both_OR_complete)/nrow(RD_both_OR)
incomplete_indices_both_OR <- which(!complete.cases(RD_both_OR))

# ---------------------------------------------------------------

save(RD_both_OR, num.proxy_both_OR, avg.num.proxy_both_OR, complete_ratio_both_OR, incomplete_indices_both_OR,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/10.3_both/both.RData")

