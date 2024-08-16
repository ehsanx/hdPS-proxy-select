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

RD_ga_ER <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_ga_ER <- c()
# ---------------------------------------------------------------

### scenarioER: for-loop generating RD & SE results
set.seed(42)

for (i in (b+1):a) {
  path_proxy <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/6_ga/proxy_ga.data.", i, ".RData")
  
  if (file.exists(path_proxy)) {
    proxyform_name <- load(path_proxy)
    proxyform <- get(proxyform_name)
    proxy_ga <- strsplit(proxyform, split = "\\+")[[1]]
    
    path_data <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_", i, ".rds")
    data <- readRDS(path_data)
    data$idx <- data$id
    
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
    
    RD_ga_ER[i-b,] <- sum.RD_ga
    rownames(RD_ga_ER)[i-b] <- paste0("data.", i, "_ga")
    num.proxy_ga_ER <- c(num.proxy_ga_ER, length(proxy_ga))
    names(num.proxy_ga_ER)[i-b] <- paste0("data.", i, "_ga")
    
    results_ga <- c(i, sum.RD_ga)
    names(results_ga) <- c("iteration", "numProxy", "RD", "SE")
    results_ga.i <- paste0("results_ga.", i)
    assign(results_ga.i, results_ga)
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/6_ga/"
    save_path <- paste0(save_dir, results_ga.i, ".RData")
    
    save(list = results_ga.i, file = save_path)
  } else {
    RD_ga_ER[i-b,] <- c(NA, NA, NA)
    rownames(RD_ga_ER)[i-b] <- paste0("data.", i, "_ga")
    num.proxy_ga_ER <- c(num.proxy_ga_ER, NA)
    names(num.proxy_ga_ER)[i-b] <- paste0("data.", i, "_ga")
  }
}

avg.num.proxy_ga_ER <- mean(num.proxy_ga_ER[complete.cases(num.proxy_ga_ER)])
RD_ga_ER_complete <- RD_ga_ER[complete.cases(RD_ga_ER),]
complete_ratio_ga_ER <- nrow(RD_ga_ER_complete)/nrow(RD_ga_ER)
incomplete_indices_ga_ER <- which(!complete.cases(RD_ga_ER))

# ---------------------------------------------------------------

save(RD_ga_ER, num.proxy_ga_ER, avg.num.proxy_ga_ER, complete_ratio_ga_ER, incomplete_indices_ga_ER,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/6_ga/ga.RData")
