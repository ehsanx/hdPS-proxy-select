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
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 1000

RD_hybrid_ER <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_hybrid <- c()
errors <- c()
# ---------------------------------------------------------------

### scenarioER: for-loop generating RD & SE results
set.seed(42)

# Folder "scenarioER"
for (i in 1:a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenarioER/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  # Feature Selection Method 1: hybrid Formula
  proxy.df <- data[, c("idx", grep("^rec", names(data), value = TRUE))]
  
  hybrid.list <- tryCatch({
    get_prioritised_covariates(df = proxy.df,
                               patientIdVarname = "idx", 
                               exposureVector = data$exposure,
                               outcomeVector = data$outcome,
                               patientIdVector = data$idx, 
                               k = 100)
  }, error = function(e) {
    errors <<- c(errors, paste("Error at iteration", i, ":", e$message))
  })
  
  if (class(hybrid.list) == "character") {
    proxy_hybrid <- c(NA)
    sum.RD_hybrid <- c(NA, NA, NA)
    
    RD_hybrid_ER[i,] <- sum.RD_hybrid
    rownames(RD_hybrid_ER)[i] <- paste0("data.", i, "_hybrid")
    num.proxy_hybrid <- c(num.proxy_hybrid, NA)
    names(num.proxy_hybrid)[i] <- paste0("data.", i, "_hybrid")
    
    results_hybrid <- c(i, sum.RD_hybrid)
    names(results_hybrid) <- c("iteration", "numProxy", "RD", "SE")
    results_hybrid.i <- paste0("results_hybrid.", i)
    assign(results_hybrid.i, results_hybrid)
    
    iteration <- i
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/2_hybrid/"
    save_path <- paste0(save_dir, results_hybrid.i, ".RData")
    
    save(list = results_hybrid.i, file = save_path)
  } else {
    proxy.df_hybrid <- hybrid.list$autoselected_covariate_df
    hdps.data_hybrid <- merge(data[,c("idx",
                                     outcome, 
                                     exposure, 
                                     investigator.specified.covariates)],
                             proxy.df_hybrid,
                             by = "idx")
    hdps.data_hybrid$id <- hdps.data_hybrid$idx
    hdps.data_hybrid$idx <- NULL
    
    hdps.data_hybrid$exposure <- as.numeric(I(hdps.data_hybrid$obese=='Yes'))
    hdps.data_hybrid$outcome <- as.numeric(I(hdps.data_hybrid$diabetes=='Yes'))
    
    proxy_hybrid <- names(proxy.df_hybrid[,-1])
    covarsTfull <- c(investigator.specified.covariates, proxy_hybrid)
    Y.form <- as.formula(paste0(c("outcome~ exposure", covarsTfull), 
                                collapse = "+") )
    covar.mat <- model.matrix(Y.form, data = hdps.data_hybrid)[,-1]
    lasso.fit_hybrid <- glmnet::cv.glmnet(y = hdps.data_hybrid$outcome,
                                          x = covar.mat, 
                                          type.measure='mse',
                                          family="binomial",
                                          alpha = 1, 
                                          nfolds = 5)
    coef.fit <- coef(lasso.fit_hybrid,s='lambda.min',exact=TRUE)
    sel.variables <- row.names(coef.fit)[which(as.numeric(coef.fit)!=0)]
    proxy_hybrid <- proxy.list[proxy.list %in% sel.variables]
    proxyform <- paste0(proxy_hybrid, collapse = "+")
    rhsform <- paste0(c(covform, proxyform), collapse = "+")
    ps.formula <- as.formula(paste0("exposure", "~", rhsform))
    
    W.out_hybrid <- weightit(ps.formula,
                            data = hdps.data_hybrid, 
                            estimand = "ATE",
                            method = "ps")
    fit.OR_hybrid <- glm(out.formula,
                        data = hdps.data_hybrid,
                        weights = W.out_hybrid$weights,
                        family= binomial(link = "logit"))
    fit.RD_hybrid <- glm(out.formula,
                        data= hdps.data_hybrid,
                        weights= W.out_hybrid$weights,
                        family=gaussian(link= "identity"))
    sum.RD_hybrid <- c(length(proxy_hybrid), 
                      summary(fit.RD_hybrid)$coef["exposure", c("Estimate")], 
                      sqrt(sandwich::sandwich(fit.RD_hybrid)[2,2]))
    
    RD_hybrid_ER[i,] <- sum.RD_hybrid
    rownames(RD_hybrid_ER)[i] <- paste0("data.", i, "_hybrid")
    num.proxy_hybrid <- c(num.proxy_hybrid, length(proxy_hybrid))
    names(num.proxy_hybrid)[i] <- paste0("data.", i, "_hybrid")
    
    results_hybrid <- c(i, sum.RD_hybrid)
    names(results_hybrid) <- c("iteration", "numProxy", "RD", "SE")
    results_hybrid.i <- paste0("results_hybrid.", i)
    assign(results_hybrid.i, results_hybrid)
    
    iteration <- i
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/2_hybrid/"
    save_path <- paste0(save_dir, results_hybrid.i, ".RData")
    
    save(list = results_hybrid.i, file = save_path)
  }
}
# ---------------------------------------------------------------

avg.num.proxy_hybrid <- mean(num.proxy_hybrid[complete.cases(num.proxy_hybrid)])
err_ratio <- length(errors)/iteration
RD_hybrid_ER_no.error <- RD_hybrid_ER[complete.cases(RD_hybrid_ER),]
# ---------------------------------------------------------------

save(RD_hybrid_ER, num.proxy_hybrid, avg.num.proxy_hybrid, err_ratio, RD_hybrid_ER_no.error, 
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/2_hybrid/hybrid.RData")
