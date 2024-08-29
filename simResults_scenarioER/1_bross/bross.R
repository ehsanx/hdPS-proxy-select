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
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 1000

RD_bross.ER <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_bross.ER <- c()
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
  
  # Feature Selection Method 1: Bross Formula
  proxy.df <- data[, c("idx", grep("^rec", names(data), value = TRUE))]
  
  bross.list <- tryCatch({
    get_prioritised_covariates(df = proxy.df,
                               patientIdVarname = "idx", 
                               exposureVector = data$exposure,
                               outcomeVector = data$outcome,
                               patientIdVector = data$idx, 
                               k = 100)
  }, error = function(e) {
    # errors <<- c(errors, as.character(i))
    errors <<- c(errors, paste("Error at iteration", i, ":", e$message))
  })
  
  if (class(bross.list) == "character") {
    iteration <- i
    proxy_bross <- c(NA)
    sum.RD_bross.ER <- c(NA, NA, NA)
    
    RD_bross.ER[i,] <- sum.RD_bross.ER
    rownames(RD_bross.ER)[i] <- paste0("data.ER.", i, "_bross")
    num.proxy_bross.ER <- c(num.proxy_bross.ER, NA)
    names(num.proxy_bross.ER)[i] <- paste0("data.ER.", i, "_bross")
    
    results_bross <- c(i, sum.RD_bross.ER)
    names(results_bross) <- c("iteration", "numProxy", "RD", "SE")
    results_bross.ER.i <- paste0("results_bross.ER.", i)
    assign(results_bross.ER.i, results_bross)
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/1_bross/"
    save_path <- paste0(save_dir, results_bross.ER.i, ".RData")
    
    save(list = results_bross.ER.i, file = save_path)
  } else {
    proxy.df_bross <- bross.list$autoselected_covariate_df
    hdps.data_bross <- merge(data[,c("idx",
                                     "outcome", 
                                     "exposure", 
                                     investigator.specified.covariates)],
                             proxy.df_bross,
                             by = "idx")
    hdps.data_bross$id <- hdps.data_bross$idx
    hdps.data_bross$idx <- NULL
    
    #hdps.data_bross$exposure <- as.numeric(I(hdps.data_bross$obese=='Yes'))
    #hdps.data_bross$outcome <- as.numeric(I(hdps.data_bross$diabetes=='Yes'))
    
    proxy_bross <- names(proxy.df_bross[,-1])
    proxyform <- paste0(proxy_bross, collapse = "+")
    rhsformula <- paste0(c(covform, proxyform), collapse = "+")
    ps.formula <- as.formula(paste0("exposure", "~", rhsformula))
    
    W.out_bross <- weightit(ps.formula,
                            data = hdps.data_bross, 
                            estimand = "ATE",
                            method = "ps")
    fit.OR_bross <- glm(out.formula,
                        data = hdps.data_bross,
                        weights = W.out_bross$weights,
                        family= binomial(link = "logit"))
    fit.RD_bross.ER <- glm(out.formula,
                        data= hdps.data_bross,
                        weights= W.out_bross$weights,
                        family=gaussian(link= "identity"))
    sum.RD_bross.ER <- c(length(proxy_bross), 
                      summary(fit.RD_bross.ER)$coef["exposure", c("Estimate")], 
                      sqrt(sandwich::sandwich(fit.RD_bross.ER)[2,2]))
    
    RD_bross.ER[i,] <- sum.RD_bross.ER
    rownames(RD_bross.ER)[i] <- paste0("data.ER.", i, "_bross")
    num.proxy_bross.ER <- c(num.proxy_bross.ER, length(proxy_bross))
    names(num.proxy_bross.ER)[i] <- paste0("data.ER.", i, "_bross")
    
    results_bross <- c(i, sum.RD_bross.ER)
    names(results_bross) <- c("iteration", "numProxy", "RD", "SE")
    results_bross.ER.i <- paste0("results_bross.ER.", i)
    assign(results_bross.ER.i, results_bross)
    
    iteration <- i
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/1_bross/"
    save_path <- paste0(save_dir, results_bross.ER.i, ".RData")
    
    save(list = results_bross.ER.i, file = save_path)
  }
}
# ---------------------------------------------------------------

avg.num.proxy_bross.ER <- mean(num.proxy_bross.ER[complete.cases(num.proxy_bross.ER)])
err_ratio.ER <- length(errors)/iteration
RD_bross.ER_no.error <- RD_bross.ER[complete.cases(RD_bross.ER),]
# ---------------------------------------------------------------

save(RD_bross.ER, num.proxy_bross.ER, avg.num.proxy_bross.ER, err_ratio.ER, RD_bross.ER_no.error, 
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/1_bross/bross.RData")