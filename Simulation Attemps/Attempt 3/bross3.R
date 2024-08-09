library(autoCovariateSelection)
library(dplyr)
library(cobalt)
library(WeightIt)
library(tidyverse)
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

setwd("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/1_bross")

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

RD_bross <- data.frame(RD = numeric(), SE = numeric())
num.proxy_bross <- c()
errors <- c()
# ---------------------------------------------------------------

### scenario: for-loop generating RD & SE results
set.seed(42)
set.seed(42)

# Folder "scenario"
for (i in 1:a) {
  path <- paste0("../../simData/scenario/data_", i, ".rds")
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
    errors <<- c(errors, as.character(i))
  })
  
  if (class(bross.list) == "character") {
    proxy_bross <- c(NA)
    sum.RD_bross <- c(NA, NA)
    
    RD_bross[i,] <- sum.RD_bross
    rownames(RD_bross)[i] <- paste0("data.", i, "_bross")
    num.proxy_bross <- c(num.proxy_bross, NA)
    names(num.proxy_bross)[i] <- paste0("data.", i, "_bross")
    
    # proxy_bross.i <- paste0("proxy_bross.", i)
    # assign(proxy_bross.i, proxy_bross)
    num.proxy_bross.i <- paste0("num.proxy_bross.", i)
    assign(num.proxy_bross.i, NA)
    sum.RD_bross.i <- paste0("sum.RD_bross.", i)
    assign(sum.RD_bross.i, sum.RD_bross)
    
    iteration <- i
  } else {
    proxy.df_bross <- bross.list$autoselected_covariate_df
    hdps.data_bross <- merge(data[,c("idx",
                                     outcome, 
                                     exposure, 
                                     investigator.specified.covariates)],
                             proxy.df_bross,
                             by = "idx")
    hdps.data_bross$id <- hdps.data_bross$idx
    hdps.data_bross$idx <- NULL
    
    hdps.data_bross$exposure <- as.numeric(I(hdps.data_bross$obese=='Yes'))
    hdps.data_bross$outcome <- as.numeric(I(hdps.data_bross$diabetes=='Yes'))
    
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
    fit.RD_bross <- glm(out.formula,
                        data= hdps.data_bross,
                        weights= W.out_bross$weights,
                        family=gaussian(link= "identity"))
    sum.RD_bross <- c(summary(fit.RD_bross)$coef["exposure", c("Estimate")], sqrt(sandwich::sandwich(fit.RD_bross)[2,2]))
    
    RD_bross[i,] <- sum.RD_bross
    rownames(RD_bross)[i] <- paste0("data.", i, "_bross")
    num.proxy_bross <- c(num.proxy_bross, length(proxy_bross))
    names(num.proxy_bross)[i] <- paste0("data.", i, "_bross")
    
    # proxy_bross.i <- paste0("proxy_bross.", i)
    # assign(proxy_bross.i, proxy_bross)
    num.proxy_bross.i <- paste0("num.proxy_bross.", i)
    assign(num.proxy_bross.i, length(proxy_bross))
    sum.RD_bross.i <- paste0("sum.RD_bross.", i)
    assign(sum.RD_bross.i, sum.RD_bross)
    
    iteration <- i
  }
}
# ---------------------------------------------------------------

avg.num.proxy_bross <- mean(num.proxy_bross[complete.cases(num.proxy_bross)])
err_ratio <- length(errors)/iteration
RD_bross_no.error <- RD_bross[complete.cases(RD_bross),]
# ---------------------------------------------------------------

save.image(file = "bross.RData")