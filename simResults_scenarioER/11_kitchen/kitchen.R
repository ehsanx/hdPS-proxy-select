# path0 <- "E:/GitHub/hdPS-proxy-select/"
path0 <- "C:/Users/Ehsan/Documents/GitHub/hdPS-proxy-select/"

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

RD_kitchen <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_kitchen <- c()
errors <- c()
# ---------------------------------------------------------------

### scenario: for-loop generating RD & SE results
set.seed(42)

# Set up the directories
save_dir_success <- paste0(path0, "simResults_scenarioER/11_kitchen/success/")
save_dir_unsuccess <- paste0(path0, "simResults_scenarioER/11_kitchen/unsuccess/")

# Ensure the "success" directory exists
if (!file.exists(save_dir_success)) {
  dir.create(save_dir_success, recursive = TRUE)
}

# Ensure the "unsuccess" directory exists
if (!file.exists(save_dir_unsuccess)) {
  dir.create(save_dir_unsuccess, recursive = TRUE)
}

# Now you can use save_dir_success and save_dir_unsuccess in your loop for saving files



a0 <- 1
# Folder "scenario"
for (i in a0:a) {
  # path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  path <- paste0(path0, "simData/scenarioER/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  # data$idx <- data$id
  
  # Feature Selection Method 1: Bross Formula
  #proxy.df <- data[, c("idx", grep("^rec", names(data), value = TRUE))]
  #col_sums <- apply(proxy.df[,-1], 2, sum)
  #all_zero_or_one_colnames <- colnames(proxy.df)[col_sums == 0 | col_sums == nrow(proxy.df)]
  #proxy.df.filtered <- proxy.df[, !(colnames(proxy.df) %in% all_zero_or_one_colnames)]
  
  # kitchen.list <- tryCatch({
  #   get_prioritised_covariates(df = proxy.df.filtered,
  #                              patientIdVarname = "idx", 
  #                              exposureVector = data$exposure,
  #                              outcomeVector = data$outcome,
  #                              patientIdVector = data$idx, 
  #                              k = 100)
  # }, error = function(e) {
  #   # errors <<- c(errors, as.character(i))
  #   errors <<- c(errors, paste("Error at iteration", i, ":", e$message))
  # })
  # 
  # if (class(kitchen.list) == "character") {
  #   iteration <- i
  #   proxy_kitchen <- c(NA)
  #   sum.RD_kitchen <- c(NA, NA, NA)
  #   
  #   RD_kitchen[i,] <- sum.RD_kitchen
  #   rownames(RD_kitchen)[i] <- paste0("data.", i, "_kitchen")
  #   num.proxy_kitchen <- c(num.proxy_kitchen, NA)
  #   names(num.proxy_kitchen)[i] <- paste0("data.", i, "_kitchen")
  #   
  #   results_kitchen <- c(i, sum.RD_kitchen)
  #   names(results_kitchen) <- c("iteration", "numProxy", "RD", "SE")
  #   results_kitchen.i <- paste0("results_kitchen.", i)
  #   assign(results_kitchen.i, results_kitchen)
  #   
  #   #save_dir <- paste0(path0, "simResults_scenarioER/11_kitchen/unsuccess/")
  #   save_path <- paste0(save_dir_unsuccess, results_kitchen.i, ".RData")
  #   
  #   save(list = results_kitchen.i, file = save_path)
  # } else {
    # proxy.df_kitchen <- kitchen.list$autoselected_covariate_df
    # hdps.data_kitchen <- merge(data[,c("idx",
    #                                  outcome, 
    #                                  exposure, 
    #                                  investigator.specified.covariates)],
    #                          proxy.df_kitchen,
    #                          by = "idx")
    # hdps.data_kitchen$id <- hdps.data_kitchen$idx
    # hdps.data_kitchen$idx <- NULL
    
  hdps.data_kitchen <- data
  # hdps.data_kitchen$exposure <- as.numeric(I(hdps.data_kitchen$obese=='Yes'))
  # hdps.data_kitchen$outcome <- as.numeric(I(hdps.data_kitchen$diabetes=='Yes'))
    
  proxy_kitchen <- grep("^rec", names(data), value = TRUE)
  proxyform <- paste0(proxy_kitchen, collapse = "+")
  rhsformula <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsformula))
    
  W.out_kitchen <- weightit(ps.formula,
                            data = hdps.data_kitchen, 
                            estimand = "ATE",
                            method = "ps")
  # fit.OR_kitchen <- glm(out.formula,
  #                       data = hdps.data_kitchen,
  #                       weights = W.out_kitchen$weights,
  #                       family= binomial(link = "logit"))
  fit.RD_kitchen <- glm(out.formula,
                        data= hdps.data_kitchen,
                        weights= W.out_kitchen$weights,
                        family=gaussian(link= "identity"))
  sum.RD_kitchen <- c(length(proxy_kitchen), 
                      summary(fit.RD_kitchen)$coef["exposure", c("Estimate")], 
                      sqrt(sandwich::sandwich(fit.RD_kitchen)[2,2]))
    
  RD_kitchen[i,] <- sum.RD_kitchen
  rownames(RD_kitchen)[i] <- paste0("data.", i, "_kitchen")
  num.proxy_kitchen <- c(num.proxy_kitchen, length(proxy_kitchen))
  names(num.proxy_kitchen)[i] <- paste0("data.", i, "_kitchen")
    
 results_kitchen <- c(i, sum.RD_kitchen)
 names(results_kitchen) <- c("iteration", "numProxy", "RD", "SE")
 results_kitchen.i <- paste0("results_kitchen.", i)
 assign(results_kitchen.i, results_kitchen)
    
 iteration <- i
    
    #save_dir <- paste0(path0, "simResults_scenarioER/11_kitchen/success/")
  save_path <- paste0(save_dir_success, results_kitchen.i, ".RData")
    
  save(list = results_kitchen.i, file = save_path)
}
# ---------------------------------------------------------------

avg.num.proxy_kitchen <- mean(num.proxy_kitchen[complete.cases(num.proxy_kitchen)])
err_ratio <- length(errors)/iteration
RD_kitchen_no.error <- RD_kitchen[complete.cases(RD_kitchen),]
# ---------------------------------------------------------------

save(RD_kitchen, num.proxy_kitchen, avg.num.proxy_kitchen, err_ratio, RD_kitchen_no.error, 
     file = paste0(path0, "simResults_scenarioER/11_kitchen/kitchen.RData"))