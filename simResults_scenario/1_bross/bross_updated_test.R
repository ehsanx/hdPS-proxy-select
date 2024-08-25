path0 <- "E:/GitHub/hdPS-proxy-select/"

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

RD_bross <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_bross <- c()
errors <- c()
# ---------------------------------------------------------------

### scenario: for-loop generating RD & SE results
set.seed(42)

# Set up the directories
save_dir_success <- paste0(path0, "simResults_scenario/1_bross/success/")
save_dir_unsuccess <- paste0(path0, "simResults_scenario/1_bross/unsuccess/")

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
  path <- paste0(path0, "simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  
  # Feature Selection Method 1: Bross Formula
  proxy.df <- data[, c("idx", grep("^rec", names(data), value = TRUE))]
  col_sums <- apply(proxy.df[,-1], 2, sum)
  all_zero_or_one_colnames <- colnames(proxy.df)[col_sums == 0 | col_sums == nrow(proxy.df)]
  proxy.df.filtered <- proxy.df[, !(colnames(proxy.df) %in% all_zero_or_one_colnames)]
  
  bross.list <- tryCatch({
    get_prioritised_covariates(df = proxy.df.filtered,
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
    sum.RD_bross <- c(NA, NA, NA)
    
    RD_bross[i,] <- sum.RD_bross
    rownames(RD_bross)[i] <- paste0("data.", i, "_bross")
    num.proxy_bross <- c(num.proxy_bross, NA)
    names(num.proxy_bross)[i] <- paste0("data.", i, "_bross")
    
    results_bross <- c(i, sum.RD_bross)
    names(results_bross) <- c("iteration", "numProxy", "RD", "SE")
    results_bross.i <- paste0("results_bross.", i)
    assign(results_bross.i, results_bross)
    
    #save_dir <- paste0(path0, "simResults_scenario/1_bross/unsuccess/")
    save_path <- paste0(save_dir_unsuccess, results_bross.i, ".RData")
    
    save(list = results_bross.i, file = save_path)
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
    sum.RD_bross <- c(length(proxy_bross), 
                      summary(fit.RD_bross)$coef["exposure", c("Estimate")], 
                      sqrt(sandwich::sandwich(fit.RD_bross)[2,2]))
    
    RD_bross[i,] <- sum.RD_bross
    rownames(RD_bross)[i] <- paste0("data.", i, "_bross")
    num.proxy_bross <- c(num.proxy_bross, length(proxy_bross))
    names(num.proxy_bross)[i] <- paste0("data.", i, "_bross")
    
    results_bross <- c(i, sum.RD_bross)
    names(results_bross) <- c("iteration", "numProxy", "RD", "SE")
    results_bross.i <- paste0("results_bross.", i)
    assign(results_bross.i, results_bross)
    
    iteration <- i
    
    #save_dir <- paste0(path0, "simResults_scenario/1_bross/success/")
    save_path <- paste0(save_dir_success, results_bross.i, ".RData")
    
    save(list = results_bross.i, file = save_path)
  }
}
# ---------------------------------------------------------------

avg.num.proxy_bross <- mean(num.proxy_bross[complete.cases(num.proxy_bross)])
err_ratio <- length(errors)/iteration
RD_bross_no.error <- RD_bross[complete.cases(RD_bross),]
# ---------------------------------------------------------------

save(RD_bross, num.proxy_bross, avg.num.proxy_bross, err_ratio, RD_bross_no.error, 
     file = paste0(path0, "simResults_scenario/1_bross/bross.RData"))