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
# ---------------------------------------------------------------

### initialization before for-loop
# 'a' ranges from 1 to 1000 to change the number of datasets loaded in each for-loop
a <- 5

RD_svm <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_svm <- c()
errors <- c()
# ---------------------------------------------------------------

### scenario: for-loop generating RD & SE results
set.seed(42)

# Folder "scenario"
for (i in 1:a) {
  path <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  
  # found some id != idx
  data$idx <- data$id
  covar.mat <- model.matrix(Y.form, data = data)[,-1]
  
  svmTrainOutcome <- data$outcome
  svmTrainOutcome[svmTrainOutcome == 0] <- -1
  svmTrainPreds <- data.frame(covar.mat)
  
  #bounds <- t(data.frame(log2lambda1=c(-10, 10), log2lambda2=c(-10,10)))
  #colnames(bounds)<-c("lower", "upper")
  lambda1.scad <- c(seq(0.01 ,0.05, .01), seq(0.1,0.5, 0.2), 1 ) 
  
  svm.model <- tryCatch({
    svmfs(x=covar.mat,
          y = svmTrainOutcome,
          fs.method = "scad+L2",
          #bounds=bounds,
          grid.search = "discrete",
          lambda1.set = lambda1.scad,
          inner.val.method = "cv",
          show = "none",
          parms.coding = "none",
          maxIter = 50,
          seed=42)
  }, error = function(e) {
    errors <<- c(errors, paste("Error at iteration", i, "(fit):", e$message))
  })
  
  if (class(svm.model) == "character") {
    proxy_svm <- c(NA)
    sum.RD_svm <- c(NA, NA, NA)
    
    RD_svm[i,] <- sum.RD_svm
    rownames(RD_svm)[i] <- paste0("data.", i, "_svm")
    num.proxy_svm <- c(num.proxy_svm, NA)
    names(num.proxy_svm)[i] <- paste0("data.", i, "_svm")
    
    results_svm <- c(i, sum.RD_svm)
    names(results_svm) <- c("iteration", "numProxy", "RD", "SE")
    results_svm.i <- paste0("results_svm.", i)
    assign(results_svm.i, results_svm)
    
    iteration <- i
    
    save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/5_svm/"
    save_path <- paste0(save_dir, results_svm.i, ".RData")
    
    save(list = results_svm.i, file = save_path)
  } else {
    sel.variables <- tryCatch({
      colnames(covar.mat)[svm.model$model$fit.info$model.list$model$xind]
    }, error = function(e) {
      errors <<- c(errors, paste("Error at iteration", i, "(sol):", e$message))
    })
    
    if (!is.na(sel.variables[1]) && (sel.variables[1] %in% covarsTfull)) {
      proxy_svm <- proxy.list[proxy.list %in% sel.variables]
      proxyform <- paste0(proxy_svm, collapse = "+")
      rhsform <- paste0(c(covform, proxyform), collapse = "+")
      ps.formula <- as.formula(paste0("exposure", "~", rhsform))
      
      W.out_svm <- weightit(ps.formula, 
                            data = data, 
                            estimand = "ATE",
                            method = "ps") 
      fit.OR_svm <- glm(out.formula,
                        data = data,
                        weights = W.out_svm$weights,
                        family= binomial(link = "logit"))
      fit.RD_svm <- glm(out.formula,
                        data= data,
                        weights= W.out_svm$weights,
                        family=gaussian(link= "identity"))
      sum.RD_svm <- c(length(proxy_svm), 
                      summary(fit.RD_svm)$coef["exposure", c("Estimate")], 
                      sqrt(sandwich::sandwich(fit.RD_svm)[2,2]))
      
      RD_svm[i,] <- sum.RD_svm
      rownames(RD_svm)[i] <- paste0("data.", i, "_svm")
      num.proxy_svm <- c(num.proxy_svm, length(proxy_svm))
      names(num.proxy_svm)[i] <- paste0("data.", i, "_svm")
      
      results_svm <- c(i, sum.RD_svm)
      names(results_svm) <- c("iteration", "numProxy", "RD", "SE")
      results_svm.i <- paste0("results_svm.", i)
      assign(results_svm.i, results_svm)
      
      iteration <- i
      
      save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/5_svm/"
      save_path <- paste0(save_dir, results_svm.i, ".RData")
      
      save(list = results_svm.i, file = save_path)
    } else {
      proxy_svm <- c(NA)
      sum.RD_svm <- c(NA, NA, NA)
      
      RD_svm[i,] <- sum.RD_svm
      rownames(RD_svm)[i] <- paste0("data.", i, "_svm")
      num.proxy_svm <- c(num.proxy_svm, NA)
      names(num.proxy_svm)[i] <- paste0("data.", i, "_svm")
      
      results_svm <- c(i, sum.RD_svm)
      names(results_svm) <- c("iteration", "numProxy", "RD", "SE")
      results_svm.i <- paste0("results_svm.", i)
      assign(results_svm.i, results_svm)
      
      iteration <- i
      
      save_dir <- "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/5_svm/"
      save_path <- paste0(save_dir, results_svm.i, ".RData")
      
      save(list = results_svm.i, file = save_path)
    }
  }
}
# ---------------------------------------------------------------

avg.num.proxy_svm <- mean(num.proxy_svm[complete.cases(num.proxy_svm)])
err_ratio <- length(errors)/iteration
RD_svm_no.error <- RD_svm[complete.cases(RD_svm),]
# ---------------------------------------------------------------

save(RD_svm, num.proxy_svm, avg.num.proxy_svm, err_ratio, RD_svm_no.error, 
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/5_svm/svm.RData")


