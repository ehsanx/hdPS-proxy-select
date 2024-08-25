# Load necessary libraries
library(foreach)
library(doParallel)
library(autoCovariateSelection)
library(dplyr)
library(cobalt)
library(WeightIt)
library(MASS)
library(sandwich)

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)  # Use all cores except one
registerDoParallel(cl)

# Global information
path0 <- "E:/GitHub/hdPS-proxy-select/"
exposure <- "obese"
outcome <- "diabetes"
investigator.specified.covariates <- 
  c("age.cat", "sex", "education", "race", "marital", "income", "born", "year",
    "diabetes.family.history", "medical.access",
    "smoking", "diet.healthy", "physical.activity", "sleep",
    "uric.acid", "protein.total", "bilirubin.total", "phosphorus",
    "sodium", "potassium", "globulin", "calcium.total", 
    "systolicBP", "diastolicBP", "high.cholesterol")
covform <- paste0(investigator.specified.covariates, collapse = "+")
out.formula <- as.formula(paste0("outcome", "~", "exposure"))

# Initialization
a0 <- 109
a <- 1000
RD_bross <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_bross <- c()
errors <- c()

# Parallel loop
results <- foreach(i = a0:a, .combine = 'rbind', .packages = c("autoCovariateSelection", "dplyr", "cobalt", "WeightIt", "MASS", "sandwich")) %dopar% {
  path <- paste0(path0, "simData/scenario/data_", i, ".rds")
  data <- tryCatch(readRDS(path), error = function(e) return(NULL))
  
  if (is.null(data)) {
    errors <<- c(errors, paste("Error at iteration", i, ": failed to read data"))
    return(c(NA, NA, NA))
  }
  
  data$idx <- data$id
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
    errors <<- c(errors, paste("Error at iteration", i, ":", e$message))
    return(NULL)
  })
  
  if (is.null(bross.list)) {
    return(c(NA, NA, NA))
  }
  
  proxy.df_bross <- bross.list$autoselected_covariate_df
  hdps.data_bross <- merge(data[,c("idx", outcome, exposure, investigator.specified.covariates)],
                           proxy.df_bross, by = "idx")
  hdps.data_bross$id <- hdps.data_bross$idx
  hdps.data_bross$idx <- NULL
  
  hdps.data_bross$exposure <- as.numeric(I(hdps.data_bross$obese == 'Yes'))
  hdps.data_bross$outcome <- as.numeric(I(hdps.data_bross$diabetes == 'Yes'))
  
  proxy_bross <- names(proxy.df_bross[,-1])
  proxyform <- paste0(proxy_bross, collapse = "+")
  rhsformula <- paste0(c(covform, proxyform), collapse = "+")
  ps.formula <- as.formula(paste0("exposure", "~", rhsformula))
  
  W.out_bross <- weightit(ps.formula, data = hdps.data_bross, estimand = "ATE", method = "ps")
  fit.RD_bross <- glm(out.formula, data = hdps.data_bross, weights = W.out_bross$weights, family = gaussian(link = "identity"))
  
  sum.RD_bross <- c(length(proxy_bross), summary(fit.RD_bross)$coef["exposure", "Estimate"], sqrt(sandwich::sandwich(fit.RD_bross)[2,2]))
  
  # Save individual results
  results_bross.i <- paste0("results_bross.", i)
  assign(results_bross.i, sum.RD_bross)
  
  save_dir <- paste0(path0, "simResults_scenario/1_bross/individual/")
  save_path <- paste0(save_dir, results_bross.i, ".RData")
  
  save(list = results_bross.i, file = save_path)
  
  return(sum.RD_bross)
}

# Combine results
RD_bross <- do.call(rbind, results)

# Save final results
avg.num.proxy_bross <- mean(RD_bross[, "numProxy"], na.rm = TRUE)
err_ratio <- length(errors) / a
RD_bross_no.error <- RD_bross[complete.cases(RD_bross), ]

save(RD_bross, num.proxy_bross, avg.num.proxy_bross, err_ratio, RD_bross_no.error, 
     file = paste0(path0, "simResults_scenario/1_bross/bross.RData"))

# Stop the cluster
stopCluster(cl)
