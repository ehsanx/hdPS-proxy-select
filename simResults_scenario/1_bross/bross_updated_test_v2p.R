library(parallel)
library(autoCovariateSelection)
library(WeightIt)
library(lmtest)
library(sandwich)
library(dplyr)

# ---------------------------------------------------------------

### Global information
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

# Path
path0 <- "C:/Users/Ehsan/Documents/GitHub/hdPS-proxy-select/"
save_dir_success <- paste0(path0, "simResults_scenario/1_bross/success/")
save_dir_unsuccess <- paste0(path0, "simResults_scenario/1_bross/unsuccess/")

# Ensure the directories exist
dir.create(save_dir_success, recursive = TRUE, showWarnings = FALSE)
dir.create(save_dir_unsuccess, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------

### Parallel processing setup
# Number of cores to use
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Load necessary libraries on each worker
clusterEvalQ(cl, {
  library(autoCovariateSelection)
  library(WeightIt)
  library(sandwich)
  library(dplyr)
  library(lmtest)
})

# Export necessary objects and functions to the cluster
clusterExport(cl, c("covform", "out.formula", 
                    "investigator.specified.covariates", 
                    "save_dir_success", "save_dir_unsuccess", 
                    "path0", "exposure", "outcome"))

# ---------------------------------------------------------------

### Main computation: parallelized for-loop
a <- 40
results <- clusterApplyLB(cl, 1:a, function(i) {
  path <- paste0(path0, "simData/scenario/data_", i, ".rds")
  data <- readRDS(path)
  data$id <- NULL
  data$idx <- NULL
  data$idx <- 1:nrow(data)
  
  covar.mat <- model.matrix(out.formula, data = data)[,-1]
  covar.df <- as.data.frame(covar.mat)
  covar.df$idx <- 1:nrow(covar.df)
  proxy.df <- covar.df[, c("idx", grep("^rec", names(covar.df), value = TRUE))]
  
  # Check if proxy.df has columns before proceeding
  if (ncol(proxy.df) > 1) {
    col_sums <- apply(proxy.df[,-1], 2, sum)
    all_zero_or_one_colnames <- colnames(proxy.df)[col_sums == 0 | col_sums == nrow(proxy.df)]
    proxy.df.filtered <- proxy.df[, !(colnames(proxy.df) %in% all_zero_or_one_colnames)]
  } else {
    proxy.df.filtered <- data.frame(idx = proxy.df$idx)
  }
  
  result <- tryCatch({
    bross.list <- get_prioritised_covariates(df = proxy.df.filtered,
                                             patientIdVarname = "idx", 
                                             exposureVector = data$exposure,
                                             outcomeVector = data$outcome,
                                             patientIdVector = data$idx, 
                                             k = 100)
    
    if (class(bross.list) == "character") stop("No prioritized covariates found")
    
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
    proxyform <- if (length(proxy_bross) > 0) paste0(proxy_bross, collapse = "+") else ""
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
    
    results_bross <- c(i, sum.RD_bross)
    names(results_bross) <- c("iteration", "numProxy", "RD", "SE")
    
    save_path <- paste0(save_dir_success, "results_bross.", i, ".RData")
    save(list = "results_bross", file = save_path)
    
    return(results_bross)
    
  }, error = function(e) {
    results_bross <- c(i, NA, NA, NA)
    save_path <- paste0(save_dir_unsuccess, "results_bross.", i, ".RData")
    save(list = "results_bross", file = save_path)
    
    return(c(i, NA, NA, NA, paste("Error at iteration", i, ":", e$message)))
  })
  
  return(result)
})

# ---------------------------------------------------------------

### Post-processing
stopCluster(cl)

# Combine the results
results_df <- do.call(rbind, lapply(results, function(x) x[1:4]))
errors <- unlist(lapply(results, function(x) if (length(x) > 4) x[5] else NULL))

avg.num.proxy_bross <- mean(results_df[,2], na.rm = TRUE)
err_ratio <- length(errors) / a
RD_bross_no.error <- results_df[complete.cases(results_df),]

# Save the results
save(RD_bross_no.error, avg.num.proxy_bross, err_ratio, 
     file = paste0(path0, "simResults_scenario/1_bross/bross.RData"))
