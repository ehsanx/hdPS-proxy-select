# Set the working directory
path0 <- "C:/Users/Ehsan/Documents/GitHub/hdPS-proxy-select/"

# Load necessary libraries
library(autoCovariateSelection)
library(WeightIt)
library(lmtest)
library(sandwich)

# ---------------------------------------------------------------

### Global information
exposure <- "obese"
outcome <- "diabetes" 
investigator.specified.covariates <- c(
  # Demographic
  "age.cat", "sex", "education", "race", 
  "marital", "income", "born", "year",
  
  # Health history related variables/access
  "diabetes.family.history", "medical.access",
  
  # Behavioral
  "smoking", "diet.healthy", "physical.activity", "sleep",
  
  # Laboratory 
  "uric.acid", "protein.total", "bilirubin.total", "phosphorus",
  "sodium", "potassium", "globulin", "calcium.total", 
  "systolicBP", "diastolicBP", "high.cholesterol"
)

covform <- paste0(investigator.specified.covariates, collapse = "+")
out.formula <- as.formula(paste0(outcome, "~", exposure))

# Load initial dataset
pathx <- file.path(path0, "simData/scenario/data_1.rds")
data <- readRDS(pathx)

# Prepare proxy list and covariates
proxy.list <- grep("^rec", names(data), value = TRUE)
covarsTfull <- c(investigator.specified.covariates, proxy.list)
Y.form <- as.formula(paste0(outcome, "~", exposure, "+", paste0(covarsTfull, collapse = "+")))

# ---------------------------------------------------------------

### Initialization before for-loop
a <- 1000

RD_bross <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))
num.proxy_bross <- vector("numeric", a)
errors <- vector("character")

# Set up the directories
save_dir_success <- file.path(path0, "simResults_scenario/1_bross/success/")
save_dir_unsuccess <- file.path(path0, "simResults_scenario/1_bross/unsuccess/")

# Ensure directories exist
dir.create(save_dir_success, recursive = TRUE, showWarnings = FALSE)
dir.create(save_dir_unsuccess, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------

### Scenario: for-loop generating RD & SE results
set.seed(42)

for (i in 1:a) {
  # Load the dataset
  path <- file.path(path0, sprintf("simData/scenario/data_%d.rds", i))
  data <- readRDS(path)
  data$id <- NULL
  data$idx <- seq_len(nrow(data))
  
  # Prepare the covariate matrix
  covar.mat <- model.matrix(Y.form, data = data)[,-1]
  proxy.df <- as.data.frame(covar.mat)
  proxy.df$idx <- seq_len(nrow(proxy.df))
  proxy.df <- proxy.df[, c("idx", grep("^rec", names(proxy.df), value = TRUE))]
  
  # Filter out columns with all zeros or all ones
  col_sums <- colSums(proxy.df[,-1])
  proxy.df.filtered <- proxy.df[, !(colnames(proxy.df) %in% names(col_sums[col_sums == 0 | col_sums == nrow(proxy.df)]))]
  
  # Try to select covariates
  bross.list <- tryCatch({
    get_prioritised_covariates(
      df = proxy.df.filtered,
      patientIdVarname = "idx", 
      exposureVector = data[[exposure]],
      outcomeVector = data[[outcome]],
      patientIdVector = data$idx, 
      k = 100
    )
  }, error = function(e) {
    # Capture the traceback of the error
    traceback_info <- capture.output(traceback())
    
    # Capture key variables that might help in debugging
    key_info <- sprintf("Key variables at error - i: %d, exposure: %s, outcome: %s", i, exposure, outcome)
    
    # Capture the dimensions and first few rows of the filtered proxy data frame
    proxy_info <- sprintf("Proxy df dimensions: %s\nHead of proxy df: %s",
                          paste(dim(proxy.df.filtered), collapse = " x "),
                          capture.output(head(proxy.df.filtered)))
    
    # Combine all this information into one string
    error_message <- paste(
      sprintf("Error at iteration %d: %s", i, e$message),
      "Traceback:", paste(traceback_info, collapse = "\n"),
      key_info,
      proxy_info,
      sep = "\n"
    )
    
    # Store the detailed error information
    errors <<- c(errors, error_message)
    
    # Return NULL to indicate failure
    return(NULL)
  })
  
  if (is.null(bross.list)) {
    # Handle unsuccessful iteration
    RD_bross[i,] <- NA
    num.proxy_bross[i] <- NA
    
    save(list = paste0("results_bross.", i), file = file.path(save_dir_unsuccess, sprintf("results_bross_%d.RData", i)))
  } else {
    # Handle successful iteration
    proxy.df_bross <- bross.list$autoselected_covariate_df
    sel.variables <- setdiff(names(proxy.df_bross), "idx")
    proxy_bross <- intersect(proxy.list, sel.variables)
    
    rhsform <- paste0(c(covform, paste0(proxy_bross, collapse = "+")), collapse = "+")
    ps.formula <- as.formula(paste0(exposure, "~", rhsform))
    
    W.out_bross <- weightit(ps.formula, data = data, estimand = "ATE", method = "ps")
    
    fit.RD_bross <- glm(out.formula, data = data, weights = W.out_bross$weights, family = gaussian(link = "identity"))
    
    sum.RD_bross <- c(length(proxy_bross), 
                      coef(summary(fit.RD_bross))["exposure", "Estimate"], 
                      sqrt(vcovHC(fit.RD_bross, type = "HC0")["exposure", "exposure"]))
    
    RD_bross[i,] <- sum.RD_bross
    num.proxy_bross[i] <- length(proxy_bross)
    
    save(list = paste0("results_bross.", i), file = file.path(save_dir_success, sprintf("results_bross_%d.RData", i)))
  }
}

# ---------------------------------------------------------------

# Final calculations
avg.num.proxy_bross <- mean(num.proxy_bross, na.rm = TRUE)
err_ratio <- length(errors) / a
RD_bross_no.error <- na.omit(RD_bross)

save(RD_bross, num.proxy_bross, avg.num.proxy_bross, err_ratio, RD_bross_no.error, 
     file = file.path(path0, "simResults_scenario/1_bross/bross.RData"))
