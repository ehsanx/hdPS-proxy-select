a <- 1000
b <- 0
RD_rf_OR <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))

for (i in (b+1):a) {
  path_result <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/8_rf/results_rf.", i, ".RData")
  
  if (file.exists(path_result)) {
    result_name <- load(path_result)
    result <- get(result_name)
    
    RD_rf_OR[i,] <- result[2:4]
    rownames(RD_rf_OR)[i] <- result[1]
  }
}

num.proxy_rf_OR <- RD_rf_OR$numProxy
avg.num.proxy_rf_OR <- mean(num.proxy_rf_OR)

save(RD_rf_OR, num.proxy_rf_OR, avg.num.proxy_rf_OR,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioOR/8_rf/rf.RData")
