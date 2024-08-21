a <- 1000
b <- 0
RD_rf_ER <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))

for (i in (b+1):a) {
  path_result <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/8_rf/results_rf.", i, ".RData")
  
  if (file.exists(path_result)) {
    result_name <- load(path_result)
    result <- get(result_name)
    
    RD_rf_ER[i,] <- result[2:4]
    rownames(RD_rf_ER)[i] <- result[1]
  }
}

num.proxy_rf_ER <- RD_rf_ER$numProxy
avg.num.proxy_rf_ER <- mean(num.proxy_rf_ER)

save(RD_rf_ER, num.proxy_rf_ER, avg.num.proxy_rf_ER,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenarioER/8_rf/rf.RData")
