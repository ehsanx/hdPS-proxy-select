a <- 1000
b <- 0
RD_rf <- data.frame(numProxy = integer(a), RD = numeric(a), SE = numeric(a))

for (i in (b+1):a) {
  path_result <- paste0("/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/8_rf/results_rf.", i, ".RData")
  
  if (file.exists(path_result)) {
    result_name <- load(path_result)
    result <- get(result_name)
    
    RD_rf[i,] <- result[2:4]
    rownames(RD_rf)[i] <- result[1]
  }
}

num.proxy_rf <- RD_rf$numProxy
avg.num.proxy_ga <- mean(num.proxy_rf)

save(RD_rf, num.proxy_rf, avg.num.proxy_rf,
     file = "/scratch/st-mekarim-1/leiyang1/hdPS_ProxySelect/simResults_scenario/8_rf/rf.RData")
