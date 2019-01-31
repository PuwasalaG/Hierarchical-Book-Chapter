
#Required packages
require(tidyverse)
require(fpp2)
require(readxl)
require(hts)
library(zoo)
library(magrittr)
library(Matrix)
library(seasonal)
library(gridExtra)
library(MASS)
library(scoringRules)

#Expenditure data#
####Expenditure Approach - Current Prices####

Exp <- read.csv("GDP-Exp.csv")[,-1] %>% as.tibble() 

####Summing matrix####
S <- read.csv("S_mat.csv")[,-1] %>% as.matrix()

m <- 53 #Number of most disaggregate series
n <- ncol(Exp) #Total number of series in the hieararchy
B <- 5000 #Number of random samples generated from the forecast distribution for the evaluation
B <- 5000 #Number of random samples generated from the forecast distribution for the evaluation
H <- 4 #Number of forecast horizons 
N <- nrow(Exp) #Total time length of the data set

#Code to get shrinkage estimator

lowerD <- function(x)
{
  n2 <- nrow(x)
  return(diag(apply(x, 2, crossprod) / n2))
}

shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!", call. = FALSE)
  p <- ncol(x)
  n <- nrow(x)
  covm <- crossprod(x) / n
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(list(shrink.cov, c("The shrinkage intensity lambda is:",
                            round(lambda, digits = 4))))
}



#A function to calculate Enery score to evaluate full forecast densities

Energy_score <- function(Data, Real)
{
  Testing <- as.numeric(Real)
  n <- ncol(Data)
  k <- nrow(Data)
  
  
  d1_eval <- Data - matrix(rep(Testing, k), k, n, byrow = TRUE)
  ES_1_eval <- apply(d1_eval, 1, function(x) sqrt(sum(x^2)))
  #ES_1_eval <- sqrt(rowSums(d1_eval^2))
  
  d2_eval <- Data[1:k-1,] - Data[2:k,]
  ES_2_eval <- apply(d2_eval, 1, function(x) sqrt(sum(x^2)))
  #ES_2_eval <- sqrt(rowSums(d2_eval^2))
  
  ES_eval <- mean(ES_1_eval) - mean(ES_2_eval)/2
  
  return(ES_eval)
  
}

#A function to calculate log score to evaluate full forecast densities

Variogram_score <- function(Data, Real)
{
  Testing <- as.numeric(Real)
  
  Y_tilde_diff <- t(apply(Data, 1, function(x)
    (abs(outer(x,x,"-")[lower.tri(outer(x,x,"-"))]))^(0.5)))
  
  Y_tilde_diff_mean <- apply(Y_tilde_diff, 2, mean)
  
  z <- abs(outer(Testing, Testing,'-'))
  y_diff <- z[lower.tri(z)]
  y_diff <- y_diff^0.5
  C <- (y_diff - Y_tilde_diff_mean)^2
  
  sum(C)
}


#All the forecasts and related informations are stored in the DF dataframe

DF_MultiV <- tibble("Year, Qtr of forecast" = character(),
                    "F-method" = character(),
                    "R-method" = character(),
                    "Forecast Horizon" = integer(),
                    "Energy score" = numeric(),
                    "Variogram score" = numeric(),
                    "Replication" = integer())

DF_UniV <- tibble("Year, Qtr of forecast" = character(),
                  "Series" = character(),
                  "F-method" = character(),
                  "R-method" = character(),
                  "Forecast Horizon" = integer(),
                  "Actual" = double(),
                  "CRPS" = numeric(),
                  "Replication" = integer())

start_train <- c(1984, 4)
end_first_train <- c(1994, 2)
max_train=c(2017,4) #End of largest training set

first_train_length <- Exp %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(end=c(1994, 2)) %>% length() #Length of first training set

test_length <- Exp %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(start=end_first_train+c(0,1), end=max_train) %>% length #Maximum time the window expands


H <- 4

Start <- Sys.time()
for (j in 1:test_length) { #test_length
  
  #Subsetting training and testing sets
  
  Train <- Exp[1:(first_train_length + j),]
  Test <- Exp[-(1:(first_train_length + j)),]
  
  #Year_Qtr_of_forecast <- numeric(min(H, nrow(Test)))
  
  #Matrix to store residuals
  Residuals_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_all_Benchmark <- matrix(NA, nrow = nrow(Train), ncol = n) #Benchmark method: seasonal RW with a drift
  
  #Matrix to store base forecasts
  Base_ARIMA <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_ETS <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_Benchmark <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  
  for(i in 1:n) {
    
    TS <- ts(Train[,i], frequency = 4, start = start_train)
    
    #Forecsting with benchmark
    fit_Benchmark <- Arima(TS, order=c(0,0,0), seasonal=c(0,1,0), include.drift=TRUE)
    Forecast_Benchmark <- forecast(fit_Benchmark, h = min(H, nrow(Test)))
    Base_Benchmark[,i] <- Forecast_Benchmark$mean
    Residuals_all_Benchmark[,i] <- as.vector(TS - fitted(fit_Benchmark))
    
    
    ##Forecsting with ETS##
    
    fit_ETS <- ets(TS)
    Forecast_ETS <- forecast(fit_ETS, h = min(H, nrow(Test)))
    Base_ETS[,i] <- Forecast_ETS$mean
    Residuals_all_ETS[,i] <- as.vector(TS - fitted(fit_ETS))
    
    
    #Forecsting with ARIMA
    fit_ARIMA <- auto.arima(TS)
    Forecast_ARIMA <- forecast(fit_ARIMA, h = min(H, nrow(Test[,i])))
    Base_ARIMA[,i] <- Forecast_ARIMA$mean
    Residuals_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA))
    
    
  }
  
  
  #Index to get the block of bootsrap samples
  Index <- base::sample(c(1:(nrow(Residuals_all_Benchmark)-(min(H, nrow(Test))+1))), size = B , replace = TRUE)
  Index_seq <- matrix(0, B, min(H, nrow(Test[,i])))
  
  
  ## Getting incoherent sample paths ##
  
  Unrecon_future_paths_bench <- list()#To store the sample paths for different forecast horizons
  Unrecon_future_paths_ETS <- list()
  Unrecon_future_paths_ARIMA <- list()
  
  for (h in 1: min(H, nrow(Test))) {
    Unrecon_future_paths_bench[[h]] <- matrix(0, nrow = B, ncol = n)
    Unrecon_future_paths_ETS[[h]] <- matrix(0, nrow = B, ncol = n)
    Unrecon_future_paths_ARIMA[[h]] <- matrix(0, nrow = B, ncol = n)
  }
  
  
  Future_paths_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  Future_paths_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  Future_paths_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  
  for(k in 1:B)  {
    
    Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+min(H, nrow(Test[,i]))-1), by = 1)
    
    for(i in 1:n) {
      
      Future_paths_bench[,i] <- Base_Benchmark[,i] + Residuals_all_Benchmark[Index_seq[k,],i]
      Future_paths_ETS[,i] <- Base_ETS[,i] + Residuals_all_ETS[Index_seq[k,],i]
      Future_paths_ARIMA[,i] <- Base_ARIMA[,i] + Residuals_all_ARIMA[Index_seq[k,],i]
      
    }
    
    
    for (h in 1: min(H, nrow(Test))) {
      
      Unrecon_future_paths_bench[[h]][k,] <- Future_paths_bench[h,]
      Unrecon_future_paths_ETS[[h]][k,] <- Future_paths_ETS[h,]
      Unrecon_future_paths_ARIMA[[h]][k,] <- Future_paths_ARIMA[h,]
      
    }
    
  }
  
  ##Reconciliation of future paths obtained from benchmark method
  
  #Bottom up P
  
  Null.ma <- matrix(0,m,(n-m))
  BU_P <- cbind(Null.ma, diag(1,m,m))
  
  #OLS P
  OLS_P <- solve(t(S) %*% S) %*% t(S)
  
  #MinT shrink P
  targ <- lowerD(Residuals_all_Benchmark)
  shrink <- shrink.estim(Residuals_all_Benchmark,targ)
  Shr.cov_Bench <- shrink[[1]]
  Inv_Shr.cov_Bench <- solve(Shr.cov_Bench)
  
  MinT.Shr_P_Bench <- solve(t(S) %*% Inv_Shr.cov_Bench %*% S) %*% t(S) %*% Inv_Shr.cov_Bench
  
  #WLS P
  Cov_WLS_Bench <- diag(diag(Shr.cov_Bench), n, n)
  Inv_WLS <- solve(Cov_WLS_Bench)
  
  WLS_P_Bench <- solve(t(S) %*% Inv_WLS %*% S) %*% t(S) %*% Inv_WLS
  
  
  
  ###Reconciliation of base forecasts from benchmark###
  
  Reconciled_future_paths_BU_bench <- list()
  Reconciled_future_paths_OLS_bench <- list()
  Reconciled_future_paths_WLS_bench <- list()
  Reconciled_future_paths_MinT.Shr_bench <- list()
  
  #To store univariate scores
  CRPS_BU_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  #To store multivariate scores
  ES_full_BU_bench <- numeric(min(H, nrow(Test)))
  ES_full_OLS_bench <- numeric(min(H, nrow(Test)))
  ES_full_MinT.Shr_bench <- numeric(min(H, nrow(Test)))
  ES_full_WLS_bench <- numeric(min(H, nrow(Test)))
  ES_full_Unrecon_bench <- numeric(min(H, nrow(Test)))
  
  VS_full_BU_bench <- numeric(min(H, nrow(Test)))
  VS_full_OLS_bench <- numeric(min(H, nrow(Test)))
  VS_full_MinT.Shr_bench <- numeric(min(H, nrow(Test)))
  VS_full_WLS_bench <- numeric(min(H, nrow(Test)))
  VS_full_Unrecon_bench <- numeric(min(H, nrow(Test)))
  
  
  
  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_bench[[h]] <- t(S %*% BU_P %*% t(Unrecon_future_paths_bench[[h]]))
    Reconciled_future_paths_OLS_bench[[h]] <- t(S %*% OLS_P %*% t(Unrecon_future_paths_bench[[h]]))
    Reconciled_future_paths_WLS_bench[[h]] <- t(S %*% WLS_P_Bench %*% t(Unrecon_future_paths_bench[[h]]))
    Reconciled_future_paths_MinT.Shr_bench[[h]] <- t(S %*% MinT.Shr_P_Bench %*% t(Unrecon_future_paths_bench[[h]]))
    
    #Calculating Energy score for full predicive densities
    
    ES_full_BU_bench[h] <- Energy_score(Data = Reconciled_future_paths_BU_bench[[h]], Real = Test[h,])
    ES_full_OLS_bench[h] <- Energy_score(Data = Reconciled_future_paths_OLS_bench[[h]], Real = Test[h,])
    ES_full_WLS_bench[h] <- Energy_score(Data = Reconciled_future_paths_WLS_bench[[h]], Real = Test[h,])
    ES_full_MinT.Shr_bench[h] <- Energy_score(Data = Reconciled_future_paths_MinT.Shr_bench[[h]], Real = Test[h,])
    ES_full_Unrecon_bench[h] <- Energy_score(Data = Unrecon_future_paths_bench[[h]], Real = Test[h,])
    
    #Calculating Variogram score for full predicive densities
    VS_full_BU_bench[h] <- Variogram_score(Data = Reconciled_future_paths_BU_bench[[h]], Real = Test[h,])
    VS_full_OLS_bench[h] <- Variogram_score(Data = Reconciled_future_paths_OLS_bench[[h]], Real = Test[h,])
    VS_full_WLS_bench[h] <- Variogram_score(Data = Reconciled_future_paths_WLS_bench[[h]], Real = Test[h,])
    VS_full_MinT.Shr_bench[h] <- Variogram_score(Data = Reconciled_future_paths_MinT.Shr_bench[[h]], Real = Test[h,])
    VS_full_Unrecon_bench[h] <- Variogram_score(Data = Unrecon_future_paths_bench[[h]], Real = Test[h,])
    
    #Calculating CRPS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Unrecon_future_paths_bench[[h]][,i],
                                             method = "edf")
      CRPS_BU_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_BU_bench[[h]][,i],
                                        method = "edf")
      CRPS_OLS_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_OLS_bench[[h]][,i],
                                         method = "edf")
      CRPS_WLS_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_WLS_bench[[h]][,i],
                                         method = "edf")
      CRPS_MinT.Shr_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_MinT.Shr_bench[[h]][,i],
                                              method = "edf")
      
    }
    
    
    DF_MultiV <- DF_MultiV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_Benchmark$mean))[h]),
                                       "F-method" = "Benchmark",
                                       "Replication" = j)
    
    DF_UniV <- DF_UniV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_Benchmark$mean))[h]),
                                   "F-method" = "Benchmark",
                                   "Replication" = j)
    
  }
  
  
  DF_MultiV %>% filter(`F-method`=="Benchmark", `Replication`==j) %>% dplyr::select("Year, Qtr of forecast", "F-method", 
                                                                                    "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_Unrecon_bench, 
        "Variogram score" = VS_full_Unrecon_bench) -> DF_Base
  DF_Base[names(DF_MultiV)] -> DF_Base
  DF_MultiV <- rbind(DF_MultiV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_BU_bench, 
        "Variogram score" = VS_full_BU_bench) -> DF_BU
  DF_BU[names(DF_MultiV)] -> DF_BU
  DF_MultiV <- rbind(DF_MultiV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_OLS_bench, 
        "Variogram score" = VS_full_OLS_bench) -> DF_OLS
  DF_OLS[names(DF_MultiV)] -> DF_OLS
  DF_MultiV <- rbind(DF_MultiV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_WLS_bench, 
        "Variogram score" = VS_full_WLS_bench) -> DF_WLS
  DF_WLS[names(DF_MultiV)] -> DF_WLS
  DF_MultiV <- rbind(DF_MultiV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_MinT.Shr_bench, 
        "Variogram score" = VS_full_MinT.Shr_bench) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV)] -> DF_MinT.Shr
  DF_MultiV <- rbind(DF_MultiV, DF_MinT.Shr)
  
  
  #Addinng CRPS to the DF
  
  DF_UniV %>% filter(`F-method`=="Benchmark", `Replication`==j) %>% dplyr::select("Year, Qtr of forecast", "F-method", 
                                                                                  "Replication") -> Fltr
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  "R-method" = "Base", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_Unrecon_bench))) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "Bottom up", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_BU_bench))) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "OLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_OLS_bench))) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "WLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_WLS_bench))) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:4,]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:4, each = 16), "CRPS" = c(t(CRPS_MinT.Shr_bench))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  
  ##Reconciliation of future paths obtained from ETS method
  
  #MinT shrink P
  targ <- lowerD(Residuals_all_ETS)
  shrink <- shrink.estim(Residuals_all_ETS,targ)
  Shr.cov_ETS <- shrink[[1]]
  Inv_Shr.cov_ETS <- solve(Shr.cov_ETS)
  
  MinT.Shr_P_ETS <- solve(t(S) %*% Inv_Shr.cov_ETS %*% S) %*% t(S) %*% Inv_Shr.cov_ETS
  
  #WLS P
  Cov_WLS_ETS <- diag(diag(Shr.cov_ETS), n, n)
  Inv_WLS_ETS <- solve(Cov_WLS_ETS)
  
  WLS_P_ETS <- solve(t(S) %*% Inv_WLS_ETS %*% S) %*% t(S) %*% Inv_WLS_ETS
  
  
  
  ###Reconciliation of base forecasts from ETS###
  
  Reconciled_future_paths_BU_ETS <- list()
  Reconciled_future_paths_OLS_ETS <- list()
  Reconciled_future_paths_WLS_ETS <- list()
  Reconciled_future_paths_MinT.Shr_ETS <- list()
  
  #To store univariate scores
  CRPS_BU_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  #To store multivariate scores
  ES_full_BU_ETS <- numeric(min(H, nrow(Test)))
  ES_full_OLS_ETS <- numeric(min(H, nrow(Test)))
  ES_full_MinT.Shr_ETS <- numeric(min(H, nrow(Test)))
  ES_full_WLS_ETS <- numeric(min(H, nrow(Test)))
  ES_full_Unrecon_ETS <- numeric(min(H, nrow(Test)))
  
  VS_full_BU_ETS <- numeric(min(H, nrow(Test)))
  VS_full_OLS_ETS <- numeric(min(H, nrow(Test)))
  VS_full_MinT.Shr_ETS <- numeric(min(H, nrow(Test)))
  VS_full_WLS_ETS <- numeric(min(H, nrow(Test)))
  VS_full_Unrecon_ETS <- numeric(min(H, nrow(Test)))
  
  
  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ETS[[h]] <- t(S %*% BU_P %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_OLS_ETS[[h]] <- t(S %*% OLS_P %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_WLS_ETS[[h]] <- t(S %*% WLS_P_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_MinT.Shr_ETS[[h]] <- t(S %*% MinT.Shr_P_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    
    #Calculating Energy score for full predicive densities
    
    ES_full_BU_ETS[h] <- Energy_score(Data = Reconciled_future_paths_BU_ETS[[h]], Real = Test[h,])
    ES_full_OLS_ETS[h] <- Energy_score(Data = Reconciled_future_paths_OLS_ETS[[h]], Real = Test[h,])
    ES_full_WLS_ETS[h] <- Energy_score(Data = Reconciled_future_paths_WLS_ETS[[h]], Real = Test[h,])
    ES_full_MinT.Shr_ETS[h] <- Energy_score(Data = Reconciled_future_paths_MinT.Shr_ETS[[h]], Real = Test[h,])
    ES_full_Unrecon_ETS[h] <- Energy_score(Data = Unrecon_future_paths_ETS[[h]], Real = Test[h,])
    
    #Calculating Variogram score for full predicive densities
    VS_full_BU_ETS[h] <- Variogram_score(Data = Reconciled_future_paths_BU_ETS[[h]], Real = Test[h,])
    VS_full_OLS_ETS[h] <- Variogram_score(Data = Reconciled_future_paths_OLS_ETS[[h]], Real = Test[h,])
    VS_full_WLS_ETS[h] <- Variogram_score(Data = Reconciled_future_paths_WLS_ETS[[h]], Real = Test[h,])
    VS_full_MinT.Shr_ETS[h] <- Variogram_score(Data = Reconciled_future_paths_MinT.Shr_ETS[[h]], Real = Test[h,])
    VS_full_Unrecon_ETS[h] <- Variogram_score(Data = Unrecon_future_paths_ETS[[h]], Real = Test[h,])
    
    #Calculating CRPS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Unrecon_future_paths_ETS[[h]][,i],
                                           method = "edf")
      CRPS_BU_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_BU_ETS[[h]][,i],
                                      method = "edf")
      CRPS_OLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_OLS_ETS[[h]][,i],
                                       method = "edf")
      CRPS_WLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_WLS_ETS[[h]][,i],
                                       method = "edf")
      CRPS_MinT.Shr_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_MinT.Shr_ETS[[h]][,i],
                                            method = "edf")
      
    }
    
    
    DF_MultiV <- DF_MultiV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ETS$mean))[h]),
                                       "F-method" = "ETS",
                                       "Replication" = j)
    
    DF_UniV <- DF_UniV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ETS$mean))[h]),
                                   "F-method" = "ETS",
                                   "Replication" = j)
    
  }
  
  
  DF_MultiV %>% filter(`F-method`=="ETS", `Replication`==j) %>% dplyr::select("Year, Qtr of forecast", "F-method", 
                                                                              "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_Unrecon_ETS, 
        "Variogram score" = VS_full_Unrecon_ETS) -> DF_Base
  DF_Base[names(DF_MultiV)] -> DF_Base
  DF_MultiV <- rbind(DF_MultiV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_BU_ETS, 
        "Variogram score" = VS_full_BU_ETS) -> DF_BU
  DF_BU[names(DF_MultiV)] -> DF_BU
  DF_MultiV <- rbind(DF_MultiV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_OLS_ETS, 
        "Variogram score" = VS_full_OLS_ETS) -> DF_OLS
  DF_OLS[names(DF_MultiV)] -> DF_OLS
  DF_MultiV <- rbind(DF_MultiV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_WLS_ETS, 
        "Variogram score" = VS_full_WLS_ETS) -> DF_WLS
  DF_WLS[names(DF_MultiV)] -> DF_WLS
  DF_MultiV <- rbind(DF_MultiV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_MinT.Shr_ETS, 
        "Variogram score" = VS_full_MinT.Shr_ETS) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV)] -> DF_MinT.Shr
  DF_MultiV <- rbind(DF_MultiV, DF_MinT.Shr)
  
  
  #Addinng CRPS to the DF
  
  DF_UniV %>% filter(`F-method`=="ETS", `Replication`==j) %>% dplyr::select("Year, Qtr of forecast", "F-method", 
                                                                            "Replication") -> Fltr
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  "R-method" = "Base", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_Unrecon_ETS))) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "Bottom up", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_BU_ETS))) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "OLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_OLS_ETS))) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "WLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_WLS_ETS))) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_MinT.Shr_ETS))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  
  
  
  ##Reconciliation of future paths obtained from ARIMA method
  
  #MinT shrink P
  targ <- lowerD(Residuals_all_ARIMA)
  shrink <- shrink.estim(Residuals_all_ARIMA,targ)
  Shr.cov_ARIMA <- shrink[[1]]
  Inv_Shr.cov_ARIMA <- solve(Shr.cov_ARIMA)
  
  MinT.Shr_P_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
  
  #WLS P
  Cov_WLS_ARIMA <- diag(diag(Shr.cov_ARIMA), n, n)
  Inv_WLS_ARIMA <- solve(Cov_WLS_ARIMA)
  
  WLS_P_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA

  ###Reconciliation of base forecasts from ARIMA###
  
  Reconciled_future_paths_BU_ARIMA <- list()
  Reconciled_future_paths_OLS_ARIMA <- list()
  Reconciled_future_paths_WLS_ARIMA <- list()
  # Reconciled_future_paths_MinT.Sam_ARIMA <- list()
  Reconciled_future_paths_MinT.Shr_ARIMA <- list()
  
  #To store univariate scores
  CRPS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  #To store multivariate scores
  ES_full_BU_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_OLS_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_MinT.Shr_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_WLS_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_Unrecon_ARIMA <- numeric(min(H, nrow(Test)))
  
  VS_full_BU_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_OLS_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_MinT.Shr_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_WLS_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_Unrecon_ARIMA <- numeric(min(H, nrow(Test)))
  
  
  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ARIMA[[h]] <- t(S %*% BU_P %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_OLS_ARIMA[[h]] <- t(S %*% OLS_P %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_WLS_ARIMA[[h]] <- t(S %*% WLS_P_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_MinT.Shr_ARIMA[[h]] <- t(S %*% MinT.Shr_P_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    
    #Calculating Energy score for full predicive densities
    
    ES_full_BU_ARIMA[h] <- Energy_score(Data = Reconciled_future_paths_BU_ARIMA[[h]], Real = Test[h,])
    ES_full_OLS_ARIMA[h] <- Energy_score(Data = Reconciled_future_paths_OLS_ARIMA[[h]], Real = Test[h,])
    ES_full_WLS_ARIMA[h] <- Energy_score(Data = Reconciled_future_paths_WLS_ARIMA[[h]], Real = Test[h,])
    ES_full_MinT.Shr_ARIMA[h] <- Energy_score(Data = Reconciled_future_paths_MinT.Shr_ARIMA[[h]], Real = Test[h,])
    ES_full_Unrecon_ARIMA[h] <- Energy_score(Data = Unrecon_future_paths_ARIMA[[h]], Real = Test[h,])
    
    #Calculating Variogram score for full predicive densities
    VS_full_BU_ARIMA[h] <- Variogram_score(Data = Reconciled_future_paths_BU_ARIMA[[h]], Real = Test[h,])
    VS_full_OLS_ARIMA[h] <- Variogram_score(Data = Reconciled_future_paths_OLS_ARIMA[[h]], Real = Test[h,])
    VS_full_WLS_ARIMA[h] <- Variogram_score(Data = Reconciled_future_paths_WLS_ARIMA[[h]], Real = Test[h,])
    VS_full_MinT.Shr_ARIMA[h] <- Variogram_score(Data = Reconciled_future_paths_MinT.Shr_ARIMA[[h]], Real = Test[h,])
    VS_full_Unrecon_ARIMA[h] <- Variogram_score(Data = Unrecon_future_paths_ARIMA[[h]], Real = Test[h,])
    
    #Calculating CRPS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Unrecon_future_paths_ARIMA[[h]][,i],
                                             method = "edf")
      CRPS_BU_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_BU_ARIMA[[h]][,i],
                                        method = "edf")
      CRPS_OLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_OLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_WLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_WLS_ARIMA[[h]][,i],
                                         method = "edf")
      CRPS_MinT.Shr_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = Reconciled_future_paths_MinT.Shr_ARIMA[[h]][,i],
                                              method = "edf")
      
    }
    
    
    DF_MultiV <- DF_MultiV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ARIMA$mean))[h]),
                                       "F-method" = "ARIMA",
                                       "Replication" = j)
    
    DF_UniV <- DF_UniV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ARIMA$mean))[h]),
                                   "F-method" = "ARIMA",
                                   "Replication" = j)
    
  }
  
  
  DF_MultiV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% dplyr::select("Year, Qtr of forecast", "F-method", 
                                                                                "Replication") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_Unrecon_ARIMA, 
        "Variogram score" = VS_full_Unrecon_ARIMA) -> DF_Base
  DF_Base[names(DF_MultiV)] -> DF_Base
  DF_MultiV <- rbind(DF_MultiV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_BU_ARIMA, 
        "Variogram score" = VS_full_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV)] -> DF_BU
  DF_MultiV <- rbind(DF_MultiV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_OLS_ARIMA, 
        "Variogram score" = VS_full_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV)] -> DF_OLS
  DF_MultiV <- rbind(DF_MultiV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_WLS_ARIMA, 
        "Variogram score" = VS_full_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV)] -> DF_WLS
  DF_MultiV <- rbind(DF_MultiV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_MinT.Shr_ARIMA, 
        "Variogram score" = VS_full_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV)] -> DF_MinT.Shr
  DF_MultiV <- rbind(DF_MultiV, DF_MinT.Shr)
  
  
  #Addinng CRPS to the DF
  
  DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% dplyr::select("Year, Qtr of forecast", "F-method", 
                                                                              "Replication") -> Fltr
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  "R-method" = "Base", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_Unrecon_ARIMA))) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "Bottom up", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_BU_ARIMA))) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "OLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_OLS_ARIMA))) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "WLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_WLS_ARIMA))) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_MinT.Shr_ARIMA))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  
  
  
}

End <- Sys.time()

#Calculating the skill scores - Percentage improvement of the preferred forecasting method with respect to the benchmark

#Summary of ES and VS

DF_MultiV[complete.cases(DF_MultiV[ , "R-method"]),] -> DF_MultiV

#View(DF_MultiV)


DF_MultiV %>% dplyr::select(-"Year, Qtr of forecast", -"Replication") -> DF_MultScores

DF_MultScores %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
  summarise(E.ES = mean(`Energy score`), 
            E.VS = mean(`Variogram score`)) -> DF_MultScores

#DF_MultScores %>% dplyr::filter(`R-method` != "Base") -> DF_MultScore_Recon

DF_MultScores %>% dplyr::filter(`F-method`=="ETS" | `R-method`=="Base") %>% 
  dplyr::filter(`F-method`!="ARIMA") -> DF_MultScores_ETS

DF_MultScores %>% dplyr::filter(`F-method`=="ARIMA" | `R-method`=="Base") %>% 
  dplyr::filter(`F-method`!="ETS") -> DF_MultScores_ARIMA

#Using base method from each F-method to calculate the skill scores


#For ETS

DF_MultScores_ETS %>% filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_ETS 

DF_MultScores_ETS %>% filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% as_vector() -> Base_E.VS_ETS 

DF_MultScores_ETS %>% mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_ETS))*100, digits = 4),
                             SS_E.VS = round((1-(`E.VS`/Base_E.VS_ETS))*100, digits = 4)) -> DF_MultScore_SS_ETS

DF_MultScore_SS_ETS %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_ETS

DF_MultScore_SS_ETS %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_ETS

# View(SS_E.ES_ETS)
# View(SS_E.VS_ETS)

#For ARIMA

DF_MultScores_ARIMA %>% filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.ES`) %>% as_vector() -> Base_E.ES_ARIMA 

DF_MultScores_ARIMA %>% filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.VS`) %>% as_vector() -> Base_E.VS_ARIMA 


DF_MultScores_ARIMA %>% mutate(SS_E.ES = round((1-(`E.ES`/Base_E.ES_ARIMA))*100, digits = 4),
                               SS_E.VS = round((1-(`E.VS`/Base_E.VS_ARIMA))*100, digits = 4)) -> DF_MultScore_SS_ARIMA

DF_MultScore_SS_ARIMA %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_ARIMA

DF_MultScore_SS_ARIMA %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_ARIMA

# View(SS_E.ES_ARIMA)
# View(SS_E.VS_ARIMA)


##Summary of CRPS

DF_UniV[complete.cases(DF_UniV[ , "R-method"]),] -> DF_UniV

Score_arima <- tibble("Series" = character(),
                      "F-method" = character(),
                      "R-method" = character(),
                      "Forecast Horizon" = integer(),
                      "MCRPS" = numeric())


Score_ets <- tibble("Series" = character(),
                    "F-method" = character(),
                    "R-method" = character(),
                    "Forecast Horizon" = integer(),
                    "MCRPS" = numeric())

for (i in 1:n) {
  
  DF_UniV %>% filter(`Series`==names(Exp)[i]) %>% dplyr::select("F-method", "R-method", "Forecast Horizon", 
                                                                "CRPS") -> DF_Score
  
  DF_Score %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
    summarise(MCRPS = mean(`CRPS`)) -> DF_Score
  
  DF_Score %>% dplyr::filter(`F-method` == "ETS" | `R-method` == "Base") %>% 
    dplyr::filter(`F-method`!= "ARIMA") -> Score_ETS
  
  cbind(Score_ETS, "Series" = rep(names(Exp)[i], nrow(Score_ETS))) -> Score_ETS
  Score_ETS[names(Score_ets)] %>% as.tibble() -> Score_ETS
  
  Score_ets <- rbind(Score_ets, Score_ETS)
  
  
  DF_Score %>% dplyr::filter(`F-method` == "ARIMA" | `R-method` == "Base") %>% 
    dplyr::filter(`F-method`!= "ETS") -> Score_ARIMA
  
  cbind(Score_ARIMA, "Series" = rep(names(Exp)[i], nrow(Score_ARIMA))) -> Score_ARIMA
  Score_ARIMA[names(Score_arima)]  %>% as.tibble() -> Score_ARIMA
  
  Score_arima <- rbind(Score_arima, Score_ARIMA)

  }


save.image("GDP-ProbForecasting-EXP-BootstrapMethod-ExpandW.RData")

Skill.Score_ets %>% filter(`Series`=="Gdpe") %>% dplyr::select(-`Series`) %>% 
  spread(key = `Forecast Horizon`, value = `SS_CRPS`) -> Gdpe_SS_CRPS

