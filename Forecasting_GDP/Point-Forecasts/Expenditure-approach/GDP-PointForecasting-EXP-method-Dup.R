#In this code the insample data for aggregate series will be get from S%*%b where b is the insample data for bottom levels


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
library(tsibble)

#Importing data#
####Expenditure Approach - Current Prices####

Exp <- read.csv("GDP-Exp.csv")[,-1] %>% as.tibble() 
Bottom_level <- Exp[,28:80]

m <- 53 #Number of most disaggregate series
n <- ncol(Exp) #Total number of series in the hieararchy

####Summing matrix####
S <- read.csv("S_mat.csv")[,-1] %>% as.matrix()

Exp <- t(S %*% t(Bottom_level))

#Code to get shrinkage estimator

lowerD <- function(x)
{
  n2 <- nrow(x)
  return(diag(apply(x, 2, crossprod) / n2))
}

shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!")
  p1 <- ncol(x)
  n2 <- nrow(x)
  covm <- crossprod(x) / n2
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n2 * (n2 - 1))) * (crossprod(xs^2) - 1/n2 * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(list(shrink.cov, c("The shrinkage intensity lambda is:",
                            round(lambda, digits = 4))))
}

#All the forecasts and related informations are stored in the DF dataframe

DF <- tibble("Year, Qtr of forecast" = character(),
             "Series" = character(),
             "F-method" = character(),
             "R-method" = character(),
             "Forecast Horizon" = integer(),
             "Forecasts" = double(),
             "Actual" = double(),
             "Scaling Factor" = numeric(),
             "Training window_length" = integer(),
             "Replication" = integer())

start_train <- c(1984, 4)
Time.index <- as.yearqtr(1984 + seq(0, 136)/4)
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
  
  Residuals_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_all_Benchmark <- matrix(NA, nrow = nrow(Train), ncol = n) #Benchmark method: seasonal RW with a drift
  
  Base_ARIMA <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Base_ETS <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Base_Benchmark <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  
  Recon_PointF_BU_ARIMA <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_BU_ETS <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_BU_Benchmark <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  
  Recon_PointF_OLS_ARIMA <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_OLS_ETS <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_OLS_Benchmark <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  
  Recon_PointF_WLS_ARIMA <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_WLS_ETS <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_WLS_Benchmark <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  
  Recon_PointF_MinT.Shr_ARIMA <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_MinT.Shr_ETS <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_MinT.Shr_Benchmark <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  
  Recon_PointF_MinT.Sam_ARIMA <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_MinT.Sam_ETS <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  Recon_PointF_MinT.Sam_Benchmark <- matrix(NA, nrow = min(H, nrow(Test[,1])), ncol = n)
  
  fit_ARIMA <- list(n)
  fit_ETS <- list(n)
  fit_Benchmark <- list(n)
  
  
  
  for(i in 1:n) {
    
    TS <- ts(Train[,i], frequency = 4, start = start_train)
    
    #Scaling Factor for calculating MASE
    snaive(TS)$residuals %>% abs() %>% mean(., na.rm=TRUE) -> Q
    
    #Forecsting with benchmark
    fit_Benchmark[[i]] <- Arima(TS, order=c(0,0,0), seasonal=c(0,1,0), include.drift=TRUE)
    Forecast_Benchmark <- forecast(fit_Benchmark[[i]], h = min(H, nrow(Test[,i])))
    Base_Benchmark[,i] <- Forecast_Benchmark$mean
    Residuals_all_Benchmark[,i] <- as.vector(TS - fitted(fit_Benchmark[[i]]))
      

    #Adding these base forecasts from Benchmark to the DF
    for (h in 1: min(H, nrow(Test))) {
      
     DF <- DF %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_Benchmark$mean))[h]),
                     "Series" = paste(colnames(Exp)[i]),
                     "F-method" = "Benchmark", 
                     "R-method" = "Base",
                     "Forecast Horizon" = h,
                     "Forecasts" = Forecast_Benchmark$mean[h],
                     "Actual" = as.numeric(Test[h,i]),
                     "Scaling Factor" = Q,
                     "Training window_length" = first_train_length + j,
                     "Replication" = j)
    }
    
    ##Forecsting with ETS##
    
    fit_ETS[[i]] <- ets(TS)
    Forecast_ETS <- forecast(fit_ETS[[i]], h = min(H, nrow(Test[,i])))
    Base_ETS[,i] <- Forecast_ETS$mean
    Residuals_all_ETS[,i] <- as.vector(TS - fitted(fit_ETS[[i]]))
    
    #Adding these base forecasts from ETS to the DF
    for (h in 1: min(H, nrow(Test[,i]))) {
      
     DF <- DF %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ETS$mean))[h]),
                     "Series" = paste(colnames(Exp)[i]),
                     "F-method" = "ETS", 
                     "R-method" = "Base",
                     "Forecast Horizon" = h,
                     "Forecasts" = Forecast_ETS$mean[h],
                     "Actual" = as.numeric(Test[h,i]),
                     "Scaling Factor" = Q,
                     "Training window_length" = first_train_length + j,
                     "Replication" = j) 
    }
    
    #Forecsting with ARIMA
    fit_ARIMA[[i]] <- auto.arima(TS)
    Forecast_ARIMA <- forecast(fit_ARIMA[[i]], h = min(H, nrow(Test[,i])))
    Base_ARIMA[,i] <- Forecast_ARIMA$mean
    Residuals_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA[[i]]))
    
    #Adding these base forecasts from ARIMA to the DF
    for (h in 1: min(H, nrow(Test[,i]))) {
      
      DF <- DF %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ARIMA$mean))[h]),
                     "Series" = paste(colnames(Exp)[i]),
                     "F-method" = "ARIMA", 
                     "R-method" = "Base",
                     "Forecast Horizon" = h,
                     "Forecasts" = Forecast_ARIMA$mean[h],
                     "Actual" = as.numeric(Test[h,i]),
                     "Scaling Factor" = Q,
                     "Training window_length" = first_train_length + j,
                     "Replication" = j)
    }   
    
  }

    
    ###Reconciliation of base forecasts from benchmark###

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
    
    MinT.shr_P_Bench <- solve(t(S) %*% Inv_Shr.cov_Bench %*% S) %*% t(S) %*% Inv_Shr.cov_Bench

    #WLS P
    Cov_WLS_Bench <- diag(diag(Shr.cov_Bench), n, n)
    Inv_WLS <- solve(Cov_WLS_Bench)

    WLS_P_Bench <- solve(t(S) %*% Inv_WLS %*% S) %*% t(S) %*% Inv_WLS

    #Reconciliation of base forecasts from benchmark#
    Recon_PointF_BU_Benchmark <- t(S %*% BU_P %*% t(Base_Benchmark))
    Recon_PointF_OLS_Benchmark <- t(S %*% OLS_P %*% t(Base_Benchmark))
    Recon_PointF_WLS_Benchmark <- t(S %*% WLS_P_Bench %*% t(Base_Benchmark))
    Recon_PointF_MinT.Shr_Benchmark <- t(S %*% MinT.shr_P_Bench %*% t(Base_Benchmark))

    #Adding these reconcilied forecasts from Benchmark-base to the DF
    
    Fltr <- DF %>% filter(`F-method`=="Benchmark", `R-method`=="Base", 
                         `Training window_length` == (first_train_length + j), `Replication`==j) %>% select(-"Forecasts", -"R-method")
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_BU_Benchmark), "R-method" = "Bottom-up") -> Df_BU
    Df_BU[names(DF)] -> Df_BU
    DF <- rbind(DF, Df_BU)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_OLS_Benchmark), "R-method" = "OLS") -> Df_OLS
    Df_OLS[names(DF)] -> Df_OLS
    DF <- rbind(DF, Df_OLS)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_WLS_Benchmark), "R-method" = "WLS") -> Df_WLS
    Df_WLS[names(DF)] -> Df_WLS
    DF <- rbind(DF, Df_WLS)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_MinT.Shr_Benchmark), "R-method" = "MinT Shrink") -> Df_MShr
    Df_MShr[names(DF)] -> Df_MShr
    DF <- rbind(DF, Df_MShr)
    
    
   
    ###Reconciliation of base forecasts from ETS###
    
    #MinT shrink P
    targ <- lowerD(Residuals_all_ETS)
    shrink <- shrink.estim(Residuals_all_ETS,targ)
    Shr.cov_ETS <- shrink[[1]]
    
    Inv_Shr.cov_ETS <- solve(Shr.cov_ETS)
    MinT.shr_P_ETS <- solve(t(S) %*% Inv_Shr.cov_ETS %*% S) %*% t(S) %*% Inv_Shr.cov_ETS
    
    
    #WLS P
    Cov_WLS_ETS <- diag(diag(Shr.cov_ETS), n, n)
    Inv_WLS_ETS <- solve(Cov_WLS_ETS)
    
    WLS_P_ETS <- solve(t(S) %*% Inv_WLS_ETS %*% S) %*% t(S) %*% Inv_WLS_ETS
    
    #Reconciliation of base forecasts from ETS#
    Recon_PointF_BU_ETS <- t(S %*% BU_P %*% t(Base_ETS))
    Recon_PointF_OLS_ETS <- t(S %*% OLS_P %*% t(Base_ETS))
    Recon_PointF_WLS_ETS <- t(S %*% WLS_P_ETS %*% t(Base_ETS))
    Recon_PointF_MinT.Shr_ETS <- t(S %*% MinT.shr_P_ETS %*% t(Base_ETS))
    
    #Adding these reconcilied forecasts from Benchmark-base to the DF
    
    Fltr <- DF %>% filter(`F-method`=="ETS", `R-method`=="Base", 
                          `Training window_length` == (first_train_length + j), `Replication`==j) %>% select(-"Forecasts", -"R-method")
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_BU_ETS), "R-method" = "Bottom-up") -> Df_BU
    Df_BU[names(DF)] -> Df_BU
    DF <- rbind(DF, Df_BU)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_OLS_ETS), "R-method" = "OLS") -> Df_OLS
    Df_OLS[names(DF)] -> Df_OLS
    DF <- rbind(DF, Df_OLS)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_WLS_ETS), "R-method" = "WLS") -> Df_WLS
    Df_WLS[names(DF)] -> Df_WLS
    DF <- rbind(DF, Df_WLS)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_MinT.Shr_ETS), "R-method" = "MinT Shrink") -> Df_MShr
    Df_MShr[names(DF)] -> Df_MShr
    DF <- rbind(DF, Df_MShr)
    
    
    ###Reconciliation of base forecasts from ARIMA###
    
    #MinT shrink P
    targ <- lowerD(Residuals_all_ARIMA)
    shrink <- shrink.estim(Residuals_all_ARIMA,targ)
    Shr.cov_ARIMA <- shrink[[1]]
    
    Inv_Shr.cov_ARIMA <- solve(Shr.cov_ARIMA)
    MinT.shr_P_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
    
    #WLS P
    Cov_WLS_ARIMA <- diag(diag(Shr.cov_ARIMA), n, n)
    Inv_WLS_ARIMA <- solve(Cov_WLS_ARIMA)
    
    WLS_P_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
    
    #Reconciliation of base forecasts from ARMA#
    Recon_PointF_BU_ARIMA <- t(S %*% BU_P %*% t(Base_ARIMA))
    Recon_PointF_OLS_ARIMA <- t(S %*% OLS_P %*% t(Base_ARIMA))
    Recon_PointF_WLS_ARIMA <- t(S %*% WLS_P_ARIMA %*% t(Base_ARIMA))
    Recon_PointF_MinT.Shr_ARIMA <- t(S %*% MinT.shr_P_ARIMA %*% t(Base_ARIMA))
    
    #Adding these reconcilied forecasts from Benchmark-base to the DF
    
    Fltr <- DF %>% filter(`F-method`=="ARIMA", `R-method`=="Base", 
                          `Training window_length` == (first_train_length + j), `Replication`==j) %>% select(-"Forecasts", -"R-method")
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_BU_ARIMA), "R-method" = "Bottom-up") -> Df_BU
    Df_BU[names(DF)] -> Df_BU
    DF <- rbind(DF, Df_BU)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_OLS_ARIMA), "R-method" = "OLS") -> Df_OLS
    Df_OLS[names(DF)] -> Df_OLS
    DF <- rbind(DF, Df_OLS)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_WLS_ARIMA), "R-method" = "WLS") -> Df_WLS
    Df_WLS[names(DF)] -> Df_WLS
    DF <- rbind(DF, Df_WLS)
    
    cbind(Fltr, "Forecasts" = as.vector(Recon_PointF_MinT.Shr_ARIMA), "R-method" = "MinT Shrink") -> Df_MShr
    Df_MShr[names(DF)] -> Df_MShr
    DF <- rbind(DF, Df_MShr)
    
 
  
}

End <- Sys.time()


#Evaluation

DF %>% mutate(SquaredE = (`Actual` - `Forecasts`)^2,
              ScaledE = (`Actual` - `Forecasts`)/`Scaling Factor`) -> DF

#Calculating the skill scores - Percentage improvement of the preferred forecasting method with respect to the benchmark
Skill.Score <- list(16)
SS_RMSE <- list(16)
SS_MASE <- list(16)

for (i in 1:n) {
  
  DF %>% filter(`Series`==names(Exp)[i]) %>% select("F-method", "R-method", "Forecast Horizon", 
                                                    "SquaredE", "ScaledE") -> Skill.Score[[i]]
  
  Skill.Score[[i]] %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
    summarise(RMSE = sqrt(mean(`SquaredE`)), MASE = sqrt(mean(abs(`ScaledE`)))) -> Skill.Score[[i]]
  
  #GDPi %>% spread(key = `Forecast Horizon`, value = `RMSE`)
  
  Skill.Score[[i]] %>% filter(`F-method`=="Benchmark", `R-method`=="Base") %>%
    slice() %>% 
    ungroup() %>%
    select(`RMSE`) %>% as_vector() -> bm_RMSE
  
  Skill.Score[[i]] %>% filter(`F-method`=="Benchmark", `R-method`=="Base") %>%
    slice() %>% 
    ungroup() %>%
    select(`MASE`) %>% as_vector() -> bm_MASE
  
  Skill.Score[[i]] %>% mutate(SS_RMSE = round((1-(`RMSE`/bm_RMSE))*100, digits = 4), 
                          SS_MASE = round((1-(`MASE`/bm_MASE))*100, digits = 4)) -> Skill.Score[[i]]  
  
  Skill.Score[[i]] %>%  select(-`RMSE`, -`MASE`, -`SS_MASE`) %>% 
    spread(key = `Forecast Horizon`, value = `SS_RMSE`) -> SS_RMSE[[i]]
  
  Skill.Score[[i]] %>%  select(-`RMSE`, -`MASE`, -`SS_RMSE`) %>% 
    spread(key = `Forecast Horizon`, value = `SS_MASE`) -> SS_MASE[[i]]
  
  
}



GDPi_SS.RMSE <- SS_RMSE[[1]]
TFi_SS.RMSE <- SS_RMSE[[2]]
TFiGos_SS.RMSE <- SS_RMSE[[3]]
TFiCoe_SS.RMSE <- SS_RMSE[[4]]
TFiGosCop_SS.RMSE <- SS_RMSE[[5]]
TFiGosCopNfn_SS.RMSE <- SS_RMSE[[6]]

GDPi_SS.MASE <- SS_MASE[[1]]
TFi_SS.MASE <- SS_MASE[[2]]
TFiGos_SS.MASE <- SS_MASE[[3]]
TFiCoe_SS.MASE <- SS_MASE[[4]]
TFiGosCop_SS.MASE <- SS_MASE[[5]]
TFiGosCopNfn_SS.MASE <- SS_MASE[[6]]

