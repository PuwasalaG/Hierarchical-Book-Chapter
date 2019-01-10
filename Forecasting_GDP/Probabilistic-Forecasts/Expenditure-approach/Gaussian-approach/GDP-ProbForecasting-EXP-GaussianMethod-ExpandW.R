
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
k <- 5000 #Number of random samples generated from the forecast distribution for the evaluation

#Function to get shrinkage estimator

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

#This function is used to generate random numbers from the degenerate Gaussian distributions
rnorm_degenerate <- function(mu, Sigma, n, k)
{
  Sigma <- matrix(Sigma, n, n)
  SVD <- svd((Sigma + t(Sigma))/2)
  
  SVD$d <- abs(zapsmall(SVD$d))
  U <- SVD$u
  D <- diag(sqrt(SVD$d))
  m1 <- sum(SVD$d > 0)
  
  X <- mvtnorm::rmvnorm(k, mean = rep(0, m1), diag(1,m1,m1))
  X <- cbind(X, matrix(0, nrow = k, ncol = n-m1))
  
  Mu <- matrix(rep(mu, k), k, n, byrow = TRUE)
  
  Y <- t(U %*% D %*% t(X)) + Mu
  
  return(Y)
  
}

#A function to calculate Enery score to evaluate forecast densities

Energy_score <- function(Data, Real)
{
  n <- ncol(Data)
  k <- nrow(Data)
  
  Testing <- as.numeric(Real)
  
  d1_eval <- Data - matrix(rep(Testing, k), k, n, byrow = TRUE)
  ES_1_eval <- sqrt(rowSums(d1_eval^2))
  
  d2_eval <- Data[1:k-1,] - Data[2:k,]
  ES_2_eval <- sqrt(rowSums(d2_eval^2))
  
  ES_eval <- mean(ES_1_eval) - mean(ES_2_eval)/2
  
  return(ES_eval)
  
}

#A function to calculate log score to evaluate forecast densities

Log_score_bottom <- function(P, Sigma.hat, Base, real)
{
  real = as.numeric(real)
  
  Bottom_mean <- P %*% Base
  Bottom_cov <- P %*% Sigma.hat %*% t(P)
  Eigen <- eigen(Bottom_cov)$values
  
  log_det <- log(prod(Eigen))
  Inv_bottom_cov <- solve(Bottom_cov)
  Mean_shift <- c(real-Bottom_mean)
  
  (1/2)*((m*log(2*pi)) + log_det + 
           t(Mean_shift)%*%Inv_bottom_cov%*%Mean_shift)
  
}

#A function to calculate variogram score to evaluate forecast densities


Variogram_score <- function(Data, real)
{
  Testing <- as.numeric(real)
  
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
                    "Log score" = numeric(),
                    "Training window_length" = integer(),
                    "Replication" = integer())

DF_UniV <- tibble("Year, Qtr of forecast" = character(),
                    "Series" = character(),
                    "F-method" = character(),
                    "R-method" = character(),
                    "Forecast Horizon" = integer(),
                    "Actual" = double(),
                    "CRPS" = numeric(),
                    "LS" = numeric(),
                    "Training window_length" = integer(),
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
  
  #Year_Qtr_of_forecast <- numeric(min(H, nrow(Test))) #This is important to get the starting point in rolling window approach
  
  #To store residuals
  Residuals_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  Residuals_all_Benchmark <- matrix(NA, nrow = nrow(Train), ncol = n) #Benchmark method: seasonal RW with a drift
  
  #To store base forecasts
  Base_ARIMA <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_ETS <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  Base_Benchmark <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  #To store multivariate scores
  
  ES_full_BU_bench <- numeric(min(H, nrow(Test)))
  ES_full_OLS_bench <- numeric(min(H, nrow(Test)))
  ES_full_MinT.Shr_bench <- numeric(min(H, nrow(Test)))
  ES_full_WLS_bench <- numeric(min(H, nrow(Test)))
  ES_full_unrecon_bench <- numeric(min(H, nrow(Test)))
  
  LS_bottom_MinT.Shr_bench <- numeric(min(H, nrow(Test)))
  LS_bottom_WLS_bench <- numeric(min(H, nrow(Test)))
  LS_bottom_OLS_bench <- numeric(min(H, nrow(Test)))
  LS_bottom_BU_bench <- numeric(min(H, nrow(Test)))
  
  VS_full_BU_bench <- numeric(min(H, nrow(Test)))
  VS_full_OLS_bench <- numeric(min(H, nrow(Test)))
  VS_full_MinT.Shr_bench <- numeric(min(H, nrow(Test)))
  VS_full_WLS_bench <- numeric(min(H, nrow(Test)))
  VS_full_Unrecon_bench <- numeric(min(H, nrow(Test)))
  
  ES_full_BU_ETS <- numeric(min(H, nrow(Test)))
  ES_full_OLS_ETS <- numeric(min(H, nrow(Test)))
  ES_full_MinT.Shr_ETS <- numeric(min(H, nrow(Test)))
  ES_full_WLS_ETS <- numeric(min(H, nrow(Test)))
  ES_full_unrecon_ETS <- numeric(min(H, nrow(Test)))
  
  LS_bottom_MinT.Shr_ETS <- numeric(min(H, nrow(Test)))
  LS_bottom_WLS_ETS <- numeric(min(H, nrow(Test)))
  LS_bottom_OLS_ETS <- numeric(min(H, nrow(Test)))
  LS_bottom_BU_ETS <- numeric(min(H, nrow(Test)))
  
  VS_full_BU_ETS <- numeric(min(H, nrow(Test)))
  VS_full_OLS_ETS <- numeric(min(H, nrow(Test)))
  VS_full_MinT.Shr_ETS <- numeric(min(H, nrow(Test)))
  VS_full_WLS_ETS <- numeric(min(H, nrow(Test)))
  VS_full_Unrecon_ETS <- numeric(min(H, nrow(Test)))
  
  ES_full_BU_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_OLS_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_MinT.Shr_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_WLS_ARIMA <- numeric(min(H, nrow(Test)))
  ES_full_unrecon_ARIMA <- numeric(min(H, nrow(Test)))
  
  LS_bottom_MinT.Shr_ARIMA <- numeric(min(H, nrow(Test)))
  LS_bottom_WLS_ARIMA <- numeric(min(H, nrow(Test)))
  LS_bottom_OLS_ARIMA <- numeric(min(H, nrow(Test)))
  LS_bottom_BU_ARIMA <- numeric(min(H, nrow(Test)))
  
  VS_full_BU_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_OLS_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_MinT.Shr_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_WLS_ARIMA <- numeric(min(H, nrow(Test)))
  VS_full_Unrecon_ARIMA <- numeric(min(H, nrow(Test)))
  
  #To store univariate scores
  
  CRPS_BU_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  CRPS_BU_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  CRPS_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  CRPS_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  
  LS_UniV_BU_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_OLS_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_WLS_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_MinT.Shr_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_Unrecon_bench <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  LS_UniV_BU_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_OLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_WLS_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_MinT.Shr_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_Unrecon_ETS <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  LS_UniV_BU_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_OLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_WLS_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_MinT.Shr_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  LS_UniV_Unrecon_ARIMA <- matrix(0, nrow = min(H, nrow(Test)), ncol = n)
  
  #Model fitting, base forecasts, and residuals  
  for(i in 1:n) {
    
    TS <- ts(Train[,i], frequency = 4, start = start_train)
    
    #Forecsting with benchmark
    fit_Benchmark <- Arima(TS, order=c(0,0,0), seasonal=c(0,1,0), include.drift=TRUE)
    Forecast_Benchmark <- forecast(fit_Benchmark, h = min(H, nrow(Test[,i])))
    Base_Benchmark[,i] <- Forecast_Benchmark$mean
    Residuals_all_Benchmark[,i] <- as.vector(TS - fitted(fit_Benchmark))
    
    
    ##Forecsting with ETS##
    
    fit_ETS <- ets(TS)
    Forecast_ETS <- forecast(fit_ETS, h = min(H, nrow(Test[,i])))
    Base_ETS[,i] <- Forecast_ETS$mean
    Residuals_all_ETS[,i] <- as.vector(TS - fitted(fit_ETS))
    
    
    #Forecsting with ARIMA
    fit_ARIMA <- auto.arima(TS)
    Forecast_ARIMA <- forecast(fit_ARIMA, h = min(H, nrow(Test[,i])))
    Base_ARIMA[,i] <- Forecast_ARIMA$mean
    Residuals_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA))
    
    
  }
  
  
  ###Reconciliation of base forecasts from benchmark###
  
  #Getting different P matrises
  
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
  Inv_WLS_Bench <- solve(Cov_WLS_Bench)
  
  WLS_P_Bench <- solve(t(S) %*% Inv_WLS_Bench %*% S) %*% t(S) %*% Inv_WLS_Bench
  
  #Reconciled point and variance forecasts
  
  #Reconciled point forecasts for the bottom levels of the hierarchy (Followed from benchmark)#
  Recon_PointF_bottom_BU_Bench <- t(BU_P %*% t(Base_Benchmark))
  Recon_PointF_bottom_OLS_Bench <- t(OLS_P %*% t(Base_Benchmark))
  Recon_PointF_bottom_WLS_Bench <- t(WLS_P_Bench %*% t(Base_Benchmark))
  Recon_PointF_bottom_MinT.Shr_Bench <- t(MinT.Shr_P_Bench %*% t(Base_Benchmark))
  
  #Reconciled bottom level variance forecasts (Followed from benchmark)#
  Recon_Var.Cov_bottom_BU_Bench <- BU_P %*% Shr.cov_Bench %*% t(BU_P)
  Recon_Var.Cov_bottom_OLS_Bench <- OLS_P %*% Shr.cov_Bench %*% t(OLS_P)
  Recon_Var.Cov_bottom_WLS_Bench <- WLS_P_Bench %*% Shr.cov_Bench %*% t(WLS_P_Bench)
  Recon_Var.Cov_bottom_MinT.Shr_Bench <- MinT.Shr_P_Bench %*% Shr.cov_Bench %*% t(MinT.Shr_P_Bench)
  
  #Reconciled point forecasts for the full hierarchy (Followed from benchmark)#
  Recon_PointF_BU_Bench <- t(S %*% BU_P %*% t(Base_Benchmark))
  Recon_PointF_OLS_Bench <- t(S %*% OLS_P %*% t(Base_Benchmark))
  Recon_PointF_WLS_Bench <- t(S %*% WLS_P_Bench %*% t(Base_Benchmark))
  Recon_PointF_MinT.Shr_Bench <- t(S %*% MinT.Shr_P_Bench %*% t(Base_Benchmark))
  
  #Reconciled variance forecasts for the full hierarchy (Followed from benchmark)#
  Recon_Var.Cov_BU_Bench <- S %*% BU_P %*% Shr.cov_Bench %*% t(S %*% BU_P)
  Recon_Var.Cov_OLS_Bench <- S %*% OLS_P %*% Shr.cov_Bench %*% t(S %*% OLS_P)
  Recon_Var.Cov_WLS_Bench <- S %*% WLS_P_Bench %*% Shr.cov_Bench %*% t(S %*% WLS_P_Bench)
  Recon_Var.Cov_MinT.Shr_Bench <- S %*% MinT.Shr_P_Bench %*% Shr.cov_Bench %*% t(S %*% MinT.Shr_P_Bench)
  
  
  for (h in 1: min(H, nrow(Test))) {
    
    #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
    #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
    # function to generate random samples)
    
    X_full_BU_bench <- rnorm_degenerate(mu = Recon_PointF_BU_Bench[h,], 
                                        Sigma = Recon_Var.Cov_BU_Bench, k = k, n = n)
    
    X_full_OLS_bench <- rnorm_degenerate(mu = Recon_PointF_OLS_Bench[h,], 
                                         Sigma = Recon_Var.Cov_OLS_Bench, k = k, n = n)
    
    X_full_WLS_bench <- rnorm_degenerate(mu = Recon_PointF_WLS_Bench[h,], 
                                         Sigma = Recon_Var.Cov_WLS_Bench, k = k, n = n)
    
    X_full_MinT.Shr_bench <- rnorm_degenerate(mu = Recon_PointF_MinT.Shr_Bench[h,], 
                                              Sigma = Recon_Var.Cov_MinT.Shr_Bench, k = k, n = n)
    
    X_full_unrecon_bench <- rnorm_degenerate(mu = Base_Benchmark[h,], Sigma = Shr.cov_Bench, k = k, n = n)
    
    ###Evaluating the reconciled forecasts from different forecasting methods
    #(only the bottom levels are evaluated)
    
    #Calculating Energy score for full predicive densities
    
    ES_full_BU_bench[h] <- Energy_score(Data = X_full_BU_bench, Real = Test[h,])
    ES_full_OLS_bench[h] <- Energy_score(Data = X_full_OLS_bench, Real = Test[h,])
    ES_full_WLS_bench[h] <- Energy_score(Data = X_full_WLS_bench, Real = Test[h,])
    ES_full_MinT.Shr_bench[h] <- Energy_score(Data = X_full_MinT.Shr_bench, Real = Test[h,])
    ES_full_unrecon_bench[h] <- Energy_score(Data = X_full_unrecon_bench, Real = Test[h,])
    
    #Calculating Log score for bottom level predictive densities
    
    LS_bottom_BU_bench[h] <- Log_score_bottom(P = BU_P, Sigma.hat = Shr.cov_Bench, Base = Base_Benchmark[h,],
                                              real = Test[h,(n-m+1):n])
    
    LS_bottom_OLS_bench[h] <- Log_score_bottom(P = OLS_P, Sigma.hat = Shr.cov_Bench, 
                                               Base = Base_Benchmark[h,], real = Test[h,(n-m+1):n])
    
    LS_bottom_WLS_bench[h] <- Log_score_bottom(P = WLS_P_Bench, Sigma.hat = Shr.cov_Bench,
                                               Base = Base_Benchmark[h,], real = Test[h,(n-m+1):n])
    
    LS_bottom_MinT.Shr_bench[h] <- Log_score_bottom(P = MinT.Shr_P_Bench, Sigma.hat = Shr.cov_Bench, 
                                                    Base = Base_Benchmark[h,], real = Test[h,(n-m+1):n])
    
    
    
    #Evaluating the density forecasts using Variogram based score
    
    VS_full_BU_bench[h] <- Variogram_score(Data = X_full_BU_bench, real = Test[h,])
    VS_full_OLS_bench[h] <- Variogram_score(Data = X_full_OLS_bench, real = Test[h,])
    VS_full_WLS_bench[h] <- Variogram_score(Data = X_full_WLS_bench, real = Test[h,])
    VS_full_MinT.Shr_bench[h] <- Variogram_score(Data = X_full_MinT.Shr_bench, real = Test[h,])
    VS_full_Unrecon_bench[h] <- Variogram_score(Data = X_full_unrecon_bench, real = Test[h,])
    
    
    
    #Calculating CRPS and LS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_unrecon_bench[,i],
                                             method = "edf")
      CRPS_BU_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_BU_bench[,i],
                                        method = "edf")
      CRPS_OLS_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_OLS_bench[,i],
                                         method = "edf")
      CRPS_WLS_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_WLS_bench[,i],
                                         method = "edf")
      CRPS_MinT.Shr_bench[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Shr_bench[,i],
                                              method = "edf")
      
      LS_UniV_Unrecon_bench[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Base_Benchmark[h,i], 
                                           sd = sqrt(Shr.cov_Bench[i,i]),log = TRUE)
      
      LS_UniV_BU_bench[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_BU_Bench[h,i], 
                                      sd = sqrt(Recon_Var.Cov_BU_Bench[i,i]),log = TRUE)
      
      LS_UniV_OLS_bench[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_OLS_Bench[h,i], 
                                       sd = sqrt(Recon_Var.Cov_OLS_Bench[i,i]),log = TRUE)
      
      LS_UniV_WLS_bench[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_WLS_Bench[h,i], 
                                       sd = sqrt(Recon_Var.Cov_WLS_Bench[i,i]),log = TRUE)
      
      LS_UniV_MinT.Shr_bench[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_MinT.Shr_Bench[h,i], 
                                            sd = sqrt(Recon_Var.Cov_MinT.Shr_Bench[i,i]),log = TRUE)
      
    }
    
    
    DF_MultiV <- DF_MultiV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_Benchmark$mean))[h]),
                                       "F-method" = "Benchmark",
                                       "Replication" = j, 
                                       "Training window_length" = first_train_length + j)
    
    DF_UniV <- DF_UniV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_Benchmark$mean))[h]),
                                   "F-method" = "Benchmark",
                                   "Replication" = j,
                                   "Training window_length" = first_train_length + j)
  }
  
  DF_MultiV %>% filter(`F-method`=="Benchmark", `Replication`==j) %>%
    dplyr::select("Year, Qtr of forecast", "F-method", "Replication", "Training window_length") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_unrecon_bench, 
        "Variogram score" = VS_full_Unrecon_bench, "Log score" = c(NA)) -> DF_Base
  DF_Base[names(DF_MultiV)] -> DF_Base
  DF_MultiV <- rbind(DF_MultiV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_BU_bench, 
        "Variogram score" = VS_full_BU_bench, "Log score" = LS_bottom_BU_bench) -> DF_BU
  DF_BU[names(DF_MultiV)] -> DF_BU
  DF_MultiV <- rbind(DF_MultiV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_OLS_bench, 
        "Variogram score" = VS_full_OLS_bench, "Log score" = LS_bottom_OLS_bench) -> DF_OLS
  DF_OLS[names(DF_MultiV)] -> DF_OLS
  DF_MultiV <- rbind(DF_MultiV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_WLS_bench, 
        "Variogram score" = VS_full_WLS_bench, "Log score" = LS_bottom_WLS_bench) -> DF_WLS
  DF_WLS[names(DF_MultiV)] -> DF_WLS
  DF_MultiV <- rbind(DF_MultiV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_MinT.Shr_bench, 
        "Variogram score" = VS_full_MinT.Shr_bench, "Log score" = LS_bottom_MinT.Shr_bench) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV)] -> DF_MinT.Shr
  DF_MultiV <- rbind(DF_MultiV, DF_MinT.Shr)
  
  
  #Addinng CRPS and LS to the DF
  
  DF_UniV %>% filter(`F-method`=="Benchmark", `Replication`==j) %>% 
    dplyr::select("Year, Qtr of forecast", "F-method", "Replication", "Training window_length") -> Fltr
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:4,]))),  "R-method" = "Base", 
        "Forecast Horizon" = rep(1:4, each = 16), "CRPS" = c(t(CRPS_Unrecon_bench)), 
        "LS" = c(t(LS_UniV_Unrecon_bench)) ) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:4,]))), "R-method" = "Bottom up", 
        "Forecast Horizon" = rep(1:4, each = 16), "CRPS" = c(t(CRPS_BU_bench)), 
        "LS" = c(t(LS_UniV_BU_bench))) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:4,]))), "R-method" = "OLS", 
        "Forecast Horizon" = rep(1:4, each = 16), "CRPS" = c(t(CRPS_OLS_bench)), 
        "LS" = c(t(LS_UniV_OLS_bench))) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:4,]))), "R-method" = "WLS", 
        "Forecast Horizon" = rep(1:4, each = 16), "CRPS" = c(t(CRPS_WLS_bench)), 
        "LS" = c(t(LS_UniV_WLS_bench))) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:4,]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:4, each = 16), "CRPS" = c(t(CRPS_MinT.Shr_bench)), 
        "LS" = c(t(LS_UniV_MinT.Shr_bench))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  
  ##Reconciling densities from ETS models##
  
  #Getting P matrices
  
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
  
  #Reconciled bottom level point forecasts (Followed from ETS)#
  Recon_PointF_bottom_BU_ETS <- t(BU_P %*% t(Base_ETS))
  Recon_PointF_bottom_OLS_ETS <- t(OLS_P %*% t(Base_ETS))
  Recon_PointF_bottom_WLS_ETS <- t(WLS_P_ETS %*% t(Base_ETS))
  Recon_PointF_bottom_MinT.Shr_ETS <- t(MinT.Shr_P_ETS %*% t(Base_ETS))
  
  #Reconciled bottom level variance forecasts (Followed from ETS)#
  Recon_Var.Cov_bottom_BU_ETS <- BU_P %*% Shr.cov_ETS %*% t(BU_P)
  Recon_Var.Cov_bottom_OLS_ETS <- OLS_P %*% Shr.cov_ETS %*% t(OLS_P)
  Recon_Var.Cov_bottom_WLS_ETS <- WLS_P_ETS %*% Shr.cov_ETS %*% t(WLS_P_ETS)
  Recon_Var.Cov_bottom_MinT.Shr_ETS <- MinT.Shr_P_ETS %*% Shr.cov_ETS %*% t(MinT.Shr_P_ETS)
  
  
  #Reconciled point forecasts for the full hierarchy (Followed from ETS)#
  Recon_PointF_BU_ETS <- t(S %*% BU_P %*% t(Base_ETS))
  Recon_PointF_OLS_ETS <- t(S %*% OLS_P %*% t(Base_ETS))
  Recon_PointF_WLS_ETS <- t(S %*% WLS_P_ETS %*% t(Base_ETS))
  Recon_PointF_MinT.Shr_ETS <- t(S %*% MinT.Shr_P_ETS %*% t(Base_ETS))
  
  #Reconciled variance forecasts for the full hierarchy (Followed from ETS)#
  Recon_Var.Cov_BU_ETS <- S %*% BU_P %*% Shr.cov_ETS %*% t(S %*% BU_P)
  Recon_Var.Cov_OLS_ETS <- S %*% OLS_P %*% Shr.cov_ETS %*% t(S %*% OLS_P)
  Recon_Var.Cov_WLS_ETS <- S %*% WLS_P_ETS %*% Shr.cov_ETS %*% t(S %*% WLS_P_ETS)
  Recon_Var.Cov_MinT.Shr_ETS <- S %*% MinT.Shr_P_ETS %*% Shr.cov_ETS %*% t(S %*% MinT.Shr_P_ETS)
  
  
  for (h in 1: min(H, nrow(Test))) {
    
    #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
    #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
    # function to generate random samples)
    
    X_full_BU_ETS <- rnorm_degenerate(mu = Recon_PointF_BU_ETS[h,], 
                                      Sigma = Recon_Var.Cov_BU_ETS, k = k, n = n)
    
    X_full_OLS_ETS <- rnorm_degenerate(mu = Recon_PointF_OLS_ETS[h,], 
                                       Sigma = Recon_Var.Cov_OLS_ETS, k = k, n = n)
    
    X_full_WLS_ETS <- rnorm_degenerate(mu = Recon_PointF_WLS_ETS[h,], 
                                       Sigma = Recon_Var.Cov_WLS_ETS, k = k, n = n)
    
    X_full_MinT.Shr_ETS <- rnorm_degenerate(mu = Recon_PointF_MinT.Shr_ETS[h,], 
                                            Sigma = Recon_Var.Cov_MinT.Shr_ETS, k = k, n = n)
    
    X_full_unrecon_ETS <- mvrnorm(n = k, mu = Base_ETS[h,], Sigma = Shr.cov_ETS)
    
    ###Evaluating the reconciled forecasts from different forecasting methods
    #(only the bottom levels are evaluated)
    
    #Calculating Energy score for full predicive densities
    
    ES_full_BU_ETS[h] <- Energy_score(Data = X_full_BU_ETS, Real = Test[h,])
    ES_full_OLS_ETS[h] <- Energy_score(Data = X_full_OLS_ETS, Real = Test[h,])
    ES_full_WLS_ETS[h] <- Energy_score(Data = X_full_WLS_ETS, Real = Test[h,])
    ES_full_MinT.Shr_ETS[h] <- Energy_score(Data = X_full_MinT.Shr_ETS, Real = Test[h,])
    ES_full_unrecon_ETS[h] <- Energy_score(Data = X_full_unrecon_ETS, Real = Test[h,])
    
    #Calculating Log score for bottom level predictive densities
    
    LS_bottom_BU_ETS[h] <- Log_score_bottom(P = BU_P, Sigma.hat = Shr.cov_ETS, Base = Base_ETS[h,],
                                            real = Test[h,(n-m+1):n])
    
    LS_bottom_OLS_ETS[h] <- Log_score_bottom(P = OLS_P, Sigma.hat = Shr.cov_ETS, 
                                             Base = Base_ETS[h,], real = Test[h,(n-m+1):n])
    
    LS_bottom_WLS_ETS[h] <- Log_score_bottom(P = WLS_P_ETS, Sigma.hat = Shr.cov_ETS,
                                             Base = Base_ETS[h,], real = Test[h,(n-m+1):n])
    
    LS_bottom_MinT.Shr_ETS[h] <- Log_score_bottom(P = MinT.Shr_P_ETS, Sigma.hat = Shr.cov_ETS, 
                                                  Base = Base_ETS[h,], real = Test[h,(n-m+1):n])
    
    
    
    #Evaluating the density forecasts using Variogram based score
    
    VS_full_BU_ETS[h] <- Variogram_score(Data = X_full_BU_ETS, real = Test[h,])
    VS_full_OLS_ETS[h] <- Variogram_score(Data = X_full_OLS_ETS, real = Test[h,])
    VS_full_WLS_ETS[h] <- Variogram_score(Data = X_full_WLS_ETS, real = Test[h,])
    VS_full_MinT.Shr_ETS[h] <- Variogram_score(Data = X_full_MinT.Shr_ETS, real = Test[h,])
    VS_full_Unrecon_ETS[h] <- Variogram_score(Data = X_full_unrecon_ETS, real = Test[h,])
    
    #Calculating CRPS and LS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_unrecon_ETS[,i],
                                           method = "edf")
      CRPS_BU_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_BU_ETS[,i],
                                      method = "edf")
      CRPS_OLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_OLS_ETS[,i],
                                       method = "edf")
      CRPS_WLS_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_WLS_ETS[,i],
                                       method = "edf")
      CRPS_MinT.Shr_ETS[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Shr_ETS[,i],
                                            method = "edf")
      
      LS_UniV_Unrecon_ETS[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Base_ETS[h,i], 
                                         sd = sqrt(Shr.cov_ETS[i,i]),log = TRUE)
      
      LS_UniV_BU_ETS[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_BU_ETS[h,i], 
                                    sd = sqrt(Recon_Var.Cov_BU_ETS[i,i]),log = TRUE)
      
      LS_UniV_OLS_ETS[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_OLS_ETS[h,i], 
                                     sd = sqrt(Recon_Var.Cov_OLS_ETS[i,i]),log = TRUE)
      
      LS_UniV_WLS_ETS[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_WLS_ETS[h,i], 
                                     sd = sqrt(Recon_Var.Cov_WLS_ETS[i,i]),log = TRUE)
      
      LS_UniV_MinT.Shr_ETS[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_MinT.Shr_ETS[h,i], 
                                          sd = sqrt(Recon_Var.Cov_MinT.Shr_ETS[i,i]),log = TRUE)
      
    }
    
    
    DF_MultiV <- DF_MultiV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ETS$mean))[h]),
                                       "F-method" = "ETS",
                                       "Replication" = j,
                                       "Training window_length" = first_train_length + j)
    
    DF_UniV <- DF_UniV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ETS$mean))[h]),
                                   "F-method" = "ETS",
                                   "Replication" = j, 
                                   "Training window_length" = first_train_length + j)
    
    
  }
  
  DF_MultiV %>% filter(`F-method`=="ETS", `Replication`==j) %>%
    dplyr::select("Year, Qtr of forecast", "F-method", "Replication", "Training window_length") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_unrecon_ETS, 
        "Variogram score" = VS_full_Unrecon_ETS, "Log score" = c(NA)) -> DF_Base
  DF_Base[names(DF_MultiV)] -> DF_Base
  DF_MultiV <- rbind(DF_MultiV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_BU_ETS, 
        "Variogram score" = VS_full_BU_ETS, "Log score" = LS_bottom_BU_ETS) -> DF_BU
  DF_BU[names(DF_MultiV)] -> DF_BU
  DF_MultiV <- rbind(DF_MultiV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_OLS_ETS, 
        "Variogram score" = VS_full_OLS_ETS, "Log score" = LS_bottom_OLS_ETS) -> DF_OLS
  DF_OLS[names(DF_MultiV)] -> DF_OLS
  DF_MultiV <- rbind(DF_MultiV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_WLS_ETS, 
        "Variogram score" = VS_full_WLS_ETS, "Log score" = LS_bottom_WLS_ETS) -> DF_WLS
  DF_WLS[names(DF_MultiV)] -> DF_WLS
  DF_MultiV <- rbind(DF_MultiV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_MinT.Shr_ETS, 
        "Variogram score" = VS_full_MinT.Shr_ETS, "Log score" = LS_bottom_MinT.Shr_ETS) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV)] -> DF_MinT.Shr
  DF_MultiV <- rbind(DF_MultiV, DF_MinT.Shr)
  
  #Addinng CRPS and LS to the DF
  
  DF_UniV %>% filter(`F-method`=="ETS", `Replication`==j) %>% 
    dplyr::select("Year, Qtr of forecast", "F-method", "Replication", "Training window_length") -> Fltr
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  "R-method" = "Base", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_Unrecon_ETS)), 
        "LS" = c(t(LS_UniV_Unrecon_ETS)) ) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "Bottom up", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_BU_ETS)), 
        "LS" = c(t(LS_UniV_BU_ETS))) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "OLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_OLS_ETS)), 
        "LS" = c(t(LS_UniV_OLS_ETS))) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "WLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_WLS_ETS)), 
        "LS" = c(t(LS_UniV_WLS_ETS))) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_MinT.Shr_ETS)), 
        "LS" = c(t(LS_UniV_MinT.Shr_ETS))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  
  
  
  ##Reconciling densities from ARIMA models##
  
  # Getting different P matrices
  
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
  
  #Reconciled bottom level point forecasts (Followed from benchmark)#
  Recon_PointF_bottom_BU_ARIMA <- t(BU_P %*% t(Base_ARIMA))
  Recon_PointF_bottom_OLS_ARIMA <- t(OLS_P %*% t(Base_ARIMA))
  Recon_PointF_bottom_WLS_ARIMA <- t(WLS_P_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_bottom_MinT.Shr_ARIMA <- t(MinT.Shr_P_ARIMA %*% t(Base_ARIMA))
  
  #Reconciled bottom level variance forecasts (Followed from benchmark)#
  Recon_Var.Cov_bottom_BU_ARIMA <- BU_P %*% Shr.cov_ARIMA %*% t(BU_P)
  Recon_Var.Cov_bottom_OLS_ARIMA <- OLS_P %*% Shr.cov_ARIMA %*% t(OLS_P)
  Recon_Var.Cov_bottom_WLS_ARIMA <- WLS_P_ARIMA %*% Shr.cov_ARIMA %*% t(WLS_P_ARIMA)
  Recon_Var.Cov_bottom_MinT.Shr_ARIMA <- MinT.Shr_P_ARIMA %*% Shr.cov_ARIMA %*% t(MinT.Shr_P_ARIMA)
  
  
  #Reconciled point forecasts for the full hierarchy (Followed from benchmark)#
  Recon_PointF_BU_ARIMA <- t(S %*% BU_P %*% t(Base_ARIMA))
  Recon_PointF_OLS_ARIMA <- t(S %*% OLS_P %*% t(Base_ARIMA))
  Recon_PointF_WLS_ARIMA <- t(S %*% WLS_P_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_MinT.Shr_ARIMA <- t(S %*% MinT.Shr_P_ARIMA %*% t(Base_ARIMA))
  
  #Reconciled variance forecasts for the full hierarchy (Followed from benchmark)#
  Recon_Var.Cov_BU_ARIMA <- S %*% BU_P %*% Shr.cov_ARIMA %*% t(S %*% BU_P)
  Recon_Var.Cov_OLS_ARIMA <- S %*% OLS_P %*% Shr.cov_ARIMA %*% t(S %*% OLS_P)
  Recon_Var.Cov_WLS_ARIMA <- S %*% WLS_P_ARIMA %*% Shr.cov_ARIMA %*% t(S %*% WLS_P_ARIMA)
  Recon_Var.Cov_MinT.Shr_ARIMA <- S %*% MinT.Shr_P_ARIMA %*% Shr.cov_ARIMA %*% t(S %*% MinT.Shr_P_ARIMA)
  
  
  for (h in 1: min(H, nrow(Test))) {
    
    #Obtaining random samples from the possible forecast Gaussian densities of the full hierarchy
    #(Since the Guassian distribution for the full hierarchy is degenerate, we use the rnorm_degenerate
    # function to generate random samples)
    
    X_full_BU_ARIMA <- rnorm_degenerate(mu = Recon_PointF_BU_ARIMA[h,], 
                                        Sigma = Recon_Var.Cov_BU_ARIMA, k = k, n = n)
    
    X_full_OLS_ARIMA <- rnorm_degenerate(mu = Recon_PointF_OLS_ARIMA[h,], 
                                         Sigma = Recon_Var.Cov_OLS_ARIMA, k = k, n = n)
    
    X_full_WLS_ARIMA <- rnorm_degenerate(mu = Recon_PointF_WLS_ARIMA[h,], 
                                         Sigma = Recon_Var.Cov_WLS_ARIMA, k = k, n = n)
    
    X_full_MinT.Shr_ARIMA <- rnorm_degenerate(mu = Recon_PointF_MinT.Shr_ARIMA[h,], 
                                              Sigma = Recon_Var.Cov_MinT.Shr_ARIMA, k = k, n = n)
    
    X_full_unrecon_ARIMA <- mvrnorm(n = k, mu = Base_ARIMA[h,], Sigma = Shr.cov_ARIMA)
    
    ###Evaluating the reconciled forecasts from different forecasting methods
    #(only the bottom levels are evaluated)
    
    #Calculating Energy score for full predicive densities
    
    ES_full_BU_ARIMA[h] <- Energy_score(Data = X_full_BU_ARIMA, Real = Test[h,])
    ES_full_OLS_ARIMA[h] <- Energy_score(Data = X_full_OLS_ARIMA, Real = Test[h,])
    ES_full_WLS_ARIMA[h] <- Energy_score(Data = X_full_WLS_ARIMA, Real = Test[h,])
    ES_full_MinT.Shr_ARIMA[h] <- Energy_score(Data = X_full_MinT.Shr_ARIMA, Real = Test[h,])
    ES_full_unrecon_ARIMA[h] <- Energy_score(Data = X_full_unrecon_ARIMA, Real = Test[h,])
    
    #Calculating Log score for bottom level predictive densities
    
    LS_bottom_BU_ARIMA[h] <- Log_score_bottom(P = BU_P, Sigma.hat = Shr.cov_ARIMA, Base = Base_ARIMA[h,],
                                              real = Test[h,(n-m+1):n])
    
    LS_bottom_OLS_ARIMA[h] <- Log_score_bottom(P = OLS_P, Sigma.hat = Shr.cov_ARIMA, 
                                               Base = Base_ARIMA[h,], real = Test[h,(n-m+1):n])
    
    LS_bottom_WLS_ARIMA[h] <- Log_score_bottom(P = WLS_P_ARIMA, Sigma.hat = Shr.cov_ARIMA,
                                               Base = Base_ARIMA[h,], real = Test[h,(n-m+1):n])
    
    LS_bottom_MinT.Shr_ARIMA[h] <- Log_score_bottom(P = MinT.Shr_P_ARIMA, Sigma.hat = Shr.cov_ARIMA, 
                                                    Base = Base_ARIMA[h,], real = Test[h,(n-m+1):n])
    
    
    
    #Evaluating the density forecasts using Variogram based score
    
    VS_full_BU_ARIMA[h] <- Variogram_score(Data = X_full_BU_ARIMA, real = Test[h,])
    VS_full_OLS_ARIMA[h] <- Variogram_score(Data = X_full_OLS_ARIMA, real = Test[h,])
    VS_full_WLS_ARIMA[h] <- Variogram_score(Data = X_full_WLS_ARIMA, real = Test[h,])
    VS_full_MinT.Shr_ARIMA[h] <- Variogram_score(Data = X_full_MinT.Shr_ARIMA, real = Test[h,])
    VS_full_Unrecon_ARIMA[h] <- Variogram_score(Data = X_full_unrecon_ARIMA, real = Test[h,])
    
    
    #Calculating CRPS and LS for univariate predictive densities
    
    for (i in 1:n) {
      
      CRPS_Unrecon_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_unrecon_ARIMA[,i],
                                             method = "edf")
      CRPS_BU_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_BU_ARIMA[,i],
                                        method = "edf")
      CRPS_OLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_OLS_ARIMA[,i],
                                         method = "edf")
      CRPS_WLS_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_WLS_ARIMA[,i],
                                         method = "edf")
      CRPS_MinT.Shr_ARIMA[h,i] <- crps_sample(as.numeric(Test[h,i]), dat = X_full_MinT.Shr_ARIMA[,i],
                                              method = "edf")
      
      LS_UniV_Unrecon_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Base_ARIMA[h,i], 
                                           sd = sqrt(Shr.cov_ARIMA[i,i]),log = TRUE)
      
      LS_UniV_BU_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_BU_ARIMA[h,i], 
                                      sd = sqrt(Recon_Var.Cov_BU_ARIMA[i,i]),log = TRUE)
      
      LS_UniV_OLS_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_OLS_ARIMA[h,i], 
                                       sd = sqrt(Recon_Var.Cov_OLS_ARIMA[i,i]),log = TRUE)
      
      LS_UniV_WLS_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_WLS_ARIMA[h,i], 
                                       sd = sqrt(Recon_Var.Cov_WLS_ARIMA[i,i]),log = TRUE)
      
      LS_UniV_MinT.Shr_ARIMA[h,i] <- -dnorm(as.numeric(Test[h,i]), mean = Recon_PointF_MinT.Shr_ARIMA[h,i], 
                                            sd = sqrt(Recon_Var.Cov_MinT.Shr_ARIMA[i,i]),log = TRUE)
      
    }
    
    
    DF_MultiV <- DF_MultiV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ARIMA$mean))[h]),
                                       "F-method" = "ARIMA",
                                       "Replication" = j, 
                                       "Training window_length" = first_train_length + j)
    
    DF_UniV <- DF_UniV %>% add_row("Year, Qtr of forecast" = paste(as.yearqtr(time(Forecast_ARIMA$mean))[h]),
                                   "F-method" = "ARIMA",
                                   "Replication" = j, 
                                   "Training window_length" = first_train_length + j)
    
  }
  
  DF_MultiV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>%
    dplyr::select("Year, Qtr of forecast", "F-method", "Replication", "Training window_length") -> Fltr
  
  cbind(Fltr, "R-method" = "Base", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_unrecon_ARIMA, 
        "Variogram score" = VS_full_Unrecon_ARIMA, "Log score" = c(NA)) -> DF_Base
  DF_Base[names(DF_MultiV)] -> DF_Base
  DF_MultiV <- rbind(DF_MultiV, DF_Base)
  
  cbind(Fltr, "R-method" = "Bottom up", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_BU_ARIMA, 
        "Variogram score" = VS_full_BU_ARIMA, "Log score" = LS_bottom_BU_ARIMA) -> DF_BU
  DF_BU[names(DF_MultiV)] -> DF_BU
  DF_MultiV <- rbind(DF_MultiV, DF_BU)
  
  cbind(Fltr, "R-method" = "OLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_OLS_ARIMA, 
        "Variogram score" = VS_full_OLS_ARIMA, "Log score" = LS_bottom_OLS_ARIMA) -> DF_OLS
  DF_OLS[names(DF_MultiV)] -> DF_OLS
  DF_MultiV <- rbind(DF_MultiV, DF_OLS)
  
  cbind(Fltr, "R-method" = "WLS", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_WLS_ARIMA, 
        "Variogram score" = VS_full_WLS_ARIMA, "Log score" = LS_bottom_WLS_ARIMA) -> DF_WLS
  DF_WLS[names(DF_MultiV)] -> DF_WLS
  DF_MultiV <- rbind(DF_MultiV, DF_WLS)
  
  cbind(Fltr, "R-method" = "MinT.Shr", "Forecast Horizon" = c(1: min(H, nrow(Test))), "Energy score" = ES_full_MinT.Shr_ARIMA, 
        "Variogram score" = VS_full_MinT.Shr_ARIMA, "Log score" = LS_bottom_MinT.Shr_ARIMA) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_MultiV)] -> DF_MinT.Shr
  DF_MultiV <- rbind(DF_MultiV, DF_MinT.Shr)
  
  
  #Addinng CRPS and LS to the DF
  
  DF_UniV %>% filter(`F-method`=="ARIMA", `Replication`==j) %>% 
    dplyr::select("Year, Qtr of forecast", "F-method", "Replication", "Training window_length") -> Fltr
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))),  "R-method" = "Base", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_Unrecon_ARIMA)), 
        "LS" = c(t(LS_UniV_Unrecon_ARIMA)) ) -> DF_Base
  DF_Base[names(DF_UniV)] -> DF_Base
  DF_UniV <- rbind(DF_UniV, DF_Base)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "Bottom up", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_BU_ARIMA)), 
        "LS" = c(t(LS_UniV_BU_ARIMA))) -> DF_BU
  DF_BU[names(DF_UniV)] -> DF_BU
  DF_UniV <- rbind(DF_UniV, DF_BU)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "OLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_OLS_ARIMA)), 
        "LS" = c(t(LS_UniV_OLS_ARIMA))) -> DF_OLS
  DF_OLS[names(DF_UniV)] -> DF_OLS
  DF_UniV <- rbind(DF_UniV, DF_OLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "WLS", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_WLS_ARIMA)), 
        "LS" = c(t(LS_UniV_WLS_ARIMA))) -> DF_WLS
  DF_WLS[names(DF_UniV)] -> DF_WLS
  DF_UniV <- rbind(DF_UniV, DF_WLS)
  
  cbind(Fltr, "Series" = names(Exp), "Actual" = c(t(as.matrix(Test[1:min(H, nrow(Test)),]))), "R-method" = "MinT Shrink", 
        "Forecast Horizon" = rep(1:min(H, nrow(Test)), each = 16), "CRPS" = c(t(CRPS_MinT.Shr_ARIMA)), 
        "LS" = c(t(LS_UniV_MinT.Shr_ARIMA))) -> DF_MinT.Shr
  DF_MinT.Shr[names(DF_UniV)] -> DF_MinT.Shr
  DF_UniV <- rbind(DF_UniV, DF_MinT.Shr)
  
  
  
  
}

End <- Sys.time()


#Evaluation

DF_MultiV[complete.cases(DF_MultiV[ , "R-method"]),] -> DF_MultiV

DF_MultiV %>% dplyr::select(-"Year, Qtr of forecast", -"Replication") -> DF_MultScores

DF_MultScores %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
  summarise(E.ES = mean(`Energy score`), 
            E.VS = mean(`Variogram score`), 
            E.LS = mean(`Log score`, na.rm=T)) -> DF_MultScores

#DF_Scores %>% dplyr::filter(`R-method` != "Base") -> DF_Score_Recon

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
                             SS_E.VS = round((1-(`E.VS`/Base_E.VS_ETS))*100, digits = 4)) %>%
  dplyr::select(-`E.LS`)-> DF_MultScore_SS_ETS

DF_MultScore_SS_ETS %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_ETS

DF_MultScore_SS_ETS %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_ETS

View(SS_E.ES_ETS)
View(SS_E.VS_ETS)

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
                               SS_E.VS = round((1-(`E.VS`/Base_E.VS_ARIMA))*100, digits = 4))%>%
  dplyr::select(-`E.LS`) -> DF_MultScore_SS_ARIMA

DF_MultScore_SS_ARIMA %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.VS`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.ES`) -> SS_E.ES_ARIMA

DF_MultScore_SS_ARIMA %>%  dplyr::select(-`E.ES`, -`E.VS`, -`SS_E.ES`) %>%
  spread(key = `Forecast Horizon`, value = `SS_E.VS`) -> SS_E.VS_ARIMA

# View(SS_E.ES_ARIMA)
# View(SS_E.VS_ARIMA)

#Using Log score

DF_MultScores_ETS %>% dplyr::select(-`E.ES`, -`E.VS`) %>% filter(`R-method`!= "Base") -> DF_ETS_LS

DF_MultScores_ARIMA %>% dplyr::select(-`E.ES`, -`E.VS`) %>% filter(`R-method`!= "Base") -> DF_ARIMA_LS

DF_ETS_LS %>% filter(`R-method` == "Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% as_vector() -> BU_LS_ETS

DF_ARIMA_LS %>% filter(`R-method` == "Bottom up") %>%
  slice() %>%
  ungroup() %>%
  dplyr::select(`E.LS`) %>% as_vector() -> BU_LS_ARIMA

DF_ETS_LS %>% mutate(SS.LS = round((1-(`E.LS`/BU_LS_ETS))*100, digits = 4)) %>%
  dplyr::select(-`E.LS`) -> DF_MultScore_SS_ETS_LS

DF_ARIMA_LS %>% mutate(SS.LS = round((1-(`E.LS`/BU_LS_ARIMA))*100, digits = 4))%>%
  dplyr::select(-`E.LS`) -> DF_MultScore_SS_ARIMA_LS

DF_MultScore_SS_ETS_LS %>% spread(key = `Forecast Horizon`, value = `SS.LS`) -> DF_MultScore_SS_ETS_LS

DF_MultScore_SS_ARIMA_LS %>% spread(key = `Forecast Horizon`, value = `SS.LS`) -> DF_MultScore_SS_ARIMA_LS

# View(DF_MultScore_SS_ETS_LS)
# View(DF_MultScore_SS_ARIMA_LS)

##Summary of CRPS and LS

DF_UniV[complete.cases(DF_UniV[ , "R-method"]),] -> DF_UniV

Score_arima <- tibble("Series" = character(),
                      "F-method" = character(),
                      "R-method" = character(),
                      "Forecast Horizon" = integer(),
                      "MCRPS" = numeric(), 
                      "MLS" = numeric())


Score_ets <- tibble("Series" = character(),
                    "F-method" = character(),
                    "R-method" = character(),
                    "Forecast Horizon" = integer(),
                    "MCRPS" = numeric(), 
                    "MLS" = numeric())

for (i in 1:n) {
  
  DF_UniV %>% filter(`Series`==names(Exp)[i]) %>% dplyr::select("F-method", "R-method", "Forecast Horizon", 
                                                                "CRPS", "LS") -> DF_Score
  
  DF_Score %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
    summarise(MCRPS = mean(`CRPS`), MLS = mean(`LS`)) -> DF_Score
  
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

save.image("GDP-ProbForecasting-EXP-GaussianMethod-ExpandW.RData")