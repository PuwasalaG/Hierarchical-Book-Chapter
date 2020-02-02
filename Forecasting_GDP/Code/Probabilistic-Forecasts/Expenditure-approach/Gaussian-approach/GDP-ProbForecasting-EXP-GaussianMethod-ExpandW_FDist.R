# This script generate reconciled forecast distributions and saved the values in 
# lists. These can be used to visualise the predictive distributions before and
# after the reconciliation

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

# Lists to save means and covariances of Gaussian Fdists

Unrecon_mean_ETS <- list()
Unrecon_mean_ARIMA <- list()

Recon_BU_mean_ETS <- list()
Recon_BU_mean_ARIMA <- list()

Recon_OLS_mean_ETS <- list()
Recon_OLS_mean_ARIMA <- list()

Recon_WLS_mean_ETS <- list()
Recon_WLS_mean_ARIMA <- list()

Recon_MinT_mean_ETS <- list()
Recon_MinT_mean_ARIMA <- list()

Unrecon_Cov_ETS <- list()
Unrecon_Cov_ARIMA <- list()

Recon_BU_Cov_ETS <- list()
Recon_BU_Cov_ARIMA <- list()

Recon_OLS_Cov_ETS <- list()
Recon_OLS_Cov_ARIMA <- list()

Recon_WLS_Cov_ETS <- list()
Recon_WLS_Cov_ARIMA <- list()

Recon_MinT_Cov_ETS <- list()
Recon_MinT_Cov_ARIMA <- list()


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

set.seed(1989)

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
  

  #Model fitting, base forecasts, and residuals  
  for(i in 1:n) {
    
    TS <- ts(Train[,i], frequency = 4, start = start_train)
    

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
  

  ##Reconciling densities from ETS models##
  
  #Getting P matrices
  
  #Bottom up P
  Null.ma <- matrix(0,m,(n-m))
  BU_P <- cbind(Null.ma, diag(1,m,m))
  
  #OLS P
  OLS_P <- solve(t(S) %*% S) %*% t(S)
  
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
  
  
  Unrecon_mean_ETS[[j]] <- Base_ETS
  Unrecon_mean_ARIMA[[j]] <- Base_ARIMA
  
  Recon_BU_mean_ETS[[j]] <- Recon_PointF_BU_ETS
  Recon_BU_mean_ARIMA[[j]] <- Recon_PointF_BU_ARIMA
  
  Recon_OLS_mean_ETS[[j]] <- Recon_PointF_OLS_ETS
  Recon_OLS_mean_ARIMA[[j]] <- Recon_PointF_OLS_ARIMA
  
  Recon_WLS_mean_ETS[[j]] <- Recon_PointF_WLS_ETS
  Recon_WLS_mean_ARIMA[[j]] <- Recon_PointF_WLS_ARIMA
  
  Recon_MinT_mean_ETS[[j]] <- Recon_PointF_MinT.Shr_ETS
  Recon_MinT_mean_ARIMA[[j]] <- Recon_PointF_MinT.Shr_ARIMA
  
  Unrecon_Cov_ETS[[j]] <- Shr.cov_ETS
  Unrecon_Cov_ARIMA[[j]] <- Shr.cov_ARIMA
  
  Recon_BU_Cov_ETS[[j]] <- Recon_Var.Cov_BU_ETS
  Recon_BU_Cov_ARIMA[[j]] <- Recon_Var.Cov_BU_ARIMA
  
  Recon_OLS_Cov_ETS[[j]] <- Recon_Var.Cov_OLS_ETS
  Recon_OLS_Cov_ARIMA[[j]] <- Recon_Var.Cov_OLS_ARIMA
  
  Recon_WLS_Cov_ETS[[j]] <- Recon_Var.Cov_WLS_ETS
  Recon_WLS_Cov_ARIMA[[j]] <- Recon_Var.Cov_WLS_ARIMA
  
  Recon_MinT_Cov_ETS[[j]] <- Recon_Var.Cov_MinT.Shr_ETS
  Recon_MinT_Cov_ARIMA[[j]] <- Recon_Var.Cov_MinT.Shr_ARIMA
  
  
}

End <- Sys.time()



save.image("GDP-ProbForecasting-EXP-GaussianMethod-ExpandW_FDist.RData")