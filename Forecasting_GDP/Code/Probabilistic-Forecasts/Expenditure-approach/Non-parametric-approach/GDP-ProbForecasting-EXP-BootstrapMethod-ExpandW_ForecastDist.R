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

Exp <- read.csv("GDP-Exp.csv")[,-1] %>% as_tibble() 

####Summing matrix####
S <- read.csv("S_mat.csv")[,-1] %>% as.matrix()

# Lists to save future paths
Unrecon_FP_ETS_h1 <- list()
Unrecon_FP_ARIMA_h1 <- list()

Recon_BU_FP_ETS_h1 <- list()
Recon_BU_FP_ARIMA_h1 <- list()

Recon_OLS_FP_ETS_h1 <- list()
Recon_OLS_FP_ARIMA_h1 <- list()

Recon_WLS_FP_ETS_h1 <- list()
Recon_WLS_FP_ARIMA_h1 <- list()

Recon_MinT_FP_ETS_h1 <- list()
Recon_MinT_FP_ARIMA_h1 <- list()

set.seed(1989)

m <- 53        #Number of most disaggregate series
n <- ncol(Exp) #Total number of series in the hieararchy
B <- 5000      #Number of random samples generated from the forecast distribution for the evaluation
H <- 4         #Number of forecast horizons 
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




#Following function will return the k^th future path for H forecast horizons for all n 
#series. In the returning matrix, rows represents forecast horizons
#columns represents the series
FP_func <- function(k, fit, Resid, Index, Index_seq, H, n) { 
  
  fit_eval <- fit
  ResidModel_all <- Resid
  Index_eval <- Index
  Index_seq <- Index_seq
  H <- H
  n <- n
  
  Innov <- as.list(as.data.frame(ResidModel_all[Index_seq[k,],]))
  
  return(mapply(simulate, fit_eval, future = TRUE, nsim = H, innov = Innov))
  
}




start_train <- c(1984, 4)
end_first_train <- c(1994, 2)
max_train=c(2017,4) #End of largest training set

first_train_length <- Exp %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(end=c(1994, 2)) %>% length() #Length of first training set

test_length <- Exp %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(start=end_first_train+c(0,1), end=max_train) %>% length #Maximum time the window expands


Start <- Sys.time()

for (j in 2:test_length) { #test_length
  
  #Subsetting training and testing sets
  
  Train <- Exp[1:(first_train_length + j),]
  Test <- Exp[-(1:(first_train_length + j)),]
  
  #Year_Qtr_of_forecast <- numeric(min(H, nrow(Test)))
  
  #Note that for ETS models with multiplicative errors, the model residuals are different 
  #from the insample forecast errors. To get simualted bootstrap future paths we use model residuals
  #and to calculate MinT we use forecast errors.
  
  #List to store model fit for each series - use later to simualte future paths
  fit_ARIMA <- list(n)
  fit_ETS <- list(n)
  fit_Benchmark <- list(n)
  
  #Matrix to store model residuals
  ModelResid_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  ModelResid_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  ModelResid_all_Benchmark <- matrix(NA, nrow = nrow(Train), ncol = n) #Benchmark method: seasonal RW with a drift
  
  #Matrix to store model insample forecast errors.
  ForeError_all_ARIMA <- matrix(NA, nrow = nrow(Train), ncol = n)
  ForeError_all_ETS <- matrix(NA, nrow = nrow(Train), ncol = n)
  ForeError_all_Benchmark <- matrix(NA, nrow = nrow(Train), ncol = n) #Benchmark method: seasonal RW with a drift
  
  # #Matrix to store base forecasts
  # Base_ARIMA <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  # Base_ETS <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  # Base_Benchmark <- matrix(NA, nrow = min(H, nrow(Test)), ncol = n)
  
  
  for(i in 1:n) {
    
    TS <- ts(Train[,i], frequency = 4, start = start_train)
    

    #Forecsting with ETS
    
    fit_ETS[[i]] <- ets(TS)
    Forecast_ETS <- forecast(fit_ETS[[i]], h = min(H, nrow(Test)))
    # Base_ETS[,i] <- Forecast_ETS$mean
    ModelResid_all_ETS[,i] <- residuals(fit_ETS[[i]])
    ForeError_all_ETS[,i] <- as.vector(TS - fitted(fit_ETS[[i]]))
    
    
    #Forecsting with ARIMA
    fit_ARIMA[[i]] <- auto.arima(TS)
    Forecast_ARIMA <- forecast(fit_ARIMA[[i]], h = min(H, nrow(Test[,i])))
    # Base_ARIMA[,i] <- Forecast_ARIMA$mean
    ModelResid_all_ARIMA[,i] <- residuals(fit_ARIMA[[i]])
    ForeError_all_ARIMA[,i] <- as.vector(TS - fitted(fit_ARIMA[[i]]))
    
    
  }
  
  
  
  ## Getting incoherent sample paths ##
  
  #List of lenght H to store future paths. Each element of this list corresponds to a matrix
  #that holds bootstrap future paths at forecast horizon H.
  Unrecon_future_paths_ETS <- list(min(H, nrow(Test)))
  Unrecon_future_paths_ARIMA <- list(min(H, nrow(Test)))
  
  #Index to get the block of bootsrap samples
  Index <- base::sample(c(1:(nrow(ModelResid_all_ETS)-(H-1))), size = B , replace = TRUE)
  Index_seq <- matrix(0, B, H)
  
  for (k in 1:B) {
    
    Index_seq[k,] <- seq(from = Index[k], to = (Index[k]+H-1), by = 1)
    
  }
  
  #Simulating future paths
  Start_FP <- Sys.time()
  

  fp_ETS <-  lapply(c(1:B), FP_func, fit = fit_ETS, 
                    Resid = ModelResid_all_ETS, Index = Index, 
                    Index_seq = Index_seq, H=H, n=n)
  
  fp_ARIMA <-  lapply(c(1:B), FP_func, fit = fit_ARIMA, 
                      Resid = ModelResid_all_ARIMA, Index = Index, 
                      Index_seq = Index_seq, H=H, n=n)
  
  End_FP <- Sys.time()
  
  for (h in 1:min(H, nrow(Test))) {
    
    Unrecon_future_paths_ETS[[h]] <- plyr::laply(fp_ETS, function(y) y[h,])
    Unrecon_future_paths_ARIMA[[h]] <- plyr::laply(fp_ARIMA, function(y) y[h,])
    
  }
  
  
  
  

  
  ##Reconciliation of future paths obtained from ETS method
  
  #Bottom up 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  
  #MinT shrink G
  targ <- lowerD(ForeError_all_ETS)
  shrink <- shrink.estim(ForeError_all_ETS,targ)
  Shr.cov_ETS <- shrink[[1]]
  Inv_Shr.cov_ETS <- solve(Shr.cov_ETS)
  
  MinT.Shr_G_ETS <- solve(t(S) %*% Inv_Shr.cov_ETS %*% S) %*% t(S) %*% Inv_Shr.cov_ETS
  
  #WLS G
  Cov_WLS_ETS <- diag(diag(Shr.cov_ETS), n, n)
  Inv_WLS_ETS <- solve(Cov_WLS_ETS)
  
  WLS_G_ETS <- solve(t(S) %*% Inv_WLS_ETS %*% S) %*% t(S) %*% Inv_WLS_ETS
  
  
  
  ###Reconciliation of base forecasts from ETS###
  
  Reconciled_future_paths_BU_ETS <- list()
  Reconciled_future_paths_OLS_ETS <- list()
  Reconciled_future_paths_WLS_ETS <- list()
  # Reconciled_future_paths_MinT.Sam_ETS <- list()
  Reconciled_future_paths_MinT.Shr_ETS <- list()
  
 
  
  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ETS[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_OLS_ETS[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_WLS_ETS[[h]] <- t(S %*% WLS_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    # Reconciled_future_paths_MinT.Sam_ETS[[h]] <- t(S %*% MinT.Sam_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_MinT.Shr_ETS[[h]] <- t(S %*% MinT.Shr_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    

  }
  
 
  
  ##Reconciliation of future paths obtained from ARIMA method
  
  #MinT shrink G
  targ <- lowerD(ForeError_all_ARIMA)
  shrink <- shrink.estim(ForeError_all_ARIMA,targ)
  Shr.cov_ARIMA <- shrink[[1]]
  Inv_Shr.cov_ARIMA <- solve(Shr.cov_ARIMA)
  
  MinT.Shr_G_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
  
  # #MinT Sample G
  # n1 <- nrow(ForeError_all_ARIMA)
  # Sam.cov_ARIMA <- crossprod(ForeError_all_ARIMA)/n1
  # Inv_Sam.cov_ARIMA <- solve(Sam.cov_ARIMA)
  # 
  # MinT.Sam_G_ARIMA <- solve(t(S) %*% Inv_Sam.cov_ARIMA %*% S) %*% t(S) %*% Inv_Sam.cov_ARIMA
  
  #WLS G
  Cov_WLS_ARIMA <- diag(diag(Shr.cov_ARIMA), n, n)
  Inv_WLS_ARIMA <- solve(Cov_WLS_ARIMA)
  
  WLS_G_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
  
  ###Reconciliation of base forecasts from ARIMA###
  
  Reconciled_future_paths_BU_ARIMA <- list()
  Reconciled_future_paths_OLS_ARIMA <- list()
  Reconciled_future_paths_WLS_ARIMA <- list()
  # Reconciled_future_paths_MinT.Sam_ARIMA <- list()
  Reconciled_future_paths_MinT.Shr_ARIMA <- list()
  

  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ARIMA[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_OLS_ARIMA[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_WLS_ARIMA[[h]] <- t(S %*% WLS_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    # Reconciled_future_paths_MinT.Sam_ARIMA[[h]] <- t(S %*% MinT.Sam_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_MinT.Shr_ARIMA[[h]] <- t(S %*% MinT.Shr_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    

  }
  
  # Saving forecast distributions into the lists
  
  Unrecon_FP_ETS_h1[[j]] <- Unrecon_future_paths_ETS[[1]]
  Unrecon_FP_ARIMA_h1[[j]] <- Unrecon_future_paths_ARIMA[[1]]
  
  Recon_BU_FP_ETS_h1[[j]] <- Reconciled_future_paths_BU_ETS[[1]]
  Recon_BU_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_BU_ARIMA[[1]]
  
  Recon_OLS_FP_ETS_h1[[j]] <- Reconciled_future_paths_OLS_ETS[[1]]
  Recon_OLS_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_OLS_ARIMA[[1]]
  
  Recon_WLS_FP_ETS_h1[[j]] <- Reconciled_future_paths_WLS_ETS[[1]]
  Recon_WLS_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_WLS_ARIMA[[1]]
  
  Recon_MinT_FP_ETS_h1[[j]] <- Reconciled_future_paths_MinT.Shr_ETS[[1]]
  Recon_MinT_FP_ARIMA_h1[[j]] <- Reconciled_future_paths_MinT.Shr_ARIMA[[1]]
  
  
  
}

End <- Sys.time()


save.image("Exp-ProbForecasts-BootstrapApproach-ExpandingW_FDist.RData")

