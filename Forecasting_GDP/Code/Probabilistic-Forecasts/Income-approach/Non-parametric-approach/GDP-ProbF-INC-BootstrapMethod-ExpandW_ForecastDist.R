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

#Importing data#
####Income Approach - Current Prices####

MDI <- read_excel("Master Data File.xlsx", sheet=5, skip = 9) #Master Data for Income

#a_t = Gdpi, Tfi, TfiGos, TfiCoe, TfiGosCop, TfiGosCopNfn,
#b_t = TfiGosCopNfnPub, TfiGosCopNfnPvt, TfiGosCopFin,TfiGosGvt,TfiGosDwl,TfiGmi,TfiCoeWns,TfiCoeEsc,Tsi,Sdi


Inc <- tibble(Gdpi = MDI %>% pull("A2302467A") %>% ts(start=c(1959,3), frequency=4) %>%
                window(start=c(1984,4))) #GDP(I)


#Total factor income (GOS + GMI)
Inc %>% add_column(Tfi = MDI %>% pull("A2302411R") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Total factor income

#All sectors GOS (Corporations + Gen. Govn. + Dwellings)
Inc %>% add_column(TfiGos = MDI %>% pull("A2302409C") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #All sectors ;  Gross operating surplus

#Compensation of employees (Wages and salaries + Social contribution)
Inc %>% add_column(TfiCoe = MDI %>% pull("A2302401K") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Compensation of employees

#Total corporations (Non-financila(Pub + Pvt) + Financial)
Inc %>% add_column(TfiGosCop = MDI %>% pull("A2302406W") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Total corporations ;  Gross operating surplus

#Non-financial corporations (Public + Private)
Inc %>% add_column(TfiGosCopNfn = MDI %>% pull("A2302404T") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Non-financial corporations ;  Gross operating surplus

#Public non-financial corporations
Inc %>% add_column(TfiGosCopNfnPub = MDI %>% pull("A2302403R") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Public non-financial corporations ;  Gross operating surplus

#Private non-financial corporations
Inc %>% add_column(TfiGosCopNfnPvt = MDI %>% pull("A2323369L") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Private non-financial corporations ;  Gross operating surplus

#Financial corporations
Inc %>% add_column(TfiGosCopFin = MDI %>% pull("A2302405V") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Financial corporations ;  Gross operating surplus


#General government
Inc %>% add_column(TfiGosGvt = MDI %>% pull("A2298711F") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #General government ;  Gross operating surplus

#Dwellings
Inc %>% add_column(TfiGosDwl = MDI %>% pull("A2302408A") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Dwellings owned by persons ;  Gross operating surplus

#Gross mixed income
Inc %>% add_column(TfiGmi = MDI %>% pull("A2302410L") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Gross mixed income

#Compensation of employees - Wages and salaries
Inc %>% add_column(TfiCoeWns = MDI %>% pull("A2302399K") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc 

#Compensation of employees - Employers' social contributions
Inc %>% add_column(TfiCoeEsc = MDI %>% pull("A2302400J") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Compensation of employees - Employers' social contributions

#Taxes less subsidies (I)
Inc %>% add_column(Tsi = MDI %>% pull("A2302412T") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc 

#Statistical Discrepancy (I)
Inc %>% add_column(Sdi = MDI %>% pull("A2302413V") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc 

set.seed(1989)

m <- 10 #Number of most disaggregate series
n <- ncol(Inc) #Total number of series in the hieararchy
B <- 5000 #Number of random samples generated from the forecast distribution for the evaluation

####Summing matrix####
S <- matrix(c(1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,0,0,
              1,1,1,1,1,0,0,0,0,0,
              0,0,0,0,0,0,1,1,0,0,
              1,1,1,0,0,0,0,0,0,0,
              1,1,0,0,0,0,0,0,0,0,
              diag(m)), nrow=m) %>% t

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

first_train_length <- Inc %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(end=c(1994, 2)) %>% length() #Length of first training set

test_length <- Inc %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(start=end_first_train+c(0,1), end=max_train) %>% length #Maximum time the window expands


H <- 4

Start <- Sys.time()
for (j in 2:test_length) { #test_length
  
  #Subsetting training and testing sets
  
  Train <- Inc[1:(first_train_length + j),]
  Test <- Inc[-(1:(first_train_length + j)),]
  
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
    
    #Forecsting with benchmark
    fit_Benchmark[[i]] <- Arima(TS, order=c(0,0,0), seasonal=c(0,1,0), include.drift=TRUE)
    Forecast_Benchmark <- forecast(fit_Benchmark[[i]], h = min(H, nrow(Test)))
    # Base_Benchmark[,i] <- Forecast_Benchmark$mean
    ModelResid_all_Benchmark[,i] <- residuals(fit_Benchmark[[i]])
    ForeError_all_Benchmark[,i] <- as.vector(TS - fitted(fit_Benchmark[[i]]))
    
    
    ##Forecsting with ETS##
    
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
  Unrecon_future_paths_bench <- list(min(H, nrow(Test)))
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
  
  fp_Bench <-  lapply(c(1:B), FP_func, fit = fit_Benchmark, 
                    Resid = ModelResid_all_Benchmark, Index = Index, 
                    Index_seq = Index_seq, H=H, n=n)
  
  fp_ETS <-  lapply(c(1:B), FP_func, fit = fit_ETS, 
                    Resid = ModelResid_all_ETS, Index = Index, 
                    Index_seq = Index_seq, H=H, n=n)
  
  fp_ARIMA <-  lapply(c(1:B), FP_func, fit = fit_ARIMA, 
                    Resid = ModelResid_all_ARIMA, Index = Index, 
                    Index_seq = Index_seq, H=H, n=n)
  
  End_FP <- Sys.time()
  
  for (h in 1:min(H, nrow(Test))) {
    
    Unrecon_future_paths_bench[[h]] <- plyr::laply(fp_Bench, function(y) y[h,])
    Unrecon_future_paths_ETS[[h]] <- plyr::laply(fp_ETS, function(y) y[h,])
    Unrecon_future_paths_ARIMA[[h]] <- plyr::laply(fp_ARIMA, function(y) y[h,])
  
  }
  
  
  
 
  ##Reconciliation of future paths obtained from ETS method
  
  #Bottom up 
  
  Null.ma <- matrix(0,m,(n-m))
  BU_G <- cbind(Null.ma, diag(1,m,m))
  
  #OLS G
  OLS_G <- solve(t(S) %*% S) %*% t(S)
  
  
  #MinT Sample G
  n1 <- nrow(ForeError_all_ETS)
  Sam.cov_ETS <- crossprod(ForeError_all_ETS)/n1
  Inv_Sam.cov_ETS <- solve(Sam.cov_ETS)
  
  MinT.Sam_G_ETS <- solve(t(S) %*% Inv_Sam.cov_ETS %*% S) %*% t(S) %*% Inv_Sam.cov_ETS
  
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
  Reconciled_future_paths_MinT.Sam_ETS <- list()
  Reconciled_future_paths_MinT.Shr_ETS <- list()
  

  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ETS[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_OLS_ETS[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_WLS_ETS[[h]] <- t(S %*% WLS_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_MinT.Sam_ETS[[h]] <- t(S %*% MinT.Sam_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    Reconciled_future_paths_MinT.Shr_ETS[[h]] <- t(S %*% MinT.Shr_G_ETS %*% t(Unrecon_future_paths_ETS[[h]]))
    

  }
  

  
  ##Reconciliation of future paths obtained from ARIMA method
  
  #MinT shrink G
  targ <- lowerD(ForeError_all_ARIMA)
  shrink <- shrink.estim(ForeError_all_ARIMA,targ)
  Shr.cov_ARIMA <- shrink[[1]]
  Inv_Shr.cov_ARIMA <- solve(Shr.cov_ARIMA)
  
  MinT.Shr_G_ARIMA <- solve(t(S) %*% Inv_Shr.cov_ARIMA %*% S) %*% t(S) %*% Inv_Shr.cov_ARIMA
  
  #MinT Sample G
  n1 <- nrow(ForeError_all_ARIMA)
  Sam.cov_ARIMA <- crossprod(ForeError_all_ARIMA)/n1
  Inv_Sam.cov_ARIMA <- solve(Sam.cov_ARIMA)
  
  MinT.Sam_G_ARIMA <- solve(t(S) %*% Inv_Sam.cov_ARIMA %*% S) %*% t(S) %*% Inv_Sam.cov_ARIMA
  
  #WLS G
  Cov_WLS_ARIMA <- diag(diag(Shr.cov_ARIMA), n, n)
  Inv_WLS_ARIMA <- solve(Cov_WLS_ARIMA)
  
  WLS_G_ARIMA <- solve(t(S) %*% Inv_WLS_ARIMA %*% S) %*% t(S) %*% Inv_WLS_ARIMA
  
  ###Reconciliation of base forecasts from ARIMA###
  
  Reconciled_future_paths_BU_ARIMA <- list()
  Reconciled_future_paths_OLS_ARIMA <- list()
  Reconciled_future_paths_WLS_ARIMA <- list()
  Reconciled_future_paths_MinT.Sam_ARIMA <- list()
  Reconciled_future_paths_MinT.Shr_ARIMA <- list()


  for (h in 1: min(H, nrow(Test))) {
    
    Reconciled_future_paths_BU_ARIMA[[h]] <- t(S %*% BU_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_OLS_ARIMA[[h]] <- t(S %*% OLS_G %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_WLS_ARIMA[[h]] <- t(S %*% WLS_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
    Reconciled_future_paths_MinT.Sam_ARIMA[[h]] <- t(S %*% MinT.Sam_G_ARIMA %*% t(Unrecon_future_paths_ARIMA[[h]]))
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



save.image("GDP-ProbForecasting-Inc-BootstrapMethod-ExpandW_Results_FDistributions.RData")

