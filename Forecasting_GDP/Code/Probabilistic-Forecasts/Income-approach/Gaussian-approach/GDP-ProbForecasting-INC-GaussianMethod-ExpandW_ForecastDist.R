
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
k <- 5000 #Number of random samples generated from the forecast distribution for the evaluation

####Summing matrix####
S <- matrix(c(1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,0,0,
              1,1,1,1,1,0,0,0,0,0,
              0,0,0,0,0,0,1,1,0,0,
              1,1,1,0,0,0,0,0,0,0,
              1,1,0,0,0,0,0,0,0,0,
              diag(m)), nrow=m) %>% t


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

start_train <- c(1984, 4)
end_first_train <- c(1994, 2)
max_train=c(2017,4) #End of largest training set

first_train_length <- Inc %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(end=c(1994, 2)) %>% length() #Length of first training set

test_length <- Inc %>% pull(.,1) %>% ts(start = c(1984, 4), frequency = 4) %>% window(start=end_first_train+c(0,1), end=max_train) %>% length #Maximum time the window expands


H <- 4

Start <- Sys.time()
for (j in 1:test_length) { #test_length
  
  #Subsetting training and testing sets
  Train <- Inc[1:(first_train_length + j),]
  Test <- Inc[-(1:(first_train_length + j)),]
  
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
  
  #MinT Sample P
  n1 <- nrow(Residuals_all_ETS)
  Sam.cov_ETS <- crossprod(Residuals_all_ETS)/n1
  Inv_Sam.cov_ETS <- solve(Sam.cov_ETS)
  
  MinT.Sam_P_ETS <- solve(t(S) %*% Inv_Sam.cov_ETS %*% S) %*% t(S) %*% Inv_Sam.cov_ETS
  
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
  Recon_PointF_bottom_MinT.Sam_ETS <- t(MinT.Sam_P_ETS %*% t(Base_ETS))
  Recon_PointF_bottom_MinT.Shr_ETS <- t(MinT.Shr_P_ETS %*% t(Base_ETS))
  
  #Reconciled bottom level variance forecasts (Followed from ETS)#
  Recon_Var.Cov_bottom_BU_ETS <- BU_P %*% Shr.cov_ETS %*% t(BU_P)
  Recon_Var.Cov_bottom_OLS_ETS <- OLS_P %*% Shr.cov_ETS %*% t(OLS_P)
  Recon_Var.Cov_bottom_WLS_ETS <- WLS_P_ETS %*% Shr.cov_ETS %*% t(WLS_P_ETS)
  Recon_Var.Cov_bottom_MinT.Sam_ETS <- MinT.Sam_P_ETS %*% Sam.cov_ETS %*% t(MinT.Sam_P_ETS)
  Recon_Var.Cov_bottom_MinT.Shr_ETS <- MinT.Shr_P_ETS %*% Shr.cov_ETS %*% t(MinT.Shr_P_ETS)
  
  
  #Reconciled point forecasts for the full hierarchy (Followed from ETS)#
  Recon_PointF_BU_ETS <- t(S %*% BU_P %*% t(Base_ETS))
  Recon_PointF_OLS_ETS <- t(S %*% OLS_P %*% t(Base_ETS))
  Recon_PointF_WLS_ETS <- t(S %*% WLS_P_ETS %*% t(Base_ETS))
  Recon_PointF_MinT.Sam_ETS <- t(S %*% MinT.Sam_P_ETS %*% t(Base_ETS))
  Recon_PointF_MinT.Shr_ETS <- t(S %*% MinT.Shr_P_ETS %*% t(Base_ETS))
  
  #Reconciled variance forecasts for the full hierarchy (Followed from ETS)#
  Recon_Var.Cov_BU_ETS <- S %*% BU_P %*% Shr.cov_ETS %*% t(S %*% BU_P)
  Recon_Var.Cov_OLS_ETS <- S %*% OLS_P %*% Shr.cov_ETS %*% t(S %*% OLS_P)
  Recon_Var.Cov_WLS_ETS <- S %*% WLS_P_ETS %*% Shr.cov_ETS %*% t(S %*% WLS_P_ETS)
  Recon_Var.Cov_MinT.Sam_ETS <- S %*% MinT.Sam_P_ETS %*% Sam.cov_ETS %*% t(S %*% MinT.Sam_P_ETS)
  Recon_Var.Cov_MinT.Shr_ETS <- S %*% MinT.Shr_P_ETS %*% Shr.cov_ETS %*% t(S %*% MinT.Shr_P_ETS)
  
  

  
  
  ##Reconciling densities from ARIMA models##
  
  # Getting different P matrices
  
  #MinT Sample P
  n1 <- nrow(Residuals_all_ARIMA)
  Sam.cov_ARIMA <- crossprod(Residuals_all_ARIMA)/n1
  Inv_Sam.cov_ARIMA <- solve(Sam.cov_ARIMA)
  
  MinT.Sam_P_ARIMA <- solve(t(S) %*% Inv_Sam.cov_ARIMA %*% S) %*% t(S) %*% Inv_Sam.cov_ARIMA
  
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
  Recon_PointF_bottom_MinT.Sam_ARIMA <- t(MinT.Sam_P_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_bottom_MinT.Shr_ARIMA <- t(MinT.Shr_P_ARIMA %*% t(Base_ARIMA))
  
  #Reconciled bottom level variance forecasts (Followed from benchmark)#
  Recon_Var.Cov_bottom_BU_ARIMA <- BU_P %*% Shr.cov_ARIMA %*% t(BU_P)
  Recon_Var.Cov_bottom_OLS_ARIMA <- OLS_P %*% Shr.cov_ARIMA %*% t(OLS_P)
  Recon_Var.Cov_bottom_WLS_ARIMA <- WLS_P_ARIMA %*% Shr.cov_ARIMA %*% t(WLS_P_ARIMA)
  Recon_Var.Cov_bottom_MinT.Sam_ARIMA <- MinT.Sam_P_ARIMA %*% Sam.cov_ARIMA %*% t(MinT.Sam_P_ARIMA)
  Recon_Var.Cov_bottom_MinT.Shr_ARIMA <- MinT.Shr_P_ARIMA %*% Shr.cov_ARIMA %*% t(MinT.Shr_P_ARIMA)
  
  
  #Reconciled point forecasts for the full hierarchy (Followed from benchmark)#
  Recon_PointF_BU_ARIMA <- t(S %*% BU_P %*% t(Base_ARIMA))
  Recon_PointF_OLS_ARIMA <- t(S %*% OLS_P %*% t(Base_ARIMA))
  Recon_PointF_WLS_ARIMA <- t(S %*% WLS_P_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_MinT.Sam_ARIMA <- t(S %*% MinT.Sam_P_ARIMA %*% t(Base_ARIMA))
  Recon_PointF_MinT.Shr_ARIMA <- t(S %*% MinT.Shr_P_ARIMA %*% t(Base_ARIMA))
  
  #Reconciled variance forecasts for the full hierarchy (Followed from benchmark)#
  Recon_Var.Cov_BU_ARIMA <- S %*% BU_P %*% Shr.cov_ARIMA %*% t(S %*% BU_P)
  Recon_Var.Cov_OLS_ARIMA <- S %*% OLS_P %*% Shr.cov_ARIMA %*% t(S %*% OLS_P)
  Recon_Var.Cov_WLS_ARIMA <- S %*% WLS_P_ARIMA %*% Shr.cov_ARIMA %*% t(S %*% WLS_P_ARIMA)
  Recon_Var.Cov_MinT.Sam_ARIMA <- S %*% MinT.Sam_P_ARIMA %*% Sam.cov_ARIMA %*% t(S %*% MinT.Sam_P_ARIMA)
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


save.image("GDP-ProbForecasting-Inc-GaussianMethod-ExpandW_Results_FDist.RData")
