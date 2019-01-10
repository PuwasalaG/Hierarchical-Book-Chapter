#Research Project
#packages:

rm(list=ls())

require(tidyverse)
require(fpp2)
require(readxl)
require(hts)
library(zoo)
library(magrittr)
library(Matrix)
library(seasonal)
library(gridExtra)

# setwd("C:/Users/Ayres/Google Drive/Master of Applied Economics & Applied Econometrics/ETF5550 - Research Project/R")

#Importing data#
####Income Approach - Current Prices####

MDI <- read_excel("Master Data File.xlsx", sheet=5, skip = 9) #Master Data for Income

Inc <- tibble(Gdpi = MDI %>% pull("A2302467A") %>% ts(start=c(1959,3), frequency=4) %>%
                window(start=c(1984,4))) #GDP(I)

Inc %>% add_column(Sdi = MDI %>% pull("A2302413V") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Statistical Discrepancy (I)
Inc %>% add_column(Tsi = MDI %>% pull("A2302412T") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Taxes less subsidies (I)

#Compensation of Employees
Inc %>% add_column(TfiCoeWns = MDI %>% pull("A2302399K") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Compensation of employees - Wages and salaries
Inc %>% add_column(TfiCoeEsc = MDI %>% pull("A2302400J") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Compensation of employees - Employers' social contributions
Inc %>% add_column(TfiCoe = MDI %>% pull("A2302401K") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Compensation of employees

##Gross Operating Surplus
#Corporations (Non-financial + Financial)
Inc %>% add_column(TfiGosCopNfnPvt = MDI %>% pull("A2323369L") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Private non-financial corporations ;  Gross operating surplus
Inc %>% add_column(TfiGosCopNfnPub = MDI %>% pull("A2302403R") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Public non-financial corporations ;  Gross operating surplus
Inc %>% add_column(TfiGosCopNfn = MDI %>% pull("A2302404T") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Non-financial corporations ;  Gross operating surplus

Inc %>% add_column(TfiGosCopFin = MDI %>% pull("A2302405V") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Financial corporations ;  Gross operating surplus

Inc %>% add_column(TfiGosCop = MDI %>% pull("A2302406W") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Total corporations ;  Gross operating surplus

#General government
Inc %>% add_column(TfiGosGvt = MDI %>% pull("A2298711F") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #General government ;  Gross operating surplus

#Dwellings
Inc %>% add_column(TfiGosDwl = MDI %>% pull("A2302408A") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Dwellings owned by persons ;  Gross operating surplus

#All sectors GOS (Corporations + Gen. Govn. + Dwellings)
Inc %>% add_column(TfiGos = MDI %>% pull("A2302409C") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #All sectors ;  Gross operating surplus

#Gross mixed income
Inc %>% add_column(TfiGmi = MDI %>% pull("A2302410L") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Gross mixed income

#Total factor income (GOS + GMI)
Inc %>% add_column(Tfi = MDI %>% pull("A2302411R") %>% ts(start=c(1959,3), frequency=4) %>% 
                     window(start=c(1984,4))) -> Inc #Total factor income

####Ancillary matrices####
S <- matrix(c(1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,1,1,1,0,0,
              1,1,1,1,1,0,0,0,0,0,
              0,0,0,0,0,0,1,1,0,0,
              1,1,1,0,0,0,0,0,0,0,
              1,1,0,0,0,0,0,0,0,0,
              diag(10)), nrow=10) %>% t
#y = Gdpi, Tfi, TfiGos, TfiCoe, TfiGosCop, TfiGosCopNfn,
#b_t = TfiGosCopNfnPub, TfiGosCopNfnPvt, TfiGosCopFin,TfiGosGvt,TfiGosDwl,TfiGmi,TfiCoeWns,TfiCoeEsc,Tsi,Sdi


P <- matrix(c(0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              0,0,0,0,0,0,
              diag(10)), nrow=10)

shrink.estim <- function(x)
{
  n <- nrow(x)
  tar<-diag(apply(x, 2, crossprod) / n)
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
  return(shrink.cov)
}


####Loop####

df <- tibble("Year, Qtr" = character(),
              "Series" = character(),
              "F-method" = character(),
              "R-method" = character(),
              "Fc Horizon" = integer(),
              "Forecasts" = double(),
              "Actual" = double(),
              "Scaling Factor" = double(),
              "Weights" = double(),
              "Tr. Set Time" = character(),
              "Test Set length" = double())

kappa.msample <- tibble("Test set length" = double(),
                        "R-method" = character(),
                        "kappa" = double(),
                        "rcond" = double())

kappa.mshrink <- tibble("Test set length" = double(),
                        "R-method" = character(),
                        "kappa" = double(),
                        "rcond" = double())


end_train=c(1994,2) #End of smallest training set
max_train=c(2017,4) #End of largest training set

first_train_length <- Inc %>% pull(.,1) %>% window(end=end_train) %>% length()
first_train_length

test_length <- Inc %>% pull(.,1) %>% window(start=end_train+c(0,1), end=max_train) %>% length
test_length #Used to specify the maximum length it should increase to

H = 4

a=3 # test length
i=1 # number of series

myplots <- list()
myplots2 <- list()

system.time(
  for(a in 1:test_length){
    #Initialization of error matrices
    Inc %>% pull(., 1) %>% subset(., end=first_train_length+a) %>% length() -> e.rows
    
    e.ets <- matrix(NaN, nrow=e.rows, ncol=16)
    e.ari <- matrix(NaN, nrow=e.rows, ncol=16)
    e.srd <- matrix(NaN, nrow=e.rows, ncol=16)
    
    #i=16
    for(i in 1:16){
      Inc %>% pull(., i) %>% subset(., end=first_train_length+a) -> train
      Inc %>% pull(., i) %>% subset(., start=length(train)+1) -> test
      
      trtime <- time(train) #Used to extract training set time
      
      #ETS
      ets(train) %>% forecast(h=H) -> fc.ets
      fc.ets$residuals %>% var() -> W.ets
      e.ets[,i] <- fc.ets$residuals
      
      #ARIMA
      auto.arima(train) %>% forecast(h=H) -> fc.ari
      # auto.arima(train, max.q=0, max.Q=0) %>% forecast(h=H) -> fc.ari
      fc.ari$residuals %>% var() -> W.ari
      e.ari[,i] <- fc.ari$residuals
      
      #Benchmark
      Arima(train, order=c(0,0,0), seasonal=c(0,1,0), include.drift=TRUE) %>% forecast(h=H) -> fc.srd
      fc.srd$residuals %>% var() -> W.srd
      e.srd[,i] <- fc.srd$residuals
      
      # if(i == 1){
      #   print(colnames(Inc)[i])
      #   print(as.yearqtr(trtime[(length(train))]))
      #   print(fc.srd$model)
      # }
      
#      myplots[[i]] <- autoplot(fc.ets)+autolayer(fc.ets$fitted) +autolayer(fc.ari$fitted)
#      myplots2[[i]] <- autoplot(fc.ets$residuals)+autolayer(fc.ari$residuals)
      
      #Scaling Factor
      snaive(train)$residuals %>% abs() %>% mean(., na.rm=TRUE) -> Q
      
      #h=1
      for (h in 1:min(H,length(test))){
        df %>%
          add_row("Year, Qtr"=paste(as.yearqtr(time(fc.ets$mean)))[h],
                  "Series"=paste(colnames(Inc)[i]),
                  "F-method" = str_sub(fc.ets$method,1,3),
                  "R-method" = paste("Base"),
                  "Fc Horizon" = h,
                  "Forecasts" = fc.ets$mean[h],
                  "Actual" = test[h],
                  "Scaling Factor" = Q,
                  "Weights" = W.ets,
                  "Test Set length" = a,
                  "Tr. Set Time" = paste(as.yearqtr(trtime[(length(train))]))) -> df
        }
      
      for (h in 1:min(H,length(test))){ 
        df %>%
          add_row("Year, Qtr"=paste(as.yearqtr(time(fc.ari$mean)))[h],
                  "Series"=paste(colnames(Inc)[i]),
                  "F-method" = str_sub(fc.ari$method,1,5),
                  "R-method" = paste("Base"),
                  "Fc Horizon" = h,
                  "Forecasts" = fc.ari$mean[h],
                  "Actual" = test[h],
                  "Scaling Factor" = Q,
                  "Weights" = W.ari,
                  "Test Set length" = a,
                  "Tr. Set Time" = paste(as.yearqtr(trtime[(length(train))]))) -> df
        }
        
        
      for (h in 1:min(H,length(test))){ 
        df %>%
          add_row("Year, Qtr"=paste(as.yearqtr(time(fc.srd$mean)))[h],
                  "Series"=paste(colnames(Inc)[i]),
                  "F-method" = paste("Benchmark"),
                  "R-method" = paste("Base"),
                  "Fc Horizon" = h,
                  "Forecasts" = fc.srd$mean[h],
                  "Actual" = test[h],
                  "Scaling Factor" = Q,
                  "Weights" = W.srd,
                  "Test Set length" = a,
                  "Tr. Set Time" = paste(as.yearqtr(trtime[(length(train))]))) -> df
        #Added in the last date of training set to make sure that the scaling factor generated is for the correct T.
        #Can remove this later, once sure that everything's correct.
      }
    }
 
    
    for (h in 1:min(H,length(test))){ 
    ###Reconciliation
      
      ##ETS
      df %>% filter(`F-method` == "ETS" & `R-method` == "Base" & `Fc Horizon` == h & `Test Set length` == a) %>% 
        arrange(match(Series, c("Gdpi",
                                "Tfi",
                                "TfiGos",
                                "TfiCoe",
                                "TfiGosCop",
                                "TfiGosCopNfn",
                                "TfiGosCopNfnPub",
                                "TfiGosCopNfnPvt",
                                "TfiGosCopFin",
                                "TfiGosGvt",
                                "TfiGosDwl",
                                "TfiGmi",
                                "TfiCoeWns",
                                "TfiCoeEsc",
                                "Tsi",
                                "Sdi"))) -> fltr
      
      fltr %>%
        select("Forecasts") %>%
        as.matrix() -> yhat
    
      #Bottom-up
      S %*% P %*% yhat -> fc.bu
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.bu, "R-method" = "Bottom-up", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      # I think you can use this here to make sure 
      # full_join(df,int) -> df
      
      
      #WLS
      S.pr <- S %>% t
      
      fltr %>%
        select("Weights") %>%
        as.matrix() %>%
        Diagonal(n=16, x=.) -> W
      
      # W <- diag(1, nrow=16)
      
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.wls
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.wls, "R-method" = "WLS", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      #MinT(Sample)
      cov(e.ets) -> W
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.mntsmp
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.mntsmp, "R-method" = "MinT(Sample)", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      kappa.msample %>%
        add_row("Test set length" = a,
                "R-method" = paste("MinT(Sample)"),
                "kappa" = kappa(W),
                "rcond" = rcond(W)) -> kappa.msample
      
      #MinT(Shrink)
      W <- shrink.estim(e.ets)
      solve(W) -> W.inv

      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.mntshr

      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int

      cbind(int, "Forecasts" = fc.mntshr, "R-method" = "MinT(Shrink)", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      kappa.mshrink %>%
        add_row("Test set length" = a,
                "R-method" = paste("MinT(Shrink)"),
                "kappa" = kappa(W),
                "rcond" = rcond(W)) -> kappa.mshrink
      
      }
    
    for (h in 1:min(H,length(test))){ 
      ##ARIMA
      df %>% filter(`F-method` == "ARIMA" & `R-method` == "Base" & `Fc Horizon` == h & `Test Set length` == a) %>% 
        arrange(match(Series, c("Gdpi",
                                "Tfi",
                                "TfiGos",
                                "TfiCoe",
                                "TfiGosCop",
                                "TfiGosCopNfn",
                                "TfiGosCopNfnPub",
                                "TfiGosCopNfnPvt",
                                "TfiGosCopFin",
                                "TfiGosGvt",
                                "TfiGosDwl",
                                "TfiGmi",
                                "TfiCoeWns",
                                "TfiCoeEsc",
                                "Tsi",
                                "Sdi"))) -> fltr
      
      fltr %>% 
        select("Forecasts") %>%
        as.matrix() -> yhat

      # Bottom-up
      S %*% P %*% yhat -> fc.bu
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.bu, "R-method" = "Bottom-up", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      #WLS
      S.pr <- S %>% t
      
      fltr %>%
        select("Weights") %>%
        as.matrix() %>%
        Diagonal(n=16, x=.) -> W
      
      # W <- diag(1, nrow=16)
      
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.wls
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.wls, "R-method" = "WLS", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      #MinT(Sample)
      cov(e.ari) -> W
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.mntsmp
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.mntsmp, "R-method" = "MinT(Sample)", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      #MinT(Shrink)
      W <- shrink.estim(e.ari)
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.mntshr
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.mntshr, "R-method" = "MinT(Shrink)", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
    }
    
    for (h in 1:min(H,length(test))){ 
      ##Benchmark
      df %>% filter(`F-method` == "Benchmark" & `R-method` == "Base" & `Fc Horizon` == h & `Test Set length` == a) %>% 
        arrange(match(Series, c("Gdpi",
                                "Tfi",
                                "TfiGos",
                                "TfiCoe",
                                "TfiGosCop",
                                "TfiGosCopNfn",
                                "TfiGosCopNfnPub",
                                "TfiGosCopNfnPvt",
                                "TfiGosCopFin",
                                "TfiGosGvt",
                                "TfiGosDwl",
                                "TfiGmi",
                                "TfiCoeWns",
                                "TfiCoeEsc",
                                "Tsi",
                                "Sdi"))) -> fltr
      
      fltr %>% 
        select("Forecasts") %>%
        as.matrix() -> yhat

      #Bottom-up
      S %*% P %*% yhat -> fc.bu
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.bu, "R-method" = "Bottom-up", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      #WLS
      S.pr <- S %>% t
      
      fltr %>%
        select("Weights") %>%
        as.matrix() %>%
        Diagonal(n=16, x=.) -> W
      
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.wls
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.wls, "R-method" = "WLS", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      #MinT(Sample)
      cov(e.srd) -> W
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.mntsmp
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.mntsmp, "R-method" = "MinT(Sample)", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
      
      #MinT(Shrink)
      W <- shrink.estim(e.srd)
      solve(W) -> W.inv
      
      S %*% solve((S.pr %*% W.inv %*% S)) %*% S.pr %*% W.inv %*% yhat %>% as.matrix() -> fc.mntshr
      
      fltr %>%
        select(-"Forecasts", -"Weights", -"R-method") -> int
      
      cbind(int, "Forecasts" = fc.mntshr, "R-method" = "MinT(Shrink)", "Weights"=NaN) %>% as.tibble() -> int
      int[c(1,2,3,10,4,9,5,6,11,8,7)] %>% rbind(., df) -> df
    }
  })

View(df)

####Error Calculations####
# user  system elapsed
# 50.72    0.01   50.78

# df %>%
#   group_by(., Series, `F-method`, `R-method`, `Fc Horizon`) %>%
#   View()

# df %>%
#   group_by(., `Series`, `F-method`, `R-method`, `Fc Horizon`) %>%
#   summarise(.,mean(`Scaling Factor`)) %>%
#   View()

# df %>%
#   group_by(., `Series`, `F-method`, `R-method`, `Fc Horizon`) %>%
#   summarise(., RMSE = sqrt(mean((Actual-Forecasts)^2)),
#             MASE = mean(abs(Actual-Forecasts))/mean(`Scaling Factor`)) %>%
#   View()

df %>%
  group_by(., `Series`, `F-method`, `R-method`, `Fc Horizon`) %>%
  summarise(., RMSE = sqrt(mean((Actual-Forecasts)^2)),
            MASE = mean(abs(Actual-Forecasts)/`Scaling Factor`)) -> Summ1
class(Summ1)
View(Summ1)

####Gdpi - Level 0####
Summ1 %>%
  filter(`Series` == "Gdpi", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("MASE") %>%
  as_vector() -> Bm.e
Bm.e

Summ1 %>%
  filter(`Series` == "Gdpi", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("RMSE") %>%
  as_vector() -> Bm.e2
Bm.e2

Summ1 %>%
  filter(., `Series` == "Gdpi") %>%
  mutate(., MASEr = round((((MASE/Bm.e)-1)*100), digits=3),
         RMSEr = round((((RMSE/Bm.e2)-1)*100), digits=3)) -> Summ2
View(Summ2)

df %>% filter(`Series`=="TfiGosCopNfnPvt", `F-method`=="ETS", `R-method`%in%c("Base", "WLS"), `Fc Horizon`==1) %>%
  select("Year, Qtr", "R-method", "Forecasts") %>%
  spread(., key=`R-method`, value=Forecasts)->aa 
  cor(aa$WLS,aa$Base)


# #MASE
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ARIMA") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ETS") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "Benchmark") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()

#RMSE
Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ARIMA") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ETS") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "Benchmark") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

####Error plots####
df %>%
  filter(`Series`=="Gdpi", `Fc Horizon`==1) %>% View

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "Benchmark", `R-method` == "Base", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.benchmark

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "Base", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "Bottom-up", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> BU.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "WLS", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> WLS.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "MinT(Sample)", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MSample.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "MinT(Shrink)", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MShrink.Arima

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(BU.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot1

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(WLS.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot2

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MSample.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot3

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MShrink.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot4

grid.arrange(Plot1, Plot2, Plot3, Plot4)

####Forecast plots####
df %>%
  filter(`Series`=="Gdpi", `Fc Horizon`==1) %>% View

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "Benchmark", `R-method` == "Base", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.benchmark

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "Base", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "Bottom-up", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> BU.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "WLS", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> WLS.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "MinT(Sample)", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MSample.Arima

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ARIMA", `R-method` == "MinT(Shrink)", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MShrink.Arima

Inc %>% pull(.,"Gdpi") %>%
  window(., start=c(1990,4)) -> Gdpi 

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  ylab("Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot0
Plot0

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(BU.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot1

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(WLS.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot2

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MSample.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot3

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MShrink.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot4

grid.arrange(Plot1, Plot2, Plot3, Plot4)


ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.Arima) +
  autolayer(MShrink.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom")

####Other Series####
#Total factor income (Tfi) - Level 1
Summ1 %>%
  filter(`Series` == "Tfi", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("MASE") %>%
  as_vector() -> Bm.e3
Bm.e3

Summ1 %>%
  filter(`Series` == "Tfi", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("RMSE") %>%
  as_vector() -> Bm.e4
Bm.e4

Summ1 %>%
  filter(., `Series` == "Tfi") %>%
  mutate(., MASEr = round((((MASE/Bm.e3)-1)*100), digits=3),
         RMSEr = round((((RMSE/Bm.e4)-1)*100), digits=3)) -> Summ2

# #MASE
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ARIMA") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ETS") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "Benchmark") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()


#RMSE
Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ARIMA") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ETS") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "Benchmark") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()


#Gross operating surplus (TfiGos) - Level 2
Summ1 %>%
  filter(`Series` == "TfiGos", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("MASE") %>%
  as_vector() -> Bm.e3
Bm.e3

Summ1 %>%
  filter(`Series` == "TfiGos", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("RMSE") %>%
  as_vector() -> Bm.e4
Bm.e4

Summ1 %>%
  filter(., `Series` == "TfiGos") %>%
  mutate(., MASEr = round((((MASE/Bm.e3)-1)*100), digits=3),
         RMSEr = round((((RMSE/Bm.e4)-1)*100), digits=3)) -> Summ2

# #MASE
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ARIMA") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ETS") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "Benchmark") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()


#RMSE
Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ARIMA") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ETS") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "Benchmark") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()


#Total corporations (TfiGosCop) - Level 3
Summ1 %>%
  filter(`Series` == "TfiGosCop", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("MASE") %>%
  as_vector() -> Bm.e3
Bm.e3

Summ1 %>%
  filter(`Series` == "TfiGosCop", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("RMSE") %>%
  as_vector() -> Bm.e4
Bm.e4

Summ1 %>%
  filter(., `Series` == "TfiGosCop") %>%
  mutate(., MASEr = round((((MASE/Bm.e3)-1)*100), digits=3),
         RMSEr = round((((RMSE/Bm.e4)-1)*100), digits=3)) -> Summ2
View(Summ2)

# #MASE
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ARIMA") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ETS") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "Benchmark") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()


#RMSE
Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ARIMA") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ETS") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "Benchmark") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()


#Non-financial corporations (TfiGosCopNfn) - Level 4
Summ1 %>%
  filter(`Series` == "TfiGosCopNfn", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("MASE") %>%
  as_vector() -> Bm.e3
Bm.e3

Summ1 %>%
  filter(`Series` == "TfiGosCopNfn", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("RMSE") %>%
  as_vector() -> Bm.e4
Bm.e4

Summ1 %>%
  filter(., `Series` == "TfiGosCopNfn") %>%
  mutate(., MASEr = round((((MASE/Bm.e3)-1)*100), digits=3),
         RMSEr = round((((RMSE/Bm.e4)-1)*100), digits=3)) -> Summ2
View(Summ2)

# #MASE
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ARIMA") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ETS") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "Benchmark") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()


#RMSE
Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ARIMA") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ETS") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "Benchmark") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

#Non-financial corporations - Private (TfiGosCopNfnPvt) - Level 5
Summ1 %>%
  filter(`Series` == "TfiGosCopNfnPvt", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("MASE") %>%
  as_vector() -> Bm.e3
Bm.e3

Summ1 %>%
  filter(`Series` == "TfiGosCopNfnPvt", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("RMSE") %>%
  as_vector() -> Bm.e4
Bm.e4

Summ1 %>%
  filter(., `Series` == "TfiGosCopNfnPvt") %>%
  mutate(., MASEr = round((((MASE/Bm.e3)-1)*100), digits=3),
         RMSEr = round((((RMSE/Bm.e4)-1)*100), digits=3)) -> Summ2
View(Summ2)

# #MASE
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ARIMA") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ETS") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "Benchmark") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()


#RMSE
Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ARIMA") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ETS") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "Benchmark") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()


#Compensation of Employees (TfiCoe) - Level 2
Summ1 %>%
  filter(`Series` == "TfiCoe", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("MASE") %>%
  as_vector() -> Bm.e3
Bm.e3

Summ1 %>%
  filter(`Series` == "TfiCoe", `F-method` == "Benchmark", `R-method` == "Base") %>%
  slice() %>%
  ungroup %>%
  select("RMSE") %>%
  as_vector() -> Bm.e4
Bm.e4

Summ1 %>%
  filter(., `Series` == "TfiCoe") %>%
  mutate(., MASEr = round((((MASE/Bm.e3)-1)*100), digits=3),
         RMSEr = round((((RMSE/Bm.e4)-1)*100), digits=3)) -> Summ2
View(Summ2)

# #MASE
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ARIMA") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "ETS") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()
# 
# Summ2 %>%
#   ungroup %>%
#   filter(`F-method` == "Benchmark") %>%
#   select("R-method", "Fc Horizon", "MASEr") %>%
#   spread(., key = "Fc Horizon", value="MASEr") %>%
#   knitr::kable()


#RMSE
Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ARIMA") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "ETS") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

Summ2 %>%
  ungroup %>%
  filter(`F-method` == "Benchmark") %>%
  select("R-method", "Fc Horizon", "RMSEr") %>%
  spread(., key = "Fc Horizon", value="RMSEr") %>%
  knitr::kable()

####Graphs for other series####
#Errors
x = "Tfi"
df %>% 
  filter(`Series` == x, `F-method` == "Benchmark", `R-method` == "Base", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.benchmark

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "Base", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "Bottom-up", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> BU.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "WLS", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> WLS.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "MinT(Sample)", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MSample.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "MinT(Shrink)", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MShrink.Arima

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(BU.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot1

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(WLS.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot2

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MSample.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot3

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MShrink.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot4

grid.arrange(Plot1, Plot2, Plot3, Plot4)

#Forecasts
df %>% 
  filter(`Series` == x, `F-method` == "Benchmark", `R-method` == "Base", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.benchmark

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "Base", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "Bottom-up", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> BU.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "WLS", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> WLS.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "MinT(Sample)", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MSample.Arima

df %>% 
  filter(`Series` == x, `F-method` == "ARIMA", `R-method` == "MinT(Shrink)", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MShrink.Arima

Inc %>% pull(.,x) %>%
  window(., start=c(1990,4)) -> Gdpi 

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  ylab("Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom")

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(BU.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot1

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(WLS.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot2

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MSample.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot3

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.Arima) +
  autolayer(MShrink.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot4

grid.arrange(Plot1, Plot2, Plot3, Plot4)


ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.Arima) +
  autolayer(MShrink.Arima) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom")


####Gdpi - ETS####
#Error plots
df %>%
  filter(`Series`=="Gdpi", `Fc Horizon`==1) %>% View

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "Benchmark", `R-method` == "Base", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.benchmark

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "Base", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "Bottom-up", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> BU.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "WLS", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> WLS.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "MinT(Sample)", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MSample.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "MinT(Shrink)", `Fc Horizon`==1) %>% 
  mutate(., Error = Actual-Forecasts) %>%
  arrange(., `Year, Qtr`) %>%
  select("Error") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MShrink.ETS

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(BU.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot1

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(WLS.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot2

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(MSample.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot3

ggplot() +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(MShrink.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot4

grid.arrange(Plot1, Plot2, Plot3, Plot4)

#Forecast plots
df %>%
  filter(`Series`=="Gdpi", `Fc Horizon`==1) %>% View

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "Benchmark", `R-method` == "Base", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.benchmark

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "Base", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> Base.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "Bottom-up", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> BU.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "WLS", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> WLS.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "MinT(Sample)", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MSample.ETS

df %>% 
  filter(`Series` == "Gdpi", `F-method` == "ETS", `R-method` == "MinT(Shrink)", `Fc Horizon`==1) %>% 
  arrange(., `Year, Qtr`) %>%
  select("Forecasts") %>%
  ts(., start=c(1994, 4), frequency = 4) -> MShrink.ETS

Inc %>% pull(.,"Gdpi") %>%
  window(., start=c(1990,4)) -> Gdpi 

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  ylab("Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot0
Plot0

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(BU.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot1

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(WLS.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot2

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(MSample.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot3

ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.benchmark) +
  autolayer(Base.ETS) +
  autolayer(MShrink.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom") -> Plot4

grid.arrange(Plot1, Plot2, Plot3, Plot4)


ggplot() +
  autolayer(Gdpi, color="black") +
  autolayer(Base.ETS) +
  autolayer(MShrink.ETS) +
  ylab("Actual-Forecasts") +
  xlab("Time") +
  guides(color=guide_legend(title="Forecasts:")) +
  theme(legend.position = "bottom")

