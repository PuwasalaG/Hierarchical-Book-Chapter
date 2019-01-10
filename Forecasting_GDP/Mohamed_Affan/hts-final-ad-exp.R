#Research Project
#packages:
require(tidyverse)
require(fpp2)
require(readxl)
require(hts)
library(zoo)
library(magrittr)
library(Matrix)

rm(list=ls())
setwd("C:/Users/Ayres/Google Drive/Master of Applied Economics & Applied Econometrics/ETF5550 - Research Project/R")

####Expenditure Approach - Current Prices####
MDC2 <- read_excel("Master Data File.xlsx", sheet=6, skip = 9) #Master Data for Expenditure - Current Prices

Gdpe <- MDC2 %>% pull("A2302467A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #GDP(E)

Sde <- MDC2 %>% pull("A2302566J") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Statistical Discrepancy(E)

###Exports
Exp <- MDC2 %>% pull("A2302564C") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Exports of goods and services

###Imports
Imp <- MDC2 %>% pull("A2302565F") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Imports of goods and services

###Gross National Expenditure (Domestic Final Demand + Change in Inventories)
Gne <- MDC2 %>% pull("A2302563A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Gross national expenditure

###Change in Inventories
GneCii <- MDC2 %>% pull("A2302562X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Changes in inventories

##Domestic Final Demand (FCE total + GFCF Total)
GneDfd <- MDC2 %>% pull("A2302558J") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Domestic final demand

####Final Consumption Expenditure (FCE) - Current Prices####
#Government
GneDfdFceGvtNatDef <- MDC2 %>% pull("A2302523J") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - National ;  Final consumption expenditure - Defence
GneDfdFceGvtNatNdf <- MDC2 %>% pull("A2302524K") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - National ;  Final consumption expenditure - Non-defence
GneDfdFceGvtNat <- MDC2 %>% pull("A2302525L") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - National ; Final consumption expenditure

GneDfdFceGvtSnl <- MDC2 %>% pull("A2302526R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - State and local ;  Final consumption expenditure
GneDfdFceGvt <- MDC2 %>% pull("A2302527T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government ;  Final consumption expenditure

####################### #Household
GneDfdFceHfc <- MDC2 %>% pull("A2302528V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Households ;  Final consumption expenditure

#Total FCE
GneDfdFce <- MDC2 %>% pull("A2302529W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #All sectors ;  Final consumption expenditure



####Gross Fixed Capital Formation - Current Prices####
###Private
##Dwellings
GneDfdGfcPvtTdwNnu <- MDC2 %>% pull("A2302543T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Dwellings - New and Used
GneDfdGfcPvtTdwAna <- MDC2 %>% pull("A2302544V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Dwellings - Alterations and additions
GneDfdGfcPvtTdw <- MDC2 %>% pull("A2302545W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Dwellings - Total

##Ownership Transfer Costs
GneDfdGfcPvtOtc <- MDC2 %>% pull("A2302546X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Ownership transfer costs

##Private Business Investments
#Non-dwelling Construction
GneDfdGfcPvtPbiNdcNbd <- MDC2 %>% pull("A2302533L") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Non-dwelling construction - New building
GneDfdGfcPvtPbiNdcNec <- MDC2 %>% pull("A2302534R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Non-dwelling construction - New engineering construction
GneDfdGfcPvtPbiNdcSha <- MDC2 %>% pull("A2302535T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Non-dwelling construction - Net purchase of second hand assets
GneDfdGfcPvtPbiNdc <- MDC2 %>% pull("A2302536V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Non-dwelling construction - Total

#Non-dwelling Machinery
GneDfdGfcPvtPbiNdmNew <- MDC2 %>% pull("A2302530F") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Machinery and equipment - New
GneDfdGfcPvtPbiNdmSha <- MDC2 %>% pull("A2302531J") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Machinery and equipment - Net purchase of second hand assets
GneDfdGfcPvtPbiNdm <- MDC2 %>% pull("A2302532K") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Machinery and equipment - Total

#Cultivated biological resources
GneDfdGfcPvtPbiCbr <- MDC2 %>% pull("A2716219R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Cultivated biological resources

#Intellectual Property
GneDfdGfcPvtPbiIprRnd <- MDC2 %>% pull("A2716221A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Intellectual property products - Research and development
GneDfdGfcPvtPbiIprMnp <- MDC2 %>% pull("A2302539A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Intellectual property products - Mineral and petroleum exploration
GneDfdGfcPvtPbiIprCom <- MDC2 %>% pull("A2302538X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Intellectual property products - Computer software
GneDfdGfcPvtPbiIprArt <- MDC2 %>% pull("A2302540K") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Intellectual property products - Artistic originals
GneDfdGfcPvtPbiIpr <- MDC2 %>% pull("A2716220X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Intellectual property products Total

#Total Private Business Investment (IP+CBR+NDW+NDC+D)
GneDfdGfcPvtPbi <- MDC2 %>% pull("A2302542R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation - Total private business investment


#Total Private Gross Capital Formation (TPBI+Tot. Dwellings+Ownership Transfer Costs)
GneDfdGfcPvt <- MDC2 %>% pull("A2302547A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Gross fixed capital formation


###Public
##Public corporations
GneDfdGfcPubPcpCmw <- MDC2 %>% pull("A2302548C") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Public corporations - Commonwealth ;  Gross fixed capital formation
GneDfdGfcPubPcpSnl <- MDC2 %>% pull("A2302549F") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Public corporations - State and local ;  Gross fixed capital formation
GneDfdGfcPubPcp <- MDC2 %>% pull("A2302550R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Public corporations ;  Gross fixed capital formation Total

##General government (National+State and local)
GneDfdGfcPubGvtNatDef <- MDC2 %>% pull("A2302551T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - National ;  Gross fixed capital formation - Defence
GneDfdGfcPubGvtNatNdf <- MDC2 %>% pull("A2302552V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - National ;  Gross fixed capital formation - Non-defence
GneDfdGfcPubGvtNat <- MDC2 %>% pull("A2302553W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - National ;  Gross fixed capital formation Total

GneDfdGfcPubGvtSnl <- MDC2 %>% pull("A2302554X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government - State and local ;  Gross fixed capital formation

#Total
GneDfdGfcPubGvt <- MDC2 %>% pull("A2302555A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #General government ;  Gross fixed capital formation


#Public GFCF (General Government Total + Public coporations Total )
GneDfdGfcPub <- MDC2 %>% pull("A2302556C") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Public ;  Gross fixed capital formation

#GFCF Total (Public GFCF + Private GFCF)
GneDfdGfc <- MDC2 %>% pull("A2302557F") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #All sectors ;  Gross fixed capital formation


####Changes in Inventories - Current Prices####
CIE2 <- read_excel("Master Data File.xlsx", sheet=7, skip = 9) #Master Data for Changes in Inventories under Expenditure Approach - Current Prices

##############################
citot2 <- CIE2 %>% pull("A2302562X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Changes in Inventories

GneCiiPfm <- CIE2 %>% pull("A2302560V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Farm

GneCiiPba <- CIE2 %>% pull("A2302561W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Public authorities


GneCiiPnf <- CIE2 %>% pull("A2302559K") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Non-farm Total

GneCiiPnfMin <- CIE2 %>% pull("A83722619L") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Mining (B)
GneCiiPnfMan <- CIE2 %>% pull("A3348511X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Manufacturing (C)
GneCiiPnfWht <- CIE2 %>% pull("A3348512A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Wholesale trade (F)
GneCiiPnfRet <- CIE2 %>% pull("A3348513C") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Retail trade (G)
GneCiiPnfOnf <- CIE2 %>% pull("A2302273C") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Private ;  Non-farm ;  Other non-farm industries


####Household Final Consumption Expenditure - Current Prices####
HFCE2 <- read_excel("Master Data File.xlsx", sheet=8, skip = 9) #Master Data for HFCE under Expenditure Approach - Curr Prices

##############################
hfce2 <- HFCE2 %>% pull("A2302254W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Household Final Consumption Expenditure

GneDfdFceHfcFud <- HFCE2 %>% pull("A2302237V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Food

GneDfdFceHfcAbt <- HFCE2 %>% pull("A3605816F") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Alcoholic beverages and tobacco
GneDfdFceHfcAbtCig <- HFCE2 %>% pull("A2302238W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Cigarettes and tobacco
GneDfdFceHfcAbtAlc <- HFCE2 %>% pull("A2302239X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Alcoholic beverages

GneDfdFceHfcCnf <- HFCE2 %>% pull("A2302240J") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Clothing and footwear

GneDfdFceHfcHwe <- HFCE2 %>% pull("A3605680F") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Housing, water, electricity, gas and other fuels
GneDfdFceHfcHweRnt <- HFCE2 %>% pull("A3605681J") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Actual and imputed rent for housing
GneDfdFceHfcHweWsc <- HFCE2 %>% pull("A3605682K") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Water and sewerage charges
GneDfdFceHfcHweEgf <- HFCE2 %>% pull("A2302242L") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Electricity, gas and other fuel

GneDfdFceHfcFhe <- HFCE2 %>% pull("A2302243R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Furnishings and household equipment
GneDfdFceHfcFheFnt <- HFCE2 %>% pull("A3605683L") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Furniture, floor coverings and household goods
GneDfdFceHfcFheApp <- HFCE2 %>% pull("A3605684R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Household appliances
GneDfdFceHfcFheTls <- HFCE2 %>% pull("A3605685T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Household tools

GneDfdFceHfcHlt <- HFCE2 %>% pull("A2302244T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Health
GneDfdFceHfcHltMed <- HFCE2 %>% pull("A3605686V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Medicines, medical aids and therapeutic appliances
GneDfdFceHfcHltHsv <- HFCE2 %>% pull("A3605687W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Total health services

GneDfdFceHfcTpt <- HFCE2 %>% pull("A3605688X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Transport
GneDfdFceHfcTptPvh <- HFCE2 %>% pull("A2302245V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Purchase of vehicles
GneDfdFceHfcTptOvh <- HFCE2 %>% pull("A2302246W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Operation of vehicles
GneDfdFceHfcTptTsv <- HFCE2 %>% pull("A2302247X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Transport services

GneDfdFceHfcCom <- HFCE2 %>% pull("A2302248A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Communications

GneDfdFceHfcRnc <- HFCE2 %>% pull("A2302249C") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Recreation and culture

GneDfdFceHfcEdc <- HFCE2 %>% pull("A2302250L") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Education services

GneDfdFceHfcHcr <- HFCE2 %>% pull("A2302251R") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Hotels, cafes and restaurants
GneDfdFceHfcHcrCsv <- HFCE2 %>% pull("A3605694V") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Catering services
GneDfdFceHfcHcrAsv <- HFCE2 %>% pull("A3605695W") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Accommodation services

GneDfdFceHfcMis <- HFCE2 %>% pull("A3605696X") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Miscellaneous goods and services
GneDfdFceHfcMisOgd <- HFCE2 %>% pull("A3605697A") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Other goods
GneDfdFceHfcMisIfs <- HFCE2 %>% pull("A2302252T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Insurance and other financial services
GneDfdFceHfcMisOsv <- HFCE2 %>% pull("A3606485T") %>% ts(start=c(1959,3), frequency=4) %>% 
  window(start=c(1984,4)) #Other services

####Dataframe####
Exp <- tibble(Gdpe, Sde, Exp, Imp, Gne, GneCii, GneDfd, GneDfdFceGvtNatDef, GneDfdFceGvtNatNdf, GneDfdFceGvtNat, GneDfdFceGvtSnl,
              GneDfdFceGvt, GneDfdFceHfc, GneDfdFce, GneDfdGfcPvtTdwNnu, GneDfdGfcPvtTdwAna, GneDfdGfcPvtTdw,
              GneDfdGfcPvtOtc, GneDfdGfcPvtPbiNdcNbd, GneDfdGfcPvtPbiNdcNec, GneDfdGfcPvtPbiNdcSha, GneDfdGfcPvtPbiNdc,
              GneDfdGfcPvtPbiNdmNew, GneDfdGfcPvtPbiNdmSha, GneDfdGfcPvtPbiNdm, GneDfdGfcPvtPbiCbr, GneDfdGfcPvtPbiIprRnd,
              GneDfdGfcPvtPbiIprMnp, GneDfdGfcPvtPbiIprCom, GneDfdGfcPvtPbiIprArt, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbi, GneDfdGfcPvt,
              GneDfdGfcPubPcpCmw, GneDfdGfcPubPcpSnl, GneDfdGfcPubPcp, GneDfdGfcPubGvtNatDef, GneDfdGfcPubGvtNatNdf, GneDfdGfcPubGvtNat,
              GneDfdGfcPubGvtSnl, GneDfdGfcPubGvt, GneDfdGfcPub, GneDfdGfc, GneCiiPfm, GneCiiPba, GneCiiPnf, GneCiiPnfMin, GneCiiPnfMan,
              GneCiiPnfWht, GneCiiPnfRet, GneCiiPnfOnf, GneDfdFceHfcFud, GneDfdFceHfcAbt, GneDfdFceHfcAbtCig, GneDfdFceHfcAbtAlc,
              GneDfdFceHfcCnf, GneDfdFceHfcHwe, GneDfdFceHfcHweRnt, GneDfdFceHfcHweWsc, GneDfdFceHfcHweEgf, GneDfdFceHfcFhe,
              GneDfdFceHfcFheFnt, GneDfdFceHfcFheApp, GneDfdFceHfcFheTls, GneDfdFceHfcHlt, GneDfdFceHfcHltMed, GneDfdFceHfcHltHsv,
              GneDfdFceHfcTpt, GneDfdFceHfcTptPvh, GneDfdFceHfcTptOvh, GneDfdFceHfcTptTsv, GneDfdFceHfcCom, GneDfdFceHfcRnc,
              GneDfdFceHfcEdc, GneDfdFceHfcHcr, GneDfdFceHfcHcrCsv, GneDfdFceHfcHcrAsv, GneDfdFceHfcMis, GneDfdFceHfcMisOgd,
              GneDfdFceHfcMisIfs, GneDfdFceHfcMisOsv)

rm(list=setdiff(ls(), "Exp"))

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

end_train=c(2016,4) #End of smallest training set
max_train=c(2017,4) #End of largest training set

first_train_length <- Exp %>% pull(.,1) %>% window(end=end_train) %>% length()
first_train_length

test_length <- Exp %>% pull(.,1) %>% window(start=end_train+c(0,1), end=max_train) %>% length
test_length #Used to specify the maximum length it should increase to

H = 12

system.time(
  for(a in 1:test_length){
    #Initialization of error matrices
    Exp %>% pull(., 1) %>% subset(., end=first_train_length+a) %>% length() -> e.rows
    
    e.ets <- matrix(NaN, nrow=e.rows, ncol=81)
    e.ari <- matrix(NaN, nrow=e.rows, ncol=81)
    e.srd <- matrix(NaN, nrow=e.rows, ncol=81)
    
    for(i in 1:81){
      Exp %>% pull(., i) %>% subset(., end=first_train_length+a) -> train
      Exp %>% pull(., i) %>% subset(., start=length(train)+1) -> test
      
      trtime <- time(train) #Used to extract training set time
      
      #Base
      ets(train) %>% forecast(h=H) -> fc.ets
      fc.ets$residuals %>% var() -> W.ets
      e.ets[,i] <- fc.ets$residuals
      
      #ARIMA
      auto.arima(train) %>% forecast(h=H) -> fc.ari
      fc.ari$residuals %>% var() -> W.ari
      e.ari[,i] <- fc.ari$residuals
      
      #Benchmark
      Arima(train, order=c(0,0,0), seasonal=c(0,1,0), include.drift=TRUE) %>% forecast(h=H) -> fc.srd
      fc.srd$residuals %>% var() -> W.srd
      e.srd[,i] <- fc.srd$residuals
      
      #Scaling Factor
      snaive(train)$residuals %>% abs() %>% mean(., na.rm=TRUE) -> Q
      
      for (h in 1:min(12,length(test))){
        df %>%
          add_row("Year, Qtr"=paste(as.yearqtr(time(fc.ets$mean)))[h],
                  "Series"=paste(colnames(Exp)[i]),
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
      
      for (h in 1:min(12,length(test))){ 
        df %>%
          add_row("Year, Qtr"=paste(as.yearqtr(time(fc.ari$mean)))[h],
                  "Series"=paste(colnames(Exp)[i]),
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
      
      
      for (h in 1:min(12,length(test))){ 
        df %>%
          add_row("Year, Qtr"=paste(as.yearqtr(time(fc.srd$mean)))[h],
                  "Series"=paste(colnames(Exp)[i]),
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
  })

View(df)

# user  system elapsed 
# 275.11    0.36  277.38 
