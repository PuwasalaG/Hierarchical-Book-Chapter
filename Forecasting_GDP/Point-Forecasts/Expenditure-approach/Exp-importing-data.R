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

ExpMinImp <- Exp - Imp #Exports minus Imports


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
Exp <- tibble(Gdpe, Gne, GneDfd, GneCii, GneDfdFce, GneDfdGfc, GneCiiPnf, GneDfdFceGvt, GneDfdFceHfc, GneDfdGfcPub, 
              GneDfdGfcPvt, GneDfdFceGvtNat, GneDfdGfcPubGvt, GneDfdGfcPubPcp, GneDfdGfcPvtTdw, GneDfdGfcPvtPbi, GneDfdFceHfcAbt,
              GneDfdFceHfcMis, GneDfdFceHfcTpt, GneDfdFceHfcHcr, GneDfdFceHfcHlt, GneDfdFceHfcFhe, GneDfdFceHfcHwe,
              GneDfdGfcPubGvtNat, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbiNdc, GneDfdGfcPvtPbiNdm,
              GneDfdFceGvtNatNdf, GneDfdFceGvtNatDef, GneDfdFceGvtSnl, GneDfdGfcPubGvtNatNdf, GneDfdGfcPubGvtNatDef, 
              GneDfdGfcPubGvtSnl, GneDfdGfcPubPcpCmw, GneDfdGfcPubPcpSnl, GneDfdGfcPvtTdwNnu, GneDfdGfcPvtTdwAna, GneDfdGfcPvtPbiIprRnd,
              GneDfdGfcPvtPbiIprMnp, GneDfdGfcPvtPbiIprCom, GneDfdGfcPvtPbiIprArt, GneDfdGfcPvtPbiNdcNbd, GneDfdGfcPvtPbiNdcNec,
              GneDfdGfcPvtPbiNdcSha, GneDfdGfcPvtPbiNdmNew, GneDfdGfcPvtPbiNdmSha, GneDfdGfcPvtPbiCbr, GneDfdGfcPvtOtc, 
              GneDfdFceHfcAbtAlc, GneDfdFceHfcAbtCig, GneDfdFceHfcMisOgd, GneDfdFceHfcMisOsv, GneDfdFceHfcMisIfs, GneDfdFceHfcTptTsv,
              GneDfdFceHfcTptPvh, GneDfdFceHfcTptOvh, GneDfdFceHfcHcrAsv, GneDfdFceHfcHcrCsv, GneDfdFceHfcHltHsv, GneDfdFceHfcHltMed,
              GneDfdFceHfcFheFnt, GneDfdFceHfcFheTls, GneDfdFceHfcFheApp, GneDfdFceHfcHweRnt, GneDfdFceHfcHweWsc, GneDfdFceHfcHweEgf,
              GneDfdFceHfcFud, GneDfdFceHfcCnf, GneDfdFceHfcRnc, GneDfdFceHfcEdc, GneDfdFceHfcCom, GneCiiPnfMin, GneCiiPnfMan, GneCiiPnfWht,
              GneCiiPnfRet, GneCiiPnfOnf, GneCiiPba, GneCiiPfm, Sde, ExpMinImp)


rm(list=setdiff(ls(), "Exp"))

#Creating S matrix

s1 <- rep(1, 53)
s2 <- c(rep(1, 51), 0, 0)
s3 <- c(rep(1, 44), rep(0, 9))
s4 <- c(rep(0, 44), rep(1, 7), rep(0, 2))
s5 <- c(rep(1, 3), rep(0, 18), rep(1, 23), rep(0, 9))
s6 <- c(rep(0, 3), rep(1, 18), rep(0, 32))  
s7 <- c(rep(0, 44), rep(1, 5), rep(0, 4)) 
s8 <- c(rep(1, 3), rep(0, 50))
s9 <- c(rep(0, 21), rep(1, 23), rep(0, 9))  
s10 <- c(rep(0, 3), rep(1, 5), rep(0, 45))  
s11 <- c(rep(0, 8), rep(1, 13), rep(0, 32))  
s12 <- c(rep(1, 2), rep(0, 51))  
s13 <- c(rep(0, 3), rep(1, 3), rep(0, 47))  
s14 <- c(rep(0, 6), rep(1, 2), rep(0, 45))  
s15 <- c(rep(0, 8), rep(1, 2), rep(0, 43))  
s16 <- c(rep(0, 10), rep(1, 10), rep(0, 33))  
s17 <- c(rep(0, 21), rep(1, 2), rep(0, 30))
s18 <- c(rep(0, 23), rep(1, 3), rep(0, 27))
s19 <- c(rep(0, 26), rep(1, 3), rep(0, 24))
s20 <- c(rep(0, 29), rep(1, 2), rep(0, 22))  
s21 <- c(rep(0, 31), rep(1, 2), rep(0, 20))
s22 <- c(rep(0, 33), rep(1, 3), rep(0, 17))
s23 <- c(rep(0, 36), rep(1, 3), rep(0, 14))
s24 <- c(rep(0, 3), rep(1, 2), rep(0, 48))
s25 <- c(rep(0, 10), rep(1, 4), rep(0, 39))
s26 <- c(rep(0, 14), rep(1, 3), rep(0, 36))
s27 <- c(rep(0, 17), rep(1, 2), rep(0, 34))
s28_80 <- diag(1, nrow = 53, ncol = 53)
S <- rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23, s24,
           s25, s26, s27, s28_80)


