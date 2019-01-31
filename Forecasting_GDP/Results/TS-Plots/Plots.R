
library(tidyverse)
library(ggplot2)
library(fpp2)
library(zoo)
library(gridExtra)
library(ggpubr)

# setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Final-results")


### Time series plots for Income and Expenditure approach

##To show different characteristics of different graphs
#In income hierarchy - Gdpi, TfiGosCopNfn, TfiGosCopNfnPub, Sdi

load("Results-Income-approach/PointForecasts-ExpandingW.RData")

Inc %>% dplyr::select(c("Gdpi")) %>% mutate_at(vars("Gdpi"), funs(./1000)) %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ billions") -> INC_GDP_TSplot

Inc %>% dplyr::select(c("TfiGosCopNfn", "TfiGosCopNfnPub", "Sdi")) %>% 
  mutate_at(vars("TfiGosCopNfn", "TfiGosCopNfnPub", "Sdi"), funs(./1000)) %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ billions") -> INC_char_of_evels_TSplots

load("Results-Expenditure-approach/PointForecasts-ExpandingW.RData")

Exp %>% dplyr::select(c("GneCii", "GneDfdGfcPvtPbiNdmSha", "GneCiiPba")) %>% 
  mutate_at(vars("GneCii", "GneDfdGfcPvtPbiNdmSha", "GneCiiPba"), funs(./1000)) %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ billions") -> EXP_char_of_levels_TSplots

grid.arrange( arrangeGrob(INC_char_of_evels_TSplots, top = "Income"),
              arrangeGrob(EXP_char_of_levels_TSplots, top = "Expenditure"),
              ncol=2) -> INC_EXP_plots

grid.arrange( arrangeGrob(INC_GDP_TSplot, top = "GDP"), 
              arrangeGrob(INC_EXP_plots), ncol = 1, heights = c(2,4))

#TS plots for each series of the hierarchy - Income hierarchy

Inc %>% dplyr::select(c(names(Inc)[1:6])) %>% ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = FALSE) -> INC_aggregates_TS

Inc %>% dplyr::select(c(names(Inc)[7:11])) %>% ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = FALSE) -> INC_bottomLevel_TS_1

Inc %>% dplyr::select(c(names(Inc)[12:16])) %>% ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = FALSE) -> INC_bottomLevel_TS_2

ggarrange(INC_bottomLevel_TS_1, INC_bottomLevel_TS_2, ncol = 2, legend = "bottom")


rm(list = ls())
load("Results-Expenditure-approach/PointForecasts-ExpandingW.RData")

Exp %>% dplyr::select("Gdpe", "Gne", "ExpMinImp", "Sde") %>% rename("GDP(E)" = "Gdpe", "GNE" = "Gne", 
                                                             "ExportsLessImports" = "ExpMinImp", "SD" = "Sde") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") 

Exp %>% select("GneDfd", "GneDfdFce", "GneDfdGfc") %>% rename("DFD" = "GneDfd", "FCE" = "GneDfdFce", 
                                                              "GDCF" = "GneDfdGfc") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") 

Exp %>% select("GneCii" , "GneCiiPnf", "GneCiiPfm", "GneCiiPba") %>% rename("Total" = "GneCii", 
                                                                            "Private non-farm" = "GneCiiPnf", "Private farm" = "GneCiiPfm", 
                                                                            "Public authorities" = "GneCiiPba") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") 

Exp %>% select("GneCiiPnfMin", "GneCiiPnfMan", "GneCiiPnfWht", "GneCiiPnfRet", "GneCiiPnfOnf") %>% 
  rename("Mining" = "GneCiiPnfMin", "Manufacturing" = "GneCiiPnfMan", "Wholesale trade" = "GneCiiPnfWht", 
         "Retail trade" = "GneCiiPnfRet", "Other non-farm industries" = "GneCiiPnfOnf") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") 

Exp %>% select("GneDfdFceGvt", "GneDfdFceGvtNat", "GneDfdFceGvtNatNdf", "GneDfdFceGvtNatDef", "GneDfdFceGvtSnl") %>% 
  rename("Total Gov" = "GneDfdFceGvt", "National" = "GneDfdFceGvtNat", "Defence" = "GneDfdFceGvtNatNdf", 
        "Non-defence" = "GneDfdFceGvtNatDef", "State and local" = "GneDfdFceGvtSnl") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million")
 
Exp %>% select("GneDfdGfc", "GneDfdGfcPub", "GneDfdGfcPvt") %>% 
  rename("GFCF" = "GneDfdGfc", "Public" = "GneDfdGfcPub", "Private" = "GneDfdGfcPvt") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million")

Exp %>% select("GneDfdGfcPubGvt", "GneDfdGfcPubGvtNat", "GneDfdGfcPubGvtNatNdf", "GneDfdGfcPubGvtNatDef", "GneDfdGfcPubGvtSnl") %>% 
  rename("Gen.Government" = "GneDfdGfcPubGvt", "National" = "GneDfdGfcPubGvtNat", "Non-defence" = "GneDfdGfcPubGvtNatNdf", 
         "Defence" = "GneDfdGfcPubGvtNatDef", "State and local" = "GneDfdGfcPubGvtSnl") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million")

Exp %>% select("GneDfdGfcPubPcp", "GneDfdGfcPubPcpCmw", "GneDfdGfcPubPcpSnl") %>% 
  rename("Pub. corporations" = "GneDfdGfcPubPcp", "Commonwealth" = "GneDfdGfcPubPcpCmw", "State and local" = "GneDfdGfcPubPcpSnl") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million")

Exp %>% select("GneDfdGfcPvtTdw", "GneDfdGfcPvtTdwNnu", "GneDfdGfcPvtTdwAna", "GneDfdGfcPvtOtc") %>% 
  rename("Total dwelling" = "GneDfdGfcPvtTdw", "New and used" = "GneDfdGfcPvtTdwNnu", "Alterations" = "GneDfdGfcPvtTdwAna", 
         "Ownership transfer" = "GneDfdGfcPvtOtc") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million")

Exp %>% select("GneDfdGfcPvtPbi", "GneDfdGfcPvtPbiIpr", "GneDfdGfcPvtPbiCbr", "GneDfdGfcPvtPbiNdc", "GneDfdGfcPvtPbiNdm") %>% 
  rename("Tot pvt business investment" = "GneDfdGfcPvtPbi", "Intellectual property" = "GneDfdGfcPvtPbiIpr", 
         "Cultivated biological" = "GneDfdGfcPvtPbiCbr", "Non-dwelling constr" = "GneDfdGfcPvtPbiNdc", 
         "Non-dwelling machinery" = "GneDfdGfcPvtPbiNdm") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million")

Exp %>% select("GneDfdGfcPvtPbiIprRnd", "GneDfdGfcPvtPbiIprMnp", "GneDfdGfcPvtPbiIprCom", "GneDfdGfcPvtPbiIprArt") %>% 
  rename("IP-R&D" = "GneDfdGfcPvtPbiIprRnd", "IP-Mineral" = "GneDfdGfcPvtPbiIprMnp", "IP-Computer software" = "GneDfdGfcPvtPbiIprCom", 
         "IP-Artistic originals" = "GneDfdGfcPvtPbiIprArt") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") -> TPBI_1

Exp %>% select("GneDfdGfcPvtPbiNdcNbd", "GneDfdGfcPvtPbiNdcNec", "GneDfdGfcPvtPbiNdcSha", "GneDfdGfcPvtPbiNdmNew",
               "GneDfdGfcPvtPbiNdmSha") %>% 
  rename("NDC-New building" = "GneDfdGfcPvtPbiNdcNbd", 
         "NDC-New engineering" = "GneDfdGfcPvtPbiNdcNec", "NDC-Second hand assets" = "GneDfdGfcPvtPbiNdcSha", 
         "NDM-New" = "GneDfdGfcPvtPbiNdmNew", "NDM-second hand" = "GneDfdGfcPvtPbiNdmSha") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") -> TPBI_2

ggarrange(TPBI_1, TPBI_2, ncol = 2)

Exp %>% select("GneDfdFceHfc", "GneDfdFceHfcAbt", "GneDfdFceHfcMis", "GneDfdFceHfcTpt") %>% 
  rename("HFCE" = "GneDfdFceHfc", "Alcohol and tobacco" = "GneDfdFceHfcAbt", "Miscellaneous goods" = "GneDfdFceHfcMis", 
         "Transport" = "GneDfdFceHfcTpt") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") -> HFCE_main_1

Exp %>% select("GneDfdFceHfcHcr", "GneDfdFceHfcHlt", "GneDfdFceHfcFhe", "GneDfdFceHfcHwe") %>% 
  rename("Hotels, cafes & restaurants" = "GneDfdFceHfcHcr", 
         "Health" = "GneDfdFceHfcHlt", "Furnishing & HH equipments" = "GneDfdFceHfcFhe", "Housing, water, gas" = "GneDfdFceHfcHwe") %>% 
  ts(start = c(1984, 4), frequency = 4) %>% autoplot(facets = TRUE) + ylab("$ million") -> HFCE_main_2

ggarrange(HFCE_main_1, HFCE_main_2, ncol = 2)
