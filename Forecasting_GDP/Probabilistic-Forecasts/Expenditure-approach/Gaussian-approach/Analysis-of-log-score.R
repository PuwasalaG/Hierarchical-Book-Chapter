library(tidyverse)
library(ggplot2)

setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Probabilistic-Forecasts/Expenditure-approach/Gaussian-approach")
load("GDP-ProbForecasting-Exp-GaussianMethod-ExpandW.RData")

DF_MultiV %>% dplyr::select(-`Energy score`, -`Variogram score`) %>% 
  filter(`Log score` != "NA") %>% 
  filter(`F-method` == "ETS") -> DF_Mult_LS_ETS

#View(DF_Mult_LS_ETS)

DF_Mult_LS_ETS %>% ggplot(aes(x = `Replication`, y = `Log score`, color = `R-method`)) + 
  geom_line() + facet_wrap(~`Forecast Horizon`, scales = "free_y")
