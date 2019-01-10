library(tidyverse)
library(ggplot2)

setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Probabilistic-Forecasts/Income-approach/Gaussian-approach")
load("GDP-ProbForecasting-Inc-GaussianMethod-ExpandW_Results.RData")

##Multivariate forecast distributions
DF_MultiV %>% dplyr::select(-`Energy score`, -`Variogram score`) %>% 
  filter(`Log score` != "NA") %>% 
  filter(`F-method` == "ETS") -> DF_Mult_LS_ETS

#View(DF_Mult_LS_ETS)

DF_Mult_LS_ETS %>% ggplot(aes(x = `Replication`, y = `Log score`, color = `R-method`)) + 
  geom_line() + facet_wrap(~`Forecast Horizon`, scales = "free_y")


##Univariate forecast distributions

DF_UniV %>% filter(`F-method` == "ETS" | `R-method` == "Base") %>%
  dplyr::filter(`F-method` != "ARIMA") %>% 
  dplyr::select(`Series`, `F-method`, `R-method`, `Forecast Horizon`, `LS`, `Replication`) -> DF_UniV_ETS_LS

# DF_UniV_ETS_E.LS %>% group_by(`Series`, `F-method`, `R-method`, `Forecast Horizon`) %>% 
#   summarise(E.LS = mean(`LS`, na.rm=T)) -> DF_UniV_ETS_E.LS

DF_UniV_ETS_LS %>% ungroup() %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`, `Replication`) %>% 
  summarise(Avg_LS = round(mean(`LS`), digits = 4)) -> DF_UniV_ETS_Avg.LS

DF_UniV_ETS_Avg.LS %>% ggplot(aes(x = `Replication`, y = `Avg_LS`, color = `R-method`)) + 
  geom_line() + facet_wrap(~ `Forecast Horizon`, scales = "free_y")
