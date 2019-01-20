
library(tidyverse)
library(ggplot2)
library(fpp2)
library(zoo)
library(gridExtra)

setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Final-results")

load("Income-approach/ProbForecasts-GaussianApproach-ExpandingW.RData")

### Log score is inflated and ES and VS are not

DF_MultiV %>% dplyr::select(-`Energy score`, -`Variogram score`) %>% 
  filter(`Log score` != "NA") %>% 
  filter(`F-method` == "ETS") -> DF_Mult_LS_ETS

DF_Mult_LS_ETS %>% ggplot(aes(x = `Replication`, y = `Log score`, color = `R-method`)) + 
  geom_line() + facet_wrap(~`Forecast Horizon`, scales = "free_y") + ggtitle("Multivariate log scores for ETS method")

DF_MultiV %>% dplyr::select(-`Energy score`, -`Variogram score`) %>% 
  filter(`Log score` != "NA") %>% 
  filter(`F-method` == "ARIMA") -> DF_Mult_LS_ARIMA

DF_Mult_LS_ARIMA %>% ggplot(aes(x = `Replication`, y = `Log score`, color = `R-method`)) + 
  geom_line() + facet_wrap(~`Forecast Horizon`, scales = "free_y") + ggtitle("Multivariate log scores for ARIMA method")


DF_MultiV %>% filter(`Log score` != "NA") %>%
  filter(`Forecast Horizon` == 1) %>%
  filter(`R-method` == "MinT.Shr") %>%
  gather(`Energy score`, `Log score`, `Variogram score`, key = "Scoring rule", value = "Score") %>%
  ggplot(aes(x = `Replication`, y = `Score`)) + 
  geom_line() + facet_wrap(~`Scoring rule`, scales = "free_y") 



### Time series plots for Income and Expenditure approach

load("Income-approach/PointForecasts-ExpandingW.RData")

Time <- as.yearqtr(1984 + seq(0, 136)/4)[-(1:3)]

Inc_to_plot <- cbind(Time, Inc) %>% as.tibble() %>% 
  gather(-`Time`, key = "Series_name", value = "Value") %>%  
  mutate(Series_name = factor(Series_name, levels = names(Inc)))

Inc_to_plot %>% filter(`Series_name` %in% c(names(Inc)[1:6])) %>% 
  ggplot(aes(x = `Time`, y = `Value`)) + geom_line() + scale_x_continuous(breaks = seq(1984,2018,15)) + 
  facet_wrap(~Series_name, scales = "free_y") + ylab("$ million") -> Inc_aggre

Inc_to_plot %>% filter(`Series_name` %in% c(names(Inc)[7:16])) %>% 
  ggplot(aes(x = `Time`, y = `Value`)) + geom_line() + scale_x_continuous(breaks = seq(1984,2018,15)) + 
  facet_wrap(~Series_name, scales = "free_y") + ylab("$ million") -> Inc_disaggre

grid.arrange(Inc_aggre, Inc_disaggre)

load("Expenditure-approach/PointForecasts-ExpandingW.RData")

Time <- as.yearqtr(1984 + seq(0, 136)/4)[-(1:3)]

Exp_to_plot <- cbind(Time, Exp) %>% as.tibble() %>% 
  gather(-`Time`, key = "Series_name", value = "Value") %>%  
  mutate(Series_name = factor(Series_name, levels = names(Exp))) 

Exp_to_plot %>% filter(`Series_name` %in% c(names(Exp)[1:27])) %>% 
  ggplot(aes(x = `Time`, y = `Value`)) + geom_line() + scale_x_continuous(breaks = seq(1984,2018,15)) + 
  facet_wrap(~Series_name, scales = "free_y") + ylab("$ million") -> Exp_aggre

Exp_to_plot %>% filter(`Series_name` %in% c(names(Exp)[28:80])) %>% 
  ggplot(aes(x = `Time`, y = `Value`)) + geom_line() + scale_x_continuous(breaks = seq(1984,2018,15)) + 
  facet_wrap(~Series_name, scales = "free_y") + ylab("$ million") -> Exp_disaggre

grid.arrange(Exp_aggre, Exp_disaggre)
