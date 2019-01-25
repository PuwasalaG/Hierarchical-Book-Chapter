library(tidyverse)
library(ggplot2)
library(fpp2)
library(zoo)
library(gridExtra)
library(ggpubr)

load("Income-approach/ProbForecasts-BootstrapApproach-ExpandingW.RData")

colnames(Unrecon_future_paths_ARIMA[[1]]) <- c(names(Inc))
Unrecon_future_paths_ARIMA[[1]] %>% as.data.frame() %>%  gather(key = "Series", value = "Prob_forecasts") %>% 
  ggplot(aes(x = Prob_forecasts)) + geom_density( bw = 100) + facet_wrap(~Series, scales = "free")
