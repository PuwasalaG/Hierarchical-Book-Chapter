
library(tidyverse)
library(ggplot2)

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
