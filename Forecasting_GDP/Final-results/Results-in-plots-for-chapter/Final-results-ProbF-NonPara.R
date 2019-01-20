#This contains the code for creating results in plots. Here we present the results for ARIMA approach only. 
#ETS results were leftout from the report

require(tidyverse)
require(fpp2)
require(ggplot2)
require(gridExtra)
require(grid)
require(ggpubr)
setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Final-results/Results-in-plots-for-chapter")

## NonPara-Probabilistic-forecasting - Income approach

rm(list = ls())
load("Prob-forecasting/INC-ProbForecasts-BootstrapApproach-ExpandingW.RData")

#Scores for Multivariate forecasts 

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT Shrink")

SS_E.ES_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 4, 7), c("Bottom-up", "MinT Sample", "MinT Shrink", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> INC_ARIMA_ES

INC_ARIMA_ES %>% gather(-Method, key = "h", value = "ES") -> INC_ARIMA_ES

SS_E.VS_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 4, 7), c("Bottom-up", "MinT Sample", "MinT Shrink", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>% 
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> INC_ARIMA_VS

INC_ARIMA_VS %>% gather(-Method, key = "h", value = "ES") -> INC_ARIMA_VS

INC_ARIMA_ES %>% left_join(INC_ARIMA_VS, by = c("Method", "h")) %>% 
  rename("ES" = "ES.x", "VS" = "ES.y") %>% 
  filter(Method != "Base") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> INC_MultivS_ES_VS

#Plot for ES only
INC_ARIMA_ES %>% ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) + 
  ylab("Skill score %") + ggtitle("Energy Score")  -> Plot_INC_MultivS_ES

#Plot for ES and VS
INC_MultivS_ES_VS %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule))) %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") -> Plot_INC_MultivS_ES_VS



#Scores for Univariate forecasts 

#Summary of most aggregate series

#ARIMA
Score_arima %>% dplyr::filter(`Series`=="Gdpi" ) -> Score_GDPI_ARIMA

Score_GDPI_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MCRPS`) %>% as_vector() -> GDPI_ARIMA.base_CRPS

Score_GDPI_ARIMA %>% mutate(SS_CRPS = round((1-(`MCRPS`/GDPI_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_GDPI_ARIMA 


Skill.Score_GDPI_ARIMA %>% dplyr::select(-`MCRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`, -`Series`) %>%  
  mutate(`R-method` = replace(`R-method`, c(7,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> GDPI.ARIMA_CRPS

GDPI.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("GDP(I)") -> INC_GDPI_ProbF_NonPara 


#All aggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[1:6])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 2)) -> Score_INC_aggregates_ARIMA

Score_INC_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> INC_ARIMA.base_CRPS

Score_INC_aggregates_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/INC_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_INC_aggregates_ARIMA  


Skill.Score_INC_aggregates_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(7,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> INC_Aggregates.ARIMA_CRPS

INC_Aggregates.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("Across all aggregate levels") -> INC_Aggregate_ProbF_NonPara 


#All disaggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[7:16])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 2)) -> Score_INC_disaggregates_ARIMA

Score_INC_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> INC_ARIMA.base_CRPS

Score_INC_disaggregates_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/INC_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_INC_disaggregates_ARIMA  


Skill.Score_INC_disaggregates_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(7,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> INC_Disaggregates.ARIMA_CRPS

INC_Disaggregates.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("Across all bottom level series") -> INC_Disaggregate_ProbF_NonPara 


#All series summary

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 2)) -> Score_INC_All.series_ARIMA

Score_INC_All.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> INC_ARIMA.base_CRPS

Score_INC_All.series_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/INC_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_INC_All.series_ARIMA  


Skill.Score_INC_All.series_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(7,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> INC_All.series.ARIMA_CRPS

INC_All.series.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("Across all levels") -> INC_All.series_ProbF_NonPara 


ggarrange(INC_GDPI_ProbF_NonPara, INC_Aggregate_ProbF_NonPara, INC_Disaggregate_ProbF_NonPara, INC_All.series_ProbF_NonPara, 
          ncol=2, nrow=2, common.legend = TRUE, legend="bottom")




## Nonparametric-Probabilistic-forecasting - Expenditure approach

rm(list = ls())
load("Prob-forecasting/EXP-ProbForecasts-BootstrapApproach-ExpandingW.RData")

#Scores for Multivariate forecasts 

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT Shrink")

SS_E.ES_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 6), c("Bottom-up", "MinT Shrink", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> EXP_ARIMA_ES

EXP_ARIMA_ES %>% gather(-Method, key = "h", value = "ES") -> EXP_ARIMA_ES

SS_E.VS_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 6), c("Bottom-up", "MinT Shrink", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>% 
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> EXP_ARIMA_VS

EXP_ARIMA_VS %>% gather(-Method, key = "h", value = "ES") -> EXP_ARIMA_VS

EXP_ARIMA_ES %>% left_join(EXP_ARIMA_VS, by = c("Method", "h")) %>% 
  rename("ES" = "ES.x", "VS" = "ES.y") %>% 
  filter(Method != "Base") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> EXP_MultivS_ES_VS

#Plot for ES only
EXP_ARIMA_ES %>% ggplot(aes(x = h, y = ES, color = Method)) + 
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) + 
  ylab("Skill score %") + ggtitle("Energy Score")  -> Plot_EXP_MultivS_ES

#Plot for ES and VS
EXP_MultivS_ES_VS %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule))) %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") -> Plot_EXP_MultivS_ES_VS



#Scores for Univariate forecasts 

#Summary of most aggregate series

#ARIMA
Score_arima %>% dplyr::filter(`Series`=="Gdpe" ) -> Score_GDPE_ARIMA

Score_GDPE_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MCRPS`) %>% as_vector() -> GDPE_ARIMA.base_CRPS

Score_GDPE_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`MCRPS`/GDPE_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_GDPE_ARIMA 


Skill.Score_GDPE_ARIMA %>% dplyr::select(-`MCRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`, -`Series`) %>%  
  mutate(`R-method` = replace(`R-method`, c(6,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> GDPE.ARIMA_CRPS

GDPE.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("GDP(E)") -> EXP_GDPE_ProbF_NonPara 


#All aggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Exp)[1:27])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 2)) -> Score_EXP_aggregates_ARIMA

Score_EXP_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> EXP_ARIMA.base_CRPS

Score_EXP_aggregates_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/EXP_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_EXP_aggregates_ARIMA  


Skill.Score_EXP_aggregates_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(6,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> EXP_Aggregates.ARIMA_CRPS

EXP_Aggregates.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("Across all aggregate levels") -> EXP_Aggregate_ProbF_NonPara 


#All disaggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Exp)[28:80])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 2)) -> Score_EXP_disaggregates_ARIMA

Score_EXP_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> EXP_ARIMA.base_CRPS

Score_EXP_disaggregates_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/EXP_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_EXP_disaggregates_ARIMA  


Skill.Score_EXP_disaggregates_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(6,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> EXP_Disaggregates.ARIMA_CRPS

EXP_Disaggregates.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("Across all bottom level series") -> EXP_Disaggregate_ProbF_NonPara 


#All series summary

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 2)) -> Score_EXP_All.series_ARIMA

Score_EXP_All.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> EXP_ARIMA.base_CRPS

Score_EXP_All.series_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/EXP_ARIMA.base_CRPS))*100, digits = 2)) -> Skill.Score_EXP_All.series_ARIMA  


Skill.Score_EXP_All.series_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(6,2), c("Benchmark", "Bottom-up"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> EXP_All.series.ARIMA_CRPS

EXP_All.series.ARIMA_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  ggplot(aes(x = h, y = Score, color = Method)) +
  geom_point(shape = 18, size = 3) + 
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Red", "green", "Blue", "Purple")) +
  ylab("Skill score %") + ggtitle("Across all levels") -> EXP_All.series_ProbF_NonPara 


ggarrange(EXP_GDPE_ProbF_NonPara, EXP_Aggregate_ProbF_NonPara, EXP_Disaggregate_ProbF_NonPara, EXP_All.series_ProbF_NonPara, 
          ncol=2, nrow=2, common.legend = TRUE, legend="bottom")






