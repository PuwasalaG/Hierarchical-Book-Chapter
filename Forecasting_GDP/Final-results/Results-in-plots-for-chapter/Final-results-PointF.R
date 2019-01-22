#This contains the code for creating results in plots. Here we present the results for ARIMA approach only. 
#ETS results were leftout from the report

require(tidyverse)
require(fpp2)
require(ggplot2)
require(gridExtra)
require(grid)
require(ggpubr)
setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Final-results/Results-in-plots-for-chapter")

## Point-forecasting - Income approach
rm(list = ls())
load("Point-forecasting/INC-PointForecasts-ExpandingW.RData")

#Results for most aggregate level

Method_Order <- c("Benchmark", "Base", "Bottom-up", "OLS", "WLS", "MinT Shrink")

#ARIMA
GDPI_PointF_arima <- Score_arima %>% dplyr::filter(`Series`=="Gdpi") 

GDPI_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> GDPI_ARIMA.base_MSE

GDPI_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> GDPI_ARIMA.base_MASE

GDPI_PointF_arima %>% mutate(SS_MSE = round((1-(`MSE`/GDPI_ARIMA.base_MSE))*100, digits = 2), 
                             SS_MASE = round((1-(`MASE`/GDPI_ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_GDPI_ARIMA 

SS.GDPI_ARIMA_MSE <- Skill.Score_GDPI_ARIMA %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPI_ARIMA_MSE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPI_ARIMA_MSE


SS.GDPI_ARIMA_MASE <- Skill.Score_GDPI_ARIMA %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPI_ARIMA_MASE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPI_ARIMA_MASE

SS.GDPI_ARIMA_MSE %>% left_join(SS.GDPI_ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  filter(Method != "Base") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> GDPI_PointF

GDPI_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                       Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values=0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + ggtitle("GDP(I)") -> INC_PointF_GDPI

##All aggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[1:6])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> Score_aggregates_ARIMA

Score_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ARIMA.base_MSE

Score_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ARIMA.base_MASE

Score_aggregates_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ARIMA.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_aggregates_ARIMA  


Skill.Score_aggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Aggregates.ARIMA_MSE

Aggregates.ARIMA_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Aggregates.ARIMA_MSE

Skill.Score_aggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Aggregates.ARIMA_MASE

Aggregates.ARIMA_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Aggregates.ARIMA_MASE

Aggregates.ARIMA_MSE %>% left_join(Aggregates.ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> Aggregate_PointF

Aggregate_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                            Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values = 0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + 
  ggtitle("Across all aggregate levels") -> INC_PointF_Aggregates

##All disaggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[7:16])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> Score_disaggregates_ARIMA

Score_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ARIMA.base_MSE

Score_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ARIMA.base_MASE

Score_disaggregates_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ARIMA.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_disaggregates_ARIMA  


Skill.Score_disaggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Disaggregates.ARIMA_MSE

Disaggregates.ARIMA_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Disaggregates.ARIMA_MSE

Skill.Score_disaggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Disaggregates.ARIMA_MASE

Disaggregates.ARIMA_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Disaggregates.ARIMA_MASE

Disaggregates.ARIMA_MSE %>% left_join(Disaggregates.ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> Disaggregate_PointF

Disaggregate_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                               Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values = 0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + 
  ggtitle("Across all bottom level series") -> INC_PointF_Disaggregates


##Across all levels

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> Score_all.series_ARIMA

Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ARIMA.base_MSE

Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ARIMA.base_MASE

Score_all.series_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ARIMA.base_MSE))*100, digits = 2), 
                                     SS_MASE = round((1-(`Avg_MASE`/ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_all.series_ARIMA  


Skill.Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> All.series.ARIMA_MSE

All.series.ARIMA_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> All.series.ARIMA_MSE

Skill.Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> All.series.ARIMA_MASE

All.series.ARIMA_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> All.series.ARIMA_MASE

All.series.ARIMA_MSE %>% left_join(All.series.ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> All.series_PointF

All.series_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                             Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values = 0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + 
  ggtitle("Across all levels") -> INC_PointF_All.series


ggarrange(INC_PointF_GDPI, INC_PointF_Aggregates, INC_PointF_Disaggregates, INC_PointF_All.series, ncol=2, nrow=2, 
          common.legend = TRUE, legend="bottom")






## Point-forecasting - Expenditure approach
rm(list = ls())
load("Point-forecasting/EXP-PointForecasts-ExpandingW.RData")

#Results for most aggregate level

Method_Order <- c("Benchmark", "Base", "Bottom-up", "OLS", "WLS", "MinT Shrink")

#ARIMA
GDPE_PointF_arima <- Score_arima %>% dplyr::filter(`Series`=="Gdpe") 

GDPE_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> GDPE_ARIMA.base_MSE

GDPE_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> GDPE_ARIMA.base_MASE

GDPE_PointF_arima %>% mutate(SS_MSE = round((1-(`MSE`/GDPE_ARIMA.base_MSE))*100, digits = 2), 
                             SS_MASE = round((1-(`MASE`/GDPE_ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_GDPE_ARIMA 

SS.GDPE_ARIMA_MSE <- Skill.Score_GDPE_ARIMA %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPE_ARIMA_MSE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPE_ARIMA_MSE


SS.GDPE_ARIMA_MASE <- Skill.Score_GDPE_ARIMA %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPE_ARIMA_MASE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPE_ARIMA_MASE

SS.GDPE_ARIMA_MSE %>% left_join(SS.GDPE_ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  filter(Method != "Base") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> GDPE_PointF

GDPE_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                       Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values = 0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + ggtitle("GDP(E)") -> EXP_PointF_GDPE

##All aggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Exp)[1:27])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> Score_EXP_aggregates_ARIMA

Score_EXP_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> EXP_ARIMA.base_MSE

Score_EXP_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> EXP_ARIMA.base_MASE

Score_EXP_aggregates_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/EXP_ARIMA.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/EXP_ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_EXP_aggregates_ARIMA  


Skill.Score_EXP_aggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_Aggregates.ARIMA_MSE

EXP_Aggregates.ARIMA_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> EXP_Aggregates.ARIMA_MSE

Skill.Score_EXP_aggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_Aggregates.ARIMA_MASE

EXP_Aggregates.ARIMA_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> EXP_Aggregates.ARIMA_MASE

EXP_Aggregates.ARIMA_MSE %>% left_join(EXP_Aggregates.ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> EXP_Aggregate_PointF

EXP_Aggregate_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                                Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values = 0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + 
  ggtitle("Across all aggregate levels") -> EXP_PointF_Aggregates

##All disaggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Exp)[28:80])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> Score_EXP_disaggregates_ARIMA

Score_EXP_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> EXP_ARIMA.base_MSE

Score_EXP_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> EXP_ARIMA.base_MASE

Score_EXP_disaggregates_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/EXP_ARIMA.base_MSE))*100, digits = 2), 
                                     SS_MASE = round((1-(`Avg_MASE`/EXP_ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_EXP_disaggregates_ARIMA  


Skill.Score_EXP_disaggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_Disaggregates.ARIMA_MSE

EXP_Disaggregates.ARIMA_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> EXP_Disaggregates.ARIMA_MSE

Skill.Score_EXP_disaggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_Disaggregates.ARIMA_MASE

EXP_Disaggregates.ARIMA_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> EXP_Disaggregates.ARIMA_MASE

EXP_Disaggregates.ARIMA_MSE %>% left_join(EXP_Disaggregates.ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> EXP_Disaggregate_PointF

EXP_Disaggregate_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                                   Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values = 0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + 
  ggtitle("Across all bottom level series") -> EXP_PointF_Disaggregates


##Across all levels

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> Score_EXP_all.series_ARIMA

Score_EXP_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> EXP_ARIMA.base_MSE

Score_EXP_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> EXP_ARIMA.base_MASE

Score_EXP_all.series_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/EXP_ARIMA.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/EXP_ARIMA.base_MASE))*100, digits = 2)) -> Skill.Score_EXP_all.series_ARIMA  


Skill.Score_EXP_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_All.series.ARIMA_MSE

EXP_All.series.ARIMA_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> EXP_All.series.ARIMA_MSE

Skill.Score_EXP_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_All.series.ARIMA_MASE

EXP_All.series.ARIMA_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> EXP_All.series.ARIMA_MASE

EXP_All.series.ARIMA_MSE %>% left_join(EXP_All.series.ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  gather(-Method, -h, key = "Scoring_rule", value = "Score") -> EXP_All.series_PointF

EXP_All.series_PointF %>% mutate(Scoring_rule = factor(Scoring_rule, levels = unique(Scoring_rule)),
                                 Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "Blue", "Purple", "brown")) +
  scale_shape_manual(values = 0:3) +
  facet_wrap(~ `Scoring_rule`, scales = "free_y") +  ylab("Skill score %") + 
  ggtitle("Across all levels") -> EXP_PointF_All.series


ggarrange(EXP_PointF_GDPE, EXP_PointF_Aggregates, EXP_PointF_Disaggregates, EXP_PointF_All.series, ncol=2, nrow=2, 
          common.legend = TRUE, legend="bottom")




####Naive vs Base



rm(list = ls())
#Income approach
load("Point-forecasting/INC-PointForecasts-ExpandingW.RData")

Method_Order <- c("Base", "Naive")

#For top level

#MSE
INC_GDPI_PointF_arima <- Score_arima %>% dplyr::filter(`Series`=="Gdpi") 

INC_GDPI_ARIMA_MSE <- INC_GDPI_PointF_arima %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

INC_GDPI_ARIMA_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> INC_GDPI_ARIMA_MSE


INC_GDPI_ARIMA_MSE %>% ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE ") + ggtitle("Top level")+ 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_GDPI_MSE


#MASE
INC_GDPI_ARIMA_MASE <- INC_GDPI_PointF_arima %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

INC_GDPI_ARIMA_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> INC_GDPI_ARIMA_MASE

INC_GDPI_ARIMA_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MASE ") + ggtitle("Top level")  + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_GDPI_MASE


##All levels 
Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> INC_Score_all.series_ARIMA

#MSE
INC_Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MSE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> INC_All.series.ARIMA_MSE

INC_All.series.ARIMA_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> INC_All.series.ARIMA_MSE

INC_All.series.ARIMA_MSE %>% ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE ") + ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_All.series_MSE

#MASE
INC_Score_all.series_ARIMA %>% dplyr::select(-`Avg_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MASE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> INC_All.series.ARIMA_MASE

INC_All.series.ARIMA_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> INC_All.series.ARIMA_MASE

INC_All.series.ARIMA_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MASE ") + ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_All.series_MASE



#Expenditure approach

rm(list=ls()[! ls() %in% c("Plot_INC_PointF_GDPI_MSE", "Plot_INC_PointF_GDPI_MASE", "Plot_INC_PointF_All.series_MSE",
                           "Plot_INC_PointF_All.series_MASE")])
load("Point-forecasting/EXP-PointForecasts-ExpandingW.RData")

Method_Order <- c("Base", "Naive")

#For top level - Income approach

#MSE
EXP_GDPE_PointF_arima <- Score_arima %>% dplyr::filter(`Series`=="Gdpe") 

EXP_GDPE_ARIMA_MSE <- EXP_GDPE_PointF_arima %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

EXP_GDPE_ARIMA_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> EXP_GDPE_ARIMA_MSE

EXP_GDPE_ARIMA_MSE %>% ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE ") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_GDPE_MSE

#MASE
EXP_GDPE_ARIMA_MASE <- EXP_GDPE_PointF_arima %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

EXP_GDPE_ARIMA_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> EXP_GDPE_ARIMA_MASE

EXP_GDPE_ARIMA_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MASE ") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_GDPE_MASE


##All levels

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> EXP_Score_all.series_ARIMA

#MSE
EXP_Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MSE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_All.series.ARIMA_MSE

EXP_All.series.ARIMA_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> EXP_All.series.ARIMA_MSE

EXP_All.series.ARIMA_MSE %>% ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE ") + ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_All.series_MSE

#MASE
EXP_Score_all.series_ARIMA %>% dplyr::select(-`Avg_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MASE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_All.series.ARIMA_MASE

EXP_All.series.ARIMA_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> EXP_All.series.ARIMA_MASE

EXP_All.series.ARIMA_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MASE ") + ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_All.series_MASE



g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend_MSE <- g_legend(Plot_INC_PointF_GDPI_MSE)

grid.arrange( arrangeGrob(Plot_INC_PointF_All.series_MSE + theme(legend.position="none"), 
                          Plot_INC_PointF_GDPI_MSE + theme(legend.position="none"), top="Income"), 
              arrangeGrob(Plot_EXP_PointF_All.series_MSE + theme(legend.position="none"), 
                          Plot_EXP_PointF_GDPE_MSE + theme(legend.position="none"), top="Expenditure"), 
              ncol=2, mylegend_MSE, heights=c(10, 1))

mylegend_MASE <- g_legend(Plot_INC_PointF_GDPI_MASE)

grid.arrange( arrangeGrob(Plot_INC_PointF_All.series_MASE + theme(legend.position="none"), 
                          Plot_INC_PointF_GDPI_MASE + theme(legend.position="none"), top="Income"), 
              arrangeGrob(Plot_EXP_PointF_All.series_MASE + theme(legend.position="none"), 
                          Plot_EXP_PointF_GDPE_MASE + theme(legend.position="none"), top="Expenditure"), 
              ncol=2, mylegend_MASE, heights=c(10, 1))
