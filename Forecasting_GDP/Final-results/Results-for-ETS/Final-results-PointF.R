#This contains the code for creating results in plots. Here we present the results for ETS approach only. 
#ETS results were leftout from the report

#Results summarised for ETS based point forecasts

require(tidyverse)
require(fpp2)
require(ggplot2)
require(gridExtra)
require(grid)
require(ggpubr)
require(gtable)
setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Final-results/Results-in-plots-for-chapter")

## Point-forecasting - Income approach

rm(list = ls())
load("Results-Point-forecasts/INC-PointForecasts-ExpandingW.RData")

#Results for most aggregate level

Method_Order <- c("Benchmark", "Base", "Bottom-up", "OLS", "WLS", "MinT(Shrink)")

#ets
GDPI_PointF_ets <- Score_ets %>% dplyr::filter(`Series`=="Gdpi") 

GDPI_PointF_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> GDPI_ets.base_MSE

GDPI_PointF_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> GDPI_ets.base_MASE

GDPI_PointF_ets %>% mutate(SS_MSE = round((1-(`MSE`/GDPI_ets.base_MSE))*100, digits = 2), 
                             SS_MASE = round((1-(`MASE`/GDPI_ets.base_MASE))*100, digits = 2)) -> Skill.Score_GDPI_ets 

SS.GDPI_ets_MSE <- Skill.Score_GDPI_ets %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPI_ets_MSE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPI_ets_MSE


SS.GDPI_ets_MASE <- Skill.Score_GDPI_ets %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPI_ets_MASE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPI_ets_MASE

SS.GDPI_ets_MSE %>% left_join(SS.GDPI_ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  filter(Method != "Base") -> GDPI_PointF

GDPI_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MSE) %") + ggtitle("Top level") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_GDPI_MSE

GDPI_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MASE) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_GDPI_MASE

##All aggregate summary

Score_ets %>% dplyr::filter(`Series` %in% c(names(Inc)[1:6])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = mean(`MSE`), Avg_MASE = mean(`MASE`)) -> Score_aggregates_ets

Score_aggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ets.base_MSE

Score_aggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ets.base_MASE

Score_aggregates_ets %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ets.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/ets.base_MASE))*100, digits = 2)) -> Skill.Score_aggregates_ets  


Skill.Score_aggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Aggregates.ets_MSE

Aggregates.ets_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Aggregates.ets_MSE

Skill.Score_aggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Aggregates.ets_MASE

Aggregates.ets_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Aggregates.ets_MASE

Aggregates.ets_MSE %>% left_join(Aggregates.ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Aggregate_PointF

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Aggregate levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_Aggregates_MSE

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MASE) %") + 
  ggtitle("Aggregate levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_Aggregates_MASE

##All disaggregate summary

Score_ets %>% dplyr::filter(`Series` %in% c(names(Inc)[7:16])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = mean(`MSE`), Avg_MASE = mean(`MASE`)) -> Score_disaggregates_ets

Score_disaggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ets.base_MSE

Score_disaggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ets.base_MASE

Score_disaggregates_ets %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ets.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/ets.base_MASE))*100, digits = 2)) -> Skill.Score_disaggregates_ets  


Skill.Score_disaggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Disaggregates.ets_MSE

Disaggregates.ets_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Disaggregates.ets_MSE

Skill.Score_disaggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Disaggregates.ets_MASE

Disaggregates.ets_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Disaggregates.ets_MASE

Disaggregates.ets_MSE %>% left_join(Disaggregates.ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Disaggregate_PointF

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Bottom level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_Disaggregates_MSE

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MASE) %") + 
  ggtitle("Bottom level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank())  -> INC_PointF_Disaggregates_MASE

##Across all levels

Score_ets %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = mean(`MSE`), Avg_MASE = mean(`MASE`)) -> Score_all.series_ets

Score_all.series_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ets.base_MSE

Score_all.series_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ets.base_MASE

Score_all.series_ets %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ets.base_MSE))*100, digits = 2), 
                                     SS_MASE = round((1-(`Avg_MASE`/ets.base_MASE))*100, digits = 2)) -> Skill.Score_all.series_ets  


Skill.Score_all.series_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> All.series.ets_MSE

All.series.ets_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> All.series.ets_MSE

Skill.Score_all.series_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> All.series.ets_MASE

All.series.ets_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> All.series.ets_MASE

All.series.ets_MSE %>% left_join(All.series.ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> All.series_PointF

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> INC_PointF_All.series_MSE

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MASE) %") + 
  ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> INC_PointF_All.series_MASE



## Point-forecasting - Expenditure approach

rm(list=ls()[! ls() %in% c("INC_PointF_GDPI_MSE", "INC_PointF_Aggregates_MSE", "INC_PointF_Disaggregates_MSE", 
                           "INC_PointF_All.series_MSE", "INC_PointF_GDPI_MASE", "INC_PointF_Aggregates_MASE", 
                           "INC_PointF_Disaggregates_MASE", "INC_PointF_All.series_MASE")])

load("Results-Point-forecasts/EXP-PointForecasts-ExpandingW.RData")

#Results for most aggregate level

Method_Order <- c("Benchmark", "Base", "Bottom-up", "OLS", "WLS", "MinT(Shrink)")

#ets
GDPE_PointF_ets <- Score_ets %>% dplyr::filter(`Series`=="Gdpe") 

GDPE_PointF_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> GDPE_ets.base_MSE

GDPE_PointF_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> GDPE_ets.base_MASE

GDPE_PointF_ets %>% mutate(SS_MSE = round((1-(`MSE`/GDPE_ets.base_MSE))*100, digits = 2), 
                             SS_MASE = round((1-(`MASE`/GDPE_ets.base_MASE))*100, digits = 2)) -> Skill.Score_GDPE_ets 

SS.GDPE_ets_MSE <- Skill.Score_GDPE_ets %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPE_ets_MSE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPE_ets_MSE


SS.GDPE_ets_MASE <- Skill.Score_GDPE_ets %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

SS.GDPE_ets_MASE %>% filter(Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> SS.GDPE_ets_MASE

SS.GDPE_ets_MSE %>% left_join(SS.GDPE_ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") %>% 
  filter(Method != "Base") -> GDPE_PointF

GDPE_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MSE) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_GDPE_MSE

GDPE_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MASE) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_GDPE_MASE

##All aggregate summary

Score_ets %>% dplyr::filter(`Series` %in% c(names(Exp)[1:27])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = mean(`MSE`), Avg_MASE = mean(`MASE`)) -> Score_aggregates_ets

Score_aggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ets.base_MSE

Score_aggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ets.base_MASE

Score_aggregates_ets %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ets.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/ets.base_MASE))*100, digits = 2)) -> Skill.Score_aggregates_ets  


Skill.Score_aggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Aggregates.ets_MSE

Aggregates.ets_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Aggregates.ets_MSE

Skill.Score_aggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Aggregates.ets_MASE

Aggregates.ets_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Aggregates.ets_MASE

Aggregates.ets_MSE %>% left_join(Aggregates.ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Aggregate_PointF

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Aggregate levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_Aggregates_MSE

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MASE) %") + 
  ggtitle("Aggregate levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_Aggregates_MASE

##All disaggregate summary

Score_ets %>% dplyr::filter(`Series` %in% c(names(Exp)[28:80])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = mean(`MSE`), Avg_MASE = mean(`MASE`)) -> Score_disaggregates_ets

Score_disaggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ets.base_MSE

Score_disaggregates_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ets.base_MASE

Score_disaggregates_ets %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ets.base_MSE))*100, digits = 2), 
                                     SS_MASE = round((1-(`Avg_MASE`/ets.base_MASE))*100, digits = 2)) -> Skill.Score_disaggregates_ets  


Skill.Score_disaggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Disaggregates.ets_MSE

Disaggregates.ets_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Disaggregates.ets_MSE

Skill.Score_disaggregates_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> Disaggregates.ets_MASE

Disaggregates.ets_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> Disaggregates.ets_MASE

Disaggregates.ets_MSE %>% left_join(Disaggregates.ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Disaggregate_PointF

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Bottom level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_Disaggregates_MSE

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MASE) %") + 
  ggtitle("Bottom level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_Disaggregates_MASE

##Across all levels

Score_ets %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = mean(`MSE`), Avg_MASE = mean(`MASE`)) -> Score_all.series_ets

Score_all.series_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ets.base_MSE

Score_all.series_ets %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ets.base_MASE

Score_all.series_ets %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ets.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/ets.base_MASE))*100, digits = 2)) -> Skill.Score_all.series_ets  


Skill.Score_all.series_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> All.series.ets_MSE

All.series.ets_MSE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> All.series.ets_MSE

Skill.Score_all.series_ets %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1,4), c("Benchmark", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> All.series.ets_MASE

All.series.ets_MASE %>% filter(Method != "Base", Method != "Benchmark") %>% 
  gather(-`Method`, key = "h", value = "MSE") -> All.series.ets_MASE

All.series.ets_MSE %>% left_join(All.series.ets_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> All.series_PointF

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_All.series_MSE

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MASE) %") + 
  ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_All.series_MASE

rm(list=ls()[! ls() %in% c("INC_PointF_GDPI_MSE", "INC_PointF_Aggregates_MSE", "INC_PointF_Disaggregates_MSE", 
                           "INC_PointF_All.series_MSE", "INC_PointF_GDPI_MASE", "INC_PointF_Aggregates_MASE", 
                           "INC_PointF_Disaggregates_MASE", "INC_PointF_All.series_MASE",
                           "EXP_PointF_GDPE_MSE", "EXP_PointF_Aggregates_MSE", "EXP_PointF_Disaggregates_MSE", 
                           "EXP_PointF_All.series_MSE", "EXP_PointF_GDPE_MASE", "EXP_PointF_Aggregates_MASE", 
                           "EXP_PointF_Disaggregates_MASE", "EXP_PointF_All.series_MASE")])


 # g_legend<-function(a.gplot){
 #   tmp <- ggplot_gtable(ggplot_build(a.gplot))
 #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
 #   legend <- tmp$grobs[[leg]]
 #   return(legend)}
 # 
 # mylegend_MSE <- g_legend(INC_PointF_GDPI_MSE)

legend = gtable_filter(ggplotGrob(INC_PointF_GDPI_MSE), "guide-box")


grid.arrange( arrangeGrob(INC_PointF_All.series_MSE + theme(legend.position="none"), 
                          INC_PointF_GDPI_MSE + theme(legend.position="none"), 
                          INC_PointF_Aggregates_MSE + theme(legend.position="none"), 
                          INC_PointF_Disaggregates_MSE + theme(legend.position="none"), top="Income", ncol = 1),
              arrangeGrob(EXP_PointF_All.series_MSE + theme(legend.position="none"), 
                          EXP_PointF_GDPE_MSE + theme(legend.position="none"), 
                          EXP_PointF_Aggregates_MSE + theme(legend.position="none"), 
                          EXP_PointF_Disaggregates_MSE + theme(legend.position="none"), top="Expenditure", 
                          ncol = 1),
              left = textGrob("Skill score (MSE) %", rot = 90, vjust = 1), ncol=2, legend, heights=c(10, 1))


grid.arrange( arrangeGrob(INC_PointF_All.series_MASE + theme(legend.position="none"), 
                          INC_PointF_GDPI_MASE + theme(legend.position="none"), 
                          INC_PointF_Aggregates_MASE + theme(legend.position="none"), 
                          INC_PointF_Disaggregates_MASE + theme(legend.position="none"), top="Income", ncol = 1),
              arrangeGrob(EXP_PointF_All.series_MASE + theme(legend.position="none"), 
                          EXP_PointF_GDPE_MASE + theme(legend.position="none"), 
                          EXP_PointF_Aggregates_MASE + theme(legend.position="none"), 
                          EXP_PointF_Disaggregates_MASE + theme(legend.position="none"), top="Expenditure", 
                          ncol = 1),
              left = textGrob("Skill score (MASE) %", rot = 90, vjust = 1), ncol=2, legend, heights=c(10, 1))

