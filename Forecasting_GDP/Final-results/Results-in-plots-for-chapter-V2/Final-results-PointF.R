#This contains the code for creating results in plots. Here we present the results for ARIMA approach only. 
#ETS results were leftout from the report

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
  filter(Method != "Base") -> GDPI_PointF

GDPI_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MSE) %") + ggtitle("Top level") +
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_GDPI_MSE

GDPI_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MASE) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_GDPI_MASE

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
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Aggregate_PointF

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Aggregate levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_Aggregates_MSE

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
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
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Disaggregate_PointF

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Bottom level") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_blank()) -> INC_PointF_Disaggregates_MSE

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
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
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> All.series_PointF

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> INC_PointF_All.series_MSE

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
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
  filter(Method != "Base") -> GDPE_PointF

GDPE_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MSE) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_GDPE_MSE

GDPE_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values=0:3) +
  ylab("Skill score (MASE) %") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_GDPE_MASE

##All aggregate summary

Score_arima %>% dplyr::filter(`Series` %in% c(names(Exp)[1:27])) %>%
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
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Aggregate_PointF

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Aggregate levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_Aggregates_MSE

Aggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
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

Score_arima %>% dplyr::filter(`Series` %in% c(names(Exp)[28:80])) %>%
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
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> Disaggregate_PointF

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("Bottom level") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_Disaggregates_MSE

Disaggregate_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
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
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> All.series_PointF

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (MSE) %") + 
  ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic"))+
  theme(axis.title.y = element_blank()) -> EXP_PointF_All.series_MSE

All.series_PointF %>% mutate(Method = factor(Method, levels = c("MinT Shrink", "WLS", "OLS", "Bottom-up"))) %>% 
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


grid.arrange( arrangeGrob(INC_PointF_All.series_MSE+ theme(legend.position="none"), 
                          INC_PointF_GDPI_MSE + theme(legend.position="none"),
                          INC_PointF_Aggregates_MSE + theme(legend.position="none"), 
                          INC_PointF_Disaggregates_MSE + theme(legend.position="none"), top="Income", ncol = 1), 
              arrangeGrob(EXP_PointF_All.series_MSE + theme(legend.position="none"),
                          EXP_PointF_GDPE_MSE + theme(legend.position="none"),
                          EXP_PointF_Aggregates_MSE+ theme(legend.position="none"), 
                          EXP_PointF_Disaggregates_MSE + theme(legend.position="none"), top="Expenditure", ncol = 1), 
              left = textGrob("Skill score (MSE) %", rot = 90, vjust = 1),
              ncol=2, legend, heights=c(10, 1))

grid.arrange( arrangeGrob(INC_PointF_All.series_MASE+ theme(legend.position="none"), 
                          INC_PointF_GDPI_MASE + theme(legend.position="none"),
                          INC_PointF_Aggregates_MASE + theme(legend.position="none"), 
                          INC_PointF_Disaggregates_MASE + theme(legend.position="none"), top="Income"), 
              arrangeGrob(EXP_PointF_All.series_MASE + theme(legend.position="none"),
                          EXP_PointF_GDPE_MASE + theme(legend.position="none"),
                          EXP_PointF_Aggregates_MASE+ theme(legend.position="none"), 
                          EXP_PointF_Disaggregates_MASE + theme(legend.position="none"), top="Expenditure"), 
              left = textGrob("Skill score (MASE) %", rot = 90, vjust = 1),
              ncol=2, legend, heights=c(10, 1))


ggarrange(INC_PointF_All.series_MSE, INC_PointF_GDPI_MSE, INC_PointF_Aggregates_MSE, 
          INC_PointF_Disaggregates_MSE, ncol = 1, nrow = 4, common.legend = TRUE, legend = "none", 
          align = "v") -> p1_MSE

ggarrange(EXP_PointF_All.series_MSE, EXP_PointF_GDPE_MSE, EXP_PointF_Aggregates_MSE, 
          EXP_PointF_Disaggregates_MSE, ncol = 1, nrow = 4, common.legend = TRUE, legend = "none", 
          align = "v") -> p2_MSE







grid.arrange( arrangeGrob(p1_MSE, top="Income"), 
              arrangeGrob(p2_MSE+ theme(legend.position="none"), top="Expenditure"), 
              left = textGrob("Skill score (MSE) %", rot = 90, vjust = 1),
              ncol=2, legend, heights=c(10, 1))



ggarrange(INC_PointF_All.series_MASE, INC_PointF_GDPI_MASE, INC_PointF_Aggregates_MASE, 
          INC_PointF_Disaggregates_MASE, ncol = 1, nrow = 4, common.legend = TRUE, legend = "none", 
          align = "v") -> p1_MASE

ggarrange(EXP_PointF_All.series_MASE, EXP_PointF_GDPE_MASE, EXP_PointF_Aggregates_MASE, 
          EXP_PointF_Disaggregates_MASE, ncol = 1, nrow = 4, common.legend = TRUE, legend = "none", 
          align = "v") -> p2_MASE


grid.arrange( arrangeGrob(p1_MASE, top="Income", left = textGrob("Skill score (MASE) %", rot = 90, vjust = 1)), 
              arrangeGrob(p2_MASE+ theme(legend.position="none"), top="Expenditure"), 
              ncol=2, legend, heights=c(10, 1))


grid.arrange( arrangeGrob(Plot_INC_MultivS_Gauss + theme(legend.position="none"), 
                          Plot_INC_MultivS_NonPara + theme(legend.position="none"), top="Income"), 
              arrangeGrob(Plot_EXP_MultivS_Gauss + theme(legend.position="none"), 
                          Plot_EXP_MultivS_NonPara + theme(legend.position="none"), top="Expenditure"), 
              ncol=2, mylegend, heights=c(10, 1))



