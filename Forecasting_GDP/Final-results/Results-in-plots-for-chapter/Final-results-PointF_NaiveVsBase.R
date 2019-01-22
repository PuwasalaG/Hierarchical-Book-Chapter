require(tidyverse)
require(fpp2)
require(ggplot2)
require(gridExtra)
require(grid)
require(ggpubr)
setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Final-results/Results-in-plots-for-chapter")


#Naive vs Base based skill scores

## Point-forecasting - Income approach
rm(list = ls())
load("Point-forecasting/INC-PointForecasts-ExpandingW.RData")
 

#Results for most aggregate level

Method_Order <- c("Naive")

#ARIMA
INC_GDPI_PointF_arima <- Score_arima %>% dplyr::filter(`Series`=="Gdpi") 

INC_GDPI_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> INC_GDPI_ARIMA.base_MSE

INC_GDPI_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> INC_GDPI_ARIMA.base_MASE

INC_GDPI_PointF_arima %>% mutate(SS_MSE = round((1-(`MSE`/INC_GDPI_ARIMA.base_MSE))*100, digits = 2), 
                             SS_MASE = round((1-(`MASE`/INC_GDPI_ARIMA.base_MASE))*100, digits = 2)) -> INC_Skill.Score_GDPI_ARIMA 

INC_SS.GDPI_ARIMA_MSE <- INC_Skill.Score_GDPI_ARIMA %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

INC_SS.GDPI_ARIMA_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> INC_SS.GDPI_ARIMA_MSE


INC_SS.GDPI_ARIMA_MASE <- INC_Skill.Score_GDPI_ARIMA %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

INC_SS.GDPI_ARIMA_MASE %>% gather(-`Method`, key = "h", value = "MSE") -> INC_SS.GDPI_ARIMA_MASE

INC_SS.GDPI_ARIMA_MSE %>% left_join(INC_SS.GDPI_ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> INC_GDPI_PointF

INC_GDPI_PointF %>% ggplot(aes(x = h, y = MSE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  ylab("Skill score (MSE) % ") + ggtitle("Top level")+ 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_GDPI_MSE

INC_GDPI_PointF %>% ggplot(aes(x = h, y = MASE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  ylab("Skill score (MASE) % ") + ggtitle("Top level")+ 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_GDPI_MASE


##Across all levels

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> INC_Score_all.series_ARIMA

INC_Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> INC_ARIMA.base_MSE

INC_Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> INC_ARIMA.base_MASE

INC_Score_all.series_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/INC_ARIMA.base_MSE))*100, digits = 2), 
                                  SS_MASE = round((1-(`Avg_MASE`/INC_ARIMA.base_MASE))*100, digits = 2)) -> INC_Skill.Score_all.series_ARIMA  


INC_Skill.Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`) %>% 
  filter(`F-method` == "Benchmark") %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) -> INC_All.series.ARIMA_SS

INC_All.series.ARIMA_SS %>% ggplot(aes(x = `Forecast Horizon`, y = SS_MSE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  
  ylab("Skill score (MSE) % ") + xlab("h") + ggtitle("All levels") + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_All.series_MSE

INC_All.series.ARIMA_SS %>% ggplot(aes(x = `Forecast Horizon`, y = SS_MASE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  
  ylab("Skill score (MASE) % ") + xlab("h") + ggtitle("All levels") + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_All.series_MASE




#Expenditure approach

rm(list=ls()[! ls() %in% c("Plot_INC_PointF_GDPI_MSE", "Plot_INC_PointF_GDPI_MASE", "Plot_INC_PointF_All.series_MSE",
                           "Plot_INC_PointF_All.series_MASE")])
load("Point-forecasting/EXP-PointForecasts-ExpandingW.RData")

Method_Order <- c("Naive")

#ARIMA
EXP_GDPE_PointF_arima <- Score_arima %>% dplyr::filter(`Series`=="Gdpe") 

EXP_GDPE_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> EXP_GDPE_ARIMA.base_MSE

EXP_GDPE_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> EXP_GDPE_ARIMA.base_MASE

EXP_GDPE_PointF_arima %>% mutate(SS_MSE = round((1-(`MSE`/EXP_GDPE_ARIMA.base_MSE))*100, digits = 2), 
                                 SS_MASE = round((1-(`MASE`/EXP_GDPE_ARIMA.base_MASE))*100, digits = 2)) -> EXP_Skill.Score_GDPE_ARIMA 

EXP_SS.GDPE_ARIMA_MSE <- EXP_Skill.Score_GDPE_ARIMA %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

EXP_SS.GDPE_ARIMA_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> EXP_SS.GDPE_ARIMA_MSE


EXP_SS.GDPE_ARIMA_MASE <- EXP_Skill.Score_GDPE_ARIMA %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

EXP_SS.GDPE_ARIMA_MASE %>% gather(-`Method`, key = "h", value = "MSE") -> EXP_SS.GDPE_ARIMA_MASE

EXP_SS.GDPE_ARIMA_MSE %>% left_join(EXP_SS.GDPE_ARIMA_MASE, by = c("Method", "h")) %>% 
  rename("MSE" = "MSE.x", "MASE" = "MSE.y") -> EXP_GDPE_PointF

EXP_GDPE_PointF %>% ggplot(aes(x = h, y = MSE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  ylab("Skill score (MSE) % ") + ggtitle("Top level")+ 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_GDPE_MSE

EXP_GDPE_PointF %>% ggplot(aes(x = h, y = MASE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  ylab("Skill score (MASE) % ") + ggtitle("Top level")+ 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_GDPE_MASE


##Across all levels

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> EXP_Score_all.series_ARIMA

EXP_Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> EXP_ARIMA.base_MSE

EXP_Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> EXP_ARIMA.base_MASE

EXP_Score_all.series_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/EXP_ARIMA.base_MSE))*100, digits = 2), 
                                      SS_MASE = round((1-(`Avg_MASE`/EXP_ARIMA.base_MASE))*100, digits = 2)) -> EXP_Skill.Score_all.series_ARIMA  


EXP_Skill.Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`) %>% 
  filter(`F-method` == "Benchmark") %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) -> EXP_All.series.ARIMA_SS

EXP_All.series.ARIMA_SS %>% ggplot(aes(x = `Forecast Horizon`, y = SS_MSE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  
  ylab("Skill score (MSE) % ") + xlab("h") + ggtitle("All levels") + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_All.series_MSE

EXP_All.series.ARIMA_SS %>% ggplot(aes(x = `Forecast Horizon`, y = SS_MASE, color = "brown")) +
  geom_hline(yintercept = 0) +
  geom_point(size = 3) +  
  ylab("Skill score (MASE) % ") + xlab("h") + ggtitle("All levels") + 
  theme(legend.position="none") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_All.series_MASE


grid.arrange( arrangeGrob(Plot_INC_PointF_All.series_MSE, Plot_INC_PointF_GDPI_MSE, top="Income"), 
              arrangeGrob(Plot_EXP_PointF_All.series_MSE, Plot_EXP_PointF_GDPE_MSE, top="Expenditure"), 
              ncol=2)

grid.arrange( arrangeGrob(Plot_INC_PointF_All.series_MASE, Plot_INC_PointF_GDPI_MASE, top="Income"), 
              arrangeGrob(Plot_EXP_PointF_All.series_MASE, Plot_EXP_PointF_GDPE_MASE, top="Expenditure"), 
              ncol=2)











####Naive vs Base with MSE and MASE (Not skill scores)



rm(list = ls())
#Income approach
load("Point-forecasting/INC-PointForecasts-ExpandingW.RData")

Method_Order <- c("Naive")

#For top level

#MSE
INC_GDPI_PointF_arima <- Score_arima %>% dplyr::filter(`Series`=="Gdpi") 

INC_GDPI_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> INC_GDPI_ARIMA.base_MSE

INC_GDPI_PointF_arima %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> INC_GDPI_ARIMA.base_MASE

INC_GDPI_PointF_arima %>% mutate(SS_MSE = round((1-(`MSE`/INC_GDPI_ARIMA.base_MSE))*100, digits = 2), 
                                 SS_MASE = round((1-(`MASE`/INC_GDPI_ARIMA.base_MASE))*100, digits = 2)) -> INC_Skill.Score_GDPI_ARIMA 


INC_SS_GDPI_ARIMA_MSE <- INC_Skill.Score_GDPI_ARIMA %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `SS_MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `SS_MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 6, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

INC_SS_GDPI_ARIMA_MSE %>% gather(-`Method`, key = "h", value = "SS_MSE") -> INC_SS_GDPI_ARIMA_MSE


INC_SS_GDPI_ARIMA_MSE %>% ggplot(aes(x = h, y = SS_MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("Skill score (MSE) % ") + ggtitle("Top level")+ 
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

