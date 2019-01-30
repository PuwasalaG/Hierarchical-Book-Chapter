#Results summarised for naive vs ETS base point forecasts

require(tidyverse)
require(fpp2)
require(ggplot2)
require(gridExtra)
require(grid)
require(ggpubr)
setwd("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Final-results/Results-in-plots-for-chapter")



####Naive vs Base with MSE and MASE 



rm(list = ls())
#Income approach
load("Results-Point-forecasts/INC-PointForecasts-ExpandingW.RData")

Method_Order <- c("Base", "Naive")

#For top level

#MSE
INC_GDPI_PointF_ets <- Score_ets %>% dplyr::filter(`Series`=="Gdpi") 

INC_GDPI_ets_MSE <- INC_GDPI_PointF_ets %>% 
  dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MSE`) %>% 
  spread(key = `Forecast Horizon`, value = `MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

INC_GDPI_ets_MSE %>% gather(-`Method`, key = "h", value = "SS_MSE") -> INC_GDPI_ets_MSE


INC_GDPI_ets_MSE %>% ggplot(aes(x = h, y = SS_MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE") + ggtitle("Top level")+ 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_GDPI_MSE


#MASE
INC_GDPI_ets_MASE <- INC_GDPI_PointF_ets %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

INC_GDPI_ets_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> INC_GDPI_ets_MASE

INC_GDPI_ets_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MASE ") + ggtitle("Top level")  + 
  theme(legend.position="bottom") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_GDPI_MASE


##All levels 
Score_ets %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> INC_Score_all.series_ets

#MSE
INC_Score_all.series_ets %>% dplyr::select(-`Avg_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MSE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> INC_All.series.ets_MSE

INC_All.series.ets_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> INC_All.series.ets_MSE

INC_All.series.ets_MSE %>% ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE ") + ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_All.series_MSE

#MASE
INC_Score_all.series_ets %>% dplyr::select(-`Avg_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MASE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> INC_All.series.ets_MASE

INC_All.series.ets_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> INC_All.series.ets_MASE

INC_All.series.ets_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MASE ") + ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_INC_PointF_All.series_MASE



#Expenditure approach

rm(list=ls()[! ls() %in% c("Plot_INC_PointF_GDPI_MSE", "Plot_INC_PointF_GDPI_MASE", "Plot_INC_PointF_All.series_MSE",
                           "Plot_INC_PointF_All.series_MASE")])
load("Results-Point-forecasts/EXP-PointForecasts-ExpandingW.RData")

Method_Order <- c("Base", "Naive")

#For top level - Income approach

#MSE
EXP_GDPE_PointF_ets <- Score_ets %>% dplyr::filter(`Series`=="Gdpe") 

EXP_GDPE_ets_MSE <- EXP_GDPE_PointF_ets %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MSE` ) %>% 
  spread(key = `Forecast Horizon`, value = `MSE`) %>% 
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

EXP_GDPE_ets_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> EXP_GDPE_ets_MSE

EXP_GDPE_ets_MSE %>% ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE ") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_GDPE_MSE

#MASE
EXP_GDPE_ets_MASE <- EXP_GDPE_PointF_ets %>% dplyr::select(`F-method`, `R-method`, `Forecast Horizon`, `MASE` ) %>% 
  spread(key = `Forecast Horizon`, value = `MASE`) %>% 
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame()

EXP_GDPE_ets_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> EXP_GDPE_ets_MASE

EXP_GDPE_ets_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MASE ") + ggtitle("Top level") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_GDPE_MASE


##All levels

Score_ets %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 2), Avg_MASE = round(mean(`MASE`), digits = 2)) -> EXP_Score_all.series_ets

#MSE
EXP_Score_all.series_ets %>% dplyr::select(-`Avg_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MSE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_All.series.ets_MSE

EXP_All.series.ets_MSE %>% gather(-`Method`, key = "h", value = "MSE") -> EXP_All.series.ets_MSE

EXP_All.series.ets_MSE %>% ggplot(aes(x = h, y = MSE, color = Method, shape = Method)) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("green3", "brown")) +
  scale_shape_manual(values=0:1) + ylab("MSE ") + ggtitle("All levels") +
  theme(plot.title = element_text(size = 10, face = "italic")) -> Plot_EXP_PointF_All.series_MSE

#MASE
EXP_Score_all.series_ets %>% dplyr::select(-`Avg_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = Avg_MASE) %>% 
  ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Naive")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) %>% as.data.frame() -> EXP_All.series.ets_MASE

EXP_All.series.ets_MASE %>% gather(-`Method`, key = "h", value = "MASE") -> EXP_All.series.ets_MASE

EXP_All.series.ets_MASE %>% ggplot(aes(x = h, y = MASE, color = Method, shape = Method)) +
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

