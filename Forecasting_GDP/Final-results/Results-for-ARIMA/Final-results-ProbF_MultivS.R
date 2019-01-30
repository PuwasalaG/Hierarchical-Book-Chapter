#Results summarised for ARIMA based multivariate probabilistic forecasts


require(tidyverse)
require(fpp2)
require(ggplot2)
require(gridExtra)
require(grid)
require(ggpubr)

rm(list = ls())
#Multivariate forecasts (Gaussian) - Income approach
load("Results-Prob-forecasts/INC-ProbForecasts-GaussianApproach-ExpandingW.RData")

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

SS_E.ES_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 4, 7), c("Bottom-up", "MinT Sample", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> INC_ARIMA_ES

INC_ARIMA_ES %>% gather(-Method, key = "h", value = "ES") -> INC_ARIMA_ES

SS_E.VS_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 4, 7), c("Bottom-up", "MinT Sample", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>% 
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> INC_ARIMA_VS

INC_ARIMA_VS %>% gather(-Method, key = "h", value = "ES") -> INC_ARIMA_VS

# INC_ARIMA_ES %>% left_join(INC_ARIMA_VS, by = c("Method", "h")) %>% 
#   rename("ES" = "ES.x", "VS" = "ES.y") %>% 
#   filter(Method != "Base") %>% 
#   gather(-Method, -h, key = "Scoring_rule", value = "Score") -> INC_MultivS_ES_VS

#Plot for ES only
INC_ARIMA_ES %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  theme(legend.position = "bottom") +
  ylab("Skill score (ES) %") + ggtitle("Gaussian approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_INC_MultivS_Gauss_ES

#Plot for VS only
INC_ARIMA_VS %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  theme(legend.position = "bottom") +
  ylab("Skill score (VS) %") + ggtitle("Gaussian approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_INC_MultivS_Gauss_VS

#Multivariate forecasts (Gaussian) - Expenditure approach
rm(list=ls()[! ls() %in% c("Plot_INC_MultivS_Gauss_ES", "Plot_INC_MultivS_Gauss_VS")])

#Scores for Multivariate forecasts 
load("Results-Prob-forecasts/EXP-ProbForecasts-GaussianApproach-ExpandingW.RData")
Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

SS_E.ES_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 6), c("Bottom-up", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> EXP_ARIMA_ES

EXP_ARIMA_ES %>% gather(-Method, key = "h", value = "ES") -> EXP_ARIMA_ES

SS_E.VS_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 6), c("Bottom-up", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>% 
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> EXP_ARIMA_VS

EXP_ARIMA_VS %>% gather(-Method, key = "h", value = "ES") -> EXP_ARIMA_VS

# EXP_ARIMA_ES %>% left_join(EXP_ARIMA_VS, by = c("Method", "h")) %>% 
#   rename("ES" = "ES.x", "VS" = "ES.y") %>% 
#   filter(Method != "Base") %>% 
#   gather(-Method, -h, key = "Scoring_rule", value = "Score") -> EXP_MultivS_ES_VS

#Plot for ES only
EXP_ARIMA_ES %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>%
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) +  
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) + 
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (ES) %") + ggtitle("Gaussian approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_EXP_MultivS_Gauss_ES

#Plot for VS only
EXP_ARIMA_VS %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>%
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) +  
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) + 
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (VS) %") + ggtitle("Gaussian approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_EXP_MultivS_Gauss_VS

#Multivariate forecasts (Non-para) - Income approach
rm(list=ls()[! ls() %in% c("Plot_INC_MultivS_Gauss_ES", "Plot_INC_MultivS_Gauss_VS", 
                           "Plot_EXP_MultivS_Gauss_ES", "Plot_EXP_MultivS_Gauss_VS")])
load("Results-Prob-forecasts/INC-ProbForecasts-BootstrapApproach-ExpandingW.RData")

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

SS_E.ES_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 4, 7), c("Bottom-up", "MinT Sample", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> INC_ARIMA_ES

INC_ARIMA_ES %>% gather(-Method, key = "h", value = "ES") -> INC_ARIMA_ES

SS_E.VS_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 4, 7), c("Bottom-up", "MinT Sample", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>% 
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> INC_ARIMA_VS

INC_ARIMA_VS %>% gather(-Method, key = "h", value = "ES") -> INC_ARIMA_VS

# INC_ARIMA_ES %>% left_join(INC_ARIMA_VS, by = c("Method", "h")) %>% 
#   rename("ES" = "ES.x", "VS" = "ES.y") %>% 
#   filter(Method != "Base") %>% 
#   gather(-Method, -h, key = "Scoring_rule", value = "Score") -> INC_MultivS_ES_VS

#Plot for ES only
INC_ARIMA_ES %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>%  
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) + 
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (ES)%") + ggtitle("Bootstrap approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10))  -> Plot_INC_MultivS_NonPara_ES

#Plot for VS only
INC_ARIMA_VS %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>%  
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) + 
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (VS)%") + ggtitle("Bootstrap approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10))  -> Plot_INC_MultivS_NonPara_VS

#Multivariate forecasts (Non-para) - Expenditure approach
rm(list=ls()[! ls() %in% c("Plot_INC_MultivS_Gauss_ES", "Plot_INC_MultivS_Gauss_VS", 
                           "Plot_EXP_MultivS_Gauss_ES", "Plot_EXP_MultivS_Gauss_VS",
                           "Plot_INC_MultivS_NonPara_ES", "Plot_INC_MultivS_NonPara_VS")])

load("Results-Prob-forecasts/EXP-ProbForecasts-BootstrapApproach-ExpandingW.RData")

#Scores for Multivariate forecasts 

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

SS_E.ES_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 6), c("Bottom-up", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>%  
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> EXP_ARIMA_ES

EXP_ARIMA_ES %>% gather(-Method, key = "h", value = "ES") -> EXP_ARIMA_ES

SS_E.VS_ARIMA %>% ungroup() %>% 
  mutate(`R-method` = replace(`R-method`, list = c(2, 3, 6), c("Bottom-up", "MinT(Shrink)", "Benchmark"))) %>%
  dplyr::select(-`F-method`) %>% 
  rename("Method" = "R-method") %>% slice(match(Method_Order, `Method`)) -> EXP_ARIMA_VS

EXP_ARIMA_VS %>% gather(-Method, key = "h", value = "ES") -> EXP_ARIMA_VS

# EXP_ARIMA_ES %>% left_join(EXP_ARIMA_VS, by = c("Method", "h")) %>% 
#   rename("ES" = "ES.x", "VS" = "ES.y") %>% 
#   filter(Method != "Base") %>% 
#   gather(-Method, -h, key = "Scoring_rule", value = "Score") -> EXP_MultivS_ES_VS

#Plot for ES only
EXP_ARIMA_ES %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) + 
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (ES)%") + ggtitle("Bootstrap approach")+
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10))  -> Plot_EXP_MultivS_NonPara_ES

#Plot for VS only
EXP_ARIMA_VS %>% mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = ES, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) + 
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (VS)%") + ggtitle("Bootstrap approach")+
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10))  -> Plot_EXP_MultivS_NonPara_VS

rm(list=ls()[! ls() %in% c("Plot_INC_MultivS_Gauss_ES", "Plot_INC_MultivS_Gauss_VS", 
                           "Plot_EXP_MultivS_Gauss_ES", "Plot_EXP_MultivS_Gauss_VS", 
                           "Plot_INC_MultivS_NonPara_ES", "Plot_INC_MultivS_NonPara_VS", 
                           "Plot_EXP_MultivS_NonPara_ES", "Plot_EXP_MultivS_NonPara_VS")])

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(Plot_INC_MultivS_Gauss_ES)

grid.arrange( arrangeGrob(Plot_INC_MultivS_Gauss_ES + theme(legend.position="none"), 
                          Plot_INC_MultivS_NonPara_ES + theme(legend.position="none"), top="Income"), 
              arrangeGrob(Plot_EXP_MultivS_Gauss_ES + theme(legend.position="none"), 
                          Plot_EXP_MultivS_NonPara_ES + theme(legend.position="none"), top="Expenditure"), 
              ncol=2, mylegend, heights=c(10, 1))


grid.arrange( arrangeGrob(Plot_INC_MultivS_Gauss_VS + theme(legend.position="none"), 
                          Plot_INC_MultivS_NonPara_VS + theme(legend.position="none"), top="Income"), 
              arrangeGrob(Plot_EXP_MultivS_Gauss_VS + theme(legend.position="none"), 
                          Plot_EXP_MultivS_NonPara_VS + theme(legend.position="none"), top="Expenditure"), 
              ncol=2, mylegend, heights=c(10, 1))


