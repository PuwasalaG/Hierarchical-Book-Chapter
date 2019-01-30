#Results summarised for ETS based univariate probabilistic forecasts

require(tidyverse)
require(fpp2)
require(ggplot2)
require(gridExtra)
require(grid)
require(ggpubr)

rm(list = ls())
#Univariate forecasts for top level (Gaussian) - Income approach
load("Results-Prob-forecasts/INC-ProbForecasts-GaussianApproach-ExpandingW.RData")

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

Score_ets %>% dplyr::filter(`Series`=="Gdpi" ) -> Score_GDPI_ETS

Score_GDPI_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MCRPS`) %>% as_vector() -> GDPI_ETS.base_CRPS

Score_GDPI_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MLS`) %>% as_vector() -> GDPI_ETS.base_LS

Score_GDPI_ETS %>% mutate(SS_CRPS = round((1-(`MCRPS`/GDPI_ETS.base_CRPS))*100, digits = 2), 
                            SS_LS = round((1-(`MLS`/GDPI_ETS.base_LS))*100, digits = 2)) -> Skill.Score_GDPI_ETS 


Skill.Score_GDPI_ETS %>% dplyr::select(-`MCRPS`, -`MLS`, -`SS_LS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`, -`Series`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1, 3, 5), c("Benchmark", "Bottom-up", "MinT(Shrink)"))) %>% 
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> GDPI.ETS_CRPS

GDPI.ETS_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>%
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) + 
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (CRPS) %") +
  theme(legend.position = "bottom") + 
  ggtitle("Gaussian approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_INC_GDPI_UnivS_Gauss 


#Univariate forecasts for top level (Gaussian) - Expenditure approach
rm(list=ls()[! ls() %in% c("Plot_INC_GDPI_UnivS_Gauss")])

load("Results-Prob-forecasts/EXP-ProbForecasts-GaussianApproach-ExpandingW.RData")

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

Score_ets %>% dplyr::filter(`Series`=="Gdpe" ) -> Score_GDPE_ETS

Score_GDPE_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MCRPS`) %>% as_vector() -> GDPE_ETS.base_CRPS

Score_GDPE_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MLS`) %>% as_vector() -> GDPE_ETS.base_LS

Score_GDPE_ETS %>% mutate(SS_CRPS = round((1-(`MCRPS`/GDPE_ETS.base_CRPS))*100, digits = 2), 
                            SS_LS = round((1-(`MLS`/GDPE_ETS.base_LS))*100, digits = 2)) -> Skill.Score_GDPE_ETS 


Skill.Score_GDPE_ETS %>% dplyr::select(-`MCRPS`, -`MLS`, -`SS_LS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`, -`Series`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1, 3, 4), c("Benchmark", "Bottom-up", "MinT(Shrink)"))) %>% 
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> GDPE.ETS_CRPS

GDPE.ETS_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>%
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (CRPS) %") + ggtitle("Gaussian approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_EXP_GDPE_UnivS_Gauss 


#Univariate forecasts for top level (Non-parametric) - Income approach
rm(list=ls()[! ls() %in% c("Plot_INC_GDPI_UnivS_Gauss", "Plot_EXP_GDPE_UnivS_Gauss")])

load("Results-Prob-forecasts/INC-ProbForecasts-BootstrapApproach-ExpandingW.RData")

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

Score_ets %>% dplyr::filter(`Series`=="Gdpi" ) -> Score_GDPI_ETS

Score_GDPI_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MCRPS`) %>% as_vector() -> GDPI_ETS.base_CRPS

Score_GDPI_ETS %>% mutate(SS_CRPS = round((1-(`MCRPS`/GDPI_ETS.base_CRPS))*100, digits = 2)) -> Skill.Score_GDPI_ETS 


Skill.Score_GDPI_ETS %>% dplyr::select(-`MCRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`, -`Series`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1, 3, 5), c("Benchmark", "Bottom-up", "MinT(Shrink)"))) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> GDPI.ETS_CRPS

GDPI.ETS_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (CRPS)%") + ggtitle("Bootstrap approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_INC_GDPI_UnivS_NonPara 


#Univariate forecasts for top level (Non-parametric) - Expenditure approach
rm(list=ls()[! ls() %in% c("Plot_INC_GDPI_UnivS_Gauss", "Plot_EXP_GDPE_UnivS_Gauss",
                           "Plot_INC_GDPI_UnivS_NonPara")])
load("Results-Prob-forecasts/EXP-ProbForecasts-BootstrapApproach-ExpandingW.RData")

Method_Order <- c("Bottom-up", "OLS", "WLS", "MinT(Shrink)")

Score_ets %>% dplyr::filter(`Series`=="Gdpe" ) -> Score_GDPE_ETS

Score_GDPE_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MCRPS`) %>% as_vector() -> GDPE_ETS.base_CRPS

Score_GDPE_ETS %>% 
  mutate(SS_CRPS = round((1-(`MCRPS`/GDPE_ETS.base_CRPS))*100, digits = 2)) -> Skill.Score_GDPE_ETS 


Skill.Score_GDPE_ETS %>% dplyr::select(-`MCRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`, -`Series`) %>%  
  mutate(`R-method` = replace(`R-method`, c(1, 3, 4), c("Benchmark", "Bottom-up", "MinT(Shrink)"))) %>% 
  rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> GDPE.ETS_CRPS

GDPE.ETS_CRPS %>% gather(-Method, key = "h", value = "Score") %>% 
  mutate(Method = factor(Method, levels = c("MinT(Shrink)", "WLS", "OLS", "Bottom-up"))) %>% 
  ggplot(aes(x = h, y = Score, color = Method, shape = Method)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point(size = 3) + 
  scale_color_manual(values = c("Green", "Blue", "Purple", "Red")) +
  scale_shape_manual(values = 0:3) +
  ylab("Skill score (CRPS) %") + ggtitle("Bootstrap approach") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(axis.title.y = element_text(size = 10)) -> Plot_EXP_GDPE_UnivS_NonPara 


rm(list=ls()[! ls() %in% c("Plot_INC_GDPI_UnivS_Gauss", "Plot_EXP_GDPE_UnivS_Gauss",
                           "Plot_INC_GDPI_UnivS_NonPara", "Plot_EXP_GDPE_UnivS_NonPara")])

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(Plot_INC_GDPI_UnivS_Gauss)

grid.arrange( arrangeGrob(Plot_INC_GDPI_UnivS_Gauss + theme(legend.position="none"), 
                          Plot_INC_GDPI_UnivS_NonPara + theme(legend.position="none"), top="Income"), 
              arrangeGrob(Plot_EXP_GDPE_UnivS_Gauss + theme(legend.position="none"), 
                          Plot_EXP_GDPE_UnivS_NonPara + theme(legend.position="none"), top="Expenditure"), 
              ncol=2, mylegend, heights=c(10, 1))

