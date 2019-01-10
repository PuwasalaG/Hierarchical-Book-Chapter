Score_arima <- tibble("Series" = character(),
                            "F-method" = character(),
                            "R-method" = character(),
                            "Forecast Horizon" = integer(),
                            "MSE" = numeric(), 
                            "MASE" = numeric())


Score_ets <- tibble("Series" = character(),
                          "F-method" = character(),
                          "R-method" = character(),
                          "Forecast Horizon" = integer(),
                          "MSE" = numeric(), 
                          "MASE" = numeric())

for (i in 1:n) {
  
  DF %>% filter(`Series`==names(Inc)[i]) %>% dplyr::select("F-method", "R-method", "Forecast Horizon", 
                                                           "SquaredE", "ScaledE") -> DF_Score
  
  DF_Score %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
    summarise(MSE = mean(`SquaredE`), MASE = mean(abs(`ScaledE`))) -> DF_Score
  
  DF_Score %>% dplyr::filter(`F-method` == "ETS" | `R-method` == "Base") %>% 
    dplyr::filter(`F-method`!= "ARIMA") -> Score_ETS
  
  cbind(Score_ETS, "Series" = rep(names(Inc)[i], nrow(Score_ETS))) -> Score_ETS
  Score_ETS[names(Score_ets)] %>% as.tibble() -> Score_ETS
  
  Score_ets <- rbind(Score_ets, Score_ETS)
  
  
  DF_Score %>% dplyr::filter(`F-method` == "ARIMA" | `R-method` == "Base") %>% 
    dplyr::filter(`F-method`!= "ETS") -> Score_ARIMA
  
  cbind(Score_ARIMA, "Series" = rep(names(Inc)[i], nrow(Score_ARIMA))) -> Score_ARIMA
  Score_ARIMA[names(Score_arima)]  %>% as.tibble() -> Score_ARIMA
  
  Score_arima <- rbind(Score_arima, Score_ARIMA)
}

# Analysis for all aggregate series

#ETS

Score_ets %>% dplyr::filter(`Series` %in% c(names(Inc)[1:6])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 4), Avg_MASE = round(mean(`MASE`), digits = 4)) -> Score_aggregates_ETS

Score_aggregates_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ETS.base_MSE

Score_aggregates_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ETS.base_MASE

Score_aggregates_ETS %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ETS.base_MSE))*100, digits = 4), 
                           SS_MASE = round((1-(`Avg_MASE`/ETS.base_MASE))*100, digits = 4)) -> Skill.Score_aggregates_ETS  


Skill.Score_aggregates_ETS %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Aggregates.ETS_MSE

Skill.Score_aggregates_ETS %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Aggregates.ETS_MASE

#ARIMA

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[1:6])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 4), Avg_MASE = round(mean(`MASE`), digits = 4)) -> Score_aggregates_ARIMA

Score_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ARIMA.base_MSE

Score_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ARIMA.base_MASE

Score_aggregates_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ARIMA.base_MSE))*100, digits = 4), 
                                SS_MASE = round((1-(`Avg_MASE`/ARIMA.base_MASE))*100, digits = 4)) -> Skill.Score_aggregates_ARIMA  


Skill.Score_aggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Aggregates.ARIMA_MSE

Skill.Score_aggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Aggregates.ARIMA_MASE


# Analysis for all disaggregate series

#ETS

Score_ets %>% dplyr::filter(`Series` %in% c(names(Inc)[7:16])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 4), Avg_MASE = round(mean(`MASE`), digits = 4)) -> Score_disaggregates_ETS

Score_disaggregates_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ETS.base_MSE

Score_disaggregates_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ETS.base_MASE

Score_disaggregates_ETS %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ETS.base_MSE))*100, digits = 4), 
                                SS_MASE = round((1-(`Avg_MASE`/ETS.base_MASE))*100, digits = 4)) -> Skill.Score_disaggregates_ETS  


Skill.Score_disaggregates_ETS %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Disaggregates.ETS_MSE

Skill.Score_disaggregates_ETS %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Disaggregates.ETS_MASE

#ARIMA

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[7:16])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 4), Avg_MASE = round(mean(`MASE`), digits = 4)) -> Score_disaggregates_ARIMA

Score_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ARIMA.base_MSE

Score_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ARIMA.base_MASE

Score_disaggregates_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ARIMA.base_MSE))*100, digits = 4), 
                                  SS_MASE = round((1-(`Avg_MASE`/ARIMA.base_MASE))*100, digits = 4)) -> Skill.Score_disaggregates_ARIMA  


Skill.Score_disaggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Disaggregates.ARIMA_MSE

Skill.Score_disaggregates_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Disaggregates.ARIMA_MASE


# Analysis for all series

#ETS

Score_ets %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 4), Avg_MASE = round(mean(`MASE`), digits = 4)) -> Score_all.series_ETS

Score_all.series_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ETS.base_MSE

Score_all.series_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ETS.base_MASE

Score_all.series_ETS %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ETS.base_MSE))*100, digits = 4), 
                                   SS_MASE = round((1-(`Avg_MASE`/ETS.base_MASE))*100, digits = 4)) -> Skill.Score_all.series_ETS  


Skill.Score_all.series_ETS %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> All.series.ETS_MSE

Skill.Score_all.series_ETS %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> All.series.ETS_MASE

#ARIMA

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_MSE = round(mean(`MSE`), digits = 4), Avg_MASE = round(mean(`MASE`), digits = 4)) -> Score_all.series_ARIMA

Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MSE`) %>% as_vector() -> ARIMA.base_MSE

Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_MASE`) %>% as_vector() -> ARIMA.base_MASE

Score_all.series_ARIMA %>% mutate(SS_MSE = round((1-(`Avg_MSE`/ARIMA.base_MSE))*100, digits = 4), 
                                     SS_MASE = round((1-(`Avg_MASE`/ARIMA.base_MASE))*100, digits = 4)) -> Skill.Score_all.series_ARIMA  


Skill.Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MASE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MSE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> All.series.ARIMA_MSE

Skill.Score_all.series_ARIMA %>% dplyr::select(-`Avg_MASE`, -`Avg_MSE`, -`SS_MSE`) %>% 
  spread(key = `Forecast Horizon`, value = SS_MASE) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 6, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> All.series.ARIMA_MASE

