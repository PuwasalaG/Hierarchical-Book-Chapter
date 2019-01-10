Score_arima <- tibble("Series" = character(),
                      "F-method" = character(),
                      "R-method" = character(),
                      "Forecast Horizon" = integer(),
                      "MCRPS" = numeric())


Score_ets <- tibble("Series" = character(),
                    "F-method" = character(),
                    "R-method" = character(),
                    "Forecast Horizon" = integer(),
                    "MCRPS" = numeric())

for (i in 1:n) {
  
  DF_UniV %>% filter(`Series`==names(Inc)[i]) %>% dplyr::select("F-method", "R-method", "Forecast Horizon", 
                                                                "CRPS") -> DF_Score
  
  DF_Score %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>% 
    summarise(MCRPS = mean(`CRPS`)) -> DF_Score
  
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
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 4)) -> Score_aggregates_ETS

Score_aggregates_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> ETS.base_CRPS

Score_aggregates_ETS %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/ETS.base_CRPS))*100, digits = 4)) -> Skill.Score_aggregates_ETS  


Skill.Score_aggregates_ETS %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Aggregates.ETS_CRPS

#ARIMA

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[1:6])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 4)) -> Score_aggregates_ARIMA

Score_aggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> ARIMA.base_CRPS

Score_aggregates_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/ARIMA.base_CRPS))*100, digits = 4)) -> Skill.Score_aggregates_ARIMA  


Skill.Score_aggregates_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 7, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Aggregates.ARIMA_CRPS


# Analysis for all disaggregate series

#ETS

Score_ets %>% dplyr::filter(`Series` %in% c(names(Inc)[7:16])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 4)) -> Score_disaggregates_ETS

Score_disaggregates_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> ETS.base_CRPS

Score_disaggregates_ETS %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/ETS.base_CRPS))*100, digits = 4)) -> Skill.Score_disaggregates_ETS  


Skill.Score_disaggregates_ETS %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Disaggregates.ETS_CRPS

#ARIMA

Score_arima %>% dplyr::filter(`Series` %in% c(names(Inc)[7:16])) %>%
  dplyr::select(-`Series`) %>% group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 4)) -> Score_disaggregates_ARIMA

Score_disaggregates_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> ARIMA.base_CRPS

Score_disaggregates_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/ARIMA.base_CRPS))*100, digits = 4)) -> Skill.Score_disaggregates_ARIMA  


Skill.Score_disaggregates_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 7, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> Disaggregates.ARIMA_CRPS

# Analysis for all series

#ETS

Score_ets %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 4)) -> Score_all.series_ETS

Score_all.series_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> ETS.base_CRPS

Score_all.series_ETS %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_LS`) %>% as_vector() -> ETS.base_LS

Score_all.series_ETS %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/ETS.base_CRPS))*100, digits = 4)) -> Skill.Score_all.series_ETS  


Skill.Score_all.series_ETS %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 1, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> All.series.ETS_CRPS

#ARIMA

Score_arima %>% dplyr::select(-`Series`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = round(mean(`MCRPS`), digits = 4)) -> Score_all.series_ARIMA

Score_all.series_ARIMA %>% dplyr::filter(`F-method`=="ARIMA", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`Avg_CRPS`) %>% as_vector() -> ARIMA.base_CRPS

Score_all.series_ARIMA %>% 
  mutate(SS_CRPS = round((1-(`Avg_CRPS`/ARIMA.base_CRPS))*100, digits = 4)) -> Skill.Score_all.series_ARIMA  


Skill.Score_all.series_ARIMA %>% dplyr::select(-`Avg_CRPS`) %>% 
  spread(key = `Forecast Horizon`, value = SS_CRPS) %>% ungroup() %>% dplyr::select(-`F-method`) %>%  
  mutate(`R-method` = replace(`R-method`, 7, "Benchmark")) %>% rename("Method" = "R-method") %>%
  slice(match(Method_Order, `Method`)) -> All.series.ARIMA_CRPS

