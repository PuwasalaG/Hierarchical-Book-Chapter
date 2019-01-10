# For point forecasts

load("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Point-Forecasts/Income-approach/GDP-PointForecasting-INC-method_Results.RData")

DF %>% select(`Series`, `F-method`, `R-method`, `Forecast Horizon`, `SquaredE`, `ScaledE`) %>% 
  group_by(`Series`, `F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(MSE = mean(`SquaredE`)) -> DF_MSE

DF_MSE %>% ungroup() %>% filter(`F-method`=="ARIMA") %>% 
  ggplot(aes(x = `Forecast Horizon`, y = `MSE`, color = `R-method`)) + 
  geom_line() + facet_wrap(~ `Series`, scales = "free_y")


#Overall average of the hierarchy

DF %>% select(`F-method`, `R-method`, `Forecast Horizon`, `SquaredE`, `ScaledE`) %>% 
  group_by(`F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(MSE = mean(`SquaredE`), MASE = mean(`ScaledE`)) -> DF_all.series

View(DF_all.series)

DF_all.series %>% dplyr::filter(`F-method` == "ETS" | `R-method` == "Base") %>% 
  dplyr::filter(`F-method`!= "ARIMA") -> DF_all.series.ETS

DF_MSE_all.series %>% dplyr::filter(`F-method` == "ETS" | `R-method` == "Base") %>% 
  dplyr::filter(`F-method`!= "ARIMA") -> DF_all.series.ARIMA 

#For ETS

DF_all.series %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MSE`) %>% as_vector() -> DF_MSE_all.series.ETS_base

DF_all.series %>% dplyr::filter(`F-method`=="ETS", `R-method`=="Base") %>%
  slice() %>% 
  ungroup() %>%
  dplyr::select(`MASE`) %>% as_vector() -> DF_MASE_all.series.ETS_base


DF_all.series.ETS %>% 
  mutate(SS_MSE = round((1-(`MSE`/DF_MSE_all.series.ETS_base))*100, digits = 4), 
                         SS_MASE = round((1-(`MASE`/DF_MASE_all.series.ETS_base))*100, digits = 4)) -> DF_all.series.ETS_SS




#Ploting Base forecasts vs OLS

DF %>% filter(`F-method` == "ARIMA") %>% 
  filter(`R-method` == "Base" | `R-method` == "OLS") %>%
  filter(`Forecast Horizon` == 1) %>%
  select(`Series`, `R-method`, `Forecasts`, `Actual`, `Replication`) -> DF_Plot

DF_Plot %>% ggplot(aes(y= `Forecasts`, x = `Replication`, color = `R-method`)) + geom_line() + 
  facet_wrap(~ `Series`, scales = "free_y") + 
  geom_line(aes(x = `Replication`, y = `Actual`), colour = "Black")


#For probabilistic forecasts - Gaussian approach

load("C:/Puwasala/PhD_Monash/Research/Hierarchical-Book-Chapter/Forecasting_GDP/Probabilistic-Forecasts/Income-approach/Gaussian-approach/GDP-ProbForecasting-Inc-GaussianMethod-ExpandW_Results.RData")

DF_UniV %>% select(`Series`, `F-method`, `R-method`, `Forecast Horizon`, `CRPS`) %>% 
  group_by(`Series`, `F-method`, `R-method`, `Forecast Horizon`) %>%
  summarise(Avg_CRPS = mean(`CRPS`)) -> DF_CRPS

DF_CRPS %>% ungroup() %>% filter(`F-method`=="ARIMA") %>% 
  ggplot(aes(x = `Forecast Horizon`, y = `Avg_CRPS`, color = `R-method`)) + 
  geom_line() + facet_wrap(~ `Series`, scales = "free_y")

