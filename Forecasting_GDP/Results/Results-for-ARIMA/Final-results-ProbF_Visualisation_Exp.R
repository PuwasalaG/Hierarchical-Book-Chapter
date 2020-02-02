require(tidyverse)
require(ggplot2)
require(ggfan)
library(gridExtra)
library(grid)
library(ggpubr)
library(forecast)
library(zoo)
library(mvtnorm)

load("Results-Prob-forecasts/Exp-ProbForecasts-BootstrapApproach-ExpandingW_FDistributions.RData")

Density.forecast_Exp_Gdp.Mint.shr <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdFceGvt.Mint.shr <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr <- matrix(0, nrow = k, ncol = test_length)

Density.forecast_Exp_Gdp.OLS <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdFceGvt.OLS <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS <- matrix(0, nrow = k, ncol = test_length)

Density.forecast_Exp_Gdp.Unrecon <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdFceGvt.Unrecon <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon <- matrix(0, nrow = k, ncol = test_length)

for (i in 1:test_length) {
  
  Density.forecast_Exp_Gdp.Mint.shr[,i] <- Recon_Mint.shr_FP_ARIMA_h1[[i]][,1]
  Density.forecast_Exp_GneDfdFceGvt.Mint.shr[,i] <- Recon_Mint.shr_FP_ARIMA_h1[[i]][,8]
  Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr[,i] <- Recon_Mint.shr_FP_ARIMA_h1[[i]][,25]
  Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr[,i] <- Recon_Mint.shr_FP_ARIMA_h1[[i]][,26]
  
  Density.forecast_Exp_Gdp.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,1]
  Density.forecast_Exp_GneDfdFceGvt.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,8]
  Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,25]
  Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS[,i] <- Recon_OLS_FP_ARIMA_h1[[i]][,26]
  
  Density.forecast_Exp_Gdp.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,1]
  Density.forecast_Exp_GneDfdFceGvt.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,8]
  Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,25]
  Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon[,i] <- Unrecon_FP_ARIMA_h1[[i]][,26]
  

}

Time <- as.yearqtr(1984 + seq(0, 4*34)/4)[-(1:3)]
Time_forecast <- as.yearqtr(1994 + seq(0, 4*24)/4)[-(1:3)]

Exp <- data.frame(Time = Time, Exp)

##--Gdp series--##

Exp_Gdp_df <- Exp %>% 
  select(Time, Gdpe) %>% 
  dplyr::rename(Time = `Time`, GDP = `Gdpe`) %>%
  gather(key = Series, value = Observed, -Time)

# OLS

DD_Exp_Gdp_OLS <- data.frame(Time_fc=Time_forecast, t(Density.forecast_Exp_Gdp.OLS))
Density.forecast_Exp_Gdp.OLS_df <- DD_Exp_Gdp_OLS %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_Exp_Gdp.OLS_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_Gdp_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Gdp")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("GDP") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_Gdp_OLS_NonPara

# Mint.shr
DD_Exp_Gdp_Mint.shr <- data.frame(Time_fc=Time_forecast, t(Density.forecast_Exp_Gdp.Mint.shr))
Density.forecast_Exp_Gdp.Mint.shr_df <- DD_Exp_Gdp_Mint.shr %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_Exp_Gdp.Mint.shr_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_Gdp_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Gdp")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("GDP") + 
  xlab("Time") + 
  ggtitle("Mint.shr") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_Gdp_Mint.shr_NonPara

DD_Exp_Gdp_Unrecon <- data.frame(Time_fc=Time_forecast, t(Density.forecast_Exp_Gdp.Unrecon))
Density.forecast_Exp_Gdp.Unrecon_df <- DD_Exp_Gdp_Unrecon %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_Exp_Gdp.Unrecon_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_Gdp_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Gdp")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("GDP") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_Gdp_Unrecon_NonPara

g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
}

mylegend <- g_legend(Plot_Exp_Gdp_Unrecon_NonPara)

grid.arrange( arrangeGrob(Plot_Exp_Gdp_Unrecon_NonPara + theme(legend.position="none",
                                                             axis.title.y = element_blank()), 
                          Plot_Exp_Gdp_OLS_NonPara + theme(legend.position="none",
                                                         axis.title.y = element_blank()),
                          Plot_Exp_Gdp_Mint.shr_NonPara + theme(legend.position="none",
                                                          axis.title.y = element_blank()),
                          top="Non-parametric Approach", ncol = 3, left = textGrob("$ millions", rot = 90, vjust = 1)), 
              ncol=1,  heights=c(10, 0.5)) -> Plot_Exp_Gdp_NonPara

# Visualising other series

Exp_other_df <- Exp %>% 
  select(Time, GneDfdFceGvt, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbiNdc) %>% 
  gather(key = Series, value = Observed, -Time)

#MinT

DD_Exp_GneDfdFceGvt_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdFceGvt.Mint.shr))
DD_Exp_GneDfdGfcPvtPbiIpr_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr))
DD_Exp_GneDfdGfcPvtPbiNdc_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr))

Density.forecast_Exp_GneDfdFceGvt.Mint.shr_df <- DD_Exp_GneDfdFceGvt_MinT %>% gather(key=Sim, value = GneDfdFceGvt, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr_df <- DD_Exp_GneDfdGfcPvtPbiIpr_MinT %>% gather(key=Sim, value = GneDfdGfcPvtPbiIpr, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr_df <- DD_Exp_GneDfdGfcPvtPbiNdc_MinT %>% gather(key=Sim, value=GneDfdGfcPvtPbiNdc, -Time_fc)

Density.forecast_Exp_other_Mint.shr <- data.frame(Density.forecast_Exp_GneDfdFceGvt.Mint.shr_df,
                                                  GneDfdGfcPvtPbiIpr = Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr_df[,'GneDfdGfcPvtPbiIpr'], 
                                                  GneDfdGfcPvtPbiNdc = Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr_df[,'GneDfdGfcPvtPbiNdc'])

Density.forecast_Exp_other_Mint.shr_df <- Density.forecast_Exp_other_Mint.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_Exp_other_Mint.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), 
                  breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_other_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                           colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("$ millions") +
  xlab("Time") + 
  ggtitle("MinT") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_other_MinT_NonPara

Exp_other_df <- Exp %>% 
  select(Time, GneDfdFceGvt, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbiNdc) %>% 
  gather(key = Series, value = Observed, -Time)

#OLS

DD_Exp_GneDfdFceGvt_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdFceGvt.OLS))
DD_Exp_GneDfdGfcPvtPbiIpr_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS))
DD_Exp_GneDfdGfcPvtPbiNdc_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS))

Density.forecast_Exp_GneDfdFceGvt.OLS_df <- DD_Exp_GneDfdFceGvt_OLS %>% gather(key=Sim, value = GneDfdFceGvt, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS_df <- DD_Exp_GneDfdGfcPvtPbiIpr_OLS %>% gather(key=Sim, value = GneDfdGfcPvtPbiIpr, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS_df <- DD_Exp_GneDfdGfcPvtPbiNdc_OLS %>% gather(key=Sim, value=GneDfdGfcPvtPbiNdc, -Time_fc)

Density.forecast_Exp_other_OLS.shr <- data.frame(Density.forecast_Exp_GneDfdFceGvt.OLS_df,
                                                 GneDfdGfcPvtPbiIpr = Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS_df[,'GneDfdGfcPvtPbiIpr'], 
                                                 GneDfdGfcPvtPbiNdc = Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS_df[,'GneDfdGfcPvtPbiNdc'])

Density.forecast_Exp_other_OLS.shr_df <- Density.forecast_Exp_other_OLS.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_Exp_other_OLS.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), 
                  breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_other_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                           colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("$ millions") +
  xlab("Time") + 
  ggtitle("OLS") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_other_OLS_NonPara


Exp_other_df <- Exp %>% 
  select(Time, GneDfdFceGvt, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbiNdc) %>% 
  gather(key = Series, value = Observed, -Time)

#Unrecon

DD_Exp_GneDfdFceGvt_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdFceGvt.Unrecon))
DD_Exp_GneDfdGfcPvtPbiIpr_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon))
DD_Exp_GneDfdGfcPvtPbiNdc_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon))

Density.forecast_Exp_GneDfdFceGvt.Unrecon_df <- DD_Exp_GneDfdFceGvt_Unrecon %>% gather(key=Sim, value = GneDfdFceGvt, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon_df <- DD_Exp_GneDfdGfcPvtPbiIpr_Unrecon %>% gather(key=Sim, value = GneDfdGfcPvtPbiIpr, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon_df <- DD_Exp_GneDfdGfcPvtPbiNdc_Unrecon %>% gather(key=Sim, value=GneDfdGfcPvtPbiNdc, -Time_fc)

Density.forecast_Exp_other_Unrecon.shr <- data.frame(Density.forecast_Exp_GneDfdFceGvt.Unrecon_df,
                                                     GneDfdGfcPvtPbiIpr = Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon_df[,'GneDfdGfcPvtPbiIpr'], 
                                                     GneDfdGfcPvtPbiNdc = Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon_df[,'GneDfdGfcPvtPbiNdc'])

Density.forecast_Exp_other_Unrecon.shr_df <- Density.forecast_Exp_other_Unrecon.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_Exp_other_Unrecon.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), 
                  breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_other_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                           colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("$ millions") +
  xlab("Time") + 
  ggtitle("Incoherent") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_other_Unrecon_NonPara


mylegend <- g_legend(Plot_Exp_other_Unrecon_NonPara)

grid.arrange( arrangeGrob(Plot_Exp_other_Unrecon_NonPara + theme(legend.position="none"),
                          Plot_Exp_other_OLS_NonPara + theme(legend.position="none"),
                          Plot_Exp_other_MinT_NonPara + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  mylegend, heights=c(10, 0.5)) 


rm(list=ls()[! ls() %in% c("Plot_Exp_other_Unrecon_NonPara", "Plot_Exp_other_OLS_NonPara,
                           ", "Plot_Exp_other_MinT_NonPara", "Plot_Exp_Gdp_NonPara")])




###########################
  ##Gaussian Approach##
###########################

load("Results-Prob-forecasts/Exp-ProbForecasts-GaussianApproach-ExpandingW_FDistributions.RData")

Density.forecast_Exp_Gdp.Mint.shr <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdFceGvt.Mint.shr <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr <- matrix(0, nrow = k, ncol = test_length)

Density.forecast_Exp_Gdp.OLS <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdFceGvt.OLS <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS <- matrix(0, nrow = k, ncol = test_length)

Density.forecast_Exp_Gdp.Unrecon <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdFceGvt.Unrecon <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon <- matrix(0, nrow = k, ncol = test_length)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon <- matrix(0, nrow = k, ncol = test_length)

for (i in 1:test_length) {
  
  Density.Mint.shr <- rmvnorm(n = k, mean = Recon_MinT_mean_ARIMA[[i]][1,],
                              sigma = Recon_MinT_Cov_ARIMA[[i]])

  Density.forecast_Exp_Gdp.Mint.shr[,i] <- Density.Mint.shr[,1]
  Density.forecast_Exp_GneDfdFceGvt.Mint.shr[,i] <- Density.Mint.shr[,8]
  Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr[,i] <- Density.Mint.shr[,25]
  Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr[,i] <- Density.Mint.shr[,26]
  
  Density.OLS <- rmvnorm(n = k, mean = Recon_OLS_mean_ARIMA[[i]][1,],
                         sigma = Recon_OLS_Cov_ARIMA[[i]])
  
  Density.forecast_Exp_Gdp.OLS[,i] <- Density.OLS[,1]
  Density.forecast_Exp_GneDfdFceGvt.OLS[,i] <- Density.OLS[,8]
  Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS[,i] <- Density.OLS[,25]
  Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS[,i] <- Density.OLS[,26]
  
  Density.Unrecon <- rmvnorm(n = k, mean = Unrecon_mean_ARIMA[[i]][1,],
                             sigma = Unrecon_Cov_ARIMA[[i]])
  
  Density.forecast_Exp_Gdp.Unrecon[,i] <- Density.Unrecon[,1]
  Density.forecast_Exp_GneDfdFceGvt.Unrecon[,i] <- Density.Unrecon[,8]
  Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon[,i] <- Density.Unrecon[,25]
  Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon[,i] <- Density.Unrecon[,26]
  
}


Time <- as.yearqtr(1984 + seq(0, 4*34)/4)[-(1:3)]
Time_forecast <- as.yearqtr(1994 + seq(0, 4*24)/4)[-(1:3)]

Exp <- data.frame(Time = Time, Exp)

##--Gdp series--##

Exp_Gdp_df <- Exp %>% 
  select(Time, Gdpe) %>% 
  dplyr::rename(Time = `Time`, GDP = `Gdpe`) %>%
  gather(key = Series, value = Observed, -Time)

# OLS

DD_Exp_Gdp_OLS <- data.frame(Time_fc=Time_forecast, t(Density.forecast_Exp_Gdp.OLS))
Density.forecast_Exp_Gdp.OLS_df <- DD_Exp_Gdp_OLS %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_Exp_Gdp.OLS_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_Gdp_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Gdp")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("GDP") + 
  xlab("Time") + 
  ggtitle("OLS") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_Gdp_OLS_Gauss

# Mint.shr
DD_Exp_Gdp_Mint.shr <- data.frame(Time_fc=Time_forecast, t(Density.forecast_Exp_Gdp.Mint.shr))
Density.forecast_Exp_Gdp.Mint.shr_df <- DD_Exp_Gdp_Mint.shr %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_Exp_Gdp.Mint.shr_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_Gdp_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Gdp")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("GDP") + 
  xlab("Time") + 
  ggtitle("Mint.shr") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_Gdp_Mint.shr_Gauss

DD_Exp_Gdp_Unrecon <- data.frame(Time_fc=Time_forecast, t(Density.forecast_Exp_Gdp.Unrecon))
Density.forecast_Exp_Gdp.Unrecon_df <- DD_Exp_Gdp_Unrecon %>% 
  gather(key=Sim, value=Australia, -Time_fc)

ggplot(Density.forecast_Exp_Gdp.Unrecon_df, aes(x=Time_fc,y=Australia)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_Gdp_df, linetype = "dotted", aes(x=Time, y=Observed, colour = "red")) + 
  scale_color_discrete(name = "Realisation", labels = c("Gdp")) +
  #scale_y_continuous(limits = c(3700, 13750), breaks = c(5000, 7500, 10000, 12500)) +
  ylab("GDP") + 
  xlab("Time") + 
  ggtitle("Incoherent") + 
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_Gdp_Unrecon_Gauss

g_legend <- function(a.gplot) {
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
  
}

mylegend <- g_legend(Plot_Exp_Gdp_Unrecon_Gauss)

grid.arrange( arrangeGrob(Plot_Exp_Gdp_Unrecon_Gauss + theme(legend.position="none",
                                                             axis.title.y = element_blank()), 
                          Plot_Exp_Gdp_OLS_Gauss + theme(legend.position="none",
                                                         axis.title.y = element_blank()),
                          Plot_Exp_Gdp_Mint.shr_Gauss + theme(legend.position="none",
                                                              axis.title.y = element_blank()),
                          top="Gaussian Approach", ncol = 3, left = textGrob("$ millions", rot = 90, vjust = 1)), 
              ncol=1,  heights=c(10, 0.5)) -> Plot_Exp_Gdp_Gauss

# Visualising other series

Exp_other_df <- Exp %>% 
  select(Time, GneDfdFceGvt, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbiNdc) %>% 
  gather(key = Series, value = Observed, -Time)

#MinT

DD_Exp_GneDfdFceGvt_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdFceGvt.Mint.shr))
DD_Exp_GneDfdGfcPvtPbiIpr_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr))
DD_Exp_GneDfdGfcPvtPbiNdc_MinT <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr))

Density.forecast_Exp_GneDfdFceGvt.Mint.shr_df <- DD_Exp_GneDfdFceGvt_MinT %>% gather(key=Sim, value = GneDfdFceGvt, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr_df <- DD_Exp_GneDfdGfcPvtPbiIpr_MinT %>% gather(key=Sim, value = GneDfdGfcPvtPbiIpr, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr_df <- DD_Exp_GneDfdGfcPvtPbiNdc_MinT %>% gather(key=Sim, value=GneDfdGfcPvtPbiNdc, -Time_fc)

Density.forecast_Exp_other_Mint.shr <- data.frame(Density.forecast_Exp_GneDfdFceGvt.Mint.shr_df,
                                                  GneDfdGfcPvtPbiIpr = Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Mint.shr_df[,'GneDfdGfcPvtPbiIpr'], 
                                                  GneDfdGfcPvtPbiNdc = Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Mint.shr_df[,'GneDfdGfcPvtPbiNdc'])

Density.forecast_Exp_other_Mint.shr_df <- Density.forecast_Exp_other_Mint.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_Exp_other_Mint.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), 
                  breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_other_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                           colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("$ millions") +
  xlab("Time") + 
  ggtitle("MinT") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_other_MinT_Gauss

Exp_other_df <- Exp %>% 
  select(Time, GneDfdFceGvt, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbiNdc) %>% 
  gather(key = Series, value = Observed, -Time)

#OLS

DD_Exp_GneDfdFceGvt_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdFceGvt.OLS))
DD_Exp_GneDfdGfcPvtPbiIpr_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS))
DD_Exp_GneDfdGfcPvtPbiNdc_OLS <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS))

Density.forecast_Exp_GneDfdFceGvt.OLS_df <- DD_Exp_GneDfdFceGvt_OLS %>% gather(key=Sim, value = GneDfdFceGvt, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS_df <- DD_Exp_GneDfdGfcPvtPbiIpr_OLS %>% gather(key=Sim, value = GneDfdGfcPvtPbiIpr, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS_df <- DD_Exp_GneDfdGfcPvtPbiNdc_OLS %>% gather(key=Sim, value=GneDfdGfcPvtPbiNdc, -Time_fc)

Density.forecast_Exp_other_OLS.shr <- data.frame(Density.forecast_Exp_GneDfdFceGvt.OLS_df,
                                                 GneDfdGfcPvtPbiIpr = Density.forecast_Exp_GneDfdGfcPvtPbiIpr.OLS_df[,'GneDfdGfcPvtPbiIpr'], 
                                                 GneDfdGfcPvtPbiNdc = Density.forecast_Exp_GneDfdGfcPvtPbiNdc.OLS_df[,'GneDfdGfcPvtPbiNdc'])

Density.forecast_Exp_other_OLS.shr_df <- Density.forecast_Exp_other_OLS.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_Exp_other_OLS.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), 
                  breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_other_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                           colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("$ millions") +
  xlab("Time") + 
  ggtitle("OLS") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_other_OLS_Gauss


Exp_other_df <- Exp %>% 
  select(Time, GneDfdFceGvt, GneDfdGfcPvtPbiIpr, GneDfdGfcPvtPbiNdc) %>% 
  gather(key = Series, value = Observed, -Time)

#Unrecon

DD_Exp_GneDfdFceGvt_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdFceGvt.Unrecon))
DD_Exp_GneDfdGfcPvtPbiIpr_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon))
DD_Exp_GneDfdGfcPvtPbiNdc_Unrecon <- data.frame(Time_fc = Time_forecast, t(Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon))

Density.forecast_Exp_GneDfdFceGvt.Unrecon_df <- DD_Exp_GneDfdFceGvt_Unrecon %>% gather(key=Sim, value = GneDfdFceGvt, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon_df <- DD_Exp_GneDfdGfcPvtPbiIpr_Unrecon %>% gather(key=Sim, value = GneDfdGfcPvtPbiIpr, -Time_fc)
Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon_df <- DD_Exp_GneDfdGfcPvtPbiNdc_Unrecon %>% gather(key=Sim, value=GneDfdGfcPvtPbiNdc, -Time_fc)

Density.forecast_Exp_other_Unrecon.shr <- data.frame(Density.forecast_Exp_GneDfdFceGvt.Unrecon_df,
                                                     GneDfdGfcPvtPbiIpr = Density.forecast_Exp_GneDfdGfcPvtPbiIpr.Unrecon_df[,'GneDfdGfcPvtPbiIpr'], 
                                                     GneDfdGfcPvtPbiNdc = Density.forecast_Exp_GneDfdGfcPvtPbiNdc.Unrecon_df[,'GneDfdGfcPvtPbiNdc'])

Density.forecast_Exp_other_Unrecon.shr_df <- Density.forecast_Exp_other_Unrecon.shr %>% 
  gather(key = Series, value = prob.forecasts, -Time_fc, -Sim)

ggplot(Density.forecast_Exp_other_Unrecon.shr_df, aes(x = Time_fc,y = prob.forecasts)) + 
  geom_fan() +
  scale_x_yearmon(format = "%Y", limits = c(1984, 2019), 
                  breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_line(data = Exp_other_df , linetype = "dotted", aes(x = Time, y = Observed, 
                                                           colour = "red")) +
  scale_color_discrete(name = "Realisation", labels = NULL) +
  facet_wrap(~Series, scales = "free_y", ncol = 1) + 
  ylab("$ millions") +
  xlab("Time") + 
  ggtitle("Incoherent") +
  theme(plot.title = element_text(size = 10, face = "italic")) +
  theme(legend.position="bottom", legend.direction="horizontal") -> Plot_Exp_other_Unrecon_Gauss


mylegend <- g_legend(Plot_Exp_other_Unrecon_Gauss)

grid.arrange( arrangeGrob(Plot_Exp_other_Unrecon_Gauss + theme(legend.position="none"),
                          Plot_Exp_other_OLS_Gauss + theme(legend.position="none"),
                          Plot_Exp_other_MinT_Gauss + theme(legend.position="none"), 
                          ncol = 3), 
              ncol=1,  mylegend, heights=c(10, 0.5)) 

# Gdpe combining plots from both approaches

grid.arrange(arrangeGrob(Plot_Exp_Gdp_Gauss, Plot_Exp_Gdp_NonPara, ncol = 1), 
             ncol = 1, mylegend, heights = c(20,1))
