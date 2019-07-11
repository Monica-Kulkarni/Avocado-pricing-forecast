#  Import Packages
# if (!require("pacman")) install.packages("evaluate") 
# pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, caret, randomForest, e1071, rpart, 
#                xgboost, h2o, corrplot, rpart.plot, corrgram, ggplot2, highcharter, 
#                ggthemes, psych, scales, treemap, treemapify, repr, cowplot, magrittr, ggpubr,
#                RColorBrewer, plotrix, ggrepel, tidyverse, gridExtra, lubridate)
options(warn=-1)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(repr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(plotrix))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tibbletime))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(smooth))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(fpp2))
options(scipen=5)

setwd("C:/Users/Monica Kulkarni/Downloads/avocado.csv")
df <- read.csv("avocado.csv")
original_df <- df
levels(df$type)

library(fpp2)
# Change the date column from factor to date
df$Date <- as.Date(df$Date, "%Y-%m-%d")
class(df$Date)

# Sort the dates
df <- df[order(as.Date(df$Date, format="%Y-%m-%d")),]

organic <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "organic")
conventional <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "conventional")

organic <- as_tbl_time(organic, index=Date)
organic <- as_period(organic, '1 month')


# Conventional Avocadoes
conventional <- as_tbl_time(conventional, index=Date)
conventional <- as_period(conventional, '1 month')

# Let's declare our data as time series
conv <- df %>% select(Date, AveragePrice, type) %>% filter(type == "conventional")
org <- df %>% select(Date, AveragePrice, type) %>% filter(type == "organic")
# Organic Avocados
conventional <- as_tbl_time(conv, index=Date)
conventional <- as_period(conventional, '1 month')
conventional$type <- NULL
# Organic Avocados
organic <- as_tbl_time(org, index=Date)
organic <- as_period(organic, '1 month')
organic$type <- NULL

conv_ts <- ts(conventional[,2], start=c(2015, 1), frequency=12)
org_ts <- ts(organic[,2], start=c(2015, 1), frequency=12)
# The difference from month to month
# To remove the trend we take the first difference
differences_conv <- diff(conv_ts)

main_diff <- autoplot(differences_conv) + theme_minimal()

seasonality_diff <- ggseasonplot(differences_conv) + theme_minimal()

plot_grid(main_diff, seasonality_diff, nrow=2)


# ARIMA Model
# Y has trend unlike difference, it will take the difference behind the scenes d=1
# Stepwise will only use some models instead of all possible combinations
# approximation uses the model that approximates the best result to save time
arima_model_cv <- auto.arima(conv_ts, d=1, D=1, stepwise=FALSE, approximation=FALSE, trace=TRUE)
arima_model_or <- auto.arima(org_ts, d=1, D=1, stepwise=FALSE, approximation=FALSE, trace=TRUE)


print(summary(arima_model_cv))
checkresiduals(arima_model_cv) + theme_minimal()

forecast_cv <- forecast(arima_model_cv, h=24)
# Include means including the last 60 months in order to see closer the forecast.
autoplot(forecast_cv, include=60) + theme_light() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#fda97c"),
                                                            legend.position="bottom", panel.background = element_rect(fill="#ffface",
                                                                                                                       size=0.5, linetype="solid", 
                                                                                                                       colour ="black")) + labs(title="Forecasting using ARIMA model \n for conventional avocados", x="Year", y="Price")
print(summary(forecast_cv))





forecast_org <- forecast(arima_model_or, h=24)
# Include means including the last 60 months in order to see closer the forecast.
autoplot(forecast_org, include=60) + theme_light() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#a3faac"),
                                                             legend.position="bottom", panel.background = element_rect(fill="#ffface",
                                                                                                                        size=0.5, linetype="solid", 
                                                                                                                        colour ="black")) + labs(title="Forecasting using ARIMA model \n for organic avocados", x="Year", y="Price")
print(summary(forecast_org))

