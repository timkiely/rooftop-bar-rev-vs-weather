

rm(list=ls())
library(tidyverse)
library(ggrepel)
library(plotly)

rev_ts <- readRDS("weather-revenue-data-v002.rds")


rev_ts_model <- 
  rev_ts %>% mutate(Happy_Hour = grepl("Happy Hour",Special)
                    ,Special_ind = ifelse(is.na(Special),FALSE,TRUE)
                    ,Rain_ind = grepl("Rain",Events)
                    ,Events = factor(Events)) %>% 
  filter(Earnings>0) %>% 
  filter(!is.na(Earnings))

rev_ts_model_ex_spec <- 
  rev_ts %>% mutate(Happy_Hour = grepl("Happy Hour",Special)
                    ,Special_ind = ifelse(is.na(Special),FALSE,TRUE)
                    ,Rain_ind = grepl("Rain",Events)
                    ,Events = factor(Events)) %>% 
  filter(Earnings>0) %>% 
  filter(!is.na(Earnings)) %>% 
  filter(Special_ind==FALSE)


# specials with labels --------------------------------------------------------
rev_ts_model %>% ggplot() + aes(x = Earnings, y = Mean_TemperatureF) + geom_point() + geom_smooth() + ggrepel::geom_text_repel(aes(label=Special), size = 2)

# rain indicator ----------------------------------------------------------
plotly::ggplotly(rev_ts_model %>% ggplot() + aes(x = Earnings, y = Mean_TemperatureF, label = Special) + geom_point(aes(color = Rain_ind)) + geom_smooth(method="lm"))



# specials indicator ------------------------------------------------------
plotly::ggplotly(rev_ts_model %>% ggplot() + aes(x = Earnings, y = Mean_TemperatureF, label = Special) + geom_point(aes(color = Special_ind)) + geom_smooth(method="lm"))


# log earnings -----------------------------------------------------------
rev_ts_model %>% ggplot() + aes(x = log(Earnings), y = Mean_TemperatureF) + geom_point() + geom_smooth()









