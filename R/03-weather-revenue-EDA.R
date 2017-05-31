

rm(list=ls())
library(tidyverse)
library(ggrepel)
library(plotly)

rev_ts <- readRDS("weather-revenue-data-v002.rds")

rev_ts <- 
  rev_ts %>% 
  mutate(Happy_Hour = grepl("Happy Hour",Special)
         , Special_ind = ifelse(is.na(Special),FALSE,TRUE)
         , Rain_ind = ifelse(PrecipitationIn>0.5,TRUE,FALSE)
         , Rain_ind = ifelse(is.na(Rain_ind),FALSE,Rain_ind)
         , Events = factor(Events))

rev_ts_model <- 
  rev_ts %>% mutate(Happy_Hour = grepl("Happy Hour",Special)
                    , Special_ind = ifelse(is.na(Special),FALSE,TRUE)
                    , Rain_ind = grepl("Rain",Events)
                    , Events = factor(Events)) %>% 
  filter(Earnings>0) %>% 
  filter(!is.na(Earnings))


# specials with labels --------------------------------------------------------
rev_ts_model %>% ggplot() + 
  aes(x = Earnings, y = Mean_TemperatureF) + 
  geom_point() + 
  geom_smooth() + 
  ggrepel::geom_text_repel(aes(label=Special), size = 2)



# rain indicator ----------------------------------------------------------
plotly::ggplotly(rev_ts %>% ggplot() + 
                   aes(x = Earnings, y = Mean_TemperatureF, label = Special) + 
                   geom_point(aes(color = Rain_ind)) + geom_smooth(method="lm"))



# specials indicator ------------------------------------------------------
plotly::ggplotly(rev_ts_model %>% ggplot() + 
                   aes(x = Earnings, y = Mean_TemperatureF, label = Special) + 
                   geom_point(aes(color = Special_ind)) + 
                   geom_smooth(method="lm"))


# log earnings -----------------------------------------------------------
rev_ts_model %>% ggplot() + aes(x = log(Earnings), y = Mean_TemperatureF) + geom_point() + geom_smooth()




# revenue when it rains? --------------------------------------------------

rev_ts %>%
  group_by(Year,Rain_ind) %>% 
  summarise(Av_Rev = mean(Earnings, na.rm=T)
            ,min(Earnings,na.rm=T),max(Earnings,na.rm=T))

ggplot() + aes(x = log(Earnings), y = Mean_TemperatureF) + 
  geom_point() + geom_smooth()






