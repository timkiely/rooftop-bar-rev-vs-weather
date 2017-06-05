
rm(list=ls())
library(tidyverse)
library(stringr)


rev_data <- read_csv("Bar Revenue Data/Daily Bar Revenue.csv")
rev_data %>% glimpse()

# create date variables -------------------------------------------------------
rev_data <- 
  rev_data %>% 
  rowwise() %>% 
  mutate(date_Day = str_extract(Date,"[^/]*$")
         ,date_Month = str_extract(Date,"[^/]*")
         ) 



# split the dataframe into individual years, add dates and re-combine ---------
rev_data_1 <- rev_data %>% select(Date,`2015 Bar Earnings`,Day, date_Month, date_Day)
rev_data_2 <- rev_data %>% select(Date,`2016 Bar Earnings`,Day2, date_Month, date_Day)
rev_data_3 <- rev_data %>% select(Date,`2017 Bar Earnings`,Day3, date_Month, date_Day)

rev_data_1_clean <- rev_data_1 %>% mutate(Year = 2015) %>% rename("Earnings"=`2015 Bar Earnings`, "Day"=Day) %>% filter(Earnings!="CLOSED") %>% mutate(Date_actual = as.Date(str_c(Date,"/",Year),format="%m/%d/%Y")) %>% select(-Date,-date_Month,-date_Day,-Year)
rev_data_2_clean <- rev_data_2 %>% mutate(Year = 2016) %>% rename("Earnings"=`2016 Bar Earnings`, "Day"=Day2) %>% filter(Earnings!="CLOSED") %>% mutate(Date_actual = as.Date(str_c(Date,"/",Year),format="%m/%d/%Y")) %>% select(-Date,-date_Month,-date_Day,-Year)
rev_data_3_clean <- rev_data_3 %>% mutate(Year = 2017) %>% rename("Earnings"=`2017 Bar Earnings`, "Day"=Day3) %>% filter(Earnings!="CLOSED") %>% mutate(Date_actual = as.Date(str_c(Date,"/",Year),format="%m/%d/%Y")) %>% select(-Date,-date_Month,-date_Day,-Year)


rev_data_clean <- 
  bind_rows(rev_data_1_clean,rev_data_2_clean,rev_data_3_clean) %>% 
  filter(Earnings!="Closed") %>% 
  mutate(Earnings = as.numeric(Earnings)) %>% 
  mutate(Day_lab = lubridate::wday(Date_actual, label = T)
         ,Month = lubridate::month(Date_actual, label = T)
         ,Year = lubridate::year(Date_actual)
         )


# do some EDA -----------------------------------------------------------------
rev_data_clean %>% ggplot() + aes(x = Date_actual, y = Earnings) + geom_line(group=1)
rev_data_clean %>% ggplot() + aes(x = Date_actual, y = Earnings, fill = Earnings) + geom_col()
rev_data_clean %>% 
  ggplot() + 
  aes(x = Date_actual, y = Earnings, fill = Earnings) + 
  geom_col()+
  facet_grid(Month~Year, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# isolate the specials --------------------------------------------------------
specials_2016 <- rev_data %>% filter(is.na(Weather),!is.na(`Events/Specials/Notes`)) %>% select(Date,`Events/Specials/Notes`) %>% rename('Special' = `Events/Specials/Notes`)
specials_2016 <- specials_2016 %>% mutate(Year = 2016, Date_actual = as.Date(str_c(Date,"/",Year),format="%m/%d/%Y"))
specials_2017 <- rev_data %>% filter(!is.na(Weather)) %>% select(Date,`Events/Specials/Notes`) %>% rename('Special' = `Events/Specials/Notes`)
specials_2017 <- specials_2017 %>% mutate(Year = 2016, Date_actual = as.Date(str_c(Date,"/",Year),format="%m/%d/%Y"))
specials <- bind_rows(specials_2016,specials_2017) %>% select(-Date,-Year)


# combine specials+revenue data -----------------------------------------------
rev_data_plus_specials <- left_join(rev_data_clean,specials, by = "Date_actual")



# create sequence of all dates ------------------------------------------------
all_date_seq <- seq.Date(from = min(rev_data_clean$Date_actual)
                          , to = max(rev_data_clean$Date_actual) 
                           , by = "day")


# map in weather data ---------------------------------------------------------

## data from https://www.wunderground.com/personal-weather-station/dashboard?ID=KNYNEWYO395#history/s20150101/e20170523/mcustom

# First batch:
# weather_raw <- read_csv("Weather-Chelsea-NY.csv")

# improived data: 
weather_raw <- read_csv("Weather Data/Weather-NYC-2017-05-30.csv")

                                                      

weather_clean <- 
  weather_raw %>% 
  mutate(PrecipitationIn = as.numeric(PrecipitationIn)
         ) %>% 
  select(-EDT,-EST)


# Join weather data to revenue data -------------------------------------------

# check to see if the date range is complete:
anti_join(data.frame("Date" = all_date_seq), weather_clean)


rev_ts <- tbl_df(left_join(weather_clean,rev_data_plus_specials, by = c("Date" = "Date_actual")))


rev_ts <- 
  rev_ts %>% 
  mutate(Day_lab = lubridate::wday(Date, label = T)
       , Month = lubridate::month(Date, label = T)
       , Year = lubridate::year(Date)
       )


saveRDS(rev_ts,"weather-revenue-data-v002.rds")





