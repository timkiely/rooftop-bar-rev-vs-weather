

library(tidyverse)
library(stringr)


rev_data <- read_csv("Daily Bar Revenue - FFI.csv")
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
weather_raw <- read_csv("Weather-NYC-2017-05-30.csv")


extract_numbers <- function(x) as.numeric(str_extract(x,str_c("[0-9][0-9][.][0-9][0-9]" # 99.99
                                                              ,"[0-9][0-9][.][0-9]" # 99.9
                                                              ,"[0-9][0-9]" # 99
                                                              ,"[0-9][.][0-9]" # 9.9
                                                              ,"[0-9][.][0-9][0-9]" # 9.99
                                                              ,"[.][0-9][0-9]" # .99
                                                              ,"^[0-9]" # 9
                                                              ,sep="|")
)
)



weather_clean <- 
  weather_raw %>% 
  mutate(Date = as.Date(str_c(Year,"/",Month,"/",Day), format = "%Y/%b/%d") 
  ) %>% 
  mutate_at(.cols = vars(`Temperature High`,`Temperature Avg`,`Temperature Low`
                         , `Dew Point High`, `Dew Point Avg`, `Dew Point Low`
                         , `Humidity High`, `Humidity Avg`, `Humidity Low`
                         , `Speed High`, `Speed Avg`, `Speed Gust`
                         , `Pressure High`,`Pressure Avg`,`Pressure Low`
                         , `Precip. Accum.`)
            ,.funs = extract_numbers
  ) %>%
  mutate(`Humidity High` = `Humidity High`/100
         ,`Humidity Avg` = `Humidity Avg`/100
         ,`Humidity Low` = `Humidity Low`/100
  ) %>% 
  select(-Day,-Month,-Year)


# Join weather data to revenue data -------------------------------------------

rev_ts1 <- left_join(data.frame("Date"=all_date_seq),weather_clean, by = "Date")
rev_ts <- tbl_df(left_join(rev_ts1,rev_data_plus_specials, by = c("Date"="Date_actual")))


rev_ts <- 
  rev_ts %>% 
  mutate(Day_lab = lubridate::wday(Date, label = T)
         ,Month = lubridate::month(Date, label = T)
         ,Year = lubridate::year(Date)
  )


saveRDS(rev_ts,"weather-revenue-data.rds")





