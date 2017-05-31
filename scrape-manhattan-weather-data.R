

# scrape weather data from wunderground 

library(weatherData)
library(tidyverse)

# look up station code
getStationCode("Manhattan", region = "NY")
getStationCode("New York", region = "NY")
checkDataAvailabilityForDateRange("KNYC", "2015-01-01", "2017-05-30")



getWeatherForYear2 <- 
  function (station_id, year, station_type = "airportCode", opt_detailed = TRUE, 
          opt_write_to_file = FALSE, opt_all_columns = TRUE) 
{
  if (year == (1900 + as.POSIXlt(Sys.Date())$year)) {
    last_day <- Sys.Date()
  }
  else {
    last_day <- paste0(year, "-12-31")
  }
  first_day <- paste0(year, "-01-01")
  getWeatherForDate(station_id, first_day, last_day, station_type, 
                    opt_detailed, opt_all_columns = opt_all_columns)
}

ny_2015 <- getWeatherForYear2(station_id="KLGA", year = 2015)
ny_2016 <- getWeatherForYear2(station_id="KLGA", year = 2016)
ny_2017 <- getWeatherForYear2(station_id="KLGA", year = 2017)

ny_2015_clean <- ny_2015 %>% select(-Date) %>% tbl_df() %>% mutate(Date = lubridate::ymd(EST))
ny_2016_clean <- ny_2016 %>% select(-Date) %>% tbl_df() %>% mutate(Date = lubridate::ymd(EST))
ny_2017_clean <- ny_2017 %>% select(-Date) %>% tbl_df() %>% mutate(Date = lubridate::ymd(EDT))


lga_data <- bind_rows(list("2015"=ny_2015_clean
                           , "2016" = ny_2016_clean
                           , "2017" = ny_2017_clean
                           )
                           , .id = "Year")


write_csv(lga_data,"Weather-NYC-2017-05-30.csv")








