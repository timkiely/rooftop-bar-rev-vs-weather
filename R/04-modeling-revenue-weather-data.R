
# Modeling the data, looking for inferences e.g., how much rev is lost to rain?

rm(list=ls())
library(tidyverse)

rev_ts <- readRDS("weather-revenue-data-v002.rds")

rev_ts_model <- 
  rev_ts %>% mutate(Happy_Hour = grepl("Happy Hour",Special)
                    , Special_ind = ifelse(is.na(Special),FALSE,TRUE)
                    , Rain_ind = ifelse(PrecipitationIn>0.5,TRUE,FALSE)
                    , Rain_ind = ifelse(is.na(Rain_ind),FALSE,Rain_ind)
                    , Events = factor(Events)) %>% 
  filter(Earnings>0) %>% 
  filter(!is.na(Earnings))



rev_ts_model_ex_specials <- 
  rev_ts %>% mutate(Happy_Hour = grepl("Happy Hour",Special)
                    , Special_ind = ifelse(is.na(Special),FALSE,TRUE)
                    , Rain_ind = ifelse(PrecipitationIn>0.5,TRUE,FALSE)
                    , Rain_ind = ifelse(is.na(Rain_ind),FALSE,Rain_ind)
                    , Events = factor(Events)) %>% 
  filter(Earnings>0) %>% 
  filter(!is.na(Earnings)) %>% 
  filter(Special_ind==FALSE)


# generate formula text  ------------------------------------------------------
## use this to generate formula text in the console:
# as.formula(c("Earnings~",paste0(names(rev_ts_model)[names(rev_ts_model)!="Earnings"], collapse = "+")))

full_model <- as.formula(
  Earnings ~  Max_TemperatureF + Mean_TemperatureF + Min_TemperatureF + 
    Max_Dew_PointF + MeanDew_PointF + Min_DewpointF + Max_Humidity + 
    Mean_Humidity + Min_Humidity + Max_Sea_Level_PressureIn + 
    Mean_Sea_Level_PressureIn + Min_Sea_Level_PressureIn + Max_VisibilityMiles + 
    Mean_VisibilityMiles + Min_VisibilityMiles + Max_Wind_SpeedMPH + 
    Mean_Wind_SpeedMPH + Max_Gust_SpeedMPH + PrecipitationIn + 
    CloudCover + Events + WindDirDegrees + Date + Earnings + 
    Day + Day_lab + Month + Year.y + Special + Year + Happy_Hour + 
    Special_ind + Rain_ind
  )


full_lm <- lm(formula = full_model, data = rev_ts_model)
summary(full_lm)



# trail 1. Full model  --------------------------------------------------------

f1 <- as.formula(
  Earnings~ 
    Mean_TemperatureF+
    #poly(Mean_TemperatureF,2)+
    #I(Mean_TemperatureF^2)+
    MeanDew_PointF+
    Mean_Humidity+
    Mean_Sea_Level_PressureIn+
    Mean_VisibilityMiles+
    Mean_Wind_SpeedMPH+
    Max_Gust_SpeedMPH+
    PrecipitationIn+
    CloudCover
    #Special
    #Events+
    #WindDirDegrees
    #Happy_Hour+
    #Special_ind
  )

f1_lm <- lm(formula = f1, data = rev_ts_model)
summary(f1_lm)


f1_glm <- glm(formula = f1, data = rev_ts_model)
summary(f1_glm)




# trial 2. temp & precipitation -----------------------------------------------
f2 <- as.formula(
  Earnings~ 
    Mean_TemperatureF+
    Max_TemperatureF+
    Min_TemperatureF+
    PrecipitationIn+
    CloudCover+
    Rain_ind
)

f2_lm <- lm(formula = f2
            , data =rev_ts_model
)
summary(f2_lm)


f2_glm <- glm(formula = f2
              , data =rev_ts_model
)
summary(f2_glm)



# tial 3 log earnings --------------------------------------------------------

f3 <- as.formula(
  log(Earnings)~ 
    Mean_TemperatureF+
    PrecipitationIn+
    CloudCover+
    Special+
    Rain_ind
)

f3_lm <- lm(formula = f3
            , data =rev_ts_model
)
summary(f3_lm)


f2_glm <- glm(formula = f2
              , data =rev_ts_model
)
summary(f2_glm)




# modeling temp -----------------------------------------------------------


f4 <- as.formula(Earnings ~ Mean_TemperatureF)

f4_lm <- lm(formula = f4
            , data = rev_ts_model %>% filter(Earnings<4000)
)
summary(f4_lm)



# effect of rain on response ----------------------------------------------

f5 <- as.formula(Earnings ~ PrecipitationIn)

f5_lm <- lm(formula = f5
            , data = rev_ts_model
)
summary(f5_lm)



