Rooftop Bar Revenue vs Weather
================

We're interested in the revenues of a rooftop bar in NYC vs the weather on corresponding days. Is there and headroom for revenue growth by, for example, building an enclosure? Would heating or covering the roofdeck bring in additional revenue? How much do you lose per year to rain?

Data
====

The revenue data was obtained independently directly from the hotel bar. The weather data was scraped from [weatherunderground.com](https://www.wunderground.com/) using the `library(weatherData)` package.

There are many potenital "stations" you can gather weather data from. It is important to find not just the most physically proximate station, but also to find a station with consitent, clean data. Some of the data API functions did not return the correct data, or the time series it returned was unduly censcored. to that end, `getStationCode("New York", region = "NY")` was helpful in retrieving the most reliable station for the NYC area: LGA.

I modified the `getWeatherForYear` function slightly, as the out-of-the-box solution wasn't scraping the API correctly. In addition, I found that scraping the years individually worked, while supplying a range of dates did not. If you wanted to scrape several years of data, one of the `purrr::map()` funtions could make that possible.

The final data was concatenated into a single dataframe and written to disk for analysis:

|  Year| EST      |  Max\_TemperatureF|  Mean\_TemperatureF|  Min\_TemperatureF|  Max\_Dew\_PointF|  MeanDew\_PointF|  Min\_DewpointF|  Max\_Humidity|  Mean\_Humidity|  Min\_Humidity|  Max\_Sea\_Level\_PressureIn|  Mean\_Sea\_Level\_PressureIn|  Min\_Sea\_Level\_PressureIn|  Max\_VisibilityMiles|  Mean\_VisibilityMiles|  Min\_VisibilityMiles|  Max\_Wind\_SpeedMPH|  Mean\_Wind\_SpeedMPH|  Max\_Gust\_SpeedMPH|  PrecipitationIn|  CloudCover| Events    |  WindDirDegrees| Date       | EDT |
|-----:|:---------|------------------:|-------------------:|------------------:|-----------------:|----------------:|---------------:|--------------:|---------------:|--------------:|----------------------------:|-----------------------------:|----------------------------:|---------------------:|----------------------:|---------------------:|--------------------:|---------------------:|--------------------:|----------------:|-----------:|:----------|---------------:|:-----------|:----|
|  2015| 2015-1-1 |                 39|                  33|                 27|                18|               10|               5|             44|              36|             27|                        30.19|                         30.08|                        29.99|                    10|                     10|                    10|                   23|                    14|                   31|             0.00|           1| NA        |             242| 2015-01-01 | NA  |
|  2015| 2015-1-2 |                 42|                  39|                 35|                22|               18|              15|             56|              46|             36|                        30.46|                         30.22|                        30.02|                    10|                     10|                    10|                   25|                    14|                   32|             0.00|           4| NA        |             277| 2015-01-02 | NA  |
|  2015| 2015-1-3 |                 41|                  36|                 31|                40|               29|              19|            100|              75|             50|                        30.60|                         30.44|                        30.09|                    10|                      4|                     1|                   16|                    10|                   20|             0.67|           7| Rain-Snow |              61| 2015-01-03 | NA  |
|  2015| 2015-1-4 |                 57|                  49|                 41|                52|               44|              30|            100|              92|             83|                        30.07|                         29.85|                        29.66|                    10|                      6|                     0|                   30|                    11|                   44|             0.31|           8| Fog-Rain  |             220| 2015-01-04 | NA  |
|  2015| 2015-1-5 |                 50|                  36|                 22|                28|               13|               3|             53|              40|             27|                        30.41|                         30.12|                        29.80|                    10|                     10|                    10|                   32|                    21|                   49|             0.00|           3| NA        |             294| 2015-01-05 | NA  |

EDA
===

For a complete list of EDA plots, run the `03-weather-revenue-EDA.R` script. Amoung many interesting relationships, the correlation betwen Temperature and Earnings proved visually appealing. Various nightly specials (many of which could be considered outliers), also proved to be an interesting candidate for the modeling step:

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

Modeling
========

Rather than attempting to create a highly predictive model, our goal here is purely inferential. In such a case, simple linear models may prove to be the best tool since they are comuputationally negligible and hihgly interpretable.

Let's look at the relationship bettwen the `Earnings` variable and both `Temp` and `Precipitation`

    ## 
    ## Call:
    ## lm(formula = f4, data = rev_ts_model %>% filter(Earnings < 4000))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1547.1  -462.7  -130.6   342.5  2601.7 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -133.402    196.417  -0.679    0.497    
    ## Mean_TemperatureF   19.692      2.782   7.079 6.54e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 640.3 on 401 degrees of freedom
    ## Multiple R-squared:  0.1111, Adjusted R-squared:  0.1089 
    ## F-statistic: 50.11 on 1 and 401 DF,  p-value: 6.536e-12

    ## 
    ## Call:
    ## lm(formula = f5, data = rev_ts_model)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1275.7  -538.8  -132.4   405.1  4118.5 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      1294.65      43.52  29.749   <2e-16 ***
    ## PrecipitationIn  -214.71     201.54  -1.065    0.287    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 764.5 on 344 degrees of freedom
    ##   (60 observations deleted due to missingness)
    ## Multiple R-squared:  0.003288,   Adjusted R-squared:  0.0003908 
    ## F-statistic: 1.135 on 1 and 344 DF,  p-value: 0.2875
