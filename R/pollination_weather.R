#' Evaluation of weather conditions for pollination on a daily series
#'
#' This function estimates the number of days with conditions
#' favorable, unfavorable and moderately favorable for insect
#' pollination of fruit trees during the flowering period using
#' daily weather data.
#'
#' Days are classified considering the classification proposed by
#' Williams and Sims (1977), by accounting the number of favorable
#' hours for pollination within a day. One hour is considered favorable
#' if the temperature is above 12.5 C, the speed of the wind below
#' 4.5 m/s and no rainfal occurs (Williams and Sims, 1977; Ramirez and
#' Davenport, 2013). Hourly wind speeds from daily values are computed
#' using the formulas proposed by (Guo et al, 2016), using mean daily
#' values (WSmed, required) and maximum ones (WSmax, optional). If 
#' only mean wind values are available, the function uses a modified
#' version of the Guo formula, so that the maximum values are obtained in 
#' daytime hours.No hourly downscaling of rainfall is performed, the 
#' function allow daily rainfall below 2.0 mm when estimating if a day
#' is favorable for pollination or not.
#'
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures,
#' wind speed and precipitation. Required columns are Year, Month, Day,
#' Tmax, Tmin, WSmed (daily mean wind speed) and Prec (precipitation).
#' WSmax (daily maximum wind speed) is optional.
#' @param lat latitude (decimal format) of the site, used to estimate hourly
#' temperatures.
#' @param fendata a dataframe with julian day of the beginning (sbloom)
#' and end (ebloom) of the flowering season. Must contain the columns
#' Year, sbloom and ebloom in that order.
#' @return a data frame with the columns Year, Sbloom (bloom start, DOY)
#' , Ebloom (end of bloom, DOY), Bloom_length (in days), Favor (number of
#' favorable days), Modfavor (number of moderately favorable days) and
#' Unfavor (number of unfavorable days).
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Guo Z, Chang C, Wang R, 2016. A novel method to downscale daily wind
#' statistics to hourly wind data for wind erosion modelling. In: Bian F.,
#' Xie Y. (eds) Geo-Informatics in Resource Management and Sustainable
#' Ecosystem. GRMSE 2015. Communications in Computer and Information Science,
#' vol 569. Springer, Berlin, Heidelberg
#'
#' Ramirez F and Davenport TL, 2013. Apple pollination: A review. Scientia
#' Horticulturae 162, 188-203.
#'
#' Williams RR, Sims FP, 1977. The importance of weather and variability
#' in flowering time when deciding pollination schemes for Cox's Orange
#' Pippin. Experimental Horticulture 29, 15-26.
#'
#' @examples
#'
#' \dontrun{
#'
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, include date and DOY information and estimate
#' #the number favorable days on each year in the series
#'
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, Tmax, Tmin, WSmed, Prec) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date))
#' Pollin_eval <- pollination_weather(Weather,Bloom_D, 41.5)
#'
#'
#' }
#' @export pollination_weather
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date make_datetime

pollination_weather <- function(climdata, fendata, lat)
{
  rain <- select(climdata,"Year","Month","Day","Prec") %>%
    mutate(Date = make_date(Year, Month, Day)) %>%
    select(Date,Prec)
  tempd <-select(climdata,"Year","Month","Day","Tmax","Tmin")
  hourt <- hourly_temps(tempd,lat) 
  hourt.df <- hourt %>%
    mutate(Datetime = make_datetime(Year, Month, Day, Hour,min = 0)) %>%
    select(Datetime,Date,Year,Month,Day,DOY,Hour,Temp)

  wind.df <- hourly_windspeed(climdata) %>%
    mutate(Datetime = make_datetime(Year, Month, Day, Hour,min = 0))

  climdata_h = merge(hourt.df,wind.df, by = "Datetime", all=TRUE) %>%
    select(Datetime,Temp,WS) %>%
    mutate(Date=date(Datetime),Year=year(Datetime),Month=month(Datetime),Day=day(Datetime),Hour=hour(Datetime)) %>%
    select(1,4:8,2:3)

  pollinw <- climdata_h %>%
    mutate(wpol= if_else(Temp>=12.5 & Temp<=30 & WS <=5.6,1,0)) %>%
    group_by(Date) %>%
    summarise(h_wpol= sum(wpol)) %>%
    right_join(rain, by = "Date", all = TRUE) %>%
    mutate(h_wpol = ifelse(Prec >= 2, 0, h_wpol)) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date), DOY=yday(Date))

  Seasons <- unique(fendata$Year)

  pollrisk_cn <- c("Year","SBloom","EBloom","Bloom_l","Favor","Modfavor","Unfavor")
  pollrisk.df <-data.frame(matrix(ncol=7, nrow=0, byrow=FALSE))
  colnames(pollrisk.df) <- pollrisk_cn

  for (sea in 1:(length(Seasons))){
    Fdate <- slice(fendata, sea)
    Anno <- as.numeric(Fdate[1])
    Sbloom <- as.numeric(Fdate[2])
    Ebloom <- as.numeric(Fdate[3])
    polhours_fil <- pollinw %>%
      filter(pollinw$Year==Anno & pollinw$DOY>=Sbloom & pollinw$DOY<=Ebloom) %>%
      mutate(Days=length(Year)) %>%
      group_by(Year) %>%
      summarise(Bloom_length = mean(Days), Favor = sum(h_wpol >=6), Unfavor = sum(h_wpol < 2), Modfavor=mean(Days)-(Favor+Unfavor))
    new.row.df <- data.frame(Sbloom,Ebloom) %>%
      cbind(polhours_fil) %>%
      select(3,1:2,4,5,7,6)
    pollrisk.df <-rbind(pollrisk.df,new.row.df)
  }
  return(pollrisk.df)
}
