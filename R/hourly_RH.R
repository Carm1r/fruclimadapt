#' Estimation of the hourly relative humidity on a daily series
#'
#' This function estimates the hourly relative humidity (RH),
#' using daily temperature and humidity data. Hourly humidity is
#' estimated with the formula used by Waichler and Wigmosta (2003)
#' which require maximum and minimum values of daily temperature
#' and relative humidity. 
#'
#' @param climdata a dataframe with daily temperatures and RH for
#' each day in a series. Must contain the columns Year, Month, Day,
#' Tmax, Tmin, RHmax (daily maximum relative humidity) and RHmin
#' (minimum daily relative humidity).
#' @param lat latitude (decimal format) of the site
#' @return data frame with columns Date, Year, Month, Day, DOY,
#' Hour, Temp and RH
#' @author Carlos Miranda
#' @references
#'
#' Luedeling E, Kunz A and Blanke M, 2013. Identification of chilling and heat
#' requirements of cherrytrees - a statistical approach.
#' International Journal of Biometeorology 57,679-689.
#'
#' Waichler SR and Wigmosta MS, 2003. Development of hourly meteorological
#' values from daily data and significance to hydrological modeling at H.J.
#' Andrews experimental forest. Journal of Hydrometeorology 4, 251-263.
#'
#' @examples
#'
#' \dontrun{
#'
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, include date and DOY information and estimate
#' #the hourly temperature and relative humidity for each day in the series.
#'
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, Tmax, Tmin, WSmed) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date))
#' RH_h <- hourly_RH_temp(Weather, 41.5)
#'
#' }
#' @export hourly_RH
#' @import data.table tidyverse zoo lubridate

hourly_RH <- function(climdata,lat)
{
  tempd <-select(climdata,"Year","Month","Day","Tmax","Tmin")
  hourt.df <- hourly_temps(tempd,lat) %>%
    select(Date, Year, Month, Day, DOY, Hour, Temp)
  temp.df <- tempd %>% mutate(Date = make_date(Year, Month, Day)) %>%
    select("Date","Tmax","Tmin") %>%
    merge(hourt.df, by="Date", all=TRUE)
  RH.df <- select(climdata,"Year","Month","Day","RHmax","RHmin") %>%
    mutate(Date = make_date(Year, Month, Day)) %>%
    select("Date","RHmax","RHmin")
  climdata_h = merge(temp.df, RH.df, by = "Date", all = TRUE) %>%
    mutate(RH = RHmax +(Temp-Tmin)*(RHmin-RHmax)/(Tmax-Tmin)) %>%
    select(Date,Year,Month,Day,DOY,Hour,Temp,RH)

  return(climdata_h)
}
