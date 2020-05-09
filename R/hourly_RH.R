#' Estimation of the hourly relative humidity on a daily series
#'
#' This function estimates the hourly relative humidity (RH),
#' using daily temperature and humidity data. Hourly humidity is
#' estimated with the formula proposed by Waichler and Wigmosta (2003)
#' which require maximum and minimum values of daily temperature
#' and relative humidity. 
#'
#' @param climdata a dataframe with daily temperatures and RH for
#' each day in a series. Must contain the columns Year, Month, Day,
#' Tmax, Tmin, RHmax (daily maximum relative humidity) and RHmin
#' (minimum daily relative humidity).
#' @param lat the latitude of the site, in decimal degrees.
#' @return dataframe with columns Date, Year, Month, Day, DOY,
#' Hour, Temp and RH
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Waichler SR and Wigmosta MS, 2003. Development of hourly meteorological
#' values from daily data and significance to hydrological modeling at H.J.
#' Andrews experimental forest. Journal of Hydrometeorology 4, 251-263.
#'
#' @examples
#'
#' \dontrun{
#' # Generate hourly relative humidity
#' Tudela_HRH <- hourly_RH(Tudela_DW, 42.13132)
#' }
#' @export hourly_RH
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date make_datetime date

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
    mutate(RH_aux = RHmax +((Temp-Tmin)/(Tmax-Tmin))*(RHmin-RHmax),
           RH = ifelse(RH_aux>RHmax,RHmax,
                       ifelse(RH_aux<RHmin,RHmin,RH_aux))) %>%
    select(Date,Year,Month,Day,DOY,Hour,Temp,RH)

  return(climdata_h)
}
