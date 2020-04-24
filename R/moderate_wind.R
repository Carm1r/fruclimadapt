#' Estimation of the daily hours with moderate wind from
#' daily weather data
#'
#' This function estimates the daily hours with less than
#' 'moderate breeze' wind (below 5.5 m/s in the Beaufort scale) from a
#' dataset with daily wind speeds. Hourly wind speeds
#' from daily values are computed using the formulas proposed
#' by (Guo et al, 2016), using mean daily values (WSmed, required)
#' and maximum ones (WSmax, optional). If only mean wind values
#' are available, the function uses a modified version of the 
#' Guo formula, so that the maximum values are obtained in 
#' daytime hours.
#'
#' @param climdata a dataframe with daily wind speed data.
#' Required columns are Year, Month, Day and WSmed. WSmax
#' is an optional data column.
#'
#' @return data frame with the columns Date, Year, Month, Day, DOY,
#' Hour and h_wind (hours with wind speed below 5.5 m/s).
#'
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Guo Z, Chang C, Wang R, 2016. A novel method to downscale daily wind
#' statistics to hourly wind data for wind erosion modelling. In: Bian F.,
#' Xie Y. (eds) Geo-Informatics in Resource Management and Sustainable
#' Ecosystem. GRMSE 2015. Communications in Computer and Information Science,
#' vol 569. Springer, Berlin, Heidelberg
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
#'    select(Year, Month, Day, WSmed) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date))
#' hours_modwind <- moderate_wind(Weather)
#'
#' }
#' @export moderate_wind
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date make_datetime

moderate_wind <- function(climdata)
{
  Viento <- select(climdata,"Year","Month","Day","WSmed","WSmax") %>%
    mutate(Datetime = make_datetime(Year, Month, Day, hour=0,min = 0),
           Date=make_date(Year, Month, Day))
  minday = as_datetime(Viento$Date[1])
  maxday = as_datetime(Viento$Date[nrow(Viento)])
  dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
    rename(Datetime=1) %>%
    mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
           Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))

  wind_h = merge(dates, Viento, by = "Date", all = TRUE) %>%
    mutate(WS = WSmed + (1/pi)*WSmed*cos((Hour+12)*pi/12)) %>%
    group_by(Date)%>%
    summarise(h_wind= sum(WS>=5.5)) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date),DOY = yday(Date)) %>%
    select(1,3:6,2)
  return(wind_h)
}
