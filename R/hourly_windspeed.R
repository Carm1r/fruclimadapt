#' Estimation of the hourly wind speed from daily mean data
#'
#' This function estimates the hourly wind speed from a
#' dataset with mean daily wind speeds. Hourly wind speeds
#' from daily means are computed using a modified version of the
#' formula proposed by (Guo et al, 2016), so that maximum speeds
#' are obtained at mid-day.
#'
#' @param climdata a dataframe with daily mean wind speed data (WSmed).
#' Must contain the columns Year, Month, Day and WSmed.
#'
#' @return data frame with the columns Date, Year, Month, Day, DOY,
#' Hour and WS (hourly wind speed).
#' @author Carlos Miranda
#' @references
#'
#' Guo Z, Chang C, Wang R, 2016. A novel method to downscale daily wind
#' statistics to hourly wind data for wind erosion modelling. In: Bian F.,
#' Xie Y. (eds) Geo-Informatics in Resource Management and Sustainable
#' Ecosystem. GRMSE 2015. Communications in Computer and Information Science,
#' vol 569. Springer, Berlin, Heidelberg
#'
#'
#' @examples
#'\dontrun{
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, WSmed) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date))
#' Windspeed_hourly <- hourly_windspeed(Weather)
#'}
#'
#' @export hourly_windspeed
#' @import data.table tidyverse zoo lubridate

hourly_windspeed <- function(climdata)
{
  Viento <- select(climdata,"Year","Month","Day","WSmed") %>%
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
    select(Date,DOY,Hour,WS) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date)) %>%
    select(1,5:7,2:4) %>%
    mutate(WS = ifelse(WS <= 0, 0, WS))
  return(wind_h)
}


