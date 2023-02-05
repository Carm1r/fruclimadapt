#' Estimation of the hourly wind speed from daily mean data
#'
#' This function estimates the hourly wind speed from a
#' dataset with mean daily wind speeds. Hourly wind speeds
#' from daily values are computed using the formulas proposed
#' by Guo et al (2016), using mean daily values (u2med, required)
#' and maximum ones (u2max, optional). If only mean wind values
#' are available, the function uses a modified version of the 
#' Guo formula, so that the maximum values are obtained in 
#' daytime hours.
#' 
#'
#' @param climdata a dataframe with daily wind speed data.
#' Required columns are Year, Month, Day and u2med. u2max
#' is an optional data column.
#'
#' @return dataframe with the columns Date, Year, Month, Day, DOY,
#' Hour and u2 (hourly wind speed, m s-1).
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
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
#'
#' # Generate hourly wind speed for the example dataset
#' library(magrittr)
#' library(dplyr)
#' library(lubridate)
#' Tudela_Hu2 <- hourly_windspeed(Tudela_DW)
#'
#' @export hourly_windspeed
#' @import magrittr dplyr 
#' @importFrom lubridate make_date make_datetime date as_datetime year month day yday hour

hourly_windspeed <- function(climdata)
{
  if(!"u2max" %in% colnames(climdata))
  {
    message("Warning: No maximum windspeed data provided,\nhourly values will
        be estimated using u2med only\n");
    Viento <- select(climdata,"Year","Month","Day","u2med") %>%
      mutate(Datetime = make_datetime(Year, Month, Day, hour=0,min = 0),
             Date=make_date(Year, Month, Day))
    minday = as_datetime(Viento$Date[1])
    maxday = as_datetime(Viento$Date[nrow(Viento)])
    dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
      rename(Datetime=1) %>%
      mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
             Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))
    
    wind_h = merge(dates, Viento, by = "Date", all = TRUE) %>%
      mutate(u2 = ifelse(u2med + (1/2)*u2med*cos((Hour+12)*pi/12)<0,0,
             u2med + (1/2)*u2med*cos((Hour+12)*pi/12)))
  }else{
    Viento <- select(climdata,"Year","Month","Day","u2med","u2max") %>%
      mutate(Datetime = make_datetime(Year, Month, Day, hour=0,min = 0),
             Date=make_date(Year, Month, Day))
    minday = as_datetime(Viento$Date[1])
    maxday = as_datetime(Viento$Date[nrow(Viento)])
    dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
      rename(Datetime=1) %>%
      mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
             Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))
    
    wind_h = merge(dates, Viento, by = "Date", all = TRUE) %>%
      mutate(u2 = ifelse(u2med + (1/pi)*u2max*cos(Hour*pi/12)<0,0,
             u2med + (1/pi)*u2max*cos(Hour*pi/12)))
  }
  wind_h <- wind_h %>% select(Date,DOY,Hour,u2) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date)) %>%
    select(1,5:7,2:4) %>%
    mutate(u2 = ifelse(u2 <= 0, 0, u2))
  return(wind_h)
}


