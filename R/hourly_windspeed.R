#' Estimation of the hourly wind speed from daily mean data
#'
#' This function estimates the hourly wind speed from a
#' dataset with mean daily wind speeds. Hourly wind speeds
#' from daily values are computed using the formulas proposed
#' by (Guo et al, 2016), using mean daily values (WSmed, required)
#' and maximum ones (WSmax, optional). If only mean wind values
#' are available, the function uses a modified version of the 
#' Guo formula, so that the maximum values are obtained in 
#' daytime hours.
#' 
#'
#' @param climdata a dataframe with daily wind speed data.
#' Required columns are Year, Month, Day and WSmed. WSmax
#' is an optional data column.
#'
#' @return data frame with the columns Date, Year, Month, Day, DOY,
#' Hour and WS (hourly wind speed).
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
#'\dontrun{
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, WSmed) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date))
#' Windspeed_hourly <- hourly_windspeed(Weather)
#'}
#'
#' @export hourly_windspeed
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date make_datetime date

hourly_windspeed <- function(climdata)
{
  if(!"WSmax" %in% colnames(climdata))
  {
    cat("Warning: No maximum windspeed data provided,\nhourly values will
        be estimated using WSmed only\n");
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
      mutate(WS = ifelse(WSmed + (1/2)*WSmed*cos((Hour+12)*pi/12)<0,0,
             WSmed + (1/2)*WSmed*cos((Hour+12)*pi/12)))
  }else{
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
      mutate(WS = ifelse(WSmed + (1/pi)*WSmax*cos(Hour*pi/12)<0,0,
             WSmed + (1/pi)*WSmax*cos(Hour*pi/12)))
  }
  wind_h <- wind_h %>% select(Date,DOY,Hour,WS) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date)) %>%
    select(1,5:7,2:4) %>%
    mutate(WS = ifelse(WS <= 0, 0, WS))
  return(wind_h)
}


