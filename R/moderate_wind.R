#' Estimation of the daily hours with moderate wind from
#' daily weather data
#'
#' This function estimates the daily hours with wind speed equal or
#' above than 'moderate breeze' wind (5.5 m s-1 in the Beaufort scale)
#' from a dataset with daily wind speeds. Hourly wind speeds
#' from daily values are computed using the formulas proposed
#' by Guo et al (2016), using mean daily values (u2med, required)
#' and maximum ones (u2max, optional). If only mean wind values
#' are available, the function uses a modified version of the 
#' Guo formula, so that the maximum values are obtained in 
#' daytime hours.
#'
#' @param climdata a dataframe with daily wind speed data.
#' Required columns are Year, Month, Day and u2med. u2max
#' is an optional data column.
#'
#' @return dataframe with the columns Date, Year, Month, Day, DOY,
#' and h_wind (hours with wind speed equal or above 5.5 m/s).
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
#' # Estimate daily hours with wind speed above moderate speeds for the example
#' # dataset
#' Tudela_Mu2 <- moderate_wind(Tudela_DW)
#'
#' }
#' @export moderate_wind
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date make_datetime

moderate_wind <- function(climdata)
{
  if(!"u2max" %in% colnames(climdata))
  {
    cat("Warning: No maximum windspeed data provided,\nhourly values will
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
  wind_h = wind_h %>%
    group_by(Date)%>%
    summarise(h_wind= sum(u2>=5.5)) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date),DOY = yday(Date)) %>%
    select(1,3:6,2)
  return(wind_h)
}
