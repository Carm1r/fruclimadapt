#' Calculation of dailiy potential evapotranspiration by Penman (1948) method  
#'
#' This function calculates the potential evapotranspiration (ETref) using daily 
#' weather data and the Penman (1948) method 
#' 
#' This version of the function requires the user to supply in weather data daily
#' values for temperature (Tmax and Tmin), relative humidity (RHmax and RHmin), 
#' solar radiation (Rad in MJ m-2 day-1) and wind speed at 2m height(u2 in m s-1).
#'
#' @param climdata a dataframe with daily weather data.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin, RHmax, RHmin, Rad, u2.
#' @param lat the latitude of the site, in decimal degrees. 
#' @param elev the elevation of the site, in meters above sea level.
#' @return climdata data frame in which Date, DOY and ET columns have been added.
#' @author Carlos Miranda
#' @references
#'
#' To be included
#'
#' @examples
#'
#' \dontrun{
#'
#' elevation <- 315
#' latitude <- 42.08
#' ET_Penman <- ET_penman(Weather, latitude, elevation)
#'}
#' @export ET_penman
#' @import data.table tidyverse zoo lubridate
#'

ET_penman <- function(climdata, lat, elev){
  lat_rad <- pi*lat/180  
  P <- 101.3 * ((293-0.0065*elev)/293)^5.26
  gamma <- (0.000665)*P
  Gsc <-0.082
  sigma <-4.903*10^-9
  climdata <- climdata %>%
    mutate(Date = make_date(Year, Month, Day))
  DOY <- yday(climdata$Date)
  Tmean <- (climdata$Tmax + climdata$Tmin)/2
  delta <- 4098*(0.6108*exp(17.27*Tmean/(Tmean+237.3)))/(Tmean+237.3)^2
  es_Tmax <- 0.6108 * exp(17.27 * climdata$Tmax / (climdata$Tmax + 237.3)) 
  es_Tmin <- 0.6108 * exp(17.27 * climdata$Tmin / (climdata$Tmin + 237.3)) 
  es <- (es_Tmax + es_Tmin)/2 
  ea <- (es_Tmin * climdata$RHmax/100 + es_Tmax * climdata$RHmin/100)/2 
  d_r <- 1 + 0.033*cos(2*pi/365 * DOY) 
  delta2 <- 0.409 * sin(2*pi/365 * DOY - 1.39)
  w_s <- acos(-tan(lat_rad) * tan(delta2))  
  N <- 24/pi * w_s 
  R_a <- (1440/pi) * Gsc * d_r * (w_s * sin(lat_rad) * sin(delta2) + cos(lat_rad) * cos(delta2) * sin(w_s)) 
  R_so <- (0.75 + (2*10^-5)* elev) * R_a 
  R_ns <- (1-0.23)*climdata$Rad
  R_nl <- sigma * ((climdata$Tmax+273.2)^4 + (climdata$Tmin+273.2)^4)/2 * 
    (0.34 - 0.14 * sqrt(ea)) * (1.35 * climdata$Rad / R_so - 0.35)
  R_n <- R_ns - R_nl
  f_u <- 2.626+1.381*climdata$u2
  Ea <- f_u * (es-ea)
  ET_penman.Daily <- delta / (delta + gamma) *(R_n/2.45) + gamma / (delta + gamma) * Ea
    
  climdata <- climdata %>% mutate(DOY = DOY, ET_penman = ET_penman.Daily) %>%
    select(Date, Year, Month, Day, DOY, everything()) %>%
    arrange(Date,DOY)
  return(climdata)
}





