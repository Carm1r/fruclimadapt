#' Calculation of dailiy reference evapotranspiration by Penman-Monteith method  
#'
#' This function calculates the reference evapotranspiration (ETref) for short
#' (ETos) and tall (ETrs) canopies using daily weather data. The method is based
#' on the FAO56 guidelines (Allen et al, 1998) and on the standardized Penman Monteith 
#' equation from the Environmental Water Resources Institute of the American 
#' Society of Civil Engineers (Allen et al, 2005).  
#' 
#' This version of the function requires the user to supply in weather data daily
#' values for temperature (Tmax and Tmin), relative humidity (RHmax and RHmin), 
#' solar radiation (Rad in MJ m-2 day-1) and wind speed at 2m height(u2 in m s-1).
#'
#' @param climdata a dataframe with daily weather data.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin, RHmax, RHmin, Rad, u2.
#' @param lat the latitude of the site, in decimal degrees. 
#' @param elev the elevation of the site, in meters above sea level.
#' @return data frame with Year, Month, Day, DOY, ETos and ETrs values.
#' @author Carlos Miranda
#' @references
#'
#' Allen RG, Pereira LS, Raes D, Smith M. 1998. Crop evapotranspiration. Guidelines
#' for computing crop water requirements. FAO Irrigation and drainage paper 56. Food 
#' and Agriculture Organization of the United Nations
#' 
#' Allen RG, Walter IA, Elliott RL, Howell TA, Itenfisu D, Jensen ME, Snyder RL 2005. 
#' The ASCE standardized reference evapotranspiration equation. Reston, VA:American 
#' Society of Civil Engineers. 59 p.
#'
#' @examples
#'
#' \dontrun{
#'
#' elevation <- 315
#' latitude <- 42.08
#' ET_PM <- ET_penman_monteith(Weather, latitude, elevation)
#'}
#' @export ET_penman_monteith
#' @import data.table tidyverse zoo lubridate
#'

ET_penman_monteith <- function(climdata, lat, elev){
  lat_rad <- pi*lat/180  
  P <- 101.3 * ((293-0.0065*elev)/293)^5.26
  gamma <- (0.000665)*P
  Gsc <-0.082
  sigma <-4.903*10^-9
  G <- 0
  climdata <- climdata %>% mutate(Date = make_date(Year, Month, Day))
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
  R_a <- (1440/pi) * Gsc * d_r * 
    (w_s * sin(lat_rad) * sin(delta2) + cos(lat_rad) * cos(delta2) * sin(w_s)) 
  R_so <- (0.75 + (2*10^-5)* elev) * R_a 
  R_ns <- (1-0.23)*climdata$Rad
  R_nl <- sigma * ((climdata$Tmax+273.2)^4 + (climdata$Tmin+273.2)^4)/2 * 
    (0.34 - 0.14 * sqrt(ea)) * (1.35 * climdata$Rad / R_so - 0.35)
  R_n <- R_ns - R_nl
  ET_os.Daily <- (0.408 * delta * (R_n - G) + gamma * 900 * climdata$u2 * (es - ea)/(Tmean + 273)) / 
      (delta + gamma * (1 + 0.34*climdata$u2)) 
  ET_rs.Daily <- (0.408 * delta * (R_n - G) + gamma * 1600 * climdata$u2 * (es - ea)/(Tmean + 273)) / 
      (delta + gamma * (1 + 0.38*climdata$u2))
  
  climdata <- climdata %>% mutate(DOY = DOY, ET_os = ET_os.Daily, ET_rs = ET_rs.Daily) %>%
    select(Date, Year, Month, Day, DOY, everything()) %>%
    arrange(Date,DOY)
  return(climdata)
}





