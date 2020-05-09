#' Calculation of daily reference evapotranspiration by Penman-Monteith method  
#'
#' This function calculates the reference evapotranspiration (ETref) for short
#' (ETos) and tall (ETrs) canopies using daily weather data. The method is based
#' on the FAO56 guidelines (Allen et al, 1998) and on the standardized Penman Monteith 
#' equation from the Environmental Water Resources Institute of the American 
#' Society of Civil Engineers (Allen et al, 2005).  
#' 
#' Minimum data requirements to calculate ET are daily temperatures (maximum 
#' and minimum temperatures, Tmax and Tmin), whereas relative humidity (RHmax and 
#' RHmin), solar radiation (Rad, MJ m-2 day-1) and mean wind speed at 2m height
#' (u2med,m s-1) are optional. If missing, the function integrates FAO56 estimations 
#' for solar radiation and vapor pressure (air humidity) from daily 
#' temperatures. If there is no information available on wind speed, the function 
#' assumes a constant value of 2 m s-1.  
#'
#' @param climdata a dataframe with daily weather data.
#'  Required columns are Year, Month, Day, Tmax and Tmin. Optional columns are
#'  RHmax, RHmin, Rad and u2med.
#' @param lat the latitude of the site, in decimal degrees. 
#' @param elev the elevation of the site, in meters above sea level.
#' @return dataframe with Year, Month, Day, DOY, ETos and ETrs values.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
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
#' #Calculate ET by Penman-Monteith method in the Tudela_DW example dataset
#' elevation <- 314
#' latitude <- 42.13132
#' ET_PM <- ET_penman_monteith(Tudela_DW, latitude, elevation)
#'}
#' @export ET_penman_monteith
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date
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
  if(!"RHmax" %in% colnames(climdata)|!"RHmin" %in% colnames(climdata))
  {
    cat("Warning: No relative humidity data provided,\nreal vapor pressure (ea) will be estimated using Tmin\n");
    ea <- 0.611 * exp(17.27*climdata$Tmin/(climdata$Tmin+237.3))
  }else{
    ea <- (es_Tmin * climdata$RHmax/100 + es_Tmax * climdata$RHmin/100)/2 
      }
  d_r <- 1 + 0.033*cos(2*pi/365 * DOY) 
  delta2 <- 0.409 * sin(2*pi/365 * DOY - 1.39)
  w_s <- acos(-tan(lat_rad) * tan(delta2))  
  N <- 24/pi * w_s 
  R_a <- (1440/pi) * Gsc * d_r * 
    (w_s * sin(lat_rad) * sin(delta2) + cos(lat_rad) * cos(delta2) * sin(w_s)) 
  R_so <- (0.75 + (2*10^-5)* elev) * R_a 
  if(!"Rad" %in% colnames(climdata))
  {
    cat("Warning: No radiation data provided,\nsolar radiation (Rs) will be derived from daily thermal difference\n");
    R_s <- 0.16*sqrt(climdata$Tmax-climdata$Tmin)*R_a
    R_ns <- (1-0.23)*R_s
    R_nl <- sigma * ((climdata$Tmax+273.2)^4 + (climdata$Tmin+273.2)^4)/2 * 
      (0.34 - 0.14 * sqrt(ea)) * (1.35 * R_s / R_so - 0.35)
  }else{
    R_ns <- (1-0.23)*climdata$Rad
    R_nl <- sigma * ((climdata$Tmax+273.2)^4 + (climdata$Tmin+273.2)^4)/2 * 
      (0.34 - 0.14 * sqrt(ea)) * (1.35 * climdata$Rad / R_so - 0.35)
  }

  R_n <- R_ns - R_nl
  if(!"u2med" %in% colnames(climdata))
  {
    cat("Warning: No windspeed data provided,\na constant windspeed of 2 ms-1 will be asumed\n");
    climdata <- climdata %>% mutate(u2med=2)
  }
  ET_os.Daily <- (0.408 * delta * (R_n - G) + gamma * 900 * climdata$u2med * (es - ea)/(Tmean + 273)) / 
      (delta + gamma * (1 + 0.34*climdata$u2med)) 
  ET_rs.Daily <- (0.408 * delta * (R_n - G) + gamma * 1600 * climdata$u2med * (es - ea)/(Tmean + 273)) / 
      (delta + gamma * (1 + 0.38*climdata$u2med))
  climdata <- climdata %>% 
    mutate(DOY = DOY, ET_os = ET_os.Daily, ET_rs = ET_rs.Daily) %>%
    select(Date, Year, Month, Day, DOY, everything()) %>%
    arrange(Date,DOY)
  return(climdata)
}






