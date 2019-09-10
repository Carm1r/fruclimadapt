#' Estimation of the sunrise and sunset hour
#'
#' This function estimates the sunrise and sunset hour
#' for a location, characterized by latitude, and the
#' day of the year (DOY). The function uses the equations by
#' Spencer (1971) and Almorox et al. (2005).
#'
#' @param latitude numeric value specifying the geographic latitude
#' (in decimal degrees) of the location of interest
#' @param DOY numeric (usually integer) value or vector specifying the
#' day of the year for which calculations should be done.
#' @return list with Sunrise, Sunset and Daylength.
#' @author Carlos Miranda
#' @references
#' Spencer JW, 1971. Fourier series representation of the position of the Sun.
#' Search 2(5), 172.
#'
#' Almorox J, Hontoria C and Benito M, 2005. Statistical validation of
#' daylength definitions for estimation of global solar radiation in Toledo,
#' Spain. Energy Conversion and Management 46(9-10), 1465-1471)
#'
#' @examples
#'
#' \dontrun{
#'
#' #create a vector with 365 days in sequence and calculate sunrise and
#' #sunset hours for that year in a site placed a 45.5?? N
#'
#' Days <- seq(1:365)
#' Sunrise_Sunset <- solar_times(41.5,Days)
#'
#' }
#' @export solar_times
#' @import data.table tidyverse zoo lubridate

solar_times <- function(latitude,DOY){
  Gamma<-2*pi/365*((DOY)-1)
  Delta<-180/pi*(0.006918-0.399912*cos(Gamma)+0.070257*sin(Gamma)-0.006758*cos(2*(Gamma))+
                   0.000907*sin(Gamma)-0.002697*cos(3*(Gamma))+0.00148*sin(3*(Gamma)))
  CosWo<-(sin(-0.8333/360*2*pi)-sin(latitude/360*2*pi)*
            sin(Delta/360*2*pi))/(cos(latitude/360*2*pi)*cos(Delta/360*2*pi))

  c_poles<-which(CosWo>=-1&CosWo<=1)

  Sunrise<-rep(-99,length(CosWo))
  Sunrise[c_poles]<-12-acos(CosWo[c_poles])/(15/360*2*pi)

  Sunset<-rep(-99,length(CosWo))
  Sunset[c_poles]<-12+acos(CosWo[c_poles])/(15/360*2*pi)

  Daylength<-Sunset-Sunrise
  Daylength[which(CosWo>1)]<-0
  Daylength[which(CosWo<(-1))]<-24

  Sunrise[which(Daylength==24)]<-99
  Sunset[which(Daylength==24)]<-99

  return(list(Sunrise=Sunrise,Sunset=Sunset,Daylength=Daylength))
  }


