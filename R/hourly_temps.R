#' Make hourly temperature record from daily data
#'
#' This function generates hourly temperatures from daily maximum and
#' minimum values, using the method proposed by Linvill (1990), which also
#' requires sunset and sunrise calculation for each day in the series.
#' Sunset and sunrise hours for a location are estimated from the latitude
#' and the day of the year (DOY) using the equations by Spencer (1971) and
#' Almorox et al. (2005).
#'
#' @param latitude the latitude (in decimal degrees) of the site.
#' @param climdata a data frame containing the columns Year, Month, Day,
#' Tmax and Tmin. Data must not contain any gap.
#' @return a data frame containing the columns Date, Year, Month, Day, DOY,
#' Hour, Sunrise (hour of sunrise), Sunset (hour of sunset), Daylength and
#' Temp (hourly temperature).
#' @author Carlos Miranda
#' @references
#'
#' Linvill DE, 1990. Calculating chilling hours and chill units from daily
#' maximum and minimum temperature observations. HortScience 25, 14-16.
#'
#' Spencer JW, 1971. Fourier series representation of the position of the Sun.
#' Search 2, 172.
#'
#' Almorox J, Hontoria C and Benito M, 2005. Statistical validation of
#' daylength definitions for estimation of global solar radiation in Toledo,
#' Spain. Energy Conversion and Management 46, 1465-1471.
#'
#' @examples
#'
#' \dontrun{
#' THourly<-hourly_temps(50.4,weather$weather)
#' }
#' @export hourly_temps
#' @import data.table tidyverse zoo lubridate

hourly_temps <- function(climdata,latitude)
{
  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date=make_date(Year, Month, Day),DOY=yday(Date))
  Day_times<-solar_times(latitude,DOY=climdata$DOY)
  climdata <- cbind(climdata,Day_times) %>%
    mutate(Tsunset = Tmin + (Tmax-Tmin)*sin(pi*(Sunset-Sunrise)/(Daylength+4)))

  Sunset <- climdata$Sunset
  Sunrise <- climdata$Sunrise
  Tmax <- climdata$Tmax
  Tmin <-climdata$Tmin
  Tsunset <- climdata$Tsunset

  prev_Sunset <- climdata$Sunset
  next_Sunrise <- climdata$Sunrise
  prev_Tmax <- climdata$Tmax
  next_Tmin <- climdata$Tmin
  prev_Tmin <- climdata$Tmin
  prev_Tsunset <- climdata$Tsunset

  for (r in 2:nrow(climdata)){
    prev_Sunset[r] <- Sunset[r-1]
    prev_Tmax[r] <- Tmax[r-1]
    prev_Tmin[r] <- Tmin[r-1]
    prev_Tsunset[r] <- Tsunset[r-1]
  }
  for (r in 1:(nrow(climdata)-1)){
    next_Sunrise[r] <- Sunrise[r+1]
    next_Tmin[r] <- Tmin[r+1]
  }
  climdata <- climdata %>% mutate(prev_Sunset=prev_Sunset,
                                  next_Sunrise=next_Sunrise,
                                  prev_Tmax=prev_Tmax,
                                  prev_Tmin=prev_Tmin,
                                  next_Tmin=next_Tmin,
                                  prev_Tsunset=prev_Tsunset)

  minday = as_datetime(climdata$Date[1])
  maxday = as_datetime(climdata$Date[nrow(climdata)])

  dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
    rename(Datetime=1) %>%
    mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
           Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))

  dates_h = merge(dates, climdata, by="Date", all = TRUE) %>%
    select(Date,Hour,Tmax,Tmin,Sunrise,Sunset,Daylength,prev_Sunset,
           next_Sunrise,prev_Tmax,prev_Tmin,next_Tmin,prev_Tsunset,Tsunset) %>%
    mutate(Year=year(Date),
           Month=month(Date),
           Day=day(Date),
           DOY=yday(Date)
           )
 dates_h <- dates_h %>% mutate(
   Tpredawn = prev_Tsunset - ((prev_Tsunset-Tmin)/
                              log(24-Daylength))*
                              log(Hour + 24-round(prev_Sunset)+1),
   Tdayhours = Tmin + (Tmax- Tmin)*
     sin(pi*(Hour-Sunrise)/(Daylength+4)),
   Tpostdusk = ifelse(Hour>=round(Sunset),Tsunset-((Tsunset-next_Tmin)/
                          log(24-(Daylength)+1))*
                          log(Hour-round(Sunset)+1)
                      ,0),
   Temp = ifelse(Hour<round(Sunrise),Tpredawn,
                 ifelse(Hour==round(Sunrise),Tmin,
                        ifelse(Hour<=Sunset,Tdayhours,
                               Tpostdusk)))
   )
 dates_h <- dates_h %>% select(Date, Year, Month, Day, DOY, Hour, Sunrise, Sunset, Daylength, Temp)
return(dates_h)
 }

