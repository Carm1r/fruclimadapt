#' Estimates the occurence of spring frosts
#'
#' The function evaluates the early and spring frosts occurring
#' during a season. It compares minimum daily temperature (Tmin) with the critical
#' temperature (Tcrit). Daily critical temperatures are linearly
#' interpolated from a user-provided dataframe with critical temperatures for
#' key phenological stages and the day of occurrence of the stages.
#' The function checks each day between the first date if Tmin is below Tcrit
#' and, if so, that day is considered as a frost day (Fday). The last day evaluated
#' is DOY 181.
#'
#' @param mintemps a dataframe with minimum daily temperatures. Must contain
#' the columns julian day of year (DOY )and the minimum daily temperature (Tmin).
#'
#' @param fendates a dataframe with the day of occurrence of the phenological
#' stages. Must contain julian day of occurrence of the phenological
#' stage (DOY) and the critical frost temperature for the stage (Tcrit)
#' @param lastday the last day (day of the year) to evaluate. By default, 
#' lastday = 181 (June 3oth).
#' @return a dataframe with the columns
#' number of days in which Tmin is equal or below Tcrit.
#' @author Carlos Miranda
#' @keywords frost prediction
#' @examples
#'
#' \dontrun{
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, include date and DOY information and estimate
#' #the #number of frost days for the year 2018
#'
#' Tmins_2018 <- Tempdata %>%
#'    select(Year, Month, Day, Tmin) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date)) %>%
#'    filter(Year==2018)
#' Frostdays_2018 <-days_frost(Tmins_2018,Tcrit)
#' }
#' @export days_frost
#' @import data.table tidyverse zoo lubridate

days_frost <- function(mintemps, fendates, lastday = 181){
  mintemps <- mintemps %>% filter(mintemps$DOY<=lastday)
  Datescrit <- rep(NA,lastday)
  len = length(fendates$DOY)
  for (i in 1:len){
    Datescrit[fendates$DOY[i]]=fendates$Tcrit[i]
  }
  #fill in the series of critical data and include in the dataset
  Tcritant <- rep(fendates$Tcrit[1], fendates$DOY[1]-1)
  Tcritpost <- rep(fendates$Tcrit[len], lastday-fendates$DOY[len])
  Tcrit_gap <- zoo(Datescrit)
  Tcrit_fgap <- na.approx(Tcrit_gap)
  Tcritcent <- coredata(Tcrit_fgap)
  mintemps$Tcrit <- c(Tcritant,Tcritcent,Tcritpost)
  #check if the min temp is below the critical and increase the counter
  mintemps <- mintemps %>% mutate(Day_frost=ifelse(Tmin<=Tcrit,1,0))

return(mintemps)
}

