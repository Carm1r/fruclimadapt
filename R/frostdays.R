#' Calculates the number of days with minimum temperature below critical values
#'
#' The function evaluates the number of early and spring frosts occurring
#' during a season. It compares minimum daily temperature (Tmin) with the critical
#' temperature (Tcrit). Daily critical temperatures are linearly
#' interpolated from a user-provided dataframe with critical temperatures for
#' key phenological stages and the day of occurrence of the stages.
#' The function checks each day between the first date suif Tmin is below Tcrit
#' and, if so, that day is considered as a frost day (Fday). The last day evaluated
#' is DOY 181.
#'
#' @param mintemps a dataframe with minimum daily temperatures. Must contain
#' the columns julian day of year (DOY )and the minimum daily temperature (Tmin).
#'
#' @param fendates a dataframe with the day of occurrence of the phenological
#' stages. Must contain julian day of occurrence of the phenological
#' stage (DOY) and the critical frost temperature for the stage (Tcrit)
#' @return a value with the number of days in which Tmin is equal or below Tcrit.
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
#' Frostdays_2018 <-frostdays(Tmins_2018,Tcrit)
#' print(Frostdays_1997)
#' }
#' @export frostdays
#' @import data.table tidyverse zoo lubridate

frostdays <- function(mintemps, fendates){
  mintemps <- mintemps %>% filter(mintemps$DOY<=181)
  Datescrit <- rep(NA,181)
  len = length(fendates$DOY)
  for (i in 1:len){
    Datescrit[fendates$DOY[i]]=fendates$Tcrit[i]
  }
  #fill in the series of critical data and include in the dataset
  Tcritant <- rep(fendates$Tcrit[1], fendates$DOY[1]-1)
  Tcritpost <- rep(fendates$Tcrit[len], 181-fendates$DOY[len])
  Tcrit_gap <- zoo(Datescrit)
  Tcrit_fgap <- na.approx(Tcrit_gap)
  Tcritcent <- coredata(Tcrit_fgap)
  mintemps$Tcrit <- c(Tcritant,Tcritcent,Tcritpost)
  #check if the min temp is below the critical and increase the counter
  Fday=0
  for (i in 1:length(mintemps$DOY)){
    if (mintemps$Tmin[i]<=mintemps$Tcrit[i]){
      Fday=Fday+1
    }
  }
return(Fday)
}

