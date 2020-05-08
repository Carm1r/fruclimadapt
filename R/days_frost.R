#' Estimates the occurence of spring frosts 
#'
#' (This is an internal function for spring_frost) not intended
#' to be stand alone used 
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
#' stage (DOY) and the critical frost temperature for 10% (LT_10) and 90%
#' (LT_90) for each stage.
#' @param lastday the last day (day of the year) to evaluate. By default, 
#' lastday = 181 (June 30th).
#' @return a dataframe with the columns
#' number of days in which Tmin is equal or below Tcrit.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @keywords internal
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
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

days_frost <- function(mintemps, fendates, lastday = 181){
  mintemps <- mintemps %>% filter(mintemps$DOY<=lastday)
  Datescrit_10 <- rep(NA,lastday)
  Datescrit_90 <- rep(NA,lastday)
  len = length(fendates$DOY)
  for (i in 1:len){
    Datescrit_10[fendates$DOY[i]]=fendates$LT_10[i]
    Datescrit_90[fendates$DOY[i]]=fendates$LT_90[i]
    }
  #fill in the series of critical data and include in the dataset
  Tcritant_LT10 <- rep(fendates$LT_10[1], fendates$DOY[1]-1)
  Tcritant_LT90 <- rep(fendates$LT_90[1], fendates$DOY[1]-1)
  Tcritpost_LT10 <- rep(fendates$LT_10[len], lastday-fendates$DOY[len])
  Tcritpost_LT90 <- rep(fendates$LT_90[len], lastday-fendates$DOY[len])
  Tcrit_gap_10 <- zoo(Datescrit_10)
  Tcrit_gap_90 <- zoo(Datescrit_90)
  Tcrit_fgap_10 <- na.approx(Tcrit_gap_10)
  Tcrit_fgap_90 <- na.approx(Tcrit_gap_90)
  Tcritcent_10 <- coredata(Tcrit_fgap_10)
  Tcritcent_90 <- coredata(Tcrit_fgap_90)
  mintemps$LT_10 <- c(Tcritant_LT10,Tcritcent_10,Tcritpost_LT10)
  mintemps$LT_90 <- c(Tcritant_LT90,Tcritcent_90,Tcritpost_LT90)
  mintemps <- mintemps %>% mutate(LT_0 = (LT_10-LT_90)/8+LT_10,
                                  LT_100 = LT_0-(LT_10-LT_90)*10/8,
                                  Dam = ifelse((LT_0-Tmin)/(LT_0-LT_100)<0,0,
                                               ifelse((LT_0-Tmin)/(LT_0-LT_100)>1,1,
                                               (LT_0-Tmin)/(LT_0-LT_100)))                                  )

return(mintemps)
}

