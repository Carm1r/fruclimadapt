#' Calculation of chill hours from hourly temperature data (Weinberger model)
#'
#' The function calculates chill hours using the Weinberger (1950),
#' or 0-7.2ºC method. Sums chill hours over winter, with one chill
#' hour accumulated for hourly temperatures between 0 and 7.2°C.
#' This is a classic method but highly inefficient, particularly
#' for warm regions and in climate change scenarios, as it disregards
#' temperature ranges that are now known to contribute to the fulfilment
#' of chilling requirements. For that reason, its use is not recommended,
#' it is offered only for educational purposes (i.e. comparison of model
#' performance) and compatibility with older bibliography.
#'
#' @param climdata a dataframe with hourly temperature data. It
#' must contain the columns Year, Month, Day, DOY, Hour, Temp.
#' @param Start parameter indicating the day of the year when chill
#' accumulation is supposed to start.
#' @return dataframe with the chill accumulated for all the seasons in the
#' dataset. Seasons begin at the start date and end the day before the start
#' date of the following year. It contains the columns Year, Month, Day, 
#' DOY and Chill.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Weinberger JH, 1950. Chilling requirements of peach varieties. Proc Am Soc
#' Hortic Sci 56, 122-128.
#'
#' @examples
#'
#' # Generate hourly temperatures
#' Tudela_HT <- hourly_temps(Tudela_DW,42.13132)
#' # Calculate chill as chill hours, starting on DOY 305
#' Chill_h <- chill_hours(Tudela_HT,305)
#' 
#' @export chill_hours
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date
#'
chill_hours <- function(climdata, Start){

  climdata <- climdata %>% mutate(HourC = ifelse(Temp<=0 | Temp >7.2,0,1))

  setDT(climdata)[, Chill := cumsum(HourC), by = rleid(DOY == Start & Hour==0)]

  climdata <- climdata %>%
    select(Year, Month, Day, DOY, Hour, Chill) %>%
    filter(Hour==23) %>%
    select(-Hour)
 }

