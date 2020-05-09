#' Calculation of chill units from hourly temperature data (Utah model)
#'
#' The function calculates chill units using the Utah model (Richardson
#'  et al, 1974). This model is characterized by differential weighting of
#' temperature ranges, including negative weights for temperatures
#' above 15.9°C. This model recognizes that different temperatures
#' vary in effectiveness in accumulating chill as well as a negative
#' influence of high temperatures on previously accumulated chill.
#' Chill Units (Utah or Anderson model) perform better than chill hours
#' for a wider range of climates, and it could be considered as the
#' ‘reference’ method nowadays, but it is ill-suited for warm or
#' Mediterranean conditions. To date, Chill portions is the best
#' existing model for most growing regions, so chill fulfilment
#' should preferably be calculated using that method, especially
#' when transferring varieties from one region to another, or in
#' studies on climate change.
#'
#' @param climdata a dataframe with hourly temperature data. It
#' must contain the columns Year, Month, Day, DOY, Temp.
#' @param Start parameter indicating the day of the year when chill
#' accumulation is supposed to start.
#' @return dataframe with the chill accumulated for all the seasons in the
#' dataset. Seasons begin at the start date and end the day before the start
#' date of the following year.
#' It contains the columns Year, Month, Day, DOY, Chill
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Richardson EA, Seeley SD and Walker DR, 1974. A model for estimating the
#' completion of rest for Redhaven and Elberta peach trees. HortScience 9,
#' 331-332.
#'
#' @examples
#'
#' \dontrun{
#' # Generate hourly temperatures
#' Tudela_HT <- hourly_temps(Tudela_DW,42.13132)
#' # Calculate chill as chill units, starting on DOY 305
#' Chill_u <- chill_units(Tudela_HT,305)
#' }
#' @export chill_units
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

chill_units <- function(climdata, Start){

  climdata <- climdata %>% mutate(Units = ifelse(Temp<=1.4,0,
                                                 ifelse(Temp<=2.4,0.5,
                                                        ifelse(Temp<=9.1,1,
                                                               ifelse(Temp<=12.4,0.5,
                                                                      ifelse(Temp<=15.9,0,
                                                                             ifelse(Temp<=18,-0.5,-1)))))))

  setDT(climdata)[, Chill := cumsum(Units), by = rleid(DOY == Start & Hour==0)]

  climdata <- climdata %>%
    select(Year, Month, Day, DOY, Hour, Chill) %>%
    filter(Hour==23) %>%
    select(-Hour)
 }

