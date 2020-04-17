#' Calculates growing degree hours using a linear method
#'
#' The function calculates the daily heat unit accumulation (GDH)
#' from hourly temperature data, using the linear model
#' proposed by Anderson and Seeley (1992). The model is defined
#' by a base, optimum and critical temperature. Heat accumulation
#' begins when temperatures are above a minimum (base temperature,
#' Tb), and growth increases linearly with temperature up to a
#' point (optimum temperature, Topt) at which there is no longer
#' an increase. The critical temperature (Tcrit) is the maximum
#' temperature at which growth will continue. The function allows
#' the user to define Tb, Topt and Tcrit.
#'
#' @param Hourdata a dataframe of hourly temperatures. This data frame
#' must have a column for Year, Month, Day, DOY (day of year),Hour, and
#' Temp (hourly temperature).
#' @param Tb the base temperatures to calculate GDH
#' @param Topt the optimal temperatures to calculate GDH
#' @param Tcrit the critical temperature
#' @return data frame with daily data. It contains the columns Date,
#' Year, Month, Day, DOY (day of the year), and GDH
#' @author Carlos Miranda
#' @references
#'
#' Anderson JL and Seeley SD, 1992. Modelling strategy in pomology:
#' Development of the Utah models. Acta Horticulturae 313, 297-306.
#'
#' @examples
#'
#' \dontrun{
#'
#' GDH <- GDH_linear(Weather,4.5,25,36)
#'
#' }
#' @export GDH_linear
#' @import data.table tidyverse zoo lubridate::make_date()

GDH_linear <- function(Hourdata,Tb,Topt,Tcrit)
{
  Hourdata <- Hourdata %>%
    mutate(Date = make_date(Year,Month,Day),
           GDH = ifelse(Temp-Tb<0 |Temp>=Tcrit,0,
                        ifelse(Temp<Topt,Temp-Tb,Topt)))
  GDH_linear <- Hourdata %>%
    group_by(Date) %>%
    summarise_all(sum)
  GDH_linear <- GDH_linear %>% select(Date,GDH) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date), DOY=yday(Date)) %>%
    select(Date, Year, Month, Day, DOY, everything()) %>%
    arrange(Date,DOY)
 return(GDH_linear)
}
