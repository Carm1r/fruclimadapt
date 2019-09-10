#' Calculates growing degree days using linear method
#'
#' The function calculates the daily heat unit accumulation (GDD)
#' from daily temperature data with a linear method based on averaging
#' the daily maximum and minimum temperatures (Arnold, 1960). Heat accumulation
#' begins when mean temperatures are above a minimum (base temperature,
#' Tb), and growth increases with linearly with temperature up to a
#' point (optimum temperature, Topt) at which there is no longer
#' an increase. The critical temperature (Tcrit) is the maximum
#' temperature at which growth will continue. The function allows
#' the user to define Tb, Topt and Tcrit.
#'
#' @param Temp_Day a dataframe of daily temperatures. This data frame must
#' have a column for Year, Month, Day, and daily minimum ("Tmin") and
#' maximum ("Tmax") temperatures
#' @param Tb the base temperature to calculate GDD
#' @param Topt the optimal temperature to calculate GDD
#' @return data frame consisting of the columns Year, Month, Day, Tmax, Tmin,
#' Tmean and GDD.
#' @author Carlos Miranda
#' @references
#'
#' Arnold,  C.Y.  1960.  Maximum-minimum  temperatures as a basis for computing
#' heat units. Proc.Amer. Soc. Hort. Sci. 76:682–692.
#'
#' @examples
#'
#' \dontrun{
#'
#' GDH <- GDD_linear(Weather,4.5,25,36)
#'
#' }
#'
#' @export GDD_linear
#' @import data.table tidyverse zoo lubridate

GDD_linear <- function(Temp_Day,Tb,Topt)
{
  Temp_Day <- Temp_Day %>%
    mutate(Tmean=(Tmax+Tmin)/2,
           GDD = ifelse(Tmean-Tb<0 |Tmean>=Tcrit,0,
                        ifelse(Tmean<Topt,Tmean-Tb,Topt-Tb)))
return(Temp_Day)
}
