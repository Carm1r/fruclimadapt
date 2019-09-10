#' Calculates growing degree hours using ASYMCUR method
#'
#' The function calculates the daily heat unit accumulation (GDH)
#' from hourly temperature data, using the ASYMCUR model
#' proposed by Anderson et al (1986). The model is a refinement
#' of the linear model proposed by Anderson and Seeley (1992) defined
#' by a base, optimum and critical temperature. Heat accumulation
#' begins when temperatures are above a minimum (base temperature,
#' Tb), and growth increases with temperature up to a
#' point (optimum temperature, Topt) at which there is no longer
#' an increase. The critical temperature (Tcrit) is the maximum
#' temperature at which growth will continue. The difference of
#' ASYMCUR model with the linear is that the former uses an
#' assymmetric curvilinear relationship to model GDH accumulation.
#' The function allows the user to define Tb, Topt and Tcrit.
#'
#' @param Hourdata a dataframe of hourly temperatures. This data frame
#' must have a column for Year, Month, Day, DOY (day of year),Hour,
#' Temp (hourly temperature) and Chill (Chill accumulated).
#' @param Tb the base temperatures to calculate GDH
#' @param Topt the optimal temperatures to calculate GDH
#' @param Tcrit the critical temperature
#' @return data frame with daily data. It contains the columns Date,
#' Year, Month, Day, DOY (day of the year), Chill, and GDH
#' @author Carlos Miranda
#' @references
#'
#' Anderson JL, Richardson EA and Kesner CD, 1986. Validation of chill
#' unit and flower bud phenology models for 'Montmorency' sour cherry.
#' Acta Horticulturae 184, 71-75.
#' Anderson JL and Seeley SD, 1992. Modelling strategy in pomology:
#' Development of the Utah models. Acta Horticulturae 313, 297-306.
#'
#' @examples
#'
#' \dontrun{
#'
#' GDH <- GDH_asymcur(Weather,4.5,25,36)
#'
#' }
#'
#' @export GDH_asymcur
#' @import data.table tidyverse zoo lubridate

GDH_asymcur <- function(Hourdata,Tb,Topt,Tcrit)
  {
  Hourdata <- Hourdata %>%
    mutate(Date = make_date(Year,Month,Day),
      GDH = ifelse(Temp-Tb<0,0,
                        ifelse(Temp<Topt,((Topt-Tb)/2 * (1+cos(pi+pi*(Temp-Tb)/(Topt-Tb)))),
                               ifelse(Temp<=Tcrit,
                                      ((Topt-Tb) * (1+cos(pi/2+pi/2*(Temp-Topt)/(Tcrit-Topt)))), 0))))
  GDH_asym <- Hourdata %>%
    group_by(Date) %>%
    summarise_all(sum)
  GDH_asym <- GDH_asym %>% select(Date,GDH) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date),DOY=yday(Date)) %>%
    select(Date, Year, Month, Day, DOY, everything()) %>%
    arrange(Date,DOY)
  return(GDH_asym)
}

