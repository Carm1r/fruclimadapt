#' Calculates growing degree hours (GDH) using ASYMCUR method
#'
#' The function calculates the daily heat unit accumulation (GDH)
#' from hourly temperature data, using the ASYMCUR model
#' proposed by Anderson et al (1986). The model is a refinement
#' of the linear model proposed by Anderson and Seeley (1992) defined
#' by a base, optimum and critical temperature. Heat accumulation
#' begins when temperatures are above a minimum (base temperature,
#' Tb), and growth increases with temperature up to a
#' point (optimum temperature, Topt) at which there is no longer
#' an increase. The critical temperature (Tcrit) is the temperature
#' above which growth ceases. The difference of ASYMCUR model with 
#' the linear by Anderson and Seeley (1992)is that the former uses 
#' an asymmetric curvilinear relationship to model GDH accumulation. 
#' The function allows the user to define Tb, Topt and Tcrit, and uses
#' as default the values set by Anderson et al (1986) for fruit trees:
#' Tb=4ºC, Topt=25ºC and Tcrit=36ºC. 
#'
#' @param Hourdata a dataframe of hourly temperatures. This data frame
#' must have a column for Year, Month, Day, DOY (day of year), Hour,
#' Temp (hourly temperature).
#' @param Tb the base temperatures to calculate GDH
#' @param Topt the optimal temperatures to calculate GDH
#' @param Tcrit the critical temperature
#' @return dataframe with daily data. It contains the columns Date,
#' Year, Month, Day, DOY (day of the year), and GDH
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
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
#' # Generate hourly temperatures for the example dataset
#' library(tidyverse)
#' library(lubridate)
#' Tudela_HT <- hourly_temps(Tudela_DW,42.13132)
#' # Calculate GDH using default threshold temperatures
#' GDH_default <- GDH_asymcur(Tudela_HT)
#' # Calculate GDH using as custom set temperature thresholds
#' # Tb=4.5, Topt=22 and Tcrit=32
#' GDH_custom <- GDH_asymcur(Tudela_HT, 4.5, 22, 32)
#' 
#'
#' @export GDH_asymcur
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

GDH_asymcur <- function(Hourdata,Tb = 4 ,Topt = 25 ,Tcrit = 36)
  {
  old <- options()         
  on.exit(options(old)) 	 
  options(scipen=999)
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

