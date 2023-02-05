#' Calculates growing degree hours (GDH) using a linear method
#'
#' The function calculates the daily heat unit accumulation (GDH)
#' from hourly temperature data, using a standard linear model or the
#' linear model proposed by Anderson and Seeley (1992). The standard
#' model is defined by a base temperature, and the Anderson and Seeley 
#' (1992) includes also optimum and critical temperatures. In both
#' variants, heat accumulation begins when temperatures are above a 
#' minimum (base temperature, Tb), and growth increases linearly with 
#' temperature. In the Anderson and Seeley (1992) variant, growth no
#' longer increases once the optimum temperature (Topt) is reached, 
#' meaning that GDH above it are constant. The critical temperature 
#' (Tcrit) is the temperature above which growth ceases (i.e. GDH=0). 
#' The function allows the user to define Tb, Topt and Tcrit, and uses
#' as default the values set by Anderson et al (1986) for fruit trees: 
#' Tb=4ºC, Topt=25ºC and Tcrit=36ºC. In the standard linear model with
#' upper thresholds, use Topt = 999 and Tcrit = 999. 
#'
#' @param Hourdata a dataframe of hourly temperatures. This data frame
#' must have a column for Year, Month, Day, DOY (day of year),Hour, and
#' Temp (hourly temperature).
#' @param Tb the base temperatures to calculate GDH
#' @param Topt an optional optimal temperatures to calculate GDH
#' @param Tcrit an optional critical temperature
#' @return dataframe with daily data. It contains the columns Date,
#' Year, Month, Day, DOY (day of the year), and GDH
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Anderson JL and Seeley SD, 1992. Modelling strategy in pomology:
#' Development of the Utah models. Acta Horticulturae 313, 297-306.
#'
#' @examples
#'
#' # Generate hourly temperatures for the example dataset
#' library(magrittr)
#' library(dplyr)
#' library(lubridate)
#' Weather <- Tudela_DW %>%
#'    filter (Tudela_DW$Year==2003)
#' Tudela_HT <- hourly_temps(Weather,42.13132)
#' # Calculate GDH using default threshold temperatures
#' GDH_default <- GDH_linear(Tudela_HT)
#' # Calculate GDH using an optimal temperature threshold with 
#' # no critical threshold
#' GDH_custom <- GDH_linear(Tudela_HT, 4.5, 22, 999)
#' 
#' @export GDH_linear
#' @import magrittr dplyr 
#' @importFrom lubridate make_date year month day yday

GDH_linear <- function(Hourdata,Tb=4,Topt=25,Tcrit=36)
{
  Hourdata <- Hourdata %>%
    mutate(Date = make_date(Year,Month,Day),
           GDH = ifelse(Temp<=Tb |Temp>=Tcrit,0,
                        ifelse(Temp<Topt,Temp-Tb,Topt-Tb)))
  GDH_linear <- Hourdata %>%
    group_by(Date) %>%
    summarise_all(sum)
  GDH_linear <- GDH_linear %>% select(Date,GDH) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date), DOY=yday(Date)) %>%
    select(Date, Year, Month, Day, DOY, everything()) %>%
    arrange(Date,DOY)
 return(GDH_linear)
}
