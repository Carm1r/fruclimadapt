#' Calculates growing degree days (GDD) using a linear method
#'
#' The function calculates the daily heat unit accumulation (GDD)
#' from daily temperature data with a linear method based on averaging
#' the daily maximum and minimum temperatures (Arnold, 1960). GDD are
#' calculated by subtracting the plant's lower base temperature (Tb) 
#' from the average daily air temperature. The user can define an upper 
#' temperature threshold (Tu) so that all temperatures above Tu will 
#' have equal value in GDD summation.
#' 
#'
#' @param Temp_Day a dataframe of daily temperatures. This data frame must
#' have a column for Year, Month, Day, and daily minimum (Tmin) and
#' maximum (Tmax) temperatures.
#' @param Tb the base temperature required to calculate GDD.
#' @param Tu an optional upper temperature threshold.
#' @return dataframe consisting of the columns Year, Month, Day, Tmax, Tmin,
#' Tmean and GDD.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Arnold,  C.Y.  1960.  Maximum-minimum temperatures as a basis for computing
#' heat units. Proc.Amer. Soc. Hort. Sci. 76:682–692.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Calculate GDD in the example dataset using 4.5ºC as base temperature and no 
#' # upper threshold.
#' GDH <- GDD_linear(Tudela_DW,4.5)
#' 
#' # Calculate GDD in the example dataset using 4.5ºC as base temperature and an 
#' # upper threshold at 25ºC.
#' GDH <- GDD_linear(Tudela_DW,4.5,25)
#' }
#'
#' @export GDD_linear
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

GDD_linear <- function(Temp_Day,Tb,Tu=999)
{
  Temp_Day <- Temp_Day %>%
    mutate(Tmean=ifelse(Tmax>Tu,(Tu+Tmin)/2,(Tmax+Tmin)/2),
           GDD = ifelse(Tmean-Tb<0,0,
                        ifelse(Tmean<Tu,Tmean-Tb,Tu-Tb))) %>%
    select(Date, Year, Month, Day, DOY, GDD)
return(Temp_Day)
}
