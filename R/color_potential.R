#' Evaluation of weather conditions for anthocyanin formation
#' in apple skin
#'
#' This function estimates the number of days that can be 
#' considered as highly favorable or unfavorable for anthocyanin 
#' accumulation in the skin of red apple cultivars during the 
#' month (30 days) before harvest. A highly favorable day (Cool
#' day) is considered when the daily maximum temperature is below
#' 26ºC, a highly unfavorable day (Hot day) when the minimum 
#' temperature is above 20ºC (Lin-Wang et al, 2011). The average
#' of maximum and minimum temperatures during the same period is 
#' also provided. The function allows testing for several harvest 
#' dates.
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param harvest a vector with expected harvest days
#' (expressed as day of the year)
#' @return dataframe with the number of highly favorable (Cool_d) 
#' and unfavorable (Hot_d) days for apple red color, as well as the 
#' average of the maximum (Tmax_avg) and minimum (Tmin_avg) 
#' temperatures for each year (Year) in the series during the 
#' 30 days previous to each harvest date (Day_h) provided.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Lin-Wang K, Micheletti D et al, 2011. High temperature reduces apple fruit
#' colour via modulation of the anthocyanin regulatory complex. Plant, Cell
#' and Environment 34, 1176-1190.
#'
#' @examples
#'
#' # Select the appropiate columns from Tudela_DW example dataset, create
#' # a vector or harvest dates and estimate the number favorable and 
#' # unfavorable days on each year in the dataset.
#' library(tidyverse)
#' Weather <- Tudela_DW %>%
#'    select(Year, Month, Day, Tmax, Tmin)
#' harvest <- c(225, 250, 275)
#' Color_assess <- color_potential(Weather, harvest)
#'
#' @export color_potential
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

color_potential <- function(climdata, harvest)
{
  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date = make_date(Year, Month, Day),
           DOY = yday(Date),
           Cool=if_else(Tmax<=26,1,0),
           Hot=ifelse(Tmin>=20,1,0))
  seasons <- unique(climdata$Year)

  colorpot_cn <- c("Year","Harvest_d","Cool_d","Hot_d","Tmax_avg","Tmin_avg")
  colorpot.df <-data.frame(matrix(ncol=6, nrow=0, byrow=FALSE))
  colnames(colorpot.df) <- colorpot_cn

  for (sea in 1:length(seasons)){
    Anno <- as.numeric(seasons[sea])
    climdata_fil <- climdata %>%
      filter(climdata$Year==Anno)
    for (nharv in 1:length(harvest)){
      Day_h <- as.numeric(harvest[nharv])
      evacol_fil <- climdata_fil %>%
        filter(climdata_fil$DOY>(Day_h-30) & climdata_fil$DOY<=Day_h) %>%
        summarise(Cool_d=sum(Cool),Hot_d=sum(Hot),Tmax_avg=mean(Tmax),Tmin_avg=mean(Tmin)) %>%
        select(Cool_d,Hot_d,Tmax_avg,Tmin_avg)
      new.row.df <- data.frame(Anno,Day_h) %>%
        cbind(evacol_fil)
      colorpot.df <-rbind(colorpot.df,new.row.df)
    }
  }
  colorpot.df <- colorpot.df %>% rename(Year=Anno)
  return(colorpot.df)
}

