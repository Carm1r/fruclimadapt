#' Calculation of night coolness index
#'
#' This function calculates a night coolness index based in the
#' Cool Night index of Tonietto (1999). Instead of calculating 
#' the mean of minimum temperatures in September/March (Northern
#' or Southern hemispheres, respectively), this function allows 
#' to define the harvest date and the number of days that will be
#' analyzed (by default, 30 days), and calculates the mean of minimum
#' temperatures in the in the specified period of days before harvest.
#' The function allows testing for several harvest dates simultaneously.
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param harvest a vector with expected harvest days
#' (expressed as day of the year)
#' @param span the period (in days) before harvest that will be analyzed. By 
#' default, this parameter is set in 30 days.
#' @return dataframe with the values of the indices. It contains the
#' columns Year, Harvest, Coolness 
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Tonietto J. 1999. Les macroclimats viticoles mondiaux et l'influence du
#' mésoclimat sur la typicité de la Syrah et du Muscat de Hambourg dans le
#' sud de la France: methodologie de carácterisation. Thése Doctorat. Ecole 
#' Nationale Supérieure Agronomique, Montpellier, 233pp.
#'
#' @examples
#'
#' # Select the appropiate columns from the Tudela_DW example dataset,
#' # create a vector or harvest dates and estimate the coolness index 
#' # for the 30 days prior to harvest on each year in the dataset.
#' library(tidyverse)
#' Weather <- Tudela_DW %>%
#'    select(Year, Month, Day, Tmax, Tmin)
#' harvest <- c(225, 250, 275)
#' coolness <- coolness_index(Weather, harvest)
#'
#' @export coolness_index
#' @import magrittr dplyr 
#' @importFrom lubridate make_date yday

coolness_index <- function(climdata, harvest, span=30)
{
  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date = make_date(Year, Month, Day),
           DOY = yday(Date))
  
  seasons <- unique(climdata$Year)

  indices_cn <- c("Year","Harvest","Coolness")
  indices.df <-data.frame(matrix(ncol=3, nrow=0, byrow=FALSE))
  colnames(indices.df) <- indices_cn

  for (sea in 1:length(seasons)){
     Anno <- as.numeric(seasons[sea])
     climdata_fil <- climdata %>%
        filter(climdata$Year==Anno)
     for (nharv in 1:length(harvest)){
        Day_h <- as.numeric(harvest[nharv])
        coolnight <- climdata_fil %>%
          filter(climdata_fil$DOY>(Day_h-span) & climdata_fil$DOY<=Day_h) %>%
          summarise(Cool_n=mean(Tmin)) %>%
          select(Cool_n) %>%
          unlist(use.names=FALSE)
      
    new.row.df <- data.frame(Anno,Day_h) %>%
        cbind(coolnight)
    
    indices.df <-rbind(indices.df,new.row.df)
    }
  }
  indices.df <- indices.df %>% rename(Year=Anno)
  return(indices.df)
}

