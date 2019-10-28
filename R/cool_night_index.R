#' Calculation of night coolness index
#'
#' This function calculates a night coolness index based in the
#' Cool Night index of Tonietto (1999) as the mean of minimum 
#' temperatures in the 30 days prior to harvest.
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param harvest a vector with expected harvest days
#' (expressed as day of the year)
#' @return data frame with the values of the indices.
#' @author Carlos Miranda
#' @references
#'
#' Tonietto J. 1999. Les macroclimats viticoles mondiaux et l'influence du
#' mésoclimat sur la typicité de la Syrah et du Muscat de Hambourg dans le
#' sud de la France: methodologie de carácterisation. Thése Doctorat. Ecole 
#' Nationale Supérieure Agronomique, Montpellier, 233pp.
#'
#' @examples
#'
#' \dontrun{
#'
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, create a vector or harvest dates and
#' #estimate the Huglin and Cool night indices on each year
#' #in the series.
#'
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, Tmax, Tmin)
#' harvest <- c(225, 250, 275)
#' coolness <- coolness_index(Weather, harvest)
#'
#'}
#' @export coolness_index
#' @import data.table tidyverse zoo lubridate

coolness_index <- function(climdata, harvest)
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
          filter(climdata_fil$DOY>(Day_h-30) & climdata_fil$DOY<=Day_h) %>%
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

