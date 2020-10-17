#' Calculation of the diurnal temperature range (DTR)
#'
#' This function calculates the mean diurnal temperature range (DTR) for
#' a custom period. Mean DTR is obtained by subtracting the daily minimum
#' temperature (Tmin) from daily maximum temperature (Tmax) and then
#' averaged for the period defined by the user, provided as the initial
#' (init) and end (end) date expressed as days of the year. The function
#' requires the initial and end dates to be in the same year.
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param init_d the initial date (as day of the year) of the evaluation period
#' @param end_d the end date (as day of the year) of the evaluation period
#' @return dataframe with the value of DTR for each year in the series. 
#' It contains the columns Year, First_d, Last_d, DTR 
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @examples
#'
#' # Select the appropiate columns from the Tudela_DW example dataset,
#' # and estimate the mean DTR for July on each year in the dataset.
#' library(tidyverse)
#' Weather <- Tudela_DW %>%
#'    select(Year, Month, Day, Tmax, Tmin)
#' DTR_July <- DTR(Weather, 182, 212)
#'
#' @export DTR
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

DTR <- function(climdata, init_d, end_d)
{
  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date = make_date(Year, Month, Day),
           DOY = yday(Date), DTR_d=Tmax-Tmin)
  
  seasons <- unique(climdata$Year)
  
  indices_cn <- c("Year","First_d","Last_d","DTR")
  indices.df <-data.frame(matrix(ncol=4, nrow=0, byrow=FALSE))
  colnames(indices.df) <- indices_cn
  
  for (sea in 1:length(seasons)){
    Anno <- as.numeric(seasons[sea])
    climdata_fil <- climdata %>%
      filter(climdata$Year==Anno)
    
      DTR <- climdata_fil %>%
        filter(climdata_fil$DOY>=(init_d) & climdata_fil$DOY<=end_d) %>%
        summarise(DTR_a=mean(DTR_d)) %>%
        select(DTR_a) %>%
        unlist(use.names=FALSE)
      
      new.row.df <- data.frame(Anno,init_d,end_d) %>%
        cbind(DTR)
      
      indices.df <-rbind(indices.df,new.row.df)
    
  }
  indices.df <- indices.df %>% rename(Year=Anno)
  return(indices.df)
}

