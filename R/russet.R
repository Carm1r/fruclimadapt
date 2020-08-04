#' Estimation of the russet risk for apple and pear fruits
#'
#' This function assesses the risk of russet in apple and pear 
#' fruits by estimating the number of hours with relative 
#' humidity above 75\% in the critical period (between 12 
#' and 30 days after full bloom, DAFB). The function requires
#' hourly temperatures and humidity, if only daily data is 
#' available, the function hourly_RH can be used to estimate them.
#'
#' @param climdata a dataframe with hourly temperature and RH
#' data. Required columns are Date, Year, Month, Day, DOY (julian day),
#' Hour and RH.
#' @param fendata a dataframe with julian day of occurrence of the full
#' bloom (F2) phenological stage.
#' Must contain the columns Year and Fday in that order.
#' @return data frame with the number of hours with RH>75\%
#' between 12 and 30 DAFB (Russet_h) each year in the series.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @examples
#'
#' # Select the appropiate columns from the example dataset
#' # Dates_BT and rename column names to make the file compatible
#' # with the function
#' Bloom <- Dates_BT %>%
#'    select(Year, sbloom) %>%
#'    rename(Fday=sbloom)
#' # Obtain estimated hourly RH from the example dataset Tudela_DW
#' RH_h <- hourly_RH(Tudela_DW, 42.13132)
#' # Estimate the number of russet-inducing days for each season
#' Russet_Risk <-russet(RH_h,Bloom)
#' 
#' @export russet
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

russet <- function(climdata, fendata)
{
  climdata <- climdata %>%
    group_by(Date) %>%
    summarise(h_russ= sum(RH>=75)) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date), DOY=yday(Date)) %>%
    select(-h_russ,h_russ)
  Seasons <- unique(fendata$Year)
  russrisks_cn <- c("Year","Russ_h")
  russrisk.df <-data.frame(matrix(ncol=2, nrow=0, byrow=FALSE))
  colnames(russrisk.df) <- russrisks_cn
  for (sea in 1:(length(Seasons))){
    Fdate <- slice(fendata, sea)
    Anno <- as.numeric(Fdate[1])
    Fday <- as.numeric(Fdate[2])
    fendata_fil <- fendata %>% filter(fendata$Year==Anno)
    russhours_fil <- climdata %>%
      filter(climdata$Year==Anno & climdata$DOY>=Fday+12 & climdata$DOY<=Fday+30)
    russhours_fil <- select(russhours_fil, h_russ)
    russhours<- sum(unlist(russhours_fil, use.names=FALSE))
    new.row.df <- data.frame(Anno,russhours)
    russrisk.df <-rbind(russrisk.df,new.row.df)
  }
  russrisk.df <- russrisk.df %>%
    rename(Year=Anno, Russet_h=russhours)
  return(russrisk.df)
}

