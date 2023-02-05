#' Estimation of the russet risk for apple and pear fruits
#'
#' This function assesses the risk of russet in fruit skins. The
#' risk is defined by the number of hours with the relative humidity (RH)
#' above a threshold during a given period. For reference, in 'Conference' 
#' pear the risk is defined by the number of hours with RH> 75\% from 
#' 12 to 30 days after full bloom (Alegre, 2013). In 'Golden' apple, 
#' the risk is defined by the number of hours with RH> 55\% from 
#' 30 to 34 days after full bloom (Barcelo-Vidal et al., 2013). 
#' The function requires hourly temperatures and humidity, 
#' if only daily data is available, the function hourly_RH can be 
#' used to estimate them.
#'
#' @param climdata a dataframe with hourly temperature and RH
#' data. Required columns are Date, Year, Month, Day, DOY (julian day),
#' Hour and RH.
#' @param fendata a dataframe with julian day of occurrence of the full
#' bloom (F2) phenological stage.
#' Must contain the columns Year and Fday in that order.
#' @param RH_crit the relative humidity threshold
#' @param init_d the initial date (as days after full bloom) of the sensitive period
#' @param end_d the end date (as days after full bloom) of the sensitive period
#' @return data frame with the number of risk hours (Russet_hours)
#' in the sensitive period for each year in the series.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Alegre S. 2013. Tecnicas de cultivo. In. VII Foro INIA "adaptacion a 
#' cambio climatico en la produccion fruticola de hueso y pepita". Madrid, 
#' Spain, pp 1-18
#' Barcelo-Vidal C, Bonany J, Martin-Fernandez JA and Carbo J. 2013. 
#' Modelling of weather parameters to predict russet on 'Golden Delicious'
#' apple. J. Hort. Sci. Biotech. 88: 624-630.
#'
#' @examples
#'
#' # Select the appropiate columns from the example dataset
#' # Dates_BT and rename column names to make the file compatible
#' # with the function
#' library(magrittr)
#' library(dplyr)
#' library(lubridate)
#' Bloom <- Dates_BT %>%
#'    select(Year, sbloom) %>%
#'    rename(Fday=sbloom) %>%
#'    filter(Year==2003)
#' # Obtain estimated hourly RH from the example dataset Tudela_DW
#' Weather <- Tudela_DW %>%
#'         filter (Tudela_DW$Year==2003)
#' RH_h <- hourly_RH(Weather, 42.13132)
#' # Estimate the number of russet-inducing days for a RH>55\% 
#' # between 30 to 34 days after full bloom for each season
#' Russet_Risk <-russet(RH_h,Bloom,55,30,34)
#' 
#' @export russet
#' @import magrittr dplyr 
#' @importFrom lubridate make_date year month day yday

russet <- function(climdata, fendata, RH_crit, init_d, end_d)
{
  climdata <- climdata %>%
    group_by(Date) %>%
    summarise(h_russ= sum(RH>=RH_crit)) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date), DOY=yday(Date)) %>%
    select(-h_russ,h_russ)
  Seasons <- unique(fendata$Year)
  russrisks_cn <- c("Year","Russ_hours")
  russrisk.df <-data.frame(matrix(ncol=2, nrow=0, byrow=FALSE))
  colnames(russrisk.df) <- russrisks_cn
  for (sea in 1:(length(Seasons))){
    Fdate <- slice(fendata, sea)
    Anno <- as.numeric(Fdate[1])
    Fday <- as.numeric(Fdate[2])
    fendata_fil <- fendata %>% filter(fendata$Year==Anno)
    russhours_fil <- climdata %>%
      filter(climdata$Year==Anno & climdata$DOY>=Fday+init_d & climdata$DOY<=Fday+end_d)
    russhours_fil <- select(russhours_fil, h_russ)
    russhours<- sum(unlist(russhours_fil, use.names=FALSE))
    new.row.df <- data.frame(Anno,russhours)
    russrisk.df <-rbind(russrisk.df,new.row.df)
  }
  russrisk.df <- russrisk.df %>%
    rename(Year=Anno, Russet_hours=russhours)
  return(russrisk.df)
}

