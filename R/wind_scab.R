#' Estimation of the risk for wind scab on fruit skin
#'
#' The function estimates the risk of wind-induced abrasion injuries
#' (wind scab) on fruit skin during the sensitive periods of the 
#' species. This function estimates as risky the daily hours with 
#' 'moderate breeze' wind (equal or above 5.5 m s-1 in the Beaufort 
#' scale) or stronger, estimated from a dataset with daily wind 
#' speeds. 
#' 
#' Hourly wind speeds from daily values are computed using the 
#' formulas proposed by (Guo et al, 2016), using mean daily 
#' values (u2med, required) and maximum ones (u2max, optional). 
#' If only mean wind values are available, the function uses a
#' modified version of the Guo formula, so that the maximum 
#' values are obtained in daytime hours.
#'
#' Sensitive periods for wind scab in plums or nectarines
#' correspond to the early stages of fruit growth, usually the
#' first three weeks after full bloom (Michailides et al 1992, 
#' Michailides and Morgan 1992), mainly due to persistent leaf
#' brushing on fruit skin between the stages '8-mm fruit' and 
#' '20-mm fruit'. A second sensitive period for cherries, plums, 
#' peaches and nectarines is pre-harvest (30 days prior to that), 
#' due to persistent friction against branches. The function 
#' allows to set both periods or only one of them.
#'
#' @param climdata a dataframe with daily wind speed data.
#' Required columns are Year, Month, Day, u2med. u2max is
#' optional.
#' @param fendata a dataframe which can contain early fruit growth and
#' harvest dates. Start of initial growth (Start_ing) and end of initial growth
#' (End_ing) dates are used to assess for early wind-induced abrasion risk and
#' harvest dates (Harvest) are used for late (pre-harvest) abrasion risks.
#' Must contain the column Year, and can contain either Start_ing and End_ing
#' or Harvest columns, or the three of them.
#' @return data frame with the columns Year, Day_s, Day_e, WA_efg (accumulated
#' hours with u2>5.5 m s-1 on early fruit growth stage), Day_h, WA_bh
#' (accumulated hours with u2>5.5 m s-1 on the month before harvest).
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Guo Z, Chang C, Wang R, 2016. A novel method to downscale daily wind
#' statistics to hourly wind data for wind erosion modelling. In: Bian F.,
#' Xie Y. (eds) Geo-Informatics in Resource Management and Sustainable
#' Ecosystem. GRMSE 2015. Communications in Computer and Information Science,
#' vol 569. Springer, Berlin, Heidelberg
#'
#' Michailides TJ, Morgan DP, Ramirez HT and Giacolini EL, 1992. Determination
#' of the period when prunes are prone to development of russet scab and
#' elucidation of the mechanism by which captan controls russet scab.
#' California dried plum board research reports 1992, 157-175.
#'
#' Michailides TJ and Morgan DP, 1992. Development of wind scab and
#' predisposition of french prune fruits to preharvest and postharvest
#' fungal decay by wind scab and russet scab.California dried plum board
#' research reports 1992, 149-156.
#'
#' @examples
#'
#' # Select the appropiate columns from the example Dates_BT dataset
#' # and estimate wind scab risk for Big Top nectarine in Tudela using
#' # the example weather dataset Tudela_DW
#' library(tidyverse)
#' library(lubridate)
#' Growth_BT <- Dates_BT %>% select(Year, Start_ing, End_ing, Harvest)
#' WindRisk_BT <- wind_scab(Tudela_DW, Growth_BT)
#' 
#' @export wind_scab
#' @import magrittr dplyr 
#' @importFrom lubridate make_date make_datetime as_datetime date year month day yday hour

wind_scab <- function(climdata,fendata)
{
  if(!"u2max" %in% colnames(climdata))
  {
    message("Warning: No maximum windspeed data provided,\nhourly values will
        be estimated using u2med only\n");
    Viento <- select(climdata,"Year","Month","Day","u2med") %>%
      mutate(Datetime = make_datetime(Year, Month, Day, hour=0,min = 0),
             Date=make_date(Year, Month, Day))
    minday = as_datetime(Viento$Date[1])
    maxday = as_datetime(Viento$Date[nrow(Viento)])
    dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
      rename(Datetime=1) %>%
      mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
             Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))
    
    wind_h = merge(dates, Viento, by = "Date", all = TRUE) %>%
      mutate(u2 = ifelse(u2med + (1/2)*u2med*cos((Hour+12)*pi/12)<0,0,
                         u2med + (1/2)*u2med*cos((Hour+12)*pi/12)))
  }else{
    Viento <- select(climdata,"Year","Month","Day","u2med","u2max") %>%
      mutate(Datetime = make_datetime(Year, Month, Day, hour=0,min = 0),
             Date=make_date(Year, Month, Day))
    minday = as_datetime(Viento$Date[1])
    maxday = as_datetime(Viento$Date[nrow(Viento)])
    dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
      rename(Datetime=1) %>%
      mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
             Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))
    
    wind_h = merge(dates, Viento, by = "Date", all = TRUE) %>%
      mutate(u2 = ifelse(u2med + (1/pi)*u2max*cos(Hour*pi/12)<0,0,
                         u2med + (1/pi)*u2max*cos(Hour*pi/12)))
  }
  wind_h <- wind_h %>% select(Date,DOY,Hour,u2) %>%
    group_by(Date)%>%
    summarise(h_wind= sum(u2>=5.5)) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date),DOY = yday(Date)) %>%
    select(1,3:6,2)

  seasons <- unique(fendata$Year)

  if(("Start_ing" %in% colnames(fendata) & "End_ing" %in% colnames(fendata))==FALSE)
  {
    message("No valid columns for initial fruit growth dates supplied");
  } else{
    earlyw_cn <- c("Year","Start_ing","End_ing","WA_efg")
    earlyw.df <-data.frame(matrix(ncol=4, nrow=0, byrow=FALSE))
    colnames(earlyw.df) <- earlyw_cn

    for (sea in 1:length(seasons)){
      Anno <- as.numeric(seasons[sea])
      climdata_fil <- wind_h %>%
        filter(wind_h$Year==Anno)
      Day_s <- as.numeric(fendata$Start_ing[sea])
      Day_e <- as.numeric(fendata$End_ing[sea])
      evawfg_fil <- climdata_fil %>%
        filter(climdata_fil$DOY>=Day_s & climdata_fil$DOY<=Day_e) %>%
        summarise(windh=sum(h_wind)) %>%
        select(windh)
      new.row.df <- data.frame(Anno,Day_s,Day_e) %>%
        cbind(evawfg_fil)
      earlyw.df <-rbind(earlyw.df,new.row.df)

      }
    earlyw.df <- earlyw.df %>% rename(WA_fg=windh)
    }
  if(("Harvest" %in% colnames(fendata))==FALSE)
  {
    message("No valid harvest date columns supplied");
  } else{
    latew_cn <- c("Year","Harvest","WA_bh")
    latew.df <-data.frame(matrix(ncol=3, nrow=0, byrow=FALSE))
    colnames(latew.df) <- latew_cn

    for (sea in 1:length(seasons)){
      Anno <- as.numeric(seasons[sea])
      climdata_fil <- wind_h %>%
        filter(wind_h$Year==Anno)
      Harv_d <- as.numeric(fendata$Harvest[sea])
      evawfg_fil <- climdata_fil %>%
        filter(climdata_fil$DOY>(Harv_d-30) & climdata_fil$DOY<=Harv_d) %>%
        summarise(windh=sum(h_wind)) %>%
        select(windh)
      new.row.df <- data.frame(Anno,Harv_d) %>%
        cbind(evawfg_fil)
      latew.df <-rbind(latew.df,new.row.df)
      }
    latew.df <- latew.df %>% rename(WA_bh=windh)
    }
  if(exists('earlyw.df') && is.data.frame(get('earlyw.df')) &
     exists('latew.df') && is.data.frame(get('latew.df'))){
    windrisk.df <-merge(earlyw.df,latew.df,by="Anno") %>%
      rename(Year=Anno)
  } else if(exists('earlyw.df') && is.data.frame(get('earlyw.df'))){
    windrisk.df <- earlyw.df %>%
      rename(Year=Anno)
  } else if(exists('latew.df') && is.data.frame(get('latew.df'))){
    windrisk.df <- latew.df %>%
      rename(Year=Anno)
  } else {
    message("No results")
  }
 return(windrisk.df)
}
