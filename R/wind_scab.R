#' Estimation of the risk for wind scab on fruit skin
#'
#' The function estimates the risk of wind-induced abrasion injuries
#' (wind scab) in fruit skin during the sensitive periods of the 
#' species. This function estimates as risky the daily hours with 
#' 'moderate breeze' wind (above 5.5 m/s in the Beaufort scale) or 
#' stronger, estimated from a dataset with daily wind speeds. 
#' 
#' Hourly wind speeds from daily values are computed using the 
#' formulas proposed by (Guo et al, 2016), using mean daily 
#' values (WSmed, required) and maximum ones (WSmax, optional). 
#' If only mean wind values are available, the function uses a
#' modified version of the Guo formula, so that the maximum 
#' values are obtained in daytime hours.
#'
#' Sensitive periods for wind scab in plums or nectarines
#' correspond to the early stages of fruit growth, usually the
#' first three weeks after full bloom (Michailides et al 1992, 
#' Michailides and Morgan 1992), mainly due to persistent leaf
#' brushing on fruit skin between the stages "8mm-fruit" and 
#' "20mm-fruit". A second sensitive period for cherries, plums, 
#' peaches and nectarines is pre-harvest (30 days prior to that), 
#' due to persistent friction against branches. The function 
#' allows to set both periods or only one of them.
#'
#' @param climdata a dataframe with daily wind speed data.
#' Required columns are Year, Month, Day, WSmed. WSmax is
#' optional.
#' @param fendata a dataframe which can contain early fruit growth and
#' harvest dates. Start of initial growth (Start_ing) and end of initial growth
#' (End_ing) dates are used to assess for early wind-induced abrasion risk and
#' harvest dates (Harvest) are used for late (pre-harvest) abrasion risks.
#' Must contain the column Year and can contain either Start_ing and End_ing
#' or Harvest columns or the three all.
#' @return data frame with the columns Year, Day_s, Day_e, WA_efg (accumulated
#' hours with WS>5.5 m/s on early fruit growth stage), Day_h, WA_bh
#' (accumulated hours with WS>5.5 m/s on the month before harvest).
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
#' \dontrun{
#'
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, include date and DOY information and estimate
#' #the number favorable days on each year in the series
#'
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, WSmed) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date))
#' WindRisk <- wind_scab(Weather,Sensit_dates)
#'
#' }
#' @export wind_scab
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date make_datetime

wind_scab <- function(climdata,fendata)
{
  if(!"WSmax" %in% colnames(climdata))
  {
    cat("Warning: No maximum windspeed data provided,\nhourly values will
        be estimated using WSmed only\n");
    Viento <- select(climdata,"Year","Month","Day","WSmed") %>%
      mutate(Datetime = make_datetime(Year, Month, Day, hour=0,min = 0),
             Date=make_date(Year, Month, Day))
    minday = as_datetime(Viento$Date[1])
    maxday = as_datetime(Viento$Date[nrow(Viento)])
    dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
      rename(Datetime=1) %>%
      mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
             Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))
    
    wind_h = merge(dates, Viento, by = "Date", all = TRUE) %>%
      mutate(WS = ifelse(WSmed + (1/2)*WSmed*cos((Hour+12)*pi/12)<0,0,
                         WSmed + (1/2)*WSmed*cos((Hour+12)*pi/12)))
  }else{
    Viento <- select(climdata,"Year","Month","Day","WSmed","WSmax") %>%
      mutate(Datetime = make_datetime(Year, Month, Day, hour=0,min = 0),
             Date=make_date(Year, Month, Day))
    minday = as_datetime(Viento$Date[1])
    maxday = as_datetime(Viento$Date[nrow(Viento)])
    dates <- as.data.frame(seq(minday, maxday, by = "hour")) %>%
      rename(Datetime=1) %>%
      mutate(Date=date(Datetime), Year=year(Datetime), Month=month(Datetime),
             Day=day(Datetime), DOY=yday(Datetime), Hour=hour(Datetime))
    
    wind_h = merge(dates, Viento, by = "Date", all = TRUE) %>%
      mutate(WS = ifelse(WSmed + (1/pi)*WSmax*cos(Hour*pi/12)<0,0,
                         WSmed + (1/pi)*WSmax*cos(Hour*pi/12)))
  }
  wind_h <- wind_h %>% select(Date,DOY,Hour,WS) %>%
    group_by(Date)%>%
    summarise(h_wind= sum(WS>=5.5)) %>%
    mutate(Year=year(Date),Month=month(Date),Day=day(Date),DOY = yday(Date)) %>%
    select(1,3:6,2)

  seasons <- unique(fendata$Year)

  if(("Start_ing" %in% colnames(fendata) & "End_ing" %in% colnames(fendata))==FALSE)
  {
    cat("No valid columns for initial fruit growth dates supplied");
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
    cat("No valid harvest date columns supplied");
  } else{
    latew_cn <- c("Year","Harvest","WA_bh")
    latew.df <-data.frame(matrix(ncol=3, nrow=0, byrow=FALSE))
    colnames(latew.df) <- latew_cn

    for (sea in 1:length(seasons)){
      Anno <- as.numeric(seasons[sea])
      climdata_fil <- wind_h %>%
        filter(wind_h$Year==Anno)
      Harvd <- as.numeric(fendata$Harvest[sea])
      evawfg_fil <- climdata_fil %>%
        filter(climdata_fil$DOY>(Harvd-30) & climdata_fil$DOY<=Harvd) %>%
        summarise(windh=sum(h_wind)) %>%
        select(windh)
      new.row.df <- data.frame(Anno,Harvd) %>%
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
    cat("No results")
  }
 return(windrisk.df)
}
