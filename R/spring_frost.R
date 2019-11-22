#' Calculates the risk of spring frosts for a climate series
#'
#' The function evaluates the number of early and spring frosts
#' on each season within a climate data series. It compares minimum
#' daily temperature (Tmin) with the critical temperature (Tcrit).
#' Daily critical temperatures are linearly interpolated from a
#' user-provided dataframe with the day of occurrence of the stages on
#' each season and a vector of critical temperatures for each stage.
#'
#' The function checks each day if Tmin is below Tcrit and,
#' if so, that day is considered as a frost day (Fday). The last day
#' evaluated each year is user defined, by default set at DOY 181, 
#' to avoid computing autumn and winter frosts.
#' The function currently works only with phenological dates occuring
#' within the same year.
#'
#' @param tempdata a dataframe with daily minimum temperatures for each
#' year in a series. Must contain the columns Year, julian day of year (DOY) and
#' the minimum daily temperature (Tmin).
#' @param fendata a dataframe with julian day of occurrence of the phenological
#' stages that can be produced by phenology_thermal_time or phenology_sequential 
#' functions. Must contain the columns Year, and Pheno_date. 
#' @param tcrit a dataframe with columns LT_10 and LT_90,  critical temperatures 
#' for all the  phenological stages indicated in fendata.
#' @param lastday the last day (day of the year) to evaluate. By default, 
#' lastday = 181 (June 30th).
#' @return a list with two data frames. The df Days_frost has the columns Year, DOY, 
#' Tmin, Tcrit and Day_Frost (indicates a day of frost with a 1 if Tmin<=Tcrit). 
#' The df Total_frosts indicates the total number of frost days each year in the
#' series and the expected damage (as % of organs).
#' @author Carlos Miranda
#' @examples
#'
#' \dontrun{
#'
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, include date and DOY information and estimate
#' #the number of frost days for the series
#'
#' Tmins <- Tempdata %>%
#'    select(Year, Month, Day, Tmin) %>%
#'    mutate(Date=make_date(Year,Month,Day),DOY=yday(Date))
#' Frisk <-spring_frost(Tmins,Phen_data,Tcrit,270)
#'
#' }
#' @export spring_frost
#' @import data.table tidyverse zoo lubridate

spring_frost <- function(tempdata,fendata,tcrit, lastday = 181){
  Years <- unique(fendata$Year)
  days_frost_cn <- c("Year","DOY","Dam")
  days_frost.df <-data.frame(matrix(ncol=3, nrow=0, byrow=FALSE))
  colnames(days_frost.df) <- days_frost_cn 
  frisks_cn <- c("Year","Frost_d","LT_10","LT_90","Damage")
  frisk.df <-data.frame(matrix(ncol=5, nrow=0, byrow=FALSE))
  colnames(frisk.df) <- frisks_cn
  for (sea in 1:(length(Years))){
    Anno = as.numeric(Years[sea])
    fendata_fil <- fendata %>% filter(fendata$Year==Anno)
    fendata_fil <- arrange(fendata_fil, fendata_fil$Pheno_date)
    tempdata_fil <- tempdata %>% filter(tempdata$Year==Anno)
    Efens <- fendata_fil$Pheno_date
    fendates <-data.frame(matrix(ncol=0, nrow=length(fendata_fil$Pheno_date), byrow=FALSE))
    fendates$DOY <-Efens
    fendates$LT_10 <- tcrit$LT_10
    fendates$LT_90 <- tcrit$LT_90
    f_risk <- days_frost(tempdata_fil,fendates,lastday) %>% 
      select(Year,DOY,LT_10,LT_90,Dam)
    f_days <- length(which(f_risk$Dam>0))
    f_dam <- f_risk$Dam
    f_surv <- 1-f_dam
    for (i in 1:(length(f_dam)-1)){
      f_surv[i+1]<-(f_surv[i] *f_surv[i+1]) 
    }
    f_dam <- (1-tail(f_surv,n=1))*100
    new.row.df <- data.frame(Anno,f_days,f_dam)
    frisk.df <-rbind(frisk.df,new.row.df)
    days_frost.df <-rbind(days_frost.df,f_risk)
  }
  frisk.df <- frisk.df %>% rename(Year=Anno,Frost_d=f_days,Damage=f_dam)
  frost.list <- list(Days_frost = days_frost.df,Damage_frosts = frisk.df)
  return(frost.list)
  }



