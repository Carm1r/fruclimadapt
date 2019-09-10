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
#' evaluated each year is DOY 181, to avoid computing autumn and winter
#' frosts.
#' The function currently works only with phenological dates occuring
#' within the same year.
#'
#' @param tempdata a dataframe with daily minimum temperatures for each
#' year in a series. Must contain the columns Year, julian day of year (DOY) and
#' the minimum daily temperature (Tmin).
#' @param fendata a dataframe with julian day of occurrence of the phenological
#' stages as produced by phenology_thermal_time or phenology_sequential functions.
#' Must contain the columns Year, Chill_comp and Pheno_date.
#' @param tcrit a vector with critical temperatures for all the phenological stages
#' indicated in fendata AND Chill_comp date.
#' @return a dataframe with the columns Year and Frost_d, which indicates
#' the number of frost days for every year in the series.
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
#' Tcrit <- c(-6.6,-4.8,-4,-1.8,-1.4,-2.3)
#' Frisk <-spring_frost(Tmins,Tcrit,Phen_data)
#'
#' }
#' @export spring_frost
#' @import data.table tidyverse zoo lubridate

spring_frost <- function(tempdata,fendata,tcrit){
  Years <- unique(fendata$Year)
  frisks_cn <- c("Year","Frost_d")
  frisk.df <-data.frame(matrix(ncol=2, nrow=0, byrow=FALSE))
  colnames(frisk.df) <- frisks_cn
  for (sea in 1:(length(Years))){
    Anno = as.numeric(Years[sea])
    fendata_fil <- fendata %>% filter(fendata$Year==Anno)
    fendata_fil <- arrange(fendata_fil, fendata_fil$Pheno_date)
    tempdata_fil <- tempdata %>% filter(tempdata$Year==Anno+1)
    Creq = fendata_fil$Chill_comp[1]
    Efens <- fendata_fil$Pheno_date
    fendates <-data.frame(matrix(ncol=0, nrow=length(Efens)+1, byrow=FALSE))
    fendates$DOY <-c(Creq,Efens)
    fendates$Tcrit <- tcrit
    frisk <- days_frost(tempdata_fil,fendates)
    Fday=0
    for (i in 1:length(mintemps$DOY)){
      if (mintemps$Tmin[i]<=mintemps$Tcrit[i]){
        Fday=Fday+1
      }
    }
    new.row.df <- data.frame(Anno,Fday)
    frisk.df <-rbind(frisk.df,new.row.df)
  }
  frisk.df <- frisk.df %>% rename(Year=Anno,Frost_d=Fday)
  return(frisk.df)
  }





