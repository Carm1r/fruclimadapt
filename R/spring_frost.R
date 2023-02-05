#' Calculates the risk of spring frosts for a climate series
#'
#' The function evaluates the number of early and spring frosts and
#' the expected frost damage on each season within a climate data 
#' series. Frost damage is assumed to be multiplicative, directly
#' related to the minimum temperature and unrelated to the duration
#' of the frost. The function is an enhanced version of the 
#' Damage Estimator Excel application program (DEST.xls) created by 
#' de Melo-Abreu and Snyder and bundled with FAO Environment and
#' Natural Resources Series 10 manual (Snyder and de Melo-Abreu, 2005). 
#' The function compares daily minimum temperature (Tmin) with the 
#' critical temperatures (Tcrit) for that day. Daily Tcrit are 
#' linearly interpolated from a user-provided dataframe with the day 
#' of occurrence of the stages on each season and a vector of critical 
#' temperatures (the lethal temperatures for 10\% (LT_10) and 90\% (LT_90)
#' of the organs) for each phenological stage. The main difference of 
#' spring_frost with DEST.xls is that the latter uses the same dates of 
#' phenological occurrence for all the years evaluated (up to 50 years of
#' data), while spring_frost is able to use the expected dates of occurrence 
#' for each year from historical records or estimations produced by the 
#' functions phenology_thermal_time or phenology_sequential, included in 
#' this package. There is no limit for the number of years evaluated. 
#' 
#' The last day in the year for the evaluation can be defined by the user, 
#' and it is set by default set at DOY 181, to avoid computing autumn 
#' and winter frosts.
#' 
#' The function currently works only with phenological dates occurring
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
#' The df Damage_frosts indicates the total number of frost days and the expected 
#' damage (as \% of organs) for every year in the series. It has the columns 
#' Year, Frost_d, Damage.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references 
#' Snyder RL, de Melo-Abreu JP. 2005. Frost Protection: fundamentals, practice and 
#' economics (2 volumes). FAO Environment and Natural Resources Service Series, 
#' No. 10 - FAO, Rome.
#' 
#' @examples
#' # Generate hourly temperatures from the first season from
#' # the example dataset Tudela_DW
#' library(magrittr)
#' library(dplyr)
#' library(lubridate)
#' Tudela_Sel <- Tudela_DW %>% filter(Tudela_DW$Year<=2002)
#' Tudela_HT <- hourly_temps(Tudela_Sel,42.13132)
#' # Calculate chill as chill portions, starting on DOY 305
#' Chill <- chill_portions(Tudela_HT,305)
#' # Calculate forcing heat as growing degree hours (GDH) with the linear model,
#' # using base temperature 4.7 C and no upper thresholds
#' GDH <- GDH_linear(Tudela_HT,4.7,999,999)
#' # Combine Chill and GDH values in a dataframe with a format compatible with
#' # the function phenology_sequential
#' Tudela_CH <- merge(Chill,GDH) %>%
#'    select(Date, Year, Month, Day, DOY, Chill,GDH) %>%
#'    arrange(Date) %>%
#'    rename(GD=GDH)
#' # Obtain the predicted dates using the example dataset with the requirements 
#' # indicated in the Bigtop_reqs example dataset and create a dataframe with a
#' # format compatible with the function spring_frost
#' Phenology_BT <- phenology_sequential(Tudela_CH, Bigtop_reqs, 305) %>% 
#'    select(Freq_Year,Freq_DOY) %>%
#'    rename(Year=Freq_Year,Pheno_date=Freq_DOY) %>%
#'    filter (Year==2002)
#' # Create a dataframe with daily minimum temperatures with the 
#' # format required by spring_frost
#' Tmin_Tudela <- Tudela_Sel %>% filter(Year==2002) %>%
#'   mutate(Date=make_date(Year,Month,Day), DOY=yday(Date)) %>%
#'   select(Year, DOY, Tmin) 
#' # Predict the number and accumulated damage of the spring frosts using the
#' # critical values contained in the example dataset Tcrits_peach and extract
#' # the dataframe with the total results for each year
#' Frost_BT <- spring_frost(Tmin_Tudela, Phenology_BT, Tcrits_peach, 181)
#' Frost_results <- as.data.frame(Frost_BT[['Damage_frosts']]) 
#' 
#' @export spring_frost
#' @import magrittr dplyr 
#' @importFrom lubridate make_date

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



