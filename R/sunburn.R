#' Evaluation of weather conditions for sunburn in apple fruit surface
#'
#' This function estimates the number of days in which apple fruit
#' surface temperature (FST) exceeds the thresholds indicated by
#' Rackso and Schrader (2012) for two types of sunburn damages.
#' 
#' Sunburn necrosis (SN), the most severe type of sunburn, with a dark
#' brown or black necrotic spot on the exposed fruit surface is considered
#' to appear when FST reaches 52ºC. Sunburn browning (SB) is the most 
#' prevalent type of sunburn on attached sun-exposed apples (acclimated to
#' high light). The threshold temperature for SB is set in 46ºC, and 
#' corresponds to the most sensitive apple cultivars (like Cameo or 
#' Honeycrisp). 
#'
#' FST is estimated from daily maximum air temperature using the expression
#' proposed by Schrader et al (2003).
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param first_d Numeric, it is the first date, indicated as day of the year
#' (DOY), in the assessment.
#' @param last_d a vector with the last(s) dates for the assessment (as DOY).
#' Examples could be harvest dates for several cultivars.
#' @return data frame with the number of days within the assessed period(s).
#' Contains the columns Year, Harvest (values from last_d), SB_browning
#' and SB_necrosis.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Rackso J and Schrader LE, 2012. Sunburn of apple fruit: Historical
#' background, recent advances and future perspectives. Critical Reviews
#' in Plant Sciences 31, 455-504.
#'
#' Schrader L, Zhang J and Sun J, 2003. Environmental stresses that cause
#' sunburn of apple. Acta Horticulturae 618, 397-405.
#'
#' @examples
#'
#' # Create one vector with start date (i.e. hand thinning) and a vector 
#' # with harvest dates to test sunburn risk for several cultivars using.
#' Thinning_d <- 135
#' Harvest_d <- c(225,245,260)
#' Sunburn_risk <- sunburn(Tudela_DW,Thinning_d, Harvest_d)
#' 
#' @export sunburn
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date


sunburn <- function(climdata,first_d, last_d)
{
  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date = make_date(Year, Month, Day),
           DOY = yday(Date),
           FST=1.17*Tmax+6.21,
           SB_browning=ifelse(FST>=46,1,0),
           SB_necrosis=ifelse(FST>=52,1,0))
  seasons <- unique(climdata$Year)

  sunbpot_cn <- c("Year","Harvest","SB_browning","SB_necrosis")
  sunbpot.df <-data.frame(matrix(ncol=5, nrow=0, byrow=FALSE))
  colnames(sunbpot.df) <- sunbpot_cn

  for (sea in 1:length(seasons)){
    Anno <- as.numeric(seasons[sea])
    climdata_fil <- climdata %>%
      filter(climdata$Year==Anno)
    for (nharv in 1:length(last_d)){
      Day_h <- as.numeric(last_d[nharv])
      evacol_fil <- climdata_fil %>%
        filter(climdata_fil$DOY>first_d & climdata_fil$DOY<=Day_h) %>%
        summarise(SB_browning=sum(SB_browning), SB_necrosis=sum(SB_necrosis)) %>%
        select(SB_browning,SB_necrosis)
      new.row.df <- data.frame(Anno,Day_h) %>%
        cbind(evacol_fil)
      sunbpot.df <-rbind(sunbpot.df,new.row.df)
    }
  }
  sunbpot.df <- sunbpot.df %>% rename(Year=Anno, Harvest=Day_h)
  return(sunbpot.df)
}
