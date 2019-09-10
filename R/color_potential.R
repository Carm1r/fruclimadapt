#' Evaluation of weather conditions for anthocyain formation
#' in apple skin
#'
#' This function estimates the number of days favorable and
#' unfavorable for anthocyanin accumulation in the skin of
#' red apple cultivars during the month (30 days) before harvest.
#' A favorable day (Cool day) is considered when the daily maximum
#' temperature is below 26ºC, an unfavorable day (Hot day) when
#' the minimum temperature is above 20ºC (Lin-Wang et al, 2011). The
#' function allows testing several harvest dates, supplied as
#' a vector.
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param harvest a vector with expected harvest days
#' (expressed as day of the year)
#' @return data frame with the number of favorable (Cool_d) and unfavorable
#' (Hot_d) days for red color, for each year (Year) in the series and
#' harvest date (Day_h) provided.
#' @author Carlos Miranda
#' @references
#'
#' Lin-Wang K, Micheletti D et al, 2011. High temperature reduces apple fruit
#' colour via modulation of the anthocyanin regulatory complex. Plant, Cell
#' and Environment 34, 1176-1190.
#'
#' @examples
#'
#' \dontrun{
#'
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, create a vector or harvest dates and
#' #estimate the number favorable and unfavorable days on each year
#' #in the series.
#'
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, Tmax, Tmin)
#' harvest <- c(225, 250, 275)
#' Colorassess <- color_potential(Weather, harvest)
#'
#'}
#' @export color_potential
#' @import data.table tidyverse zoo lubridate

color_potential <- function(climdata, harvest)
{
  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date = make_date(Year, Month, Day),
           DOY = yday(Date),
           Cool=if_else(Tmax<=26,1,0),
           Hot=ifelse(Tmin>=20,1,0))
  seasons <- unique(climdata$Year)

  colorpot_cn <- c("Year","Harvest_d","Cool_d","Hot_d")
  colorpot.df <-data.frame(matrix(ncol=4, nrow=0, byrow=FALSE))
  colnames(colorpot.df) <- colorpot_cn

  for (sea in 1:length(seasons)){
    Anno <- as.numeric(seasons[sea])
    climdata_fil <- climdata %>%
      filter(climdata$Year==Anno)
    for (nharv in 1:length(harvest)){
      Day_h <- as.numeric(harvest[nharv])
      evacol_fil <- climdata_fil %>%
        filter(climdata_fil$DOY>(Day_h-30) & climdata_fil$DOY<=Day_h) %>%
        summarise(Cool_d=sum(Cool),Hot_d=sum(Hot)) %>%
        select(Cool_d,Hot_d)
      new.row.df <- data.frame(Anno,Day_h) %>%
        cbind(evacol_fil)
      colorpot.df <-rbind(colorpot.df,new.row.df)
    }
  }
  colorpot.df <- colorpot.df %>% rename(Year=Anno)
  return(colorpot.df)
}

