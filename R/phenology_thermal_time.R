#' Prediction of phenological stages using a thermal time model
#'
#'
#' The function predicts phenological phases for a climate series
#' from daily chill and heat requirements and forcing data.
#' The thermal time model used in the fucntion consider consider that
#' only heat accumulated from a set date to a given sum explain the
#' date of occurrence of the phenological stage (i.e, it assumes that
#' dormancy release occurs before that date). The function is independent
#' of the method used to calculate forcing heat, so that forcing heat can
#' be supplied either as GDD or GDH. The function allows predicting several
#' stages (or the same for different cultivars), by supplying
#' a dataframe in which each row contains the day for starting forcing
#' and heat requirements for a phenological stage.
#'
#' @param GDH_day a dataframe with daily forcing accumulation
#' must contain the columns Year, Month, Day, DOY, GD.
#' @param Reqs a dataframe with starting dates and forcing requirements,
#' must contain the columns Dreq, Freq.
#' @return data frame with the predicted dates for every season in GDH_day
#' and stages in Reqs. For each season and phenological stage, the information
#' provided is the date in which forcing begins (Year and DOY), and the
#' date in which the phenological stage is met (Year and DOY).
#' It contains the columns Dreq, Freq, Season, Dreq_Year,
#' Dreq_DOY, Freq_Year, Freq_DOY.
#' @author Carlos Miranda
#' @examples
#'
#' \dontrun{
#'
#' Predicted_Dates <- phenology_thermal_time(Weather,Golden_reqs)
#'
#' }
#' @export phenology_thermal_time
#' @import data.table tidyverse zoo lubridate

phenology_thermal_time <- function(GDH_day,Reqs){
  Seasons <- unique(GDH_day$Year)
  Phendates_cols <- c("Dreq","Freq","Season","Dreq_Year","Dreq_DOY","Freq_Year","Freq_DOY")
  Phendates_pred <-data.frame(matrix(ncol=7, nrow=0, byrow=FALSE))
  colnames(Phendates_pred) <- Phendates_cols
  for(i in 1:nrow(Reqs)) {
    Sel_reqs <- slice(Reqs, i)
    Dreq <- as.numeric(Sel_reqs[1])
    Freq <- as.numeric(Sel_reqs[2])
    for(sea in 1:(length(Seasons)-1)){
      Anno = as.numeric(Seasons[sea])+1
      if (Dreq>=305){
        Data_fil <- GDH_day %>% filter(
          (GDH_day$Year == Anno-1 & GDH_day$DOY>= Dreq) |
            (GDH_day$Year == Anno))
      } else {
        Data_fil <- GDH_day %>% filter(
          (GDH_day$Year == Anno & GDH_day$DOY>= Dreq))
      }
      Data_fil <- arrange(Data_fil, Data_fil$Year, Data_fil$DOY)
      Data_fil2 <- Data_fil %>%
        mutate(GDacu=cumsum(GD)) %>%
        select("GDacu") %>%
        unlist(use.names=FALSE)
      Dop <-which(Data_fil2>=Freq)[1]
      Sel_Dreq <- select(slice(Data_fil, 1), Year, DOY) %>% rename(Dreq_Year=Year, Dreq_DOY=DOY)
      Sel_Freq <- select(slice(Data_fil, Dop), Year, DOY) %>% rename(Freq_Year=Year, Freq_DOY=DOY)
      if (nrow(Sel_Freq)==0){
        newRow <- data.frame(Freq_Year= Anno, Freq_DOY = -99)
        Sel_Freq <- rbind(Sel_Freq , newRow)
      }
      Sel_case <- bind_cols(Sel_reqs, Sel_Dreq, Sel_Freq) %>%
        mutate(Season=paste(Anno-1,"-",Anno)) %>%
        select(Dreq, everything())
      Phendates_pred <- bind_rows(Phendates_pred, Sel_case)
    }
  }
  return(Phendates_pred)
}

