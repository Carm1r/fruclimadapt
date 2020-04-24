#' Prediction of phenological stages using a sequential model
#'
#'
#' The function predicts phenological phases for a climate series
#' from daily chill and heat requirements and daily chill and forcing data.
#' The sequential model used in the fucntion consider that chilling and heat
#' have independent effects. It consists of an accumulation of chill up to the
#' plant requirement followed by heat up to forcing requirement, with no
#' overlap between both phases. The function is independent of the
#' method used to calculate chill and forcing heat, so that chill can
#' be supplied as chill hours, chill units or chill portions
#' (recommended, particularly for warm climates or in climate change studies),
#' forcing heat can be supplied either as GDD or GDH. The function allows
#' predicting several stages (or the same for different cultivars), by supplying
#' a dataframe in which each row contains chill and heat requirements for a
#' phenological stage.
#'
#' @param GDH_day a dataframe with daily chilling and forcing accumulation.
#' Must contain the columns Year, Month, Day, DOY, Chill, GD.
#' @param Reqs a dataframe with chilling and forcing requirements, must
#' contain the columns Creq (for chilling), Freq (for forcing heat).
#' @param Start_chill parameter indicating the day of the year when chill
#' accumulation is supposed to start.
#' @return data frame with the predicted dates for every season in GDH_day
#' and stages in Reqs. For each season and phenological stage, the information
#' provided is the date in which chill requirements are satisfied (Year and
#' DOY), and the date in which the phenological stage is met (Year and DOY).
#' It contains the columns Creq, Freq, Season, Creq_Year,
#' Creq_DOY, Freq_Year, Freq_DOY.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @examples
#'
#' \dontrun{
#'
#' Predicted_Dates <- phenology_sequential(Weather,Golden_reqs)
#'
#' }
#' @export phenology_sequential
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

phenology_sequential <- function(GDH_day,Reqs,Start_chill){
  Seasons <- unique(GDH_day$Year)
  Phendates_cols <- c("Creq","Freq","Season","Creq_Year","Creq_DOY","Freq_Year","Freq_DOY")
  Phendates_pred <-data.frame(matrix(ncol=7, nrow=0, byrow=FALSE))
  colnames(Phendates_pred) <- Phendates_cols
  for(i in 1:nrow(Reqs)) {
    Sel_reqs <- slice(Reqs, i)
    Creq <- as.numeric(Sel_reqs[1])
    Freq <- as.numeric(Sel_reqs[2])
    for(sea in 1:(length(Seasons)-1)){
      Anno = as.numeric(Seasons[sea])+1
      if (Start_chill>=305){
        Data_fil <- GDH_day %>% filter(
          (GDH_day$Year == Anno-1 & GDH_day$DOY>= Start_chill) |
            (GDH_day$Year == Anno))
      } else {
        Data_fil <- GDH_day %>% filter(
          (GDH_day$Year == Anno & GDH_day$DOY>= Start_chill))
      }
      Data_fil <- arrange(Data_fil, Data_fil$Year, Data_fil$DOY) %>%
        filter(Chill>=Creq)
      Data_fil2 <- Data_fil %>%
        mutate(GDacu=cumsum(GD)) %>%
        select("GDacu") %>%
        unlist(use.names=FALSE)
      Dop <-which(Data_fil2>=Freq)[1]
      Sel_Creq <- select(slice(Data_fil, 1), Year, DOY) %>% rename(Creq_Year=Year, Creq_DOY=DOY)
      Sel_Freq <- select(slice(Data_fil, Dop), Year, DOY) %>% rename(Freq_Year=Year, Freq_DOY=DOY)
      if (nrow(Sel_Freq)==0){
        newRow <- data.frame(Freq_Year= Anno, Freq_DOY = -99)
        Sel_Freq <- rbind(Sel_Freq , newRow)
      }
      Sel_case <- bind_cols(Sel_reqs, Sel_Creq, Sel_Freq) %>%
        mutate(Season=paste(Anno-1,"-",Anno)) %>%
        select(Creq, everything())
      Phendates_pred <- bind_rows(Phendates_pred, Sel_case)
    }
  }
  return(Phendates_pred)
}

