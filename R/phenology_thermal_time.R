#' Prediction of phenological stages using a thermal time model
#'
#'
#' The function predicts phenological phases for a climate series
#' using a certain starting date and forcing heat requirements data.
#' The thermal time model used in the function considers that
#' only heat accumulated from a set date to a given sum explain the
#' date of occurrence of the phenological stage (i.e, it assumes that
#' dormancy release occurs before that date). The function is independent
#' of the method used to calculate forcing heat, so that forcing heat can
#' be supplied either as GDD or GDH. The function allows predicting several
#' stages (or the same for different cultivars), by supplying
#' a dataframe in which each row contains the day for starting forcing
#' and heat requirements for a phenological stage.
#'
#' @param GD_day a dataframe with daily forcing accumulation. It
#' must contain the columns Year, Month, Day, DOY, GD.
#' @param Reqs a dataframe in which each row contains the start date
#' for forcing and the heat requirements of one phenological stage. 
#' It must contain the columns Dreq (the start date) and Freq (the forcing 
#' heat requirements).
#' @return dataframe with the predicted dates of occurrence of each phenological 
#' stage defined in Reqs. Columns are Dreq and Freq (start date and forcing heat 
#' requirements for the phenological stage), Season, Dreq_Year and Dreq_DOY 
#' (year and day of the year in which forcing begins), Freq_Year and Freq_DOY 
#' (year and day of the year of occurrence the phenological stage). 
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @examples
#'
#' # Calculate GDD values from a climate dataset with daily temperature data,
#' # using a base temperature of 0 C and format it to be compatible with 
#' # phenology_thermal_time
#' library(tidyverse)
#' library(lubridate)
#' Tudela_GDD <- GDD_linear(Tudela_DW,0) %>% rename(GD=GDD)
#' # Create a dataframe with start dates and forcing requirements for
#' # bloom and veraison in the GFV model for 'Chardonnay' (Parker et al, 
#' # 2013, Agric Forest Meteorol 180:249-264) in the format required for 
#' # the function
#' Dreq <- c(60,60) 
#' Freq <- c(1217,2547)
#' Chardonnay_reqs <- as.data.frame(cbind(Dreq,Freq))
#' # Obtain the predicted dates 
#' Phenology_Chardonnay <- phenology_thermal_time(Tudela_GDD,Chardonnay_reqs)
#' 
#' @export phenology_thermal_time
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date

phenology_thermal_time <- function(GD_day,Reqs){
  Seasons <- unique(GD_day$Year)
  Phendates_cols <- c("Dreq","Freq","Season","Dreq_Year","Dreq_DOY","Freq_Year","Freq_DOY")
  Phendates_pred <-data.frame(matrix(ncol=7, nrow=0, byrow=FALSE))
  colnames(Phendates_pred) <- Phendates_cols
  Phendates_pred<-Phendates_pred%>% mutate(Season = as.character(Season))
  for(i in 1:nrow(Reqs)) {
    Sel_reqs <- slice(Reqs, i)
    Dreq <- as.numeric(Sel_reqs[1])
    Freq <- as.numeric(Sel_reqs[2])
    for(sea in 1:(length(Seasons))){
      Anno = as.numeric(Seasons[sea])
      if (Dreq>=305){
        Data_fil <- GD_day %>% filter(
          (GD_day$Year == Anno-1 & GD_day$DOY>= Dreq) |
            (GD_day$Year == Anno))
      } else {
        Data_fil <- GD_day %>% filter(
          (GD_day$Year == Anno & GD_day$DOY>= Dreq))
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

