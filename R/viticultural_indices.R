#' Calculation of viticultural indices (Huglin, Winkler and Cool Nights)
#'
#' This function calculates the Heliothermal Index of Huglin (1978),
#' the Winkler index (Amerine and Winkler, 1974) and the Cool Night 
#' index (Tonietto, 1999).
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param lat the latitude of the site, in decimal degrees. Use positive values
#' for Northern latitudes and negatives for Southern.
#' @return data frame with the values of the indices.
#' @author Carlos Miranda
#' @references
#'
#' Amerine MA and Winkler AJ. 1944. Composition and quality of musts and wines
#' of California grapes. Hilgardia 15: 493-675.
#' 
#' Huglin P. 1978. Noveau mode d'evaluation des possibilites héliothermiques
#' d'un milieu viticole. In: Proceedings of the Symposium International sur
#' l'ecologie de la Vigne. Ministére de l'Agriculture et de l'Industrie 
#' Alimentaire, Contança pp 89-98.
#' 
#' Tonietto J. 1999. Les macroclimats viticoles mondiaux et l'influence du
#' mésoclimat sur la typicité de la Syrah et du Muscat de Hambourg dans le
#' sud de la France: methodologie de carácterisation. Thése Doctorat. Ecole 
#' Nationale Supérieure Agronomique, Montpellier, 233pp.
#'
#' @examples
#'
#' \dontrun{
#'
#' #select the appropiate columns from a larger dataset with date information
#' #in Year, Month, Day format, create a vector or harvest dates and
#' #estimate the Huglin and Cool night indices on each year
#' #in the series.
#'
#' Weather <- Tempdata %>%
#'    select(Year, Month, Day, Tmax, Tmin)
#' harvest <- c(225, 250, 275)
#' latitude <- 42.08
#' Vitic_indices <- viticultural_indices(Weather, harvest, latitude)
#'
#'}
#' @export viticultural_indices
#' @import data.table tidyverse zoo lubridate
#' 
viticultural_indices <- function(climdata, lat)
{
  if (abs(lat)>50){stop("latitude too high for grapevine")}
  if (lat > 0){
    start_m <- 4
    end_h <- 9
    end_w <- 10} else {
      start_m <- 10
      end_h <- 3
      end_w <- 4
    }
  ifelse(abs(lat)<=40, d<-1, 
         ifelse(abs(lat)<=42, d<-1.02,
                ifelse(abs(lat)<=44,d<-1.03,
                       ifelse(abs(lat)<=46,d<-1.04,
                              ifelse(abs(lat)<=48,d<-1.05,1.06)))))

  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date = make_date(Year, Month, Day),
           DOY = yday(Date),
           Tmean=(Tmax+Tmin)/2,
           H_day=0.5*d*((Tmean-10)+(Tmax-10)),
           W_day = ifelse(Tmean<10,0,Tmean-10))
  
  seasons <- unique(climdata$Year)

  indices_cn <- c("Year","Cool_n","Huglin","Winkler")
  indices.df <-data.frame(matrix(ncol=4, nrow=0, byrow=FALSE))
  colnames(indices.df) <- indices_cn

  for (sea in 1:length(seasons)){
    Anno <- as.numeric(seasons[sea])
    if (start_m==10){
      climdata_fil <- climdata %>% filter(
        (climdata$Year == Anno-1 & climdata$Month >= start_m) |
          (climdata$Year == Anno & climdata$Month <=end_w))
    } else {
      climdata_fil <- climdata %>% filter(
        (climdata$Year == Anno & 
           climdata$Month>= start_m &
           climdata$Month<= end_w))
    }
    Huglin <- climdata_fil %>% 
      filter (if (end_h==4) 
        {climdata_fil$Month!=4} else
          {climdata_fil$Month<=9} )%>%
      summarise(Huglin_I=sum(H_day)) %>%
      select("Huglin_I") %>%
      unlist(use.names=FALSE) 
    
    Winkler <- climdata_fil %>%
      summarise(Winkler_I=sum(W_day)) %>%
      select("Winkler_I") %>%
      unlist(use.names=FALSE)
    
    Coolnight <- climdata_fil %>%
      filter(climdata_fil$Month==end_h) %>% 
      summarise(Cool_n=mean(Tmin)) %>%
      select(Cool_n) %>%
      unlist(use.names=FALSE)
      
    new.row.df <- data.frame(Anno) %>%
        cbind(Coolnight,Huglin,Winkler)
    
    indices.df <-rbind(indices.df,new.row.df)
    }
  indices.df <- indices.df %>% rename(Year=Anno)
  return(indices.df) 
  }

