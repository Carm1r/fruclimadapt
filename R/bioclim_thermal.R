#' Calculation of bioclimatic viticultural indices focusing on temperature  
#'
#' This function calculates the Growing Season Average Temperature (GST), the
#' Heliothermal Index (HI) of Huglin, the Winkler (WI) index, the Biologically
#' Effective Degree Day (BEDD) index and the Cool Night (CI) index.
#' 
#' GST index correlates broadly to the maturity potential for grape cultivars 
#' grown across many wine regions and provides the basis for zoning viticultural
#' areas in both hemispheres (Hall and Jones, 2009). It is calculated by taking
#' the average of the growing season (April-October in Northern hemisphere, October
#' -April in Southern hemisphere).
#' 
#' HI (Huglin, 1978) is a bioclimatic heat index for viticulture regions using 
#' heliothermic potential, which calculates the temperature sum above 10ºC from 
#' April until September (Northern hemisphere) or from October until March (Southern
#'  hem.). The index takes into consideration daily maximum and average temperature, 
#' and slightly modifies the calculated total using the latitude of the location.
#' 
#' WI index (Amerine and Winkler, 1944), also known as growing degree days (GDD) 
#' classifies regions based on the accumulation of heat summation units by adding up 
#' hours above 10ºC during the growing season.
#' 
#' BEDD index (Gladstones, 1992) is another variant on calculating heat summation 
#' which incorporates upper and lower temperature thresholds (accounts for heat 
#' accumulation between 10 and 19ºC) and a day length correction similar to HI.
#' 
#' CI index (Tonietto, 1999) takes into account the minimum temperature during 
#' grape maturation, which is normally the average minimum air temperature in 
#' September/March (Northern or Southern hemispheres, respectively). 
#' 
#'
#' @param climdata a dataframe with daily maximum and minimum temperatures.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin.
#' @param lat the latitude of the site, in decimal degrees. Use positive values
#' for Northern latitudes and negatives for Southern.
#' @return data frame with the values of the indices. It contains the columns
#' Year, CI, GST, BEDD, HI, WI
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#'
#' Amerine MA and Winkler AJ. 1944. Composition and quality of musts and wines
#' of California grapes. Hilgardia 15: 493-675.
#' 
#' Gladstones J. 1992. Viticulture and environment. Winetitles, Adelaide, Australia 
#' 
#' Hall A., Jones GV. 2009. Effect of potential atmospheric warming on 
#' temperature-based indices describing Australian winegrape growing conditions.
#' Aust J Grape Wine Res 15. 97-119.
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
#' # Select the appropiate columns from a larger dataset with date information
#' # in Year, Month, Day format, and estimate indices on each year in the series.
#' library(tidyverse)
#' Weather <- Tudela_DW %>%
#'    select(Year, Month, Day, Tmax, Tmin)
#' latitude <- 42.13132
#' Tudela_BTI <- bioclim_thermal(Weather, latitude)
#'
#' @export bioclim_thermal
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date
#' 

bioclim_thermal <- function(climdata, lat)
{
  if (lat > 0){
    start_m <- 4
    end_h <- 9
    end_w <- 10} else {
      start_m <- 10
      end_h <- 3
      end_w <- 4
    }
  rlat <- abs(lat)*pi/180
  sdl <- 0
  for (i in 101:283) {
    m_lat <- 1-tan(rlat)*tan(0.409*cos(pi*i/182.625))
    Day_L <- acos(1-m_lat)*24/pi
    sdl<-sdl+Day_L
  }
  k_HI<-2.8311e-4*sdl + 0.30834
  k_BEDD<-1.1135*k_HI - 0.13520
  
  
  climdata <- select(climdata,"Year","Month","Day","Tmax","Tmin") %>%
    mutate(Date = make_date(Year, Month, Day),
           DOY = yday(Date),
           Tmean = (Tmax+Tmin)/2,
           DTR = Tmax-Tmin,
           H_day = ifelse(0.5*k_HI*((Tmean-10)+(Tmax-10))<0,0,
                          0.5*k_HI*((Tmean-10)+(Tmax-10))),
           W_day = ifelse(Tmean<10,0,Tmean-10),
           GDD_day = ifelse(Tmean-10<0,0,Tmean-10),
           DTR_adj = ifelse(DTR>13,0.25*(DTR-13), 
                            ifelse(DTR<10,0.25*(DTR-10),0)),
           BEDD_day=ifelse(GDD_day*k_BEDD+DTR_adj<=0,0,
                           ifelse(GDD_day*k_BEDD+DTR_adj<=9,GDD_day*k_BEDD+DTR_adj,9)))
  
  seasons <- unique(climdata$Year)

  indices_cn <- c("Year","CI","GST","BEDD","HI","WI")
  indices.df <-data.frame(matrix(ncol=6, nrow=0, byrow=FALSE))
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
    HI <- climdata_fil %>% 
      filter (if (end_h==4) 
        {climdata_fil$Month!=4} else
          {climdata_fil$Month<=9} )%>%
      summarise(Huglin_I=sum(H_day)) %>%
      select("Huglin_I") %>%
      unlist(use.names=FALSE) 
    
    WI <- climdata_fil %>%
      summarise(Winkler_I=sum(W_day)) %>%
      select("Winkler_I") %>%
      unlist(use.names=FALSE)
    
    GST <- climdata_fil %>%
      summarise(GST_I=mean(Tmean)) %>%
      select("GST_I") %>%
      unlist(use.names=FALSE)
    
    CI <- climdata_fil %>%
      filter(climdata_fil$Month==end_h) %>% 
      summarise(Cool_n=mean(Tmin)) %>%
      select(Cool_n) %>%
      unlist(use.names=FALSE)
    
    BEDD <- climdata_fil %>% 
      summarise(BEDD_I=sum(BEDD_day)) %>%
      select("BEDD_I") %>%
      unlist(use.names=FALSE) 
      
    new.row.df <- data.frame(Anno) %>%
        cbind(CI,GST,BEDD,HI,WI)
    
    indices.df <-rbind(indices.df,new.row.df)
    }
  indices.df <- indices.df %>% rename(Year=Anno)
  return(indices.df) 
  }

