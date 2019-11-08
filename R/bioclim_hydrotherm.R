#' Calculation of hydrothermal viticultural indices (Branas, Dryness Index)
#'
#' This function calculates the hydrotermic index of Branas, Bernon and 
#' Levandoux (Branas et al 1946) and the Dryness index (Riou et al 1994).
#' The Branas Index (BBLI) takes  into  account  the influence  of  both  
#' temperature  and  precipitation  on  grape yield  and  wine  quality.  
#' This  index  is  the  sum of  the products of monthly mean temperature 
#' (Tmean,in Celsius) and monthly  accumulated  precipitation  amount  (Prec,in mm)
#' during the April to September season (Northern Hemisphere) or October to 
#' February (Southern Hemisphere).
#' 
#' The  dryness  index  (DI)  is  measured  based  on  an adaptation  of  
#' the  potential  water  balance  of  the  soil index of Riou (Riou et al., 
#' 1994), developed specially for vineyard use. It  enables  the  characterization
#' of the  water  component  of  the  climate  in  a  grape-growing  region,
#' taking into account the climatic demand of a standard vineyard, evaporation
#' from bare soil, rainfall without deduction for surface runoff or drainage. 
#' It indicates the  potential  water  availability  in  the  soil,  related  
#' to the level of dryness in a region (Tonietto and Carbonneau, 2004). The
#' index uses potential evapotranspiration using the Penman Monteith method.
#'
#' @param climdata a dataframe with daily weather data.
#'  Must contain the columns Year, Month, Day, Tmax, Tmin, RHmax, RHmin, Prec, 
#'  Rad, u2.
#' @param lat the latitude of the site, in decimal degrees. Use positive values
#' for Northern latitudes and negatives for Southern.
#' @param elev the elevation of the site, in meters above sea level.
#' @return data frame with the values of the indices for each season in the
#' climdata file.
#' @author Carlos Miranda
#' @references
#'
#' Riou C, Carbonneau A, Becker N, Caló A, Costacurta A, Castro R, Pinto PA, 
#' Carneiro LC, Lopes C, Climaco P, Panagiotou MM, Sotes V,Beaumond HC, Burril A, 
#' Maes J, Vossen P. 1994.Le determinisme climatique de la maturation du raisin: 
#' application au zonage de la teneur em sucre dans la communaute europenne. 
#' Office des Publications Officielles des Communautes Europennes: Luxembourg, 322pp.
#' 
#' Tonietto J, Carbonneau A. 2004. A multicriteria climatic classification system 
#' for grape-growing regions worldwide. Agricultural and Forest Meteorology, 124:81-97.
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
#' @export bioclim_hydrotherm
#' @import data.table tidyverse zoo lubridate
#' 
#' 
bioclim_hydrotherm <- function(climdata, lat, elev)
{
  if (abs(lat)>50){stop("latitude too high for grapevine")}
  if (lat > 0){
    start_m <- 4
    end_d <- 9
    end_b <-8} else {
      start_m <- 10
      end_d <- 3
      end_b <- 2
    }
  k_DI <- c(0.1,0.3,0.5,0.5,0.5,0.5)
  N_n <- c(30,31,30,31,31,30)
  N_s <- c(31,30,31,31,28,31)
  Wo_m <- c(200,0,0,0,0,0)
  climdata.ET <- ET_penman_monteith(climdata,lat,elev) %>% 
    mutate(Tmean=(Tmax+Tmin)/2)
  climdata.ETm <- climdata.ET %>%
    group_by(Year, Month) %>%
    summarise(ET_m = sum(ET_os), 
              Prec_m= sum(Prec), 
              Tmean_m = mean(Tmean)) %>%
    mutate(TxP = Prec_m * Tmean_m)
  
  seasons <- unique(climdata.ETm$Year)

  indices_cn <- c("Year","DI","BBLI")
  indices.df <-data.frame(matrix(ncol=3, nrow=0, byrow=FALSE))
  colnames(indices.df) <- indices_cn

  for (sea in 1:length(seasons)){
    Anno <- as.numeric(seasons[sea])
    if (start_m==10){
      climdata_fil <- climdata.ETm %>% filter(
        (Year == Anno-1 & Month >= start_m) |
          (Year == Anno & Month <=end_d)) %>% 
        mutate(N=N_s, 
               K=k_DI, 
               Wo=Wo_m)
    } else {
      climdata_fil <- climdata.ETm %>% filter(
        (Year == Anno & 
           Month>= start_m &
           Month<= end_d)) %>%
        mutate(N=N_n,
               K=k_DI, 
               Wo=Wo_m)
    }
    Branas.sea <- climdata_fil %>% 
      filter (if (end_d==3) 
        {climdata_fil$Month!=3} else
          {climdata_fil$Month<=8} )%>%
      summarise(BBLI=sum(TxP)) %>%
      select("BBLI") %>%
      unlist(use.names=FALSE) 
    
    DI.sea <- climdata_fil %>%
      mutate(T_v = ET_m*K, 
             JPm = Prec_m/5,
             a_K=1-K,
             E_s=ET_m*a_K*JPm/N,
             W_bal=Wo+Prec_m-T_v-E_s) %>%
      summarise(DI=sum(W_bal))%>%
      select("DI") %>%
      unlist(use.names=FALSE)
    
    new.row.df <- data.frame(Anno) %>%
        cbind(DI.sea,Branas.sea)
    
    indices.df <-rbind(indices.df,new.row.df)
    }
  indices.df <- indices.df %>% rename(Year=Anno, DI=DI.sea, Branas=Branas.sea)
  return(indices.df) 
  }
