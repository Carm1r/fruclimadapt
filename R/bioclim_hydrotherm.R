#' Calculation of hydrothermal viticultural indices (Branas, Dryness Index)
#'
#' This function calculates the hydrotermic index of Branas, Bernon and 
#' Levandoux (BBLI, Branas et al 1946) and the Dryness index (Riou et al 1994).
#' 
#' The BBLI takes  into  account  the influence  of  both  
#' temperature  and  precipitation  on  grape yield  and  wine  quality.  
#' This  index  is  the  sum of  the products of monthly mean temperature 
#' (Tmean,in Celsius) and monthly  accumulated  precipitation  amount  (Prec,in mm)
#' during the April to September season (Northern Hemisphere) or October to 
#' February (Southern Hemisphere).
#' 
#' The  Dryness  index  (DI)  is  measured  based  on  an adaptation  of  
#' the  potential  water  balance  of  the  soil index of Riou (Riou et al., 
#' 1994), developed specially for vineyard use. It  enables  the  characterization
#' of the  water  component  of  the  climate  in  a  grape-growing  region,
#' taking into account the climatic demand of a standard vineyard, evaporation
#' from bare soil, rainfall without deduction for surface runoff or drainage. 
#' It indicates the  potential  water  availability  in  the  soil,  related  
#' to the level of dryness in a region (Tonietto and Carbonneau, 2004). The
#' index uses potential evapotranspiration calculated here with 
#' the Penman Monteith method.
#' 
#' Minimum data requirements to calculate the indices are daily temperatures 
#' (maximum and minimum temperatures, Tmax and Tmin) and rainfall (l m-2), 
#' whereas relative humidity (RHmax and RHmin, \%), solar radiation 
#' (Rad, MJ m-2 day-1) and mean wind speed at 2m height (u2med,m s-1) are optional. 
#' If missing, the function integrates FAO56 (Allen et al 1998) estimations 
#' for solar radiation and vapor pressure (air humidity) from daily temperatures. 
#' If there is no information available on wind speed, the function assumes a 
#' constant value of 2 m s-1.  
#'
#' @param climdata a dataframe with daily weather data, including temperature
#'  Required columns are Year, Month, Day, Tmax, Tmin and Prec. Optional columns 
#'  are RHmax, RHmin, Rad and u2med.
#' @param lat the latitude of the site, in decimal degrees. Use positive values
#' for Northern latitudes and negatives for Southern.
#' @param elev the elevation of the site, in meters above sea level.
#' @return dataframe with the values of the indices for each season in the
#' climdata file.
#' @author Carlos Miranda, \email{carlos.miranda@@unavarra.es}
#' @references
#' 
#' Allen RG, Pereira LS, Raes D, Smith M. 1998. Crop evapotranspiration. Guidelines
#' for computing crop water requirements. FAO Irrigation and drainage paper 56. Food 
#' and Agriculture Organization of the United Nations
#' 
#' Riou C, Carbonneau A, Becker N, Caló A, Costacurta A, Castro R, Pinto PA, 
#' Carneiro LC, Lopes C, Climaco P, Panagiotou MM, Sotes V,Beaumond HC, Burril A, 
#' Maes J, Vossen P. 1994. Le determinisme climatique de la maturation du raisin: 
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
#' #in Year, Month, Day format, define the values for the parameters latitude 
#' #and elevation and estimate the hydrotermal indices on each year in the series.
#'
#' Weather <- Tudela_DW %>%
#'    select(Year, Month, Day, Tmax, Tmin, Prec, HRmax, HRmin, Rad, u2med)
#' elevation <- 314
#' latitude <- 42.13132
#' Tudela_BHI <- bioclim_hydrotherm(Weather, latitude, elevation)
#'
#'}
#' @export bioclim_hydrotherm
#' @import data.table tidyverse zoo 
#' @importFrom lubridate make_date
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
