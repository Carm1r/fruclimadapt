\name{Dates_BT}
\alias{Dates_BT}
\docType{data}
\title{
Example phenological dates for Big Top nectarine in Tudela
}
\description{
Estimated dates (as day of the year, DOY) for bloom, early fruit growth stages 
and harvest for Big Top Nectarine in Tudela (2001-2010). Dates have been 
estimated using the functions included in this package.
}
\usage{data("Dates_BT")}
\format{
  A data frame with 10 observations on the following 6 variables.
  \describe{
    \item{\code{Year}}{a numeric vector, the year of the observation}
    \item{\code{sbloom}}{a numeric vector, beggining of bloom as DOY}
    \item{\code{ebloom}}{a numeric vector, end of bloom as DOY}
    \item{\code{Start_ing}}{a numeric vector, beggining of early growth stage as DOY}
    \item{\code{End_ing}}{a numeric vector, end of early growth stage as DOY}
    \item{\code{Harvest}}{a numeric vector, harvest date as DOY}
  }
}
\examples{
\dontrun{
data(Bloom_BT)
Pol_weather_BT <- pollination_weather(Tudela_DW,Bloom_BT,42.13132)
}
}

