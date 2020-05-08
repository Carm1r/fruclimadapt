\name{Bigtop_reqs}
\alias{Bigtop_reqs}
\docType{data}
\title{
Chill and heat requirements for Big Top nectarine
}
\description{
Chill and forcing heat requirements for the phenological stages between
'bud swelling' (B, 51 in Baggliolini and BBCH scales, respectively) and 'ovary 
surrounded by dying sepal crown' (I, 72) in Big Top nectarine. For use in 
combination with the example dataset Tudela_DW.
}
\usage{data("Bigtop_reqs")}
\format{
  A data frame with 7 observations on the following 2 variables.
  \describe{
    \item{\code{Creq}}{a numeric vector, chill portions}
    \item{\code{Freq}}{a numeric vector, forcing heat as growing degree hours}
  }
}
\details{
Chill requirements are in chill portions, starting chill accummulation on 1 november
(day of year 305), forcing heat is as growing degree hours calculated with linear 
method with a base temperature of 4.7 C and no optimal and upper critical thresholds.
}
\source{
Miranda C, Santesteban LG and Royo JB. 2013. Evaluation and fitting of models for
determining peach phenological stages at a regional scale. Agric Forest Meteorol 
178-179: 129-139.
}
\examples{
\dontrun{
data(Bigtop_reqs)

# Generate hourly temperatures
Tudela_HT <- hourly_temps(Tudela_DW,42.13132)
# Calculate chill as chill portions, starting on DOY 305
Chill <- chill_portions(Tudela_HT,305)
# Calculate forcing heat as growing degree hours (GDH) with linear model,
# using base temperature 4.7 C
GDH <- GDH_linear(Tudela_HT,4.7)
# Combine Chill and GDH values in a dataframe with a format compatible with
# the function phenology_sequential
Tudela_CH <- merge(Chill,GDH) \%>\%
  select(Date, Year, Month, Day, DOY, Chill,GDH) \%>\%
  arrange(Date) \%>\%
  rename(GD=GDH)
Phenology_BT <- phenology_sequential(Tudela_CH, Bigtop_reqs, 305)
}
}

