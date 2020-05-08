\name{Tcrits_peach}
\alias{Tcrits_peach}
\docType{data}
\title{
Critical frost temperatures for peach flower buds
}
\description{
Critical frost damage temperatures for peach flower buds for the phenological
stages between 'bud swelling' (B, 51 in Baggliolini and BBCH scales, 
respectively) and 'ovary surrounded by dying sepal crown' (I, 72). For use in 
combination with the example datasets Tudela_DW and Bigtop_reqs.
}
\usage{data("Tcrits_peach")}
\format{
  A data frame with 7 observations on the following 2 variables.
  \describe{
    \item{\code{LT_10}}{a numeric vector, frost temperature causing 10\% kill}
    \item{\code{LT_90}}{a numeric vector, frost temperature causing 90\% kill}
  }
}
\details{
The 10\% kill and 90\% kill imply that 30 minutes at the indicated temperature
is expected to cause 10\% and 90\% kill of the flower buds during the phenological
stage. The dataset contains the critical temperatures for the same stages in the
example dataset Bigtop_reqs. 
}
\source{
Miranda C, Santesteban LG, Royo JB. 2005. Variability in the relationship between
frost temperature and injury level for some cultivated Prunus species. HortScience
40:357-361.
}
\examples{
\dontrun{
data(Tcrits_peach)
Frost_peach <- spring_frost(Tudela_tmin, Dates_BT, Tcrits_peach)
}
}

