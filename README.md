<!-- README.md is generated from README.Rmd. Please edit that file -->

## [fruclimadapt](https://github.com/Carm1r/fruclimadapt): Evaluation tools for assessing climate adaptation of fruit tree species in [R](https://www.r-project.org).

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![Travis (.com) branch](https://img.shields.io/travis/com/Carm1r/fruclimadapt/master) <img alt="CRAN/METACRAN" src="https://img.shields.io/cran/v/fruclimadapt">
[![Code Coverage](https://img.shields.io/codecov/c/github/Carm1r/fruclimadapt/master)](https://codecov.io/github/Carm1r/fruclimadapt/master)

This package is a compilation of functions for the assessment of climate
adaptation and the identification of potential risks for grapevines and
fruit trees. Procedures in the package allow to:

  - Downscale daily meteorological variables to hourly values
  - Estimate chilling and forcing heat accumulation
  - Estimate plant phenology
  - Calculate bioclimatic indices to evaluate fruit tree and grapevine
    adaptation
  - Estimate the indicence of weather-related disorders in fruits
  - Estimate plant water requirements.

<div id="menu" />

-----

## Resources

  - [Installation](#Instal)
  - [1. Required packages](#P1)
  - [2. Example: Estimate the phenology of a peach cultivar](#P2)
  - [3. Example: Estimate the number and damage caused by spring
    frosts](#P3)

<div id="Instal" />

-----

## Installation

You can install the released version of fruclimadapt from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fruclimadapt")
```

And the development version from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("Carm1r/fruclimadapt")
```

[Menu](#menu)

<div id="P1" />

-----

## Using fruclimadapt

### 1\. Required packages

>   - **[data.table](https://CRAN.R-project.org/package=data.table)**
>   - **[lubridate](https://CRAN.R-project.org/package=lubridate)**
>   - **[tidyverse](https://CRAN.R-project.org/package=tidyverse)**
>   - **[zoo](https://CRAN.R-project.org/package=zoo)**

``` r
install.packages("data.table")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("zoo")

library(fruclimadapt)
library(data.table)
library(tidyverse)
library(zoo)
```

[Menu](#menu)

<div id="P2" />

-----

### 2\. Example. Estimate the phenology of a peach cultivar

This example shows how to use the functions *hourly\_temps*,
*chill\_portions*, *GDH\_linear* and *phenology\_sequential* to estimate
the date of occurrence of the phenological stages for a nectarine
cultivar, using daily weather data.

``` r
library(fruclimadapt)
# Generate a dataset with hourly temperatures from the dataset with daily values (Tudela_DW, included in the package)
data(Tudela_DW)
Tudela_HT <- hourly_temps(Tudela_DW,42.13132)
# Use the hourly dataset to calculate chill as chill portions and growing degree hours
# Calculate chill as chill portions, starting on DOY 305
Chill <- chill_portions(Tudela_HT,305)
# Calculate forcing heat as growing degree hours (GDH) with the linear model using base temperature 4.7 C and no upper thresholds
GDH <- GDH_linear(Tudela_HT,4.7,999,999)
# Combine the datasets Chill and GDH in a dataframe with a format compatible with the function phenology_sequential
Tudela_CH <- merge(Chill,GDH) %>%
   select(Date, Year, Month, Day, DOY, Chill,GDH) %>%
   arrange(Date) %>%
   rename(GD=GDH)
# Obtain the predicted dates for the cultivar "Big Top" using the requirement dataset included in the package (Bigtop_reqs)
data(Bigtop_reqs)
Phenology_BT <- phenology_sequential(Tudela_CH, Bigtop_reqs, 305)
```

[Menu](#menu)

<div id="P3" />

-----

### 3\. Estimate the number and damage caused by spring frosts

This example shows how to use the function *spring\_frost* to estimate
the number and accumulated damage caused by spring frosts from
budbreaking for the same nectarine cultivar used to estimate the
phenology in the previous example.

``` r
library(fruclimadapt)

# Use the dataframe with the phenological dates obtained with phenology_sequential to generate a new one with the format required by the function spring_frost
Phenology_frost <- Phenology_BT %>% 
    select(Freq_Year,Freq_DOY) %>%
    rename(Year=Freq_Year,Pheno_date=Freq_DOY)
# Extract a dataframe with daily minimum temperatures from the daily climate example dataset with the  format required by spring_frost
 Tmin_Tudela <- Tudela_DW %>% 
   mutate(Date=make_date(Year,Month,Day), DOY=yday(Date)) %>%
   select(Year, DOY, Tmin) 
# Predict the number and accumulated damage of the spring frosts using the critical values contained in the example dataset Tcrits_peach and extract the dataframe with the total results for each year
 data(Tcrits_peach)
 Frost_BT <- spring_frost(Tmin_Tudela, Phenology_frost, Tcrits_peach, 181)
 Frost_results <- as.data.frame(Frost_BT[['Damage_frosts']])
```

[Menu](#menu)

## Licenses

The R/fruclimadapt package as a whole is distributed under [GPL-3 (GNU
General Public License
version 3)](https://www.gnu.org/licenses/gpl-3.0).

## Author

[Carlos Miranda](https://github.com/Carm1r)
