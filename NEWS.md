# fruclimadapt 0.4.6
## Bug fixes 
* bioclim_hydrotherm now manages properly data from Southern Hemisphere

# fruclimadapt 0.4.5
## Bug fixes 
* Fixed a conflicting argument in right_join() used in pollination_weather().
* fruclimadapt now imports only the data.table and zoo functions required to work, instead of the entire package

# fruclimadapt 0.4.4
## New features
* New function (DTR) to calculate the mean diurnal temperature range
* color_potential now allows to define the evaluation period
* russet now allows to define the critical relative humidity and the evaluation period
* coolness_index now allows to define the evaluation period

# fruclimadapt 0.4.3
## Bug fixes 
* Changed a reference in DESCRIPTION with a wrong doi for other referencing the same method.

# fruclimadapt 0.4.2
## Bug fixes 
* Included a closing angle bracket missing in a doi reference in DESCRIPTION
* Inner angle brackets have been URLencoded in doi reference in DESCRIPTION
* Modified the examples in functions phenology_sequential() and spring_frost to be  executed in <5s.

# fruclimadapt 0.4.1
## Bug fixes 
* Used undirected quotation marks in the description text
* Added references describing the methods in the description field of DESCRIPTION
* Put notes in examples in comments
* Unwrapped the executable examples 
* Replaced cat() by message() to write information messages to the console
* Include a call of on.exit() in GDH_asymcur() to reset settings when the function is exited
