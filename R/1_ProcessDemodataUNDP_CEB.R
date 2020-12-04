##################################################
### Title: Fetch and prepare data for children 
###        ever born computation
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-11-30
##################################################

### Set up packages and global options #----------

# Clean environment 
rm( list = ls( ) )
graphics.off( )

# List of packages for session
.packages <-  
  c( "rlang", "data.table", "RODBC", "lubridate", 
     "devtools", "tictoc", "dplyr", "RCurl",
     "ungroup", "wpp2019", "ggplot2" )

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if( length( .packages[ !.inst ] ) > 0 ) 
  install.packages( .packages[ !.inst ], dependencies = TRUE )

# Load packages into session 
lapply( .packages, require, character.only = TRUE )

# Devtools packages - install if necessary
# devtools::install_github( "timriffe/DemoTools", force = TRUE)
# devtools::install_github( "cimentadaj/DDSQLtools", force = TRUE)
# devtools::install_github( "josehcms/fertestr", force = TRUE )
require( DemoTools ); require( DDSQLtools ); require( fertestr )

# Production server for UNPD (in Valencia)
options( unpd_server = "https://popdiv.dfs.un.org/DemoData/api/" )
options( scipen = 99999 )
##################################################

### Read UNPD data from server #------------------

# get UN locations and pop for filtering LA region and pop > 90k
data('UNlocations')
data('pop')

# select Latin America Locations with 90k+ population in 2020
Locations <- 
  UNlocations %>% setDT %>%
  .[ , 
     .( name, country_code, area_name, area_code ) ] %>%
  merge( 
    pop %>% setDT %>%
      .[,.( country_code, pop = `2020` ) ],
    by = 'country_code'
  ) %>%
  .[ pop >= 90, ]

# omit countries that are not found in server
myLocations <- 
  Locations[ area_code != -1 & 
               !( country_code %in% c( 28, 36, 44, 48, 52, 56,
                                       70, 90, 96, 100, 132,
                                       158, 175, 191, 196, 203,
                                       208, 233, 246, 250, 254,
                                       258, 276, 308, 312, 316,
                                       344, 352, 380, 392, 408,
                                       414, 428, 440, 442, 446,
                                       470, 474, 480, 512, 528,
                                       531, 533, 540, 554, 578,
                                       583, 616, 620, 634, 638,
                                       670, 678, 682, 690, 702,
                                       703, 732, 752, 776, 784,
                                       826, 830, 850, 882 ) ) ]$country_code 

# Loop through each location with `lapply`
myPop <- 
  lapply( myLocations, function(x) {
    
    cat("\nCountry", x, ":\n")
    res <- get_recorddata(dataProcessTypeIds = c( 2, 11, 12, 8 ), ## "Census"
                          startYear = 1950,
                          endYear = 2020,
                          indicatorIds = 319,     ## "Mean children ever born by age of mother (and by sex of child): 319=Abridged, 369=Complete"
                          # isComplete = 1,       ## 0=Abridged or 1=Complete
                          locIds = x,             ## "Brazil"
                          locAreaTypeIds = 2,     ## "Whole area"
                          subGroupIds = 2,        ## "Total or All groups"
                          includeUncertainty = FALSE )
    setDT( res )
    
    # select specific variables and sort
    resFilt <- 
      res[ SexID == 3,     # keep only values for both sexes of children
          list( SeriesID, LocID, LocName, LocTypeName, LocAreaTypeName, ## series ID charactr
                SubGroupName,  SubGroupTypeName, SubGroupCombinationID, 
                DataCatalogID, DataCatalogName, DataCatalogShortName, 
                FieldWorkStart, FieldWorkMiddle, DataProcess, DataSourceName, 
                DataSourceAuthor, DataSourceYear, DataSourceShortName, 
                DataStatusName, StatisticalConceptName, DataTypeName, 
                ModelPatternName, DataReliabilityName, PeriodTypeName, 
                PeriodGroupName, TimeUnit, FootNoteID,
                RegName, RegID, AreaName, AreaID, 
                TimeStart, TimeEnd, TimeMid, TimeLabel,                    
                SexName, SexID, AgeStart,                         ## data values
                AgeEnd, AgeLabel, DataValue ) ] %>%
      setorder( LocID, SeriesID, SexID, AgeStart )
    

    # return the result
    return( resFilt )
  })

# Merge all separate country data frames into one data frame
pop_data <- data.table( do.call( rbind, myPop ) )

write.table( pop_data, 
             file = 'data/world_ceb_demodata.csv',
             row.names = FALSE )

##################################################

### Read Fertility data from Demodata #-----------

# Loop through each location with `lapply`
myFert <- 
  lapply( myLocations, function(x) {
    
    cat("\nCountry", x, ":\n")
    res <- get_recorddata(dataProcessTypeIds = 6, ## Estimates
                          startYear = 1920,
                          endYear = 2020,
                          indicatorTypeIds = 15,  ## TFR
                          isComplete = 0, 
                          locIds = x,
                          locAreaTypeIds = 2, ## Whole area
                          subGroupIds = 2 )  ## "Total or All groups"
    setDT( res )
    
    # select specific variables and sort
    resFilt <- 
      res[, 
          list( SeriesID, LocID, LocName, LocTypeName, LocAreaTypeName, ## series ID charactr
                SubGroupName,  SubGroupTypeName, SubGroupCombinationID, 
                DataCatalogID, DataCatalogName, DataCatalogShortName, 
                FieldWorkStart, FieldWorkMiddle, DataProcess, DataSourceName, 
                DataSourceAuthor, DataSourceYear, DataSourceShortName, 
                DataStatusName, StatisticalConceptName, DataTypeName, 
                ModelPatternName, DataReliabilityName, PeriodTypeName, 
                PeriodGroupName, TimeUnit, FootNoteID,
                RegName, RegID, AreaName, AreaID, 
                TimeStart, TimeMid, TimeLabel,                    
                DataValue ) ] %>%
      setorder( LocID, DataSourceShortName, TimeMid )
    
    # return the result
    return( resFilt )
  })

# Merge all separate country data frames into one data frame
fert_data <- data.table( do.call( rbind, myFert ) )

write.table( fert_data, 
             file = 'data/latin_america_estimates_demodata_tfr.csv',
             row.names = FALSE )

##################################################