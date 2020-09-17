##################################################
### Title: Fetch and prepare population age-sex
###        data for Reverse Survival
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-09-16
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
  .[ area_code == 904, 
     .( name, country_code, area_name, area_code ) ] %>%
  merge( 
    pop %>% setDT %>%
      .[,.( country_code, pop = `2020` ) ],
    by = 'country_code'
  ) %>%
  .[ pop >= 90, ]

# ommit Curacao - no data in server
myLocations <- Locations[ country_code != 531 ]$country_code

# Loop through each location with `lapply`
myPop <- 
  lapply( myLocations, function(x) {
    
    cat("\nCountry", x, ":\n")
    res <- get_recorddata( dataProcessTypeIds = 2,  ## "Census"
                           startYear = 1950,
                           endYear = 2020,
                           indicatorTypeIds = 8,    ## "Population by age and sex"
                           isComplete = 1,          ## 0 = Abridged or 1 = Complete
                           locIds = x,              ## "Brazil"
                           locAreaTypeIds = 2,      ## "Whole area"
                           subGroupIds = 2 )        ## "Total or All groups"
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
                SexName, SexID, AgeStart,                         ## data values
                AgeEnd, AgeLabel, DataValue ) ] %>%
      setorder( LocID, SeriesID, SexID, AgeStart )
    
    # split totals and unkown in different columns
    resFilt[, DataValueUnknown := DataValue[ AgeLabel == 'Unknown' ],
            .( LocID, SeriesID, SexID ) ]
    resFilt[, DataValueTotal := DataValue[ AgeLabel == 'Total' ],
            .( LocID, SeriesID, SexID ) ]
    # if Total is missing assign DataValue sum
    resFilt[, DataValueTotal := ifelse( is.na( DataValueTotal ), 
                                        sum( DataValue ),
                                        DataValueTotal ),
            .( LocID, SeriesID, SexID ) ]
    # remove totals and unknown labels from database AgeLabel and Both Sexes counts
    resFilt <- 
      resFilt[ ! ( AgeLabel %in% c( 'Unknown', 'Total' ) ) & SexID %in% c( 1, 2 ), ]
    
    # redistribute unknown proportionally among Age Groups
    resFilt[, DataValueNew := ifelse( is.na( DataValueUnknown ),
                                      DataValue,
                                      DataValue + 
                                        DataValueUnknown * ( DataValue / DataValueTotal ) ) ]
    
    
    # return the result
    return( resFilt )
  })

# Merge all separate country data frames into one data frame
pop_data <- data.table( do.call( rbind, myPop ) )

write.table( pop_data, 
             file = 'data/latin_america_census_demodata_agex1_sex.csv',
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