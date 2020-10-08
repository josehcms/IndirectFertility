##################################################
### Title: Reverse Survival Fertility Estimation
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-09-17
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

# aux_functions
source( 'R/aux_functions.R' )
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

# ommit Curacao - no data in server
myLocations <- 
  Locations[ !( country_code %in% c( 531, 180, 232, 422, 706, 830 ) ) & 
               area_code != -1 ]$country_code

# Loop through each location with `lapply`
myPop <- 
  lapply( myLocations, function(x) {
    
    cat("\nCountry", x, ":\n")
    res <- get_recorddata( dataProcessTypeIds = 2,  ## "Census"
                           startYear = 1950,
                           endYear = 2020,
                           indicatorTypeIds = 8,    ## "Population by age and sex"
                           isComplete = 0,          ## 0 = Abridged or 1 = Complete
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
      resFilt[ ! ( AgeLabel %in% c( 'Unknown', 'Total' ) ) & 
                 SexID %in% c( 1, 2 ) &
                 ( ( AgeStart == 0 & AgeEnd == 5 ) |
                     ( AgeStart == 5 & AgeEnd == 10 ) |
                     ( AgeStart == 10 & AgeEnd == 15 ) |
                     ( AgeStart == 0 & AgeEnd == 1 ) |
                     ( AgeStart == 1 & AgeEnd == 5 ) |
                     ( AgeStart >= 15 ) ), ] %>%
      setorder( SeriesID, SexID, AgeStart ) 
    
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

# write.table( pop_data, 
#              file = 'data/latin_america_census_demodata_agex5_sex.csv',
#              row.names = FALSE )


write.table( pop_data, 
             file = 'data/world_census_demodata_agex5_sex.csv',
             row.names = FALSE )