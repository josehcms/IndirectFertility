##################################################
### Title: Fetch and prepare population age-sex
###        data from UNESCO school enrolment data
###        for children
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-11-12
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

# omit countries without data
myLocations <- 
  Locations[ !( country_code %in% c( 531, 158, 175, 732, 830  ) ) & 
               area_code != -1 ]$country_code

# Loop through each location with `lapply`
myPop <- 
  lapply( myLocations, function(x) {
    
    cat("\nCountry", x, ":\n")
    res <- 
      get_recorddata(
        #dataProcessTypeIds = 14, ## "Administrative records"
        dataProcessIds = 55,    ## "Education statistics (enrolment by level)"
        startYear = 1950,
        endYear = 2020,
        indicatorIds = 61,  ## "Population by age and sex - selected single ages"
        isComplete = 1,         ## 0=Abridged or 1=Complete
        locIds = x,             ## "Brazil"
        locAreaTypeIds = 2,     ## "Whole area"
        subGroupIds = 2 )
    
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


# datSeries with following columns by unique SeriesID:
## SeriesID, TimeLabel, SexID, AgeLabel, DataValueNew

set_dummy_children <- 
  function( datSeries ){
    
    base_dt <- 
      data.table(
        AgeLabel = 0:14
      )
    
    series  <- datSeries$SeriesID  %>% unique
    time    <- datSeries$TimeLabel %>% unique
    timemid <- datSeries$TimeMid %>% unique
    loc     <- datSeries$LocID %>% unique

    new_dt <- 
      merge(
        base_dt,
        datSeries,
        by = c( 'AgeLabel' ),
        all.x = T
      )

    new_dt$SeriesID  <- series
    new_dt$TimeLabel <- time
    new_dt$TimeMid   <- timemid
    new_dt$LocID     <- loc

    new_dt$Dummy <- is.na( new_dt$DataValueNew )
    new_dt$DataValueNew <- 
      ifelse( is.na( new_dt$DataValueNew ),
              1000,
              new_dt$DataValueNew )
    
    out_dt <- 
      new_dt[ ,.( SeriesID, LocID, TimeLabel, TimeMid,
                  AgeLabel, DataValueNew, Dummy ) ]
    
    return( out_dt )
  }

child_list_unesco <- 
  lapply( pop_data$SeriesID %>% unique,
          function( x ){
            
            temp_dt <- pop_data[ SeriesID == x,
                                 .( DataValueNew = sum( DataValueNew ) ),
                                 .( SeriesID,
                                    LocID,
                                    TimeLabel,
                                    TimeMid,
                                    AgeStart,
                                    AgeLabel ) ]
            
            out_dt <- set_dummy_children( temp_dt )
            
            return( out_dt )
          })

child_data_unesco <- 
  data.table( do.call( rbind, child_list_unesco ) ) %>%
  merge(
    pop_data[ ,.( LocID, SeriesID, LocName, DataCatalogID, DataCatalogName, 
                  DataCatalogShortName, DataSourceName, 
                  DataSourceAuthor, DataSourceYear, 
                  DataSourceShortName ) ] %>% unique,
    by = c( 'LocID', 'SeriesID' )
  )


write.table( child_data_unesco, 
             file = 'data/world_unesco_demodata_children_age.csv',
             row.names = FALSE )
