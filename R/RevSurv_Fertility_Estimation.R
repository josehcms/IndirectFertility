##################################################
### Title: Fetch and prepare population age-sex
###        data for Reverse Survival
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-09-15
##################################################

### Set up packages and global options #----------

# Clean environment 
rm( list = ls( ) )
graphics.off( )

# List of packages for session
.packages <-  
  c( "rlang", "data.table", "RODBC", "lubridate", 
     "devtools", "tictoc", "dplyr", "RCurl" )

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

##################################################

### Read UNPD data from server #------------------

myLocations <- c(76,32)

# Loop through each location with `lapply`
# This is useful if you want to work with many locations because 
# the API can only handle a limited volume of queries at once.
myPop <- 
  lapply(myLocations, function(x) {
  
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
          list( LocName, LocID, RegName, RegID, AreaName, AreaID, ## country metadata
                DataCatalogName, DataSourceID, DataSourceAuthor,  ## data info
                DataSourceYear, 
                TimeStart, TimeMid, TimeLabel,                    ## reference period
                SeriesID,                                         ## data ID
                SexName, SexID, AgeStart,                         ## data values
                AgeEnd, AgeLabel, DataValue ) ] %>%
      setorder( LocID, SeriesID, SexID, AgeStart )
    
    # split totals and unkown in different columns
    resFilt[, DataValueUnknown := DataValue[ AgeLabel == 'Unknown' ],
            .( LocID, SeriesID, SexID ) ]
    resFilt[, DataValueTotal := DataValue[ AgeLabel == 'Total' ],
            .( LocID, SeriesID, SexID ) ]
    # if Total is missing
    resFilt[, DataValueTotal := ifelse( is.na( DataValueTotal ), 
                                        sum( DataValue ),
                                        DataValueTotal ),
            .( LocID, SeriesID, SexID ) ]
    # remove totals and unknown from database AgeLabel and Both Sexes counts
    resFilt <- resFilt[ ! ( AgeLabel %in% c( 'Unknown', 'Total' ) ) & SexID %in% c( 1, 2 ), ]
    
    # redistribute unknown proportionally among Age Groups
    resFilt[, DataValueNew := ifelse( is.na( DataValueUnknown ),
                                      DataValue,
                                      DataValue + 
                                        DataValueUnknown * ( DataValue / DataValueTotal ) ) ]
  
    
    # return the result
    return( resFilt )
})

# Merge all separate country data frames into
# one data frame.
Population1 <- data.table(do.call(rbind, myPop))

# check sex tabulation
Population1[,.(LocID,SeriesID,SexID)] %>% unique %>% .[,.N,.(LocID,SeriesID)]

# adjust date format
Population1[, DateFormtd := TimeStart %>% as.Date( '%d/%m/%Y' ) ]
# female filter
fem <- Population1[ SexID == 2 & AgeLabel %in% 10:69 ]
fem[,.(LocID,SeriesID,AgeLabel)] %>% unique %>% .[,.N,.(LocID,SeriesID)] # number of groups
fem[, AgeAbrgd := cut( AgeLabel %>% as.numeric, 
                       breaks = seq( 10, 70, 5 ),
                       labels = seq( 10, 65, 5 ),
                       right  = F,
                       include.lowest = TRUE ) %>% paste0 %>% as.numeric ]
femdat <- 
  fem[,.( pop_w = sum( DataValueNew ) ),
      .( SeriesID, LocID, DataCatalogName, DataSourceAuthor, DateFormtd, AgeAbrgd ) ]

# children
child <- Population1[ AgeLabel %in% 0:14 ]

filterSeries <- 
  child[,.(LocID,SeriesID,AgeLabel)] %>% 
  unique %>% 
  .[,.N,.(LocID,SeriesID)] %>%
  .[ N == 15,]$SeriesID

child <- child[ SeriesID == filterSeries ]

### Retrieve mortality information #-------------
ltb <- FetchLifeTableWpp2019( locations = 1501,
                              year = decimal_anydate('2008-03-03'),
                              sex = 'both' )

ltf <- FetchLifeTableWpp2019( locations = 1501,
                              year = decimal_anydate('2008-03-03'),
                              sex = 'female' )
#################################################

### Retrieve fertility information #-------------
asfr <- FetchFertilityWpp2019( locations = c(76,32),
                               year = decimal_anydate( '2008-03-03' ) )$asfr

asfr_15prior <- FetchFertilityWpp2019( locations = 'Cambodia',
                                       year = decimal_anydate( '1993-03-03' ) )$asfr


#################################################

child[SeriesID == '1491861688122']
Series <- unique(Population1$SeriesID)

## create/keep a lits of identifiers for teh SeriesID we can use to merge back to upload output datasets back into SQL database
SeriesID_Characteristics <- unique(Population1[, .(SeriesID, LocID, LocName, LocTypeName, LocAreaTypeName, SubGroupName,  SubGroupTypeName, SubGroupCombinationID, 
                                                   DataCatalogID, DataCatalogName, DataCatalogShortName, FieldWorkStart, FieldWorkMiddle, DataProcess, DataSourceName, DataSourceAuthor, DataSourceYear, DataSourceShortName, 
                                                   DataStatusName, StatisticalConceptName, DataTypeName, ModelPatternName, DataReliabilityName, PeriodTypeName, PeriodGroupName, TimeUnit, FootNoteID)])
setorder(SeriesID_Characteristics, LocName, FieldWorkMiddle, DataSourceAuthor, DataSourceYear, DataSourceShortName, DataStatusName, StatisticalConceptName)


## example of testing on 1 serie:
mySerie <- Series[3]


## Children Pop
pop_c <- Population1[SeriesID==mySerie & SexID==3 & AgeStart >= 0 & AgeEnd <= 14]
## WARNING: both sexes have not yet been computed for some series


## Female Pop
pop_w <- Population1[SeriesID==mySerie & SexID==2 & AgeStart >= 10 & AgeEnd <= 65]

## be careful before using the data for analytical purpose
## *** sort *** the records (e.g., by age) if substantively important
## NEVER assume that all the records are already sorted in the order you need

## WARNING: for each Series, you need to check that all the ages needed are available, i.e., no gaps or missing ages
## if some series have problem, log-in their IDS for further investigation with our DB manager, and skip those problematic ones.



##################################################