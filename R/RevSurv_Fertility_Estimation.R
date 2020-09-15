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
     "devtools", "tictoc", "dplyr", "RCurl",
     "ungroup" )

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

myLocations <- c( 76 )

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

# Merge all separate country data frames into one data frame
pop_data <- data.table( do.call( rbind, myPop ) )

# check sex tabulation
pop_data[, .( LocID, SeriesID, SexID ) ] %>% unique %>% .[,.N,.( LocID, SeriesID ) ]

# adjust date format
pop_data[, DateFormtd := TimeStart %>% as.Date( '%d/%m/%Y' ) ]

# female filter
pop_females <- pop_data[ SexID == 2 & AgeLabel %in% 10:69 ]

check_age_females <- 
  lapply( pop_females$SeriesID %>% unique, function( x ){
    aux <- 
      pop_females[SeriesID == x ,
                  .( LocID, SeriesID, AgeLabel ) ] %>% 
      unique %>% 
      .[ , .N, .( LocID, SeriesID ) ]
  
    aux$Select <- aux$N == 60 
    
    return( aux )
} )

filterSeriesFem <- 
  do.call( rbind, check_age_females )[ Select == TRUE ]

# number of groups
pop_females[, AgeAbrgd := cut( AgeLabel %>% as.numeric, 
                       breaks = seq( 10, 70, 5 ),
                       labels = seq( 10, 65, 5 ),
                       right  = F,
                       include.lowest = TRUE ) %>% paste0 %>% as.numeric ]

pop_females <- 
  pop_females[ SeriesID %in% filterSeriesFem$SeriesID,
               .( pop_w = sum( DataValueNew ) ),
               .( SeriesID, LocID, DataCatalogName, 
                  DataSourceAuthor, DateFormtd, Age = AgeAbrgd ) ]

# children
pop_child <- pop_data[ AgeLabel %in% 0:14 ]

check_age_child <- 
  lapply( pop_child$SeriesID %>% unique, function( x ){
    aux <- 
      pop_child[SeriesID == x ,
                .( LocID, SeriesID, AgeLabel ) ] %>% 
      unique %>% 
      .[ , .N, .( LocID, SeriesID ) ]
  
    aux$Select = aux$N == 15
  
    return( aux )
    } )

filterSeriesChild <- 
  do.call( rbind, check_age_child )[ Select == TRUE ]

pop_child <- 
  pop_child[ SeriesID %in% filterSeriesChild$SeriesID,
             .( pop_c = sum( DataValueNew ) ),
             .( SeriesID, LocID, DataCatalogName, 
                DataSourceAuthor, DateFormtd, Age = AgeLabel )]

series <- 
  ( filterSeriesChild %>% 
      merge( filterSeriesFem,
             by = c( 'LocID', 'SeriesID' ) ) )$SeriesID

a <- 
lapply( series, function( x ){
  
  aux_c <- pop_child[ SeriesID == x ]
  aux_f <- pop_females[ SeriesID == x ]
  
  date_ref <- unique( c( aux_f$DateFormtd, aux_c$DateFormtd )  )
  date_ref_dec <- decimal_anydate( date_ref )
  loc <- aux_c$LocID %>% unique
  
  popx5_w = aux_f$pop_w
  ages5_w = aux_f$Age %>% unique
  popx1_c = aux_c$pop_c
  ages1_c = aux_c$Age %>% unique
  
  # 1) get child 0-5 mortality probability for reference period
  #    reference period - 5 and reference period - 10
  if( date_ref_dec < 1955 ){
    ltb_ref <- FetchLifeTableWpp2019( locations = loc,
                                      year = 1955 ,
                                      sex = 'both' )
  } else{
    ltb_ref <- FetchLifeTableWpp2019( locations = loc,
                                      year = date_ref_dec,
                                      sex = 'both' )
  }
  
  if( ( date_ref_dec - 5 ) < 1955 ){
    ltb_ref5 = ltb_ref
  } else{
    ltb_ref5 <- FetchLifeTableWpp2019( locations = loc,
                                       year = ( date_ref_dec - 5 ),
                                       sex = 'both' )
  }
  
  if( ( date_ref_dec - 10 ) < 1955 ){
    ltb_ref10 = ltb_ref5
  } else{
    ltb_ref10 <- FetchLifeTableWpp2019( locations = loc,
                                        year = ( date_ref_dec - 10 ),
                                        sex = 'both' )
  }

  q1  <-  
    ( ltb_ref$lx[ ltb_ref$x == 0 ] - ltb_ref$lx[ ltb_ref$x == 5 ] ) /
    ltb_ref$lx[ ltb_ref$x == 0 ]
  q2  <-  
    ( ltb_ref5$lx[ ltb_ref5$x == 0 ] - ltb_ref5$lx[ ltb_ref5$x == 5 ] ) /
    ltb_ref5$lx[ ltb_ref5$x == 0 ]
  q3  <-  
    ( ltb_ref10$lx[ ltb_ref10$x == 0 ] - ltb_ref10$lx[ ltb_ref10$x == 5 ] ) /
    ltb_ref10$lx[ ltb_ref10$x == 0 ]
  
  q0_5 <- c( q1, q2, q3 )
  
  # 2) get female q15_45 mortality probability for reference period
  #    reference period - 5 and reference period - 10
  
  if( date_ref_dec < 1955 ){
    ltf_ref <- FetchLifeTableWpp2019( locations = loc,
                                      year = 1955,
                                      sex = 'female' )
  } else{
    ltf_ref <- FetchLifeTableWpp2019( locations = loc,
                                      year = date_ref_dec,
                                      sex = 'female' )
  }
  
  if( ( date_ref_dec - 5 ) < 1955 ){
    ltf_ref5 = ltf_ref
  } else{
    ltf_ref5 <- FetchLifeTableWpp2019( locations = loc,
                                       year = ( date_ref_dec - 5 ),
                                       sex = 'female' )
  }
  
  if( ( date_ref_dec - 10 ) < 1955 ){
    ltf_ref10 = ltf_ref5
  } else{
    ltf_ref10 <- FetchLifeTableWpp2019( locations = loc,
                                        year = ( date_ref_dec - 10 ),
                                        sex = 'female' )
  }
  
  q1  <-  
    ( ltf_ref$lx[ ltf_ref$x == 15 ] - ltf_ref$lx[ ltf_ref$x == 45 ] ) /
    ltf_ref$lx[ ltf_ref$x == 15 ]
  q2  <-  
    ( ltf_ref5$lx[ ltf_ref5$x == 15 ] - ltf_ref5$lx[ ltf_ref5$x == 45 ] ) /
    ltf_ref5$lx[ ltf_ref5$x == 15 ]
  q3  <-  
    ( ltf_ref10$lx[ ltf_ref10$x == 15 ] - ltf_ref10$lx[ ltf_ref10$x == 45 ] ) /
    ltf_ref10$lx[ ltf_ref10$x == 15 ]
  
  q15_45f <- c( q1, q2, q3 )
  
  # 3) get female lx5 values matching ages
  lx5_w <- ltf_ref$lx[ ltf_ref$x %in% ages5_w ]
  
  # 4) get children lx1 values matching ages using ungroup pclm function
  lts_model <- pclm( x = ltb_ref$x[ 2:22 ],
                     y = ltb_ref$dx[ 2:22 ],
                     nlast = 1,
                     offset = ltb_ref$Lx[ 2:22 ] )
  lts <-
    LifeTable( x = 0:99,
               mx = c( ltb_ref$mx[1], fitted( lts_model )[ 1:99 ] ),
               lx0 = 1,
               sex = 'total' )$lt
  
  lx1_c <- lts$lx[ lts$x %in% 0:15 ]
  
  # 5) get fertility profile for current year and previous 15 period
  
  asfr <- FetchFertilityWpp2019( locations = loc,
                                 year =  date_ref_dec )$asfr
  if( ( date_ref_dec - 15 ) < 1950 ){
    asfr_15prior <- asfr
  } else{
    asfr_15prior <- FetchFertilityWpp2019( locations = loc,
                                           year = ( date_ref_dec - 15 ) )$asfr
  }
  
  
  # 6) run function
  out <- 
    data.table(
      LocID = loc,
      SeriesID = x,
      DateFormtd = date_ref,
      FertRevSurv( ages1_c, popx1_c, ages5_w, popx5_w, lx1_c, lx5_w,
                   asfr5 = asfr, asfr5_15prior = asfr_15prior,
                   q0_5, q15_45f, date_ref )
    )
    
  return( out )
} )
### Retrieve mortality information #-------------


ltf <- FetchLifeTableWpp2019( locations = c( 32, 76 ),
                              year = decimal_anydate('2008-03-03'),
                              sex = 'female' )
#################################################

### Retrieve fertility information #-------------



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