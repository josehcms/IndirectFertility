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

### Read UNPD saved data #------------------------
pop_data <- 
  fread('data/world_census_demodata_agex1_sex.csv') %>%
  .[, SeriesID := as.numeric( SeriesID ) ]
             
##################################################

### Check age and sex tabulations #---------------

# adjust date format
pop_data[, DateFormtd := TimeStart %>% as.Date( '%d/%m/%Y' ) ]

# filter females SexID == 2 and AgeLabel from 10 to 69
pop_females <- pop_data[ SexID == 2 & AgeLabel %in% 10:69 ]

# set up females 5-year age groups
pop_females[, AgeAbrgd := cut( AgeLabel %>% as.numeric, 
                               breaks = seq( 10, 70, 5 ),
                               labels = seq( 10, 65, 5 ),
                               right  = F,
                               include.lowest = TRUE ) %>% paste0 %>% as.numeric ]

# filter children (both sexes) and AgeLabel from 0 to 14
pop_child <- pop_data[ AgeLabel %in% 0:14 ]

# check sex tabulation - 2 sex IDs for each series are required
check_sex_pop <- 
  ( pop_data[, .( LocID, SeriesID, SexID ) ] %>% 
     unique %>% 
     .[,.N,.( LocID, SeriesID ) ] %>%
     .[ N != 2 ])$SeriesID # these two series will be excluded in age check

# check if children ages are complete, e.g., 0, 1, 2, 3, ..., 14 
# all included (15 ages) and if age groups are set in single ages
check_age_child <- 
  lapply( pop_child$SeriesID %>% unique, function( x ){
    
    aux_m <- 
      pop_child[ SeriesID == x & SexID == 1,
                 .( LocID, SeriesID, AgeLabel = as.numeric( AgeLabel ) ) ]
    
    aux_f <- 
      pop_child[ SeriesID == x & SexID == 2,
                 .( LocID, SeriesID, AgeLabel = as.numeric( AgeLabel ) ) ]
    
    aux <- 
      pop_child[SeriesID == x ,
                .( LocID, SeriesID, AgeLabel ) ] %>% 
      unique %>% 
      .[ , .N, .( LocID, SeriesID ) ]
  
    aux$Select <- 
      ( aux$N == 15 & 
          ( is_single( aux_m$AgeLabel ) & is_single( aux_f$AgeLabel ) ) )
  
    return( aux )
    } )

filterSeriesChild <- 
  do.call( rbind, check_age_child )

# check if women ages are complete, e.g., 10, 11, 12, ...,69 all included 
# (60 ages) and set in single age groups

check_age_females <- 
  lapply( pop_females$SeriesID %>% unique, function( x ){
    
    aux_f <- 
      pop_females[ SeriesID == x & SexID == 2,
                 .( LocID, SeriesID, AgeLabel = as.numeric( AgeLabel ) ) ]
    
    aux <- 
      pop_females[SeriesID == x ,
                  .( LocID, SeriesID, AgeLabel ) ] %>% 
      unique %>% 
      .[ , .N, .( LocID, SeriesID ) ]
    
    aux$Select <- 
      ( aux$N == 60 & is_single( aux_f$AgeLabel ) )
    
    return( aux )
    
  } )

filterSeriesFem <- 
  do.call( rbind, check_age_females )

# save selection 
revsurv_selection <- 
  pop_data[,
           .( SeriesID, LocID, LocName, DataCatalogID, 
              DataCatalogName, DataSourceName,
              TimeStart ) ] %>% unique %>%
  merge( filterSeriesChild[, .( LocID, SeriesID, 
                                ChildAgeCriteriaMet = Select ) ],
         by = c( 'LocID', 'SeriesID' ),
         all.x = TRUE ) %>%
  merge( filterSeriesFem[, .( LocID, SeriesID, 
                              FemAgeCriteriaMet = Select ) ],
         by = c( 'LocID', 'SeriesID' ),
         all.x = TRUE )

write.table( revsurv_selection,
             file = 'outputs/selected_series_revsurv_world.csv',
             row.names = FALSE )
##################################################

### Change age groups and process filters in IDs #-----

# filter children SeriesID with complete single age groups (15 categories)
# and select required variables for reverse survival estimation
pop_child <- 
  pop_child[ SeriesID %in% filterSeriesChild[ Select == TRUE ]$SeriesID,
             .( pop_c = sum( DataValueNew ) ),
             .( SeriesID, LocID, DateFormtd, 
                Age = as.numeric( AgeLabel ) ) ] %>%
  setorder( LocID, SeriesID, Age )

# filter women SeriesID with complete single age groups (60 categories)
# and select required variables for reverse survival estimation
pop_females <- 
  pop_females[ SeriesID %in% filterSeriesFem[ Select == TRUE ]$SeriesID,
               .( pop_w = sum( DataValueNew ) ),
               .( SeriesID, LocID, DateFormtd, Age = AgeAbrgd ) ] %>%
  setorder( LocID, SeriesID, Age )

# merge selected series IDs of females and children to assure overlapping
series <- 
  revsurv_selection[ ChildAgeCriteriaMet & FemAgeCriteriaMet ]$SeriesID

##################################################

### Apply reverse survival to Series #------------
RevSurvEstimates <- 
  lapply( series, function( x ){
  
  aux_c <- pop_child[ SeriesID == x ] 
  aux_f <- pop_females[ SeriesID == x ]
  
  if( !is_age_sequential( aux_c$Age ) | !is_age_sequential( aux_f$Age )  ){
    paste0( 'SeriesID number ', x, ' not in sequencial age format!!!')
  }
  
  date_ref <- unique( c( aux_f$DateFormtd, aux_c$DateFormtd )  )
  date_ref_dec <- decimal_anydate( date_ref ) # reference date in decimal
  loc <- aux_c$LocID %>% unique
  
  popx5_w = aux_f$pop_w
  ages5_w = aux_f$Age %>% unique
  popx1_c = aux_c$pop_c
  ages1_c = aux_c$Age %>% unique
  
  # 1) get child 0-5 mortality probability for reference period
  #    reference period - 5 and reference period - 10
  ltb_ref <- getWPP2019LT( locations = loc,
                           year = date_ref_dec,
                           sex = 'both' )
  ltb_ref5 <- getWPP2019LT( locations = loc,
                            year = ( date_ref_dec - 5 ),
                            sex = 'both' )
  ltb_ref10 <- getWPP2019LT( locations = loc,
                             year = ( date_ref_dec - 10 ),
                             sex = 'both' )
  
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
  
  # 2) get female q15_45 mortality probability for 
  # reference period(q1f), reference period - 5 (q2f), and reference period - 10 (q3f)
  
  ltf_ref <- getWPP2019LT( locations = loc,
                           year = date_ref_dec,
                           sex = 'female' )
  ltf_ref5 <- getWPP2019LT( locations = loc,
                            year = ( date_ref_dec - 5 ),
                            sex = 'female' )
  ltf_ref10 <- getWPP2019LT( locations = loc,
                             year = ( date_ref_dec - 10 ),
                             sex = 'female' )
  
  q1f  <-  
    ( ltf_ref$lx[ ltf_ref$x == 15 ] - ltf_ref$lx[ ltf_ref$x == 45 ] ) /
    ltf_ref$lx[ ltf_ref$x == 15 ]
  q2f  <-  
    ( ltf_ref5$lx[ ltf_ref5$x == 15 ] - ltf_ref5$lx[ ltf_ref5$x == 45 ] ) /
    ltf_ref5$lx[ ltf_ref5$x == 15 ]
  q3f  <-  
    ( ltf_ref10$lx[ ltf_ref10$x == 15 ] - ltf_ref10$lx[ ltf_ref10$x == 45 ] ) /
    ltf_ref10$lx[ ltf_ref10$x == 15 ]
  
  q15_45f <- c( q1f, q2f, q3f )
  
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
  cat( 'Country:', loc )
  outRevSurv <- 
    data.table(
      LocID = loc,
      SeriesID = x,
      DateFormtd = date_ref,
      FertRevSurv( ages1_c, popx1_c, ages5_w, popx5_w, lx1_c, lx5_w,
                   asfr, asfr_15prior,
                   q0_5, q15_45f, date_ref )
    )
    
  return( outRevSurv )
} )

outRevSurv <- do.call( rbind, RevSurvEstimates )

write.table( outRevSurv, 'outputs/reverse_survival_fertest_world_x1.csv', 
             row.names = F )
##################################################

require(data.table);require(dplyr)
aux1 <- fread('data/world_census_demodata_agex5_sex.csv') %>%
  .[,.(SeriesID, LocID, LocName, LocTypeName, LocAreaTypeName, ## series ID charactr
       SubGroupName,  SubGroupTypeName, SubGroupCombinationID, 
       DataCatalogID, DataCatalogName, DataCatalogShortName, 
       FieldWorkStart, FieldWorkMiddle, DataProcess, DataSourceName, 
       DataSourceAuthor, DataSourceYear, DataSourceShortName, 
       DataStatusName, StatisticalConceptName, DataTypeName, 
       ModelPatternName, DataReliabilityName, PeriodTypeName, 
       PeriodGroupName, TimeUnit, FootNoteID,
       RegName, RegID, AreaName, AreaID, 
       TimeStart, TimeMid, TimeLabel)] %>% unique

aux <- 
  fread('outputs/reverse_survival_fertest_world_x5.csv') %>%
  merge( aux1,
         by = c( 'LocID', 'SeriesID')) %>% 
  .[,.(SeriesID, LocID, LocName, LocTypeName, LocAreaTypeName, ## series ID charactr
       SubGroupName,  SubGroupTypeName, SubGroupCombinationID, 
       DataCatalogID, DataCatalogName, DataCatalogShortName, 
       FieldWorkStart, FieldWorkMiddle, DataProcess, DataSourceName, 
       DataSourceAuthor, DataSourceYear, DataSourceShortName, 
       DataStatusName, StatisticalConceptName, DataTypeName, 
       ModelPatternName, DataReliabilityName, PeriodTypeName, 
       PeriodGroupName, TimeUnit, FootNoteID,
       RegName, RegID, AreaName, AreaID, 
       TimeStart, TimeMid, TimeLabel, DateFormtd,year, TypeEst,
       TFR,births)]

write.table( aux,
             file = 'outputs/reverse_survival_fertest_world_x5.csv',
             row.names = F )
