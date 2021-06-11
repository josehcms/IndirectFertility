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
source( 'R/reverse_survival_wpp_function.R' )
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

# write.table( revsurv_selection,
#              file = 'outputs/selected_series_revsurv_world.csv',
#              row.names = FALSE )
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
    
    pop_w  = aux_f$pop_w
    ages_w = aux_f$Age %>% unique
    pop_c  = aux_c$pop_c
    ages_c = aux_c$Age %>% unique
    
    # 1) get child 0-5 mortality probability for reference period
    #    reference period - 5 and reference period - 10
    ltb1 <- getLifeTableWPP( locations = loc, year = ( date_ref_dec - 2.5 ), sex = 'Total' )
    ltb2 <- getLifeTableWPP( locations = loc, year = ( date_ref_dec - 7.5 ), sex = 'Total' )
    ltb3 <- getLifeTableWPP( locations = loc, year = ( date_ref_dec - 12.5 ), sex = 'Total' )
    
    nLx0_4_c <- 
      LifeTable( x = 0:99,
                 mx = c( ltb1$mx[1],
                         fitted( ungroup::pclm( x = ltb1$AgeStart[ 2:22 ],
                                                y = ltb1$dx[ 2:22 ],
                                                nlast = 1,
                                                offset = ltb1$Lx[ 2:22 ] ) )[ 1:99 ] ),
                 lx0 = 1,
                 sex = 'total' )$lt[ 1:15, 'Lx' ]
    
    
    nLx5_9_c <- 
      LifeTable( x = 0:99,
                 mx = c( ltb2$mx[1],
                         fitted( ungroup::pclm( x = ltb2$AgeStart[ 2:22 ],
                                                y = ltb2$dx[ 2:22 ],
                                                nlast = 1,
                                                offset = ltb2$Lx[ 2:22 ] ) )[ 1:99 ] ),
                 lx0 = 1,
                 sex = 'total' )$lt[ 1:15, 'Lx' ]
    
    nLx10_14_c <- 
      LifeTable( x = 0:99,
                 mx = c( ltb3$mx[1],
                         fitted( ungroup::pclm( x = ltb3$AgeStart[ 2:22 ],
                                                y = ltb3$dx[ 2:22 ],
                                                nlast = 1,
                                                offset = ltb3$Lx[ 2:22 ] ) )[ 1:99 ] ),
                 lx0 = 1,
                 sex = 'total' )$lt[ 1:15, 'Lx' ]
    
    
    # 2) get female q15_45 mortality probability for 
    # reference period(q1f), reference period - 5 (q2f), and reference period - 10 (q3f)
    
    nLx0_4_w <- 
      getLifeTableWPP( locations = loc, year = ( date_ref_dec - 2.5 ), sex = 'Female' ) %>%
      .[ , age5 := AgeStart - AgeStart %% 5 ] %>%
      .[ age5 %in% seq( 10, 65, 5 ), 
         .( Lx = sum( Lx ) ),
         .( age5 ) ] %>%
      pull( Lx )
    
    nLx5_9_w <- 
      getLifeTableWPP( locations = loc, year = ( date_ref_dec - 7.5 ), sex = 'Female' ) %>%
      .[ , age5 := AgeStart - AgeStart %% 5 ] %>%
      .[ age5 %in% seq( 10, 65, 5 ), 
         .( Lx = sum( Lx ) ),
         .( age5 ) ] %>%
      pull( Lx )
    
    nLx10_14_w <- 
      getLifeTableWPP( locations = loc, year = ( date_ref_dec - 12.5 ), sex = 'Female' ) %>%
      .[ , age5 := AgeStart - AgeStart %% 5 ] %>%
      .[ age5 %in% seq( 10, 65, 5 ), 
         .( Lx = sum( Lx ) ),
         .( age5 ) ] %>%
      pull( Lx )
    
    # 4) get fertility profile for current year and previous 15 period
    
    asfr <- FetchFertilityWpp2019( locations = loc,
                                   year =  date_ref_dec )$asfr
    
    if( ( date_ref_dec - 15 ) < 1950 ){
      asfr_15prior <- asfr
    } else{
      asfr_15prior <- FetchFertilityWpp2019( locations = loc,
                                             year = ( date_ref_dec - 15 ) )$asfr
    }
    
    
    # 5) run function
    cat( 'Country:', loc )
    outRevSurv <- 
      data.table(
        LocID = loc,
        SeriesID = x,
        DateFormtd = date_ref,
        RevSurvWPP( ages_c, pop_c, 
                    nLx0_4_c, nLx5_9_c, nLx10_14_c,
                    ages_w, 
                    pop_w, nLx0_4_w, nLx5_9_w, nLx10_14_w,
                    asfr[-1],
                    asfr_15prior[-1],
                    date_ref )
      )
    
    return( outRevSurv )
  } )

outRevSurv <- do.call( rbind, RevSurvEstimates )

write.table( outRevSurv, 'outputs/reverse_survival_fertest_world_x1_wppfunction.csv', 
             row.names = F )

##################################################