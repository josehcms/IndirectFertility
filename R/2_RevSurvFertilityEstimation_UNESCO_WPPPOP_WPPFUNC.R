##################################################
### Title: Reverse Survival Fertility Estimation
###        Using UNESCO data for Children
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

# aux_functions
source( 'R/aux_functions.R' )
source( 'R/reverse_survival_wpp_function.R' )
##################################################

### Read UNPD saved data #------------------------
pop_data <-
  fread( 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv' ) %>%
  .[ AgeGrpStart %in% seq( 10, 65, 5 ) & Time <= 2020 , 
     .( LocID, 
        TimeLabel  = Time, 
        TimeMid    = MidPeriod,
        DateFormtd = as.Date( date_decimal( MidPeriod ) ),
        AgeLabel   = AgeGrpStart,
        pop_w = PopFemale * 1000 ) ]

pop_child <- 
  fread( 'data/world_unesco_demodata_children_age.csv' ) %>%
  .[, SeriesID := as.numeric( SeriesID ) ]
##################################################

### Setup female data from x5 tabulations #---------------

pop_females <- 
  pop_data %>% copy

##################################################

### Select child data #---------------------------

# Match LocID and TimeLabel from women data
filter_child <- 
  pop_child %>%
  merge(
    pop_females[, .( LocID, TimeLabel ) ] %>% unique,
    by = c( 'LocID', 'TimeLabel' )
  )

pop_child <- 
  filter_child[ ,
                .( pop_c = sum( DataValueNew ) ),
                .( SeriesID, LocID, 
                   Age = as.numeric( AgeLabel ) ) ] %>%
  setorder( LocID, SeriesID, Age ) %>%
  merge(
    pop_child[ ,.( SeriesID, 
                   DateFormtd = date_decimal( TimeMid ) ) ] %>% unique,
    by = 'SeriesID'
  )
##################################################

### Apply reverse survival to Series #------------

seriesUnesco <- 
  filter_child[ ,.( SeriesID, LocID, TimeLabel ) ] %>% unique

pop_females <- 
  pop_females[ , .( LocID, TimeLabel, DateFormtd, Age = AgeLabel, 
                    pop_w ) ] %>%
  merge(
    seriesUnesco,
    by = c( 'LocID', 'TimeLabel' )
  )

series <- pop_females$SeriesID %>% unique

RevSurvEstimates <- 
  lapply( series, function( x ){
    
    aux_c <- pop_child[ SeriesID == x ] 
    aux_f <- pop_females[ SeriesID == x ]
    
    if( !is_age_sequential( aux_c$Age ) | !is_age_sequential( aux_f$Age )  ){
      paste0( 'SeriesID number ', x, ' not in sequencial age format!!!')
    }
    
    date_ref <- as.Date( unique( aux_c$DateFormtd ) )
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

outRevSurv[, Agelab := 0:(.N-1),
           .( LocID, SeriesID ) ]

outRevSurv <- 
  outRevSurv %>%
  merge(
    filter_child[ ,.( LocID, SeriesID, Agelab = AgeLabel, Dummy ) ],
    by = c( 'LocID', 'SeriesID', 'Agelab' )
  ) %>%
  .[ Dummy == FALSE ]

write.table( outRevSurv, 'outputs/reverse_survival_fertest_world_unesco_popwpp_wppfunction.csv', 
             row.names = F )
##################################################
