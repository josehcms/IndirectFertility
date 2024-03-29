##################################################
### Title: Reverse Survival Fertility Estimation 5 year age groups
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-10-08
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
  fread('data/world_census_demodata_agex5_sex.csv') %>%
  .[, SeriesID := as.numeric( SeriesID ) ]

##################################################

### Check age and sex tabulations #---------------

# adjust date format
pop_data[, DateFormtd := TimeStart %>% as.Date( '%d/%m/%Y' ) ]

# filter females SexID == 2 and AgeLabel from 10 to 69
pop_females <- pop_data[ SexID == 2 & 
                           AgeStart %in% seq( 10, 65, 5 ) &
                           AgeLabel %in% c( "10-14", "15-19", "20-24", "25-29",
                                            "30-34", "35-39", "40-44", "45-49", 
                                            "50-54", "55-59", "60-64", "65-69" ) ]
# check females age groups
check_age_females <- 
  lapply( pop_females$SeriesID %>% unique, function( x ){
    
    aux_f <- 
      pop_females[ SeriesID == x & SexID == 2,
                   .( LocID, SeriesID, 
                      AgeStart,
                      AgeLabel = factor( AgeLabel,
                                         levels = c( "10-14", "15-19", "20-24", "25-29",
                                                     "30-34", "35-39", "40-44", "45-49", 
                                                     "50-54", "55-59", "60-64", "65-69" ) )
                   ) ]
    
    aux <- 
      pop_females[SeriesID == x ,
                  .( LocID, SeriesID, AgeLabel ) ] %>% 
      unique %>% 
      .[ , .N, .( LocID, SeriesID ) ]
    
    aux$Select <- 
      ( aux$N == 12 & is_abridged( aux_f$AgeStart ) )
    
    return( aux )
    
  } )

filterSeriesFem <- 
  do.call( rbind, check_age_females )

# check sexID
# check sex tabulation - 2 sex IDs for each series are required
check_sex_pop <- 
  ( pop_data[, .( LocID, SeriesID, SexID ) ] %>% 
      unique %>% 
      .[,.N,.( LocID, SeriesID ) ] %>%
      .[ N != 2 ])$SeriesID # these two series will be excluded in age check

# prepare data of children for graduation
ids_ungroup <- 
  ( pop_data[ AgeStart %in% 0:14 , ] %>%
      copy %>%
      .[ , N := .N, 
         .( SeriesID, SexID ) ] %>%
      .[ N %in% c( 3, 4, 5 ) ] )$SeriesID %>% unique 

ids_ungroup <- 
  ids_ungroup[ which( ! ( ids_ungroup %in%
                            ( c( filterSeriesFem[ Select == F ]$SeriesID,
                                 check_sex_pop,
                                 ( pop_data[ AgeStart %in% 0:14 , ] %>%
                                     copy %>%
                                     .[ , N := .N, 
                                        .( SeriesID, SexID ) ] %>%
                                     .[ N %in% c( 3 ) & AgeLabel %in% c( '0', '1-4' ) ,
                                        .( SeriesID, LocID, SexID, AgeStart, 
                                           AgeEnd, AgeLabel, DataValue ) ] )$SeriesID %>% 
                                   unique,
                                 602031251235,
                                 58313328107,
                                 1931149610453 ) %>% unique ) ) ) ] 


pop_child <- 
  lapply( ids_ungroup , function( x ){
    
    print( paste0( which( ids_ungroup == x ), ' of ', length( ids_ungroup ) ) )
    
    aux <- 
      pop_data[ SeriesID == x ] %>% copy
    
    # older ages for graduation
    aux_15p <- 
      aux[ AgeStart >= 15 & AgeStart <= 60 , ] %>%
      .[, .( DataValueNew = sum( DataValueNew ) ),
        .( SeriesID, LocID, AgeStart, AgeEnd, AgeLabel ) ]
    
    if( nrow( aux_15p ) != 10 ){
      print( x )
      stop( 'Adult age range not right for series')
    }
    
    # children for graduation
    aux_0_14 <- 
      aux[ AgeStart %in% 0:14 , ]
    
    aux_0_14[, N := .N, .( SeriesID, SexID ) ]
    
    flag0_4 <- '0-4' %in% aux_0_14$AgeLabel %>% unique 
    
    Nval <- aux_0_14$N %>% unique
    
    if( ( Nval == 4 | Nval == 5 ) & flag0_4 == F ){
      aux_0_14_new <- 
        aux_0_14[ AgeLabel %in% c( '0', '1-4', '5-9', '10-14' ) ] %>%
        .[, N := .N,
          .( SeriesID, SexID ) ]
      Nval_new <- aux_0_14_new$N %>% unique
      
      if( Nval_new == 4 ){
        aux_m15 <- 
          aux_0_14_new[, 
                       list(
                         DataValueNew = sum( DataValueNew )
                       ),
                       .( SeriesID, LocID, AgeStart, AgeEnd, AgeLabel ) ]
        keep0 = T
      } else{
        print(x)
        stop( 'Wrong N from filter of series ID ( N = 4 or 5 )')
      }
      
    } else{
      
      aux_0_14_new <- 
        aux_0_14[ AgeLabel %in% c( '0-4', '5-9', '10-14' ) ] %>%
        .[, N := .N,
          .( SeriesID, SexID ) ]
      Nval_new <- aux_0_14_new$N %>% unique
      
      if( Nval_new == 3 ){
        aux_m15 <- 
          aux_0_14_new[, 
                       list(
                         DataValueNew = sum( DataValueNew )
                       ),
                       .( SeriesID, LocID, AgeStart, AgeEnd, AgeLabel ) ]
        keep0 = F 
      } else{
        print(x)
        stop( 'Wrong N from filter of series ID ( N = 3 )')
      }
      
    }
    
    pop_abrgd <- 
      rbind( aux_m15, aux_15p ) %>%
      setorder( SeriesID, LocID, AgeStart, DataValueNew )
    
    Value = pop_abrgd$DataValueNew
    Age   = pop_abrgd$AgeStart
    
    beersmod <- 
      graduate( Value, Age, method = "beers(mod)", keep0 = keep0, johnson = TRUE )
    
    sprague <-
      graduate( Value, Age, method = 'sprague', keep0 = keep0 )
    
    out <- 
      data.table(
        SeriesID = x,
        LocID    = aux$LocID %>% unique,
        AgeLabel = 0:14,
        DataValueBeers   = beersmod[ c( paste0(0:14) ) ],
        DataValueSprague = sprague[ c( paste0(0:14) ) ]
      )
    
    return( out )
  })

pop_child <- do.call( rbind, pop_child )

##################################################

### Change age groups and process filters in IDs #-----

# filter children SeriesID with complete single age groups (15 categories)
# and select required variables for reverse survival estimation
pop_child <- 
  pop_child[ ,
             .( pop_c_beers = sum( DataValueBeers ),
                pop_c_spgue = sum( DataValueSprague ) ),
             .( SeriesID, LocID, 
                Age = as.numeric( AgeLabel ) ) ] %>%
  setorder( LocID, SeriesID, Age ) %>%
  merge(
    pop_data[ ,.( SeriesID, DateFormtd ) ] %>% unique,
    by = 'SeriesID'
  )

# filter women SeriesID with complete single age groups (60 categories)
# and select required variables for reverse survival estimation
pop_females <- 
  pop_females[ SeriesID %in% filterSeriesFem[ Select == TRUE ]$SeriesID,
               .( pop_w = sum( DataValueNew ) ),
               .( SeriesID, LocID, DateFormtd, Age = AgeStart ) ] %>%
  setorder( LocID, SeriesID, Age )

# merge selected series IDs of females and children to assure overlapping
series <- 
  ids_ungroup

RevSurvEstimatesBeers <- 
  lapply( series, function( x ){
    
    aux_c <- pop_child[ SeriesID == x ] 
    aux_c[ , pop_c := pop_c_beers ]
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

outRevSurvBeers <- do.call( rbind, RevSurvEstimatesBeers )

write.table( outRevSurvBeers, 
             file = 'outputs/reverse_survival_fertest_world_x5_beers.csv', 
             row.names = F )

RevSurvEstimatesSprague <- 
  lapply( series, function( x ){
    
    aux_c <- pop_child[ SeriesID == x ] 
    aux_c[ , pop_c := pop_c_spgue ]
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

outRevSurvSprague <- do.call( rbind, RevSurvEstimatesSprague )

write.table( outRevSurvSprague, 
             file = 'outputs/reverse_survival_fertest_world_x5_sprague.csv', 
             row.names = F )

outRevSurv <- 
  rbind(
    outRevSurvSprague[ , TypeEst := 'Abridged-Sprague'],
    outRevSurvBeers[ , TypeEst := 'Abridged-BeersModified']
  )

write.table( outRevSurv, 
             file = 'outputs/reverse_survival_fertest_world_x5_graduated_wppfunction.csv', 
             row.names = F )

##################################################