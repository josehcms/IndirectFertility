##################################################
### Title: Reverse Survival Fertility Estimation 5 year age groups
### Without Graduation
### Author: Jose H C Monteiro da Silva
### Last Update: 2021-05-27
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
    
    out <- 
      data.table(
        SeriesID = x,
        LocID    = aux$LocID %>% unique,
        Age,
        DataValueX5 = Value
      )
    
    return( out )
  })

pop_child <- do.call( rbind, pop_child )

pop_child[ , Age := ifelse( Age < 5, 0, Age ) ]
##################################################

### Change age groups and process filters in IDs #-----

# filter children SeriesID with complete single age groups (15 categories)
# and select required variables for reverse survival estimation
pop_child <- 
  pop_child[ ,
             .( pop_c = sum( DataValueX5 ) ),
             .( SeriesID, LocID, 
                Age = as.numeric( Age ) ) ] %>%
  setorder( LocID, SeriesID, Age ) %>%
  merge(
    pop_data[ ,.( SeriesID, DateFormtd ) ] %>% unique,
    by = 'SeriesID'
  ) %>%
  .[ Age < 15 ]

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

RevSurvEstimatesX5 <- 
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
    popx5_c = aux_c$pop_c
    ages5_c = aux_c$Age %>% unique
    
    # 1) get reference life table for children nLx
    ltb_ref <- getWPP2019LT( locations = loc,
                             year = date_ref_dec,
                             sex = 'both' )
    
    nLx_c = c( sum( ltb_ref[ ltb_ref$x %in% c( 0, 1 ), ]$Lx ),
               ltb_ref[ ltb_ref$x == 5, ]$Lx,
               ltb_ref[ ltb_ref$x ==10, ]$Lx )
    
    l0_c = 1 
    
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
        FertRevSurvx5c( 
          ages5_c, popx5_c, ages5_w, popx5_w, 
          nLx_c, l0_c, lx5_w,
          asfr, asfr_15prior,
          q15_45f, date_ref )
      )
    
    return( outRevSurv )
  } )

outRevSurvx5 <- do.call( rbind, RevSurvEstimatesX5 )

write.table( outRevSurvx5, 
             file = 'outputs/reverse_survival_fertest_world_x5_aggregated.csv', 
             row.names = F )

##################################################
