###################################################
### Title: Aux Functions for RevSurv estimation
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-09-17
###################################################

# Retrieve life table for location and year from WPP
# mortality rates database

getWPP2019LT <- 
  function( locations = NULL, year, sex = 'both'){
  
    require( wpp2019 )
    require( MortalityLaws )
    require(fertestr)
    
    if ( !is.numeric( locations ) ){
      location_codes <- get_location_code( locations )
    } else {
      location_codes <- locations
    }
  
    
    if( year < 1955 ){
      year_sup <- 1955
      year_inf <- 1950
    } else{
      year_interv <- findInterval( x = year, vec = seq( 1950, 2020, 5 ) )
      year_sup <- seq( 1950, 2020, 5 )[ year_interv + 1 ]
      year_inf <- seq( 1950, 2020, 5 )[ year_interv ]
    }
    
    lt_df <- data.frame()
  
    for( location_code in location_codes ){
    
      if( tolower( sex ) == 'male' | tolower( sex ) == 'both' ){
      
        data('mxM')
        mx <- mxM[ mxM$country_code %in% location_code,
                   c( paste0( year_inf, '-', year_sup ) ) ]
      
        age <- mxM[ mxM$country_code %in% location_code,
                    c( 'age' ) ]
      
        ltm <-
          LifeTable( x   = age,
                     mx  = mx,
                     lx0 = 1,
                     sex = 'male' )$lt
        
        if( tolower( sex ) == 'male' ){
          lt_df <-
            rbind( lt_df,
                   data.frame( location_code = location_code,
                               location_name = get_location_name( location_code ),
                               year = year,
                               reference_period = paste0( year_inf, '-', year_sup ),
                               sex = 'male',
                               ltm ) )
          }
        }
    
      if( tolower( sex ) == 'female' | tolower( sex ) == 'both' ){
        
        data('mxF')
        mx <- mxF[ mxF$country_code %in% location_code,
                   c( paste0( year_inf, '-', year_sup ) ) ]
        
        
        age <- mxF[ mxF$country_code %in% location_code,
                    c( 'age' ) ]
        
        ltf <-
          LifeTable( x   = age,
                     mx  = mx,
                     lx0 = 1,
                     sex = 'female' )$lt
        
        if( tolower( sex ) == 'female' ){
          lt_df <-
            rbind( lt_df,
                   data.frame( location_code = location_code,
                               location_name = get_location_name( location_code ),
                               year = year,
                               reference_period = paste0( year_inf, '-', year_sup ),
                               sex = 'female',
                               ltf ) )
        }
      }
      
      if( tolower( sex ) == 'both' ){
        lxb <- ltf$lx * 0.4886 + ( 1 - 0.4886 )*ltm$lx
        
        ltb <-
          LifeTable( x   = age,
                     lx  = lxb,
                     lx0 = 1,
                     sex = 'total' )$lt
        
        lt_df <-
          rbind( lt_df,
                 data.frame( location_code = location_code,
                             location_name = get_location_name( location_code ),
                             year = year,
                             reference_period = paste0( year_inf, '-', year_sup ),
                             sex = 'both',
                             ltb ) )
      }
    }
    
    return( lt_df )
    
  }
