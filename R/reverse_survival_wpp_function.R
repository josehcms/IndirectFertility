# 
# ### Reconstruct Denominator (females)
# 
# pop_c <-  c( 281260, 261320, 268410, 286810, 278990, 293760, 293490, 302060, 315970, 267190, 326980, 280260, 354120, 356920, 354830 )
# pop_w <- c(  815930, 780320, 697160, 626430, 361650, 435880, 393760, 352520, 294280, 230200, 160590, NA )
# asfr_ref <- c( 0.0418,0.1535, 0.1482, 0.1118, 0.0708, 0.0301, 0.0032 )
# asfr_15prior <- c( 0.0533, 0.1974, 0.2144, 0.1836, 0.1332, 0.0676, 0.0134 )
# 
# nLx0_4_w <- 
#   getLifeTableWPP( 'Cambodia', year = 2010, sex = 'Female' )[AgeStart %in% 10:65 ]$Lx
# nLx5_9_w <- 
#   getLifeTableWPP( 'Cambodia', year = 2005, sex = 'Female' )[AgeStart %in% 10:65 ]$Lx
# nLx10_14_w <- 
#   getLifeTableWPP( 'Cambodia', year = 2000, sex = 'Female' )[AgeStart %in% 10:65 ]$Lx
# 
# 
# lt1 <- 
#   getLifeTableWPP( 'Cambodia', year = 2008.11, sex = 'Total' ) %>%
#   .[ , age5 := AgeStart - AgeStart %% 5 ] %>%
#   .[ age5 %in% c( 0, 5, 10 ), 
#      .( Lx = sum( Lx ) ),
#      .( age5 ) ] %>%
#   pull( Lx )
#   
# lt2 <- getLifeTableWPP( 'Cambodia', year = 2003, sex = 'Total' )
# lt3 <- getLifeTableWPP( 'Cambodia', year = 1998, sex = 'Total' )
# 
# nLx0_4_c <-
#   LifeTable( x = 0:99,
#              mx = c( lt1$mx[1], 
#                      fitted( ungroup::pclm( x = lt1$AgeStart[ 2:22 ],
#                                             y = lt1$dx[ 2:22 ],
#                                             nlast = 1,
#                                             offset = lt1$Lx[ 2:22 ] ) )[ 1:99 ] ),
#              lx0 = 1,
#              sex = 'total' )$lt[ 1:16, 'Lx' ]
# 
# nLx5_9_c <-
#   LifeTable( x = 0:99,
#              mx = c( lt2$mx[1], 
#                      fitted( ungroup::pclm( x = lt2$AgeStart[ 2:22 ],
#                                             y = lt2$dx[ 2:22 ],
#                                             nlast = 1,
#                                             offset = lt2$Lx[ 2:22 ] ) )[ 1:99 ] ),
#              lx0 = 1,
#              sex = 'total' )$lt[ 1:16, 'Lx' ]
# 
# nLx10_14_c <-
#   LifeTable( x = 0:99,
#              mx = c( lt3$mx[1], 
#                      fitted( ungroup::pclm( x = lt3$AgeStart[ 2:22 ],
#                                             y = lt3$dx[ 2:22 ],
#                                             nlast = 1,
#                                             offset = lt3$Lx[ 2:22 ] ) )[ 1:99 ] ),
#              lx0 = 1,
#              sex = 'total' )$lt[ 1:16, 'Lx' ]
# 
# 
# pop_c <-  c( 281260 + 261320 + 268410 + 286810 + 278990, 
#              293760 + 293490 + 302060 + 315970 + 267190, 
#              326980 + 280260 + 354120 + 356920 + 354830 )
# 
# ages_c = seq(0,10,5)
# 
# nLx0_4_c <-
#   LifeTable( x = 0:99,
#              mx = c( lt1$mx[1], 
#                      fitted( ungroup::pclm( x = lt1$AgeStart[ 2:22 ],
#                                             y = lt1$dx[ 2:22 ],
#                                             nlast = 1,
#                                             offset = lt1$Lx[ 2:22 ] ) )[ 1:99 ] ),
#              lx0 = 1,
#              sex = 'total' )$lt %>%
#   setDT %>%
#   .[ , age5 := x - x %% 5 ] %>%
#   .[ x < 15, .( Lx = sum( Lx ) ), age5 ] %>%
#   pull( Lx )
# 
# nLx5_9_c <-
#   LifeTable( x = 0:99,
#              mx = c( lt2$mx[1], 
#                      fitted( ungroup::pclm( x = lt2$AgeStart[ 2:22 ],
#                                             y = lt2$dx[ 2:22 ],
#                                             nlast = 1,
#                                             offset = lt2$Lx[ 2:22 ] ) )[ 1:99 ] ),
#              lx0 = 1,
#              sex = 'total' )$lt %>%
#   setDT %>%
#   .[ , age5 := x - x %% 5 ] %>%
#   .[ x < 15, .( Lx = sum( Lx ) ), age5 ] %>%
#   pull( Lx )
# 
# nLx10_14_c <-
#   LifeTable( x = 0:99,
#              mx = c( lt3$mx[1], 
#                      fitted( ungroup::pclm( x = lt3$AgeStart[ 2:22 ],
#                                             y = lt3$dx[ 2:22 ],
#                                             nlast = 1,
#                                             offset = lt3$Lx[ 2:22 ] ) )[ 1:99 ] ),
#              lx0 = 1,
#              sex = 'total' )$lt %>%
#   setDT %>%
#   .[ , age5 := x - x %% 5 ] %>%
#   .[ x < 15, .( Lx = sum( Lx ) ), age5 ] %>%
#   pull( Lx )

interpolate <- function( y1, y2, x1, x2, x ){
  y <- round( ( ( x - x1 ) / ( x2 - x1 ) ) * ( y2 - y1 ) + y1, 5 )
  return( y )
}

DenomReconst <- 
  function( ages_w, pop_w, 
            nLx0_4, nLx5_9, nLx10_14,
            asfr_std_ref, asfr_std_15prior,
            ref_year ){
    
    df_survfem <- 
      data.frame(
        ages_w,
        pop_w,
        nLx0_4, nLx5_9, nLx10_14
      )
    
    
    df_survfem$Px0_4   <- NA
    df_survfem$Px5_9   <- NA
    df_survfem$Px10_14 <- NA
    
    for( i in 2 : ( nrow( df_survfem ) ) ){
      df_survfem$Px0_4[i] <- df_survfem$nLx0_4[i] / df_survfem$nLx0_4[i-1]
      df_survfem$Px5_9[i] <- df_survfem$nLx5_9[i] / df_survfem$nLx5_9[i-1]
      df_survfem$Px10_14[i] <- df_survfem$nLx10_14[i] / df_survfem$nLx10_14[i-1]
    }
    
    
    df_survfem$pop_w0_4   <- NA
    df_survfem$pop_w5_9   <- NA
    df_survfem$pop_w10_14 <- NA
    
    for( i in 1 : ( nrow( df_survfem ) - 1 ) ){
      df_survfem$pop_w0_4[i]   <-
        df_survfem$pop_w[i+1] / 
        df_survfem$Px0_4[i]
    }
    
    for( i in 1 : ( nrow( df_survfem ) - 2 ) ){
      df_survfem$pop_w5_9[i]   <-
        df_survfem$pop_w0_4[i+1] / 
        df_survfem$Px5_9[i]
    }
    
    for( i in 1 : ( nrow( df_survfem ) - 3 ) ){
      df_survfem$pop_w10_14[i]   <-
        df_survfem$pop_w5_9[i+1] / 
        df_survfem$Px10_14[i]
    }
    
    
    df_survfem <- 
      df_survfem[ df_survfem$ages_w %in% 15:49, 
                  c( "ages_w", "pop_w", 
                     "pop_w0_4", "pop_w5_9", "pop_w10_14" ) ]
    
    year_list <- seq( ref_year - 0.5 , ref_year - 0.5 - 14, -1 )
    
    out_survfem <- data.frame()
    
    for( t in year_list ){
      
      if( t <= ref_year & t > ref_year - 5 ){
        pop1 = df_survfem$pop_w
        pop2 = df_survfem$pop_w0_4
        t1   = ref_year
        t2   = ref_year - 5
        pop_t = interpolate( pop1, pop2, t1, t2, t )
      }
      
      if( t <= ref_year - 5 & t > ref_year - 10 ){
        pop1 = df_survfem$pop_w0_4
        pop2 = df_survfem$pop_w5_9
        t1   = ref_year - 5
        t2   = ref_year - 10
        pop_t = interpolate( pop1, pop2, t1, t2, t )
      }
      
      if( t <= ref_year - 10 & t > ref_year - 15 ){
        pop1 = df_survfem$pop_w5_9
        pop2 = df_survfem$pop_w10_14
        t1   = ref_year - 10
        t2   = ref_year - 15
        pop_t = interpolate( pop1, pop2, t1, t2, t )
      }
      
      out_survfem <-
        rbind(
          out_survfem,
          data.frame(
            year     = t,
            ages_w   = df_survfem$ages_w[ df_survfem$ages_w %in% seq( 15, 45, 5 ) ],
            pop_w    = pop_t,
            asfr_std = interpolate( asfr_std_ref, 
                                    asfr_std_15prior, 
                                    ref_year - 0.5, ref_year - 14 - 0.5, 
                                    t )
          )
        )
    }
    
    return( out_survfem )
    
  }


BirthReconst <- 
  function( ages_c, pop_c,
            nLx0_4, nLx5_9, nLx10_14,
            ref_year
            ){
    
    if( length( ages_c ) > 3 ){
      flag_x5 <- FALSE
    } else{
      flag_x5 <- TRUE
    }
    
    df_survbirths <- 
      data.frame(
        ages_c,
        pop_c
      )
    
    df_survbirths$Px0_4   = NA
    df_survbirths$Px5_9   = NA
    df_survbirths$Px10_14 = NA
    
    if( flag_x5 ){

      df_survbirths$Px0_4[1]   = nLx0_4[1] / ( 5 )
      df_survbirths$Px5_9[1]   = nLx5_9[1] / ( 5 )
      df_survbirths$Px10_14[1] = nLx10_14[1] / ( 5 )
      
      
      for ( i in 2 : 3 ){
        df_survbirths$Px0_4[ i ]   = nLx0_4[ i ] / nLx0_4[ i - 1 ]
        df_survbirths$Px5_9[ i ]   = nLx5_9[ i ] / nLx5_9[ i - 1 ]
        df_survbirths$Px10_14[ i ] = nLx10_14[ i ] / nLx10_14[ i - 1 ]
      }
      
      
      Sc = c( ( df_survbirths$Px0_4[ df_survbirths$ages_c == 0 ] ),
              ( df_survbirths$Px5_9[ df_survbirths$ages_c == 0 ] *
                  df_survbirths$Px0_4[ df_survbirths$ages_c == 5 ] ),
              ( df_survbirths$Px10_14[ df_survbirths$ages_c == 0 ] *
                  df_survbirths$Px5_9[ df_survbirths$ages_c == 5 ] *
                  df_survbirths$Px0_4[ df_survbirths$ages_c == 10 ] )
      )
    } else{
      
      df_survbirths$Px0_4[1]   = nLx0_4[1]
      df_survbirths$Px5_9[1]   = nLx5_9[1]
      df_survbirths$Px10_14[1] = nLx10_14[1]
      
      for ( i in 2 : ( nrow( df_survbirths ) ) ){
        df_survbirths$Px0_4[ i ]   = nLx0_4[ i ] / nLx0_4[ i - 1 ]
        df_survbirths$Px5_9[ i ]   = nLx5_9[ i ] / nLx5_9[ i - 1 ]
        df_survbirths$Px10_14[ i ] = nLx10_14[ i ] / nLx10_14[ i - 1 ]
      }
      
      Sc = NULL
      P1 = df_survbirths$Px0_4
      P2 = df_survbirths$Px5_9
      P3 = df_survbirths$Px10_14
      
      for( i in 1 : ( nrow( df_survbirths ) ) ){
        t = i - 1
        j = i
        S = rep( NA, i )
        
        while( t >= 0 ){
          
          if( t %in% 0:2 ){
            S[j] = P1[i - t]
          }
          
          if ( t %in% 3 : 7 ){
            S[j] = P1[i - t] * ( 1 - ( t - 2 ) / 5 ) + P2[i - t] * ( ( t - 2 ) / 5 )
          }
          
          if ( t %in% 8 : 12 ){
            S[j] = P2[i - t] * ( 1 - ( t - 5 - 2 ) / 5 ) + P3[i - t] * ( ( t - 5 - 2 ) / 5 )
          }
          
          if ( t %in% 13 : 14 ){
            S[j] = P3[i - t]
          }
          
          t = t - 1
          j = j - 1
        }
        
        Sc = c( Sc, round( prod( S ), 5 ) )
        
      }
    }
    
    n = unique( diff( df_survbirths$ages_c ) )
    df_survbirths$year = ref_year - df_survbirths$ages_c - n / 2
    df_survbirths$births <- ( df_survbirths$pop_c / n ) / Sc
    
    return( df_survbirths[ , c( 'year', 'births' ) ] )
    
  }
    
RevSurvCore <-
  function(  ref_year,
             ages_c, pop_c,
             nLx0_4_c, nLx5_9_c, nLx10_14_c,
             ages_w, pop_w,
             nLx0_4_w, nLx5_9_w, nLx10_14_w,
             asfr_std_ref, asfr_std_15prior ){
    
    
    num_df <- 
      BirthReconst( ages_c = ages_c, pop_c = pop_c,
                    nLx0_4 = nLx0_4_c, nLx5_9 = nLx5_9_c, nLx10_14 = nLx10_14_c,
                    ref_year = ref_year )
    
    den_df <-
      DenomReconst( ages_w = ages_w, pop_w = pop_w, 
                    nLx0_4 = nLx0_4_w, nLx5_9 = nLx5_9_w, nLx10_14 = nLx10_14_w,
                    asfr_std_ref = asfr_std_ref, asfr_std_15prior = asfr_std_15prior,
                    ref_year = ref_year )
    
    
    RevSurvTFR <- data.frame()
    
    for( t in unique( num_df$year ) ){
      den <- sum( den_df[ den_df$year == t, ]$pop_w * 
                    den_df[ den_df$year == t, ]$asfr_std )
      num <- num_df[ num_df$year == t, ]$births
      
      RevSurvTFR <- rbind(
        RevSurvTFR,
        data.frame(
          year = t,
          TFR  = num / den,
          births = num
        )
      )
    }
    
    return( RevSurvTFR )
  }



RevSurvWPP <- 
  function( ages_c, pop_c, nLx0_4_c, nLx5_9_c, nLx10_14_c,
            ages_w, pop_w, nLx0_4_w, nLx5_9_w, nLx10_14_w,
            asfr = c( 0, 0.017, 0.055, 0.057, 0.041, 0.022, 0.007, 0.002 ),
            asfr_15prior = NULL,
            date_ref ){
  
  ref_year <- decimal_anydate( date_ref )
  
  asfr_std_ref = asfr / ( 5 * sum( asfr ) )
  asfr_std_15prior = asfr / sum( 5 * asfr )
    
  
  if( !is.null( asfr_15prior ) ){
    asfr_std_15prior <- asfr_15prior / sum( 5 * asfr_15prior )
  }
  
  
  print( paste0( 'Reverse Survival Fertility Estimation - Reference date: ',
                 substr( lubridate::date_decimal( ref_year ), 1, 10 ) ) )
  
  RevSurvTFR <- 
    RevSurvCore( ref_year,
                 ages_c, pop_c,
                 nLx0_4_c, nLx5_9_c, nLx10_14_c,
                 ages_w, pop_w,
                 nLx0_4_w, nLx5_9_w, nLx10_14_w,
                 asfr_std_ref, asfr_std_15prior )
  
  return( RevSurvTFR )
  
  }

