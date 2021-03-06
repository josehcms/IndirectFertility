###################################################
### Title: Aux Functions for RevSurv estimation
### Author: Jose H C Monteiro da Silva
### Last Update: 2021-06-07
###################################################

# 1) Retrieve life table for location and year from WPP (OLD)
# mortality rates database

getWPP2019LT <- 
  function( locations = NULL, year, sex = 'both'){
  
    require( wpp2019 )
    require( MortalityLaws )
    require( fertestr )
    
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

# 2) Interpolate
interpolate <- function( y1, y2, x1, x2, x ){
  y <- round( ( ( x - x1 ) / ( x2 - x1 ) ) * ( y2 - y1 ) + y1, 5 )
  return( y )
}

# 3) New function to retrieve life tables from WPP website
# (medium variant)

require( data.table )

# wpp_lt <- 
#   fread( 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv' )

wpp_lt <- 
  fread( 'data/WPP2019_LifeTables_MediumVariant.csv' )

getLifeTableWPP <- 
  function( locations = NULL, year, sex = 'Total' ){
    
    require( wpp2019 )
    require( MortalityLaws )
    require( fertestr )
    require( dplyr )
    
    if ( !is.numeric( locations ) ){
      location_codes <- get_location_code( locations )
    } else {
      location_codes <- locations
    }
    
    sex_code <- 
      dplyr::case_when(
        sex == 'Total'  ~ 3,
        sex == 'Male'   ~ 1,
        sex == 'Female' ~ 2,
      )
    
    base_lt <- 
      wpp_lt[ , .( LocID, Location, 
                   YearMid = MidPeriod,
                   ReferencePeriod = Time,
                   SexID, Sex, 
                   AgeStart = AgeGrpStart, 
                   mx, qx, 
                   dx = dx / 100000,
                   lx = lx / 100000, 
                   ax, 
                   Lx = Lx / 100000, 
                   ex ) ]
    
    if( year < 1953 ){
      
      lt_out <- 
        base_lt[ LocID %in% location_codes &
                   SexID == sex_code &
                   YearMid == 1953,
                 .(
                   LocID, Location, 
                   ReferenceYear = year,
                   ReferencePeriod = YearMid,
                   SexID, Sex, 
                   AgeStart, 
                   x = AgeStart, 
                   mx, qx, dx, lx, ax, Lx, ex
                 )] %>% 
        copy
      
      return( lt_out )
    
    } else{
      
      year_interv <- findInterval( x = year, vec = seq( 1950.5, 2025.5, 5 ) )
      year_sup <- seq( 1950.5, 2025.5, 5 )[ year_interv + 1 ]
      year_inf <- seq( 1950.5, 2025.5, 5 )[ year_interv ]
      
      if( abs( year - year_sup ) > abs( year - year_inf ) ){
        year_mid1 <- mean( c( year_inf, year_inf - 5 ) )
        year_mid2 <- mean( c( year_sup, year_inf ) )
      } else{
        year_mid1 <- mean( c( year_sup, year_inf ) )
        year_mid2 <- mean( c( year_sup + 5, year_sup ) )
      }
      
      lt_out1 <- 
        base_lt[ LocID %in% location_codes &
                   SexID == sex_code &
                   YearMid == year_mid1 ] %>% 
        copy
      
      lt_out2 <- 
        base_lt[ LocID %in% location_codes &
                   SexID == sex_code &
                   YearMid == year_mid2 ] %>% 
        copy
      
      ax_i <- interpolate( y1 = lt_out1$ax, x1 = year_mid1,
                           y2 = lt_out2$ax, x2 = year_mid2,
                           x = year )
      
      lx_i <- interpolate( y1 = lt_out1$lx, x1 = year_mid1,
                           y2 = lt_out2$lx, x2 = year_mid2,
                           x = year )
      
      locid   = lt_out1$LocID %>% unique
      locname = lt_out1$Location %>% unique
      sexid   = lt_out1$SexID %>% unique
      sexname = lt_out1$Sex %>% unique
      
      lt_out <- 
        LifeTable( x = lt_out1$AgeStart, 
                   lx = lx_i,
                   ax = ax_i, 
                   sex = tolower( sex ),
                   lx0 = 1 )$lt %>%
        setDT %>%
        .[ , .( LocID = locid, 
                Location = locname,
                ReferenceYear   = year,
                ReferencePeriod = paste0( year_mid1,'-', year_mid2 ),
                SexID = sexid, 
                Sex = sexname, 
                AgeStart = x, 
                x,
                mx, qx, dx, lx, ax, Lx, ex ) ]
      
      return( lt_out )
    }
    
    
    
  }



# Deduplicates script

## compute number of records per series/sex
#DT[, count := .N, by=list(SeriesID, SexID)]
## filter out series that are too incomplete (i.e., a typical age distribution by 5-year age group should have > 10 records up to age 50)
#DT <- DT[count >= 10]

deduplicates <- function(myDT) {     
  
  
  
  ## sort records per location and year to order multiple observation by multi-criteria using sort orders
  
  setorder(myDT, LocID, TimeMid, DataCatalogShortName,
           
           StatisticalConceptSort,
           
           DataStatusSort,
           
           DataProcessSort, DataProcessTypeSort,
           
           DataSourceSort, -DataSourceYear, DataSourceShortName,
           
           -DataTypeSort,
           
           DataReliabilitySort,
           
           ModelPatternName, PeriodGroupName, PeriodStart, PeriodSpan,
           
           SexSort, AgeStart, AgeSpan)
  
  
  
  ## subset key attributes to rank most authoritative series
  
  mySeries <- unique(myDT[, .(SeriesID, LocID, DataCatalogShortName, TimeMid, DataSourceShortName, DataSourceYear, DataSourceSort, DataStatusName, DataStatusSort, DataProcessSort, DataProcessTypeSort, StatisticalConceptName, StatisticalConceptSort, DataTypeName, DataTypeSort, DataReliabilityName, DataReliabilitySort)])
  
  
  
  setorder(mySeries, LocID, DataCatalogShortName,
           
           StatisticalConceptSort,
           
           DataStatusSort,
           
           DataProcessSort, DataProcessTypeSort,
           
           DataSourceSort, -DataSourceYear, DataSourceShortName,
           
           -DataTypeSort,
           
           DataReliabilitySort)
  
  
  
  ## assign rank to each set of "dups"
  
  mySeries[, nrank := 1:.N, by=list(LocID, DataCatalogShortName, trunc(TimeMid))]
  
  mySeries <- mySeries[nrank==1]
  
  
  
  ## keep only "best" version (top #1)
  
  myDT <- myDT[SeriesID %in% mySeries$SeriesID]
  
  return(myDT)
  
}

#DT <- deduplicates(DT)

############