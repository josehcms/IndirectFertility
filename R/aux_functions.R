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