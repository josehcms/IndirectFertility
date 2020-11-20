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
##################################################

### Read UNPD saved data #------------------------
pop_data <- 
  fread('data/world_census_demodata_agex5_sex.csv') %>%
  .[, SeriesID := as.numeric( SeriesID ) ]

pop_child <- 
  fread( 'data/world_unesco_demodata_children_age.csv' ) %>%
  .[, SeriesID := as.numeric( SeriesID ) ]
##################################################

### Setup female data from x5 tabulations #---------------

# adjust date format
pop_data[, DateFormtd := TimeStart %>% as.Date( '%d/%m/%Y' ) ]

# filter females SexID == 2 and AgeLabel from 10 to 69
pop_females <- pop_data[ SexID == 2 & 
                           AgeStart %in% seq( 10, 65, 5 ) &
                           AgeLabel %in% c( "10-14", "15-19", "20-24", "25-29",
                                            "30-34", "35-39", "40-44", "45-49", 
                                            "50-54", "55-59", "60-64", "65-69" ) &
                           DataTypeName == 'Population by age and sex' &
                           DataProcess == 'Census' ]
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

pop_females <- 
  pop_females[ SeriesID %in% filterSeriesFem[ Select == TRUE ]$SeriesID,
               .( pop_w = sum( DataValueNew ) ),
               .( SeriesID, LocID, DateFormtd, 
                  TimeLabel,
                  Age = AgeStart ) ] %>%
  .[ , TimeLabel := year( DateFormtd ) ] %>%
  setorder( LocID, SeriesID, Age ) 

# check countries with multiples IDs for same year

# select_uniqueID <- 
#   pop_females[,.( SeriesID, LocID, TimeLabel ) ] %>%
#   unique %>%
#   .[, .N, .( LocID, TimeLabel ) ] %>%
#   setorder( LocID, TimeLabel ) 
# 
# pop_females <- 
#   pop_females %>%
#   merge(
#     select_uniqueID,
#     by = c( 'LocID', 'TimeLabel' )
#   ) %>%
#   .[ N == 1, ]

##################################################

### Select child data #---------------------------

# Series UNESCO to female pop data
pop_females <- 
  pop_females %>%
  merge(
    pop_child[, .( LocID, SeriesID_UNESCO = SeriesID, 
                   TimeLabel ) ] %>% unique,
    by = c( 'LocID', 'TimeLabel' )
  )

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
                   DateFormtd = as.Date( date_decimal( TimeMid ) ) ) ] %>% unique,
    by = 'SeriesID'
  )
##################################################

### Apply reverse survival to Series #------------

pop_females <- 
  pop_females[ , .( SeriesID, SeriesID_UNESCO,
                    LocID, TimeLabel, DateFormtd,
                    Age, pop_w ) ] 

series_unesco <- pop_females$SeriesID_UNESCO %>% unique

RevSurvEstimates <- 
  lapply( series_unesco, function( x ){
    
    aux_c <- pop_child[ SeriesID == x ] 
    aux_f1 <- pop_females[ SeriesID_UNESCO == x ]
    
    outRevSurv <- data.table()
    
    for( s_fem in aux_f1$SeriesID %>% unique ){
      
      aux_f <- 
        aux_f1[ SeriesID == s_fem ]
      
      if( !is_age_sequential( aux_c$Age ) | !is_age_sequential( aux_f$Age )  ){
        paste0( 'SeriesID number ', x, ' not in sequencial age format!!!')
      }
      
      date_ref <- as.Date( unique( aux_c$DateFormtd ) )
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
        rbind(
          outRevSurv,
          data.table(
            LocID = loc,
            SeriesID = x,
            SeriesIDfem = s_fem,
            DateFormtd = date_ref,
            FertRevSurv( ages1_c, popx1_c, ages5_w, popx5_w, lx1_c, lx5_w,
                         asfr, asfr_15prior,
                         q0_5, q15_45f, date_ref )
          )
        )
    }
    
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

write.table( outRevSurv, 'outputs/reverse_survival_fertest_world_unesco_all.csv', 
             row.names = F )
##################################################
