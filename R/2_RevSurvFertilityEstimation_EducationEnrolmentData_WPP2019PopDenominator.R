##################################################
### Title: Reverse Survival Fertility Estimation
###        Using UNESCO data for Children
###        Using WPP2019 data for Women
### Author: Jose H C Monteiro da Silva
### Last Update: 2021-06-11
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
    
    popx5_w = aux_f$pop_w
    ages5_w = aux_f$Age %>% unique
    popx1_c = aux_c$pop_c
    ages1_c = aux_c$Age %>% unique
    
    # 1) get child 0-5 mortality probability for reference period
    #    reference period - 5 and reference period - 10
    ltb_ref <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec,
        sex       = 'Total'
      )
    
    ltb_ref0_4 <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec - 2.5,
        sex       = 'Total'
      )
    
    ltb_ref5_9 <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec - 7.5,
        sex       = 'Total'
      )
    
    ltb_ref10_14 <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec - 12.5,
        sex       = 'Total'
      )
    
    q1  <-  
      ( ltb_ref0_4$lx[ ltb_ref0_4$x == 0 ] - 
          ltb_ref0_4$lx[ ltb_ref0_4$x == 5 ] ) /
      ltb_ref0_4$lx[ ltb_ref0_4$x == 0 ]
    
    q2  <-  
      ( ltb_ref5_9$lx[ ltb_ref5_9$x == 0 ] - 
          ltb_ref5_9$lx[ ltb_ref5_9$x == 5 ] ) /
      ltb_ref5_9$lx[ ltb_ref5_9$x == 0 ]
    
    q3  <-  
      ( ltb_ref10_14$lx[ ltb_ref10_14$x == 0 ] - 
          ltb_ref10_14$lx[ ltb_ref10_14$x == 5 ] ) /
      ltb_ref10_14$lx[ ltb_ref10_14$x == 0 ]
    
    q0_5 <- c( q1, q2, q3 )
    
    # 2) get female q15_45 mortality probability for 
    # reference period(q1f), reference period - 5 (q2f), and reference period - 10 (q3f)
    
    ltf_ref <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec,
        sex       = 'Female'
      )
    
    ltf_ref0_4 <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec - 2.5,
        sex       = 'Female'
      )
    
    ltf_ref5_9 <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec - 7.5,
        sex       = 'Female'
      )
    
    ltf_ref10_14 <- 
      getLifeTableWPP(
        locations = loc,
        year      = date_ref_dec - 12.5,
        sex       = 'Female'
      )
    
    q1f  <-  
      ( ltf_ref0_4$lx[ ltf_ref0_4$x == 15 ] - 
          ltf_ref0_4$lx[ ltf_ref0_4$x == 60 ] ) /
      ltf_ref0_4$lx[ ltf_ref0_4$x == 15 ]
    q2f  <-  
      ( ltf_ref5_9$lx[ ltf_ref5_9$x == 15 ] - 
          ltf_ref5_9$lx[ ltf_ref5_9$x == 60 ] ) /
      ltf_ref5_9$lx[ ltf_ref5_9$x == 15 ]
    q3f  <-  
      ( ltf_ref10_14$lx[ ltf_ref10_14$x == 15 ] -
          ltf_ref10_14$lx[ ltf_ref10_14$x == 60 ] ) /
      ltf_ref10_14$lx[ ltf_ref10_14$x == 15 ]
    
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

outRevSurv[, Agelab := 0:(.N-1),
           .( LocID, SeriesID ) ]

outRevSurv <- 
  outRevSurv %>%
  merge(
    filter_child[ ,.( LocID, SeriesID, Agelab = AgeLabel, Dummy ) ],
    by = c( 'LocID', 'SeriesID', 'Agelab' )
  ) %>%
  .[ Dummy == FALSE ]

write.table( outRevSurv, 'outputs/reverse_survival_fertestr_world_unesco_wpp2019denominator.csv', 
             row.names = F )
##################################################
