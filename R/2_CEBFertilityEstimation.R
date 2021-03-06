##################################################
### Title: Children Ever Born Estimation using
###        Demodata
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-12-08
##################################################

### Set up packages and global options #----------

# Clean environment 
rm( list = ls( ) )
t1 <- Sys.time()
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

### Read Demodata #-------------------------------
ceb_data <- 
  rbind(
    fread( 'data/world_ceb_demodata_x5.csv' ) %>%
      .[, SeriesID := as.numeric( SeriesID ) ] %>%
      .[ AgeLabel %in% c( '40-44', '45-49', '50-54', '55-59',
                          '60-64', '65-69', '70-74', '75-79' ) ] %>%
      setorder( SeriesID, LocID, AgeStart ),
    fread( 'data/world_ceb_demodata_x1.csv' ) %>%
      .[ , SeriesID := as.numeric( SeriesID ) ] %>%
      .[ AgeLabel %in% paste0( 40:79 ) ] %>%
      setorder( SeriesID, LocID, AgeStart )
  )
  

# select series ids 
ids <- 
  ceb_data$SeriesID %>% 
  unique

##################################################

### Prepare mean ceb dataset with age shifts #----
ceb_list_shift <- 
  lapply( ids, function( x ){
    
    print( x )
    
    aux <- 
      ceb_data[ SeriesID == x ] %>% copy %>%
      setorder( AgeStart )
    
    locid    <- aux$LocID %>% unique
    locname  <- aux$LocName %>% unique
    
    mean_ceb <- aux$DataValue
    
    ages_w   <- aux$AgeStart
    age_interval <- aux$AgeEnd - aux$AgeStart
    midgroup_ages <- ( ages_w ) + age_interval / 2
    
    year_svy <- 
      aux$TimeMid %>% 
      unique 
      
    # shift estimation of mean age of childbearing to
    # time period when women were aged 25
    t_ref_mac <- 
      year_svy - ( ages_w - 25 )
      
    t_ref_mac <- 
      ifelse( t_ref_mac < 1950, 1950, t_ref_mac)
    
    mac <- 
      do.call(
        rbind,
        lapply( t_ref_mac, 
                function( x ){ 
                  out <- 
                    c( fertestr::fetch_mac_Wpp2019( location_code = locid,
                                                    year = x ) )
                  return( out )
                  } ) )
    
    
      
    year_ceb <- round( year_svy - ( midgroup_ages - mac ), 3 )

    res <- 
      data.table(
        SeriesID = x,
        LocID    = locid,
        LocName  = locname,
        year_svy = year_svy,
        ages_w = ages_w,
        year_ceb = c( year_ceb ),
        mac  = c( mac ),
        CFR  = mean_ceb,
        TypeEst = 'CEB' ) 

    return( res )
  } )

ceb_res_shift <- 
  do.call( rbind, ceb_list_shift )

out_ceb_shift <- 
  merge(
    ceb_data[,
             .( SeriesID, LocID, LocName, LocTypeName, 
                LocAreaTypeName, SubGroupName,  
                SubGroupTypeName, SubGroupCombinationID,
                DataCatalogID, DataCatalogName, DataCatalogShortName, 
                FieldWorkStart, FieldWorkMiddle, DataProcess, 
                DataSourceName, DataSourceAuthor, DataSourceYear, 
                DataSourceShortName, DataStatusName, 
                StatisticalConceptName, DataTypeName, ModelPatternName, 
                DataReliabilityName, PeriodTypeName, PeriodGroupName, 
                TimeUnit, FootNoteID, RegName, RegID, AreaName, AreaID, 
                TimeStart, TimeEnd, TimeMid, TimeLabel,                    
                SexName, SexID ) ] %>% unique,
    ceb_res_shift[ ,
                   .( SeriesID,
                      DateFormtd = as.Date( date_decimal( year_ceb ) ),
                      year = year_ceb,
                      ages_w,
                      mac,
                      CEB = CFR,
                      TypeEst = 'CEB-Age Shift' ) ],
    by = 'SeriesID'
    ) %>%
  setorder( LocID, TimeMid, SeriesID, ages_w )

write.table( out_ceb_shift, 
             'outputs/ceb_fertest_world_shift.csv',
             row.names = F )
##################################################

### Prepare mean ceb dataset without age shifts #----
ceb_list_noshift <- 
  lapply( ids, function( x ){
    
    print( x )
    
    aux <- 
      ceb_data[ SeriesID == x ] %>% copy %>%
      setorder( AgeStart )
    
    locid    <- aux$LocID %>% unique
    locname  <- aux$LocName %>% unique
    
    mean_ceb <- aux$DataValue
    ages_w   <- aux$AgeStart
    age_interval <- aux$AgeEnd - aux$AgeStart
    midgroup_ages <- ( ages_w ) + age_interval / 2
    
    year_svy <- 
      aux$TimeMid %>% 
      unique 
    
    # get mean age of childbearing based on 
    # svy year ASFR distribution
    mac <- 
      rep(
        fertestr::fetch_mac_Wpp2019( location_code = locid,
                                     year = year_svy ),
        length( ages_w )
      )
    
    year_ceb <- round( year_svy - ( midgroup_ages - mac ), 3 )
    
    res <- 
      data.table(
        SeriesID = x,
        LocID    = locid,
        LocName  = locname,
        year_svy = year_svy,
        ages_w = ages_w,
        year_ceb = c( year_ceb ),
        mac  = c( mac ),
        CFR  = mean_ceb,
        TypeEst = 'CEB' ) 
    
    return( res )
  } )

ceb_res_noshift <- 
  do.call( rbind, ceb_list_noshift )

out_ceb_noshift <- 
  merge(
    ceb_data[,
             .( SeriesID, LocID, LocName, LocTypeName, 
                LocAreaTypeName, SubGroupName,  
                SubGroupTypeName, SubGroupCombinationID,
                DataCatalogID, DataCatalogName, DataCatalogShortName, 
                FieldWorkStart, FieldWorkMiddle, DataProcess, 
                DataSourceName, DataSourceAuthor, DataSourceYear, 
                DataSourceShortName, DataStatusName, 
                StatisticalConceptName, DataTypeName, ModelPatternName, 
                DataReliabilityName, PeriodTypeName, PeriodGroupName, 
                TimeUnit, FootNoteID, RegName, RegID, AreaName, AreaID, 
                TimeStart, TimeEnd, TimeMid, TimeLabel,                    
                SexName, SexID ) ] %>% unique,
    ceb_res_noshift[ ,
                   .( SeriesID,
                      DateFormtd = as.Date( date_decimal( year_ceb ) ),
                      year = year_ceb,
                      ages_w,
                      mac,
                      CEB = CFR,
                      TypeEst = 'CEB-No Shift' ) ],
    by = 'SeriesID'
  ) %>%
  setorder( LocID, TimeMid, SeriesID, ages_w )

write.table( out_ceb_noshift, 
             'outputs/ceb_fertest_world_noshift.csv',
             row.names = F )
##################################################

### Plot Results #--------------------------------
data('tfr')

myLocations <- ceb_res_shift$LocID %>% unique

# get wpp 2019 fertility data and compute reference period
wpp_tfr <- 
  tfr %>% setDT %>%
  .[ country_code %in% myLocations ] %>%
  melt( measure.vars  = names(tfr)[3:16],
        id.vars       = c( 'country_code', 'name' ),
        value.name    = 'tfr_wpp',
        variable.name = 'period' ) %>%
  .[, period_mid := ( as.numeric( substr( period, 1, 4 ) ) + 2.5 ) ] %>%
  .[ , 
     .( LocID = country_code, 
        name, 
        year  = period_mid, 
        TFR   = tfr_wpp ) ]

plot_all <- 
  rbind(
    wpp_tfr[, .( SeriesID = 0, 
                 LocID, 
                 name, 
                 year, 
                 TFR, 
                 TypeEst = 'WPP 2019' ) ],
    ceb_res_noshift[, 
                    .( SeriesID, 
                       LocID, 
                       name = LocName,
                       year = year_ceb, 
                       TFR = CFR, 
                       TypeEst = 'CEB-No Shift' ) ],
    ceb_res_shift[, 
                  .( SeriesID, 
                     LocID, 
                     name = LocName,
                     year = year_ceb, 
                     TFR = CFR, 
                     TypeEst = 'CEB-Age Shift' ) ]
  )

pdf( file = 'figs/outputs_ceb_world.pdf', 
     width = 8, height = 6 )
for( loc in sort( plot_all$LocID %>% unique ) ){
  
  aux <- 
    plot_all[ LocID == loc, ] %>% copy
  
  title_name <- 
    paste0( aux$name %>% unique, 
            ' - ', 
            loc )
  
  plot_this <- 
    ggplot() +
    geom_point( data = aux,
                aes( x = as.numeric( year ), 
                     y = as.numeric( TFR ),
                     shape = TypeEst,
                     color = TypeEst ), 
                size  = 2 ) +
    geom_line( data = aux,
               aes( x = as.numeric( year ), 
                    y = as.numeric( TFR ),
                    color = TypeEst, linetype = TypeEst ),
               size  = 0.75 ) +
    scale_y_continuous( breaks = scales::breaks_pretty( 9 ) ) +
    scale_x_continuous( limits = c( 1930, 2020 ), 
                        breaks = seq( 1930, 2030, 10 ) ) +
    scale_color_manual( values = c( 'Abridged-BeersModified' = 'forestgreen',
                                    'CEB-Age Shift' = 'steelblue3',
                                    'CEB-No Shift'  = 'black',
                                    'UNESCO-Demodata'  = 'gray55',
                                    'WPP 2019' = 'tomato3'),
                        name = '' ) +
    scale_shape_manual( values = c( 'Abridged-BeersModified' = 4,
                                    'CEB-Age Shift' = 19,
                                    'CEB-No Shift'  = 15,
                                    'UNESCO-Demodata'   = 16,
                                    'WPP 2019' = 20 ),
                        name = '' ) +
    scale_linetype_manual( values = c( 'Abridged-BeersModified' = 'blank',
                                       'CEB-Age Shift' = 'blank',
                                       'CEB-No Shift'  = 'blank',
                                       'UNESCO-Demodata'  = 'blank',
                                       'WPP 2019' = 'solid' ),
                           name = '' ) +
    labs( title = title_name,
          y = 'TFR', 
          x = 'Year' ) +
    theme_classic() +
    theme(
      legend.position = 'top',
      axis.text = element_text( size = 10, color = 'black' ),
      plot.title = element_text( size = 15, color = 'black' ),
      plot.subtitle = element_text( size = 12, color = 'black' ),
      panel.grid.major = element_line( size = 0.15, color = 'gray81', 
                                       linetype = 5 )
    ) 
  
  print( plot_this )
  
}
dev.off()

t2 <- Sys.time()

print( t2 - t1 )
##################################################
