##################################################
### Title: Plot estimates from reverse survival
### Author: Jose H C Monteiro da Silva
### Last Update: 2021-06-11
##################################################

### Set up packages and global options #----------

# Clean environment 
rm( list = ls( ) )
graphics.off( )

# List of packages for session
.packages <-  
  c( "data.table", "lubridate", "dplyr", "wpp2019", "ggplot2" )

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if( length( .packages[ !.inst ] ) > 0 ) 
  install.packages( .packages[ !.inst ], dependencies = TRUE )

# Load packages into session 
lapply( .packages, require, character.only = TRUE )

##################################################

### Graphical check #-----------------------------

# select IDs to remove after visual inspection
manual_rmv_ids_x5 <-
  ( fread( 'aux_docs/manually_removed_ids_x5.csv' ) %>%
      .[ Remove == 'yes', ] )$SeriesID %>% as.numeric

outRevSurvx5_graduated <- 
  fread( 'outputs/reverse_survival_fertestr_world_x5_graduated.csv' ) %>%
  .[ !( as.numeric( SeriesID ) %in% manual_rmv_ids_x5 ) ]

outRevSurvx5 <-
  fread( 'outputs/reverse_survival_wppfunction_world_x5_abridged.csv' ) %>%
  .[ !( as.numeric( SeriesID ) %in% manual_rmv_ids_x5 ) ]

outRevSurvUnesco <-
  fread( 'outputs/reverse_survival_fertestr_world_unesco_wpp2019denominator.csv' )

outRevSurvx1 <- 
  fread( 'outputs/reverse_survival_fertestr_world_x1.csv' )

data('tfr')

myLocations <- 
  c( outRevSurvx5_graduated$LocID,
     outRevSurvx5$LocID,
     outRevSurvx1$LocID,
     outRevSurvUnesco$LocID ) %>% unique %>% sort

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

# prepare plot database with location names
plot_outx1 <- 
  outRevSurvx1 %>% 
  merge( wpp_tfr[, .( name, LocID ) ] %>% unique,
         by = 'LocID' )  

plot_outx5_graduated <- 
  outRevSurvx5_graduated %>% 
  merge( wpp_tfr[, .( name, LocID ) ] %>% unique,
         by = 'LocID' )  

plot_outx5 <- 
  outRevSurvx5 %>% 
  merge( wpp_tfr[, .( name, LocID ) ] %>% unique,
         by = 'LocID' )

plot_outUnesco <- 
  outRevSurvUnesco %>% 
  merge( wpp_tfr[, .( name, LocID ) ] %>% unique,
         by = 'LocID' ) 

plot_all <- 
  rbind(
    wpp_tfr[, .( SeriesID = 0, LocID, name, year, TFR, TypeEst = 'WPP 2019' ) ],
    plot_outx1[, .( SeriesID, LocID, name, year, TFR, TypeEst = 'RevSurv-x1' ) ],
    plot_outx5_graduated[, .( SeriesID, LocID, name, year, TFR, TypeEst ) ],
    plot_outx5[, .( SeriesID, LocID, name, year, TFR, TypeEst = 'RevSurv-x5 (Abridged)' ) ],
    plot_outUnesco[, .( SeriesID, LocID, name, year, TFR, TypeEst = 'Educ Enrolment' ) ]
  )

pdf( file = 'figs/outputs_revsurv_world_estimates.pdf', 
     width = 8, height = 6 )
for( loc in sort( plot_all$LocID %>% unique ) ){
  
  aux <- plot_all[ LocID == loc ]
  
  title_name <- paste0( aux$name %>% unique, ' - ', loc )
  
  if( max( aux$TFR ) > 15 | min( aux$TFR ) < 0.5 ){
    filterOutLier <- 
      aux[ TFR > 15 | TFR < 0.5, ]$SeriesID %>%
      unique
    
    aux <- aux[ !( SeriesID %in% filterOutLier ) ]
  }
  
  plot_this <- 
    ggplot() +
    geom_point( data = aux,
                aes( x = as.numeric( year ), 
                     y = as.numeric( TFR ),
                     shape = TypeEst,
                     color = TypeEst ), 
                size  = 2.1 ) +
    geom_line( data = aux,
               aes( x = as.numeric( year ), 
                    y = as.numeric( TFR ),
                    color = TypeEst, linetype = TypeEst ),
               size  = 0.75,
               alpha = 0.50 ) +
    scale_y_continuous( breaks = scales::breaks_pretty( 9 ) ) +
    scale_x_continuous( limits = c( 1930, 2020 ), 
                        breaks = seq( 1930, 2030, 10 ) ) +
    scale_color_manual( values = c( 'Abridged-BeersModified' = 'forestgreen',
                                    'Abridged-Sprague'       = 'steelblue3',
                                    'RevSurv-x5 (Abridged)'  = 'gold',
                                    'RevSurv-x1'             = 'navy',
                                    'Educ Enrolment'         = 'gray55',
                                    'WPP 2019'               = 'tomato3' ),
                        name = '' ) +
    scale_shape_manual( values = c( 'Abridged-BeersModified' = 4,
                                    'Abridged-Sprague'       = 3,
                                    'RevSurv-x5 (Abridged)'  = 19,
                                    'RevSurv-x1'             = 1,
                                    'Educ Enrolment'         = 16,
                                    'WPP 2019'               = 20 ),
                        name = '' ) +
    scale_linetype_manual( values = c( 'Abridged-BeersModified' = 'blank',
                                       'Abridged-Sprague'       = 'blank',
                                       'RevSurv-x5 (Abridged)'  = 'blank',
                                       'RevSurv-x1'             = 'blank',
                                       'Educ Enrolment'         = 'blank',
                                       'WPP 2019'               = 'solid' ),
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
##################################################
