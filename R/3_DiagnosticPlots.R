##################################################
### Title: Plot estimates
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-09-17
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

outRevSurv <- 
  fread( 'outputs/reverse_survival_fertest_latin_america.csv')

data('tfr')

myLocations <- outRevSurv$LocID %>% unique

# get wpp 2019 fertility data and compute reference period
wpp_tfr <- 
  tfr %>% setDT %>%
  .[ country_code %in% myLocations ] %>%
  melt( measure.vars  = names(tfr)[3:16],
        id.vars       = c( 'country_code', 'name' ),
        value.name    = 'tfr_wpp',
        variable.name = 'period' ) %>%
  .[, period_mid := ( as.numeric( substr( period, 1, 4 ) ) +
                        ( as.numeric( substr( period, 1, 4 ) ) - 
                            as.numeric( substr( period, 6, 9 ) ) ) / 2 + 0.5 ) ] %>%
  .[ , 
     .( LocID = country_code, 
        name, 
        year  = period_mid, 
        TFR   = tfr_wpp ) ]

# prepare plot database with location names
plot_out <- 
  outRevSurv %>% 
  merge( wpp_tfr[, .( name, LocID ) ] %>% unique,
         by = 'LocID' )

pdf( file = 'figs/outputs_revsurv_latin_america.pdf', 
     width = 8, height = 6 )
for( loc in ( plot_out$LocID %>% unique ) ){
  
  aux <- plot_out[ LocID == loc ]
  wpp_aux <- wpp_tfr[ LocID == loc ]
  title_name <- paste0( aux$name %>% unique, ' - ', loc )
  
  if( loc == 740 ){
    subt = '*Removed 1963 estimate of Suriname 1964 Census due to implausible 0-1 children counts'
    aux <- aux[ TFR < 20, ]
  } else{
    subt = ''
  }
  
  plot_this <- 
    ggplot() +
    geom_point( data = aux,
                aes( x = as.numeric( year ), 
                     y = as.numeric( TFR ) ), 
                size  = 1.75,
                shape = 1,
                color = 'black') +
    geom_line( data = wpp_aux,
               aes( x = as.numeric( year ), y = as.numeric( TFR ) ),
               color = 'tomato3',
               size  = 1.25 ) +
    geom_line( data = data.frame( x = c( 1930, 1933 ), y = c( 1.80, 1.80 ) ),
               aes( x = x, y = y ),
               size = 1.25, 
               color = 'tomato3' ) +
    geom_point( data = data.frame( x = c( 1931.5 ), y = c( 1.5 ) ),
                aes( x = x, y = y ),
                size = 1.75, 
                shape = 1,
                color = 'black' ) +
    annotate( 'text', x = 1933.25, y = 1.80, label = 'WPP 2019', hjust = 0 ) +
    annotate( 'text', x = 1933.25, y = 1.50, label = 'Reverse Survival', hjust = 0 ) +
    scale_y_continuous( breaks = scales::breaks_pretty( 9 ) ) +
    scale_x_continuous( limits = c( 1930, 2030 ), 
                        breaks = seq( 1930, 2030, 10 ) ) +
    scale_color_manual( values = c( 'revsurv' = 'black',
                                    'wpp'     = 'red' ),
                        labels = c( 'revsurv' = 'Reverse Survival\nEstimates',
                                    'wpp'     = 'WPP 2019' ),
                        name = '' ) +
    labs( title = title_name, 
          subtitle = subt,
          y = 'TFR', 
          x = 'Year' ) +
    theme_classic() +
    theme(
      legend.position = 'top',
      axis.text = element_text( size = 10, color = 'black' ),
      plot.title = element_text( size = 15, color = 'black' ),
      plot.subtitle = element_text( size = 12, color = 'black' ),
      panel.grid.major = element_line( size = 0.15, color = 'gray81', linetype = 5 )
    ) 
  
  print( plot_this )
  
}
dev.off()
##################################################
