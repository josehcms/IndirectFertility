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

# select IDs to remove after visual inspection
manual_rmv_ids_x5 <-
  ( fread( 'aux_docs/manually_removed_ids_x5.csv' ) %>%
      .[ Remove == 'yes', ] )$SeriesID %>% as.numeric

outRevSurvx5 <- 
  fread( 'outputs/reverse_survival_fertest_world_x5.csv' ) %>%
  .[ !( as.numeric( SeriesID ) %in% manual_rmv_ids_x5 ) ]

outRevSurv <- 
  fread( 'outputs/reverse_survival_fertest_world_unesco_popwpp.csv' )

outRevSurvUnesco <- 
  fread( 'outputs/reverse_survival_fertest_world_unesco_all.csv' )


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
  .[, period_mid := ( as.numeric( substr( period, 1, 4 ) ) + 2.5 ) ] %>%
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
    plot_out[, .( SeriesID, LocID, name, year, TFR, TypeEst = 'UNESCO-WPP' ) ],
    plot_outUnesco[, .( SeriesID, LocID, name, year, TFR, TypeEst = 'UNESCO-Demodata' ) ],
    plot_outx5[, .( SeriesID, LocID, name, year, TFR, TypeEst ) ]
  )

pdf( file = 'figs/outputs_revsurv_world_unesco_all.pdf', 
     width = 8, height = 6 )
for( loc in sort( plot_all$LocID %>% unique ) ){
  
  aux <- plot_all[ LocID == loc & TypeEst %in% c( 'WPP 2019', 
                                                  'UNESCO-WPP', 
                                                  'UNESCO-Demodata' ) ]
  
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
                size  = 1.75 ) +
    geom_line( data = aux,
               aes( x = as.numeric( year ), 
                    y = as.numeric( TFR ),
                    color = TypeEst, linetype = TypeEst ),
               size  = 1.25 ) +
    scale_y_continuous( breaks = scales::breaks_pretty( 9 ) ) +
    scale_x_continuous( limits = c( 1930, 2020 ), 
                        breaks = seq( 1930, 2030, 10 ) ) +
    scale_color_manual( values = c( 'Abridged-BeersModified' = 'forestgreen',
                                    'Abridged-Sprague' = 'steelblue3',
                                    'UNESCO-WPP'  = 'black',
                                    'UNESCO-Demodata'  = 'gray55',
                                    'WPP 2019' = 'tomato3'),
                        name = '' ) +
    scale_shape_manual( values = c( 'Abridged-BeersModified' = 4,
                                    'Abridged-Sprague' = 1,
                                    'UNESCO-WPP'  = 18,
                                    'UNESCO-Demodata'   = 16,
                                    'WPP 2019' = 20 ),
                        name = '' ) +
    scale_linetype_manual( values = c( 'Abridged-BeersModified' = 'blank',
                                       'Abridged-Sprague' = 'blank',
                                       'UNESCO-WPP'  = 'blank',
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
##################################################

### check implausible cases

ids <- c( 28, 152, 214, 254, 312, 388, 474, 558, 740, 862 )

myLocations <- 
  ( outRevSurvx5[ LocID %in% ids & TFR > 9, .( LocID, SeriesID ) ] %>% 
      unique )

myPop <- 
  lapply( ids, function(x) {
    
    filterSeries <- myLocations[ LocID == x ]$SeriesID
    
    res <- get_recorddata( dataProcessTypeIds = 2,  ## "Census"
                           startYear = 1950,
                           endYear = 2020,
                           indicatorTypeIds = 8,    ## "Population by age and sex"
                           isComplete = 0,          ## 0 = Abridged or 1 = Complete
                           locIds = x,              ## "Brazil"
                           locAreaTypeIds = 2,      ## "Whole area"
                           subGroupIds = 2 )        ## "Total or All groups"
    setDT( res )
    
    # select specific variables and sort
    resFilt <- 
      res[ as.numeric( SeriesID ) %in%  as.numeric( filterSeries ), 
          list( SeriesID, LocID, LocName,  DataCatalogName, DataSourceName, 
                DataSourceAuthor, DataSourceYear, 
                TimeStart, TimeMid, TimeLabel, SexID, AgeStart,                         ## data values
                AgeEnd, AgeLabel, DataValue ) ] %>%
      setorder( LocID, SeriesID, SexID, AgeStart )
    
    # split totals and unkown in different columns
    resFilt[, DataValueUnknown := DataValue[ AgeLabel == 'Unknown' ],
            .( LocID, SeriesID, SexID ) ]
    resFilt[, DataValueTotal := DataValue[ AgeLabel == 'Total' ],
            .( LocID, SeriesID, SexID ) ]
    # if Total is missing assign DataValue sum
    resFilt[, DataValueTotal := ifelse( is.na( DataValueTotal ), 
                                        sum( DataValue ),
                                        DataValueTotal ),
            .( LocID, SeriesID, SexID ) ]
    # remove totals and unknown labels from database AgeLabel and Both Sexes counts
    resFilt <- 
      resFilt[ ! ( AgeLabel %in% c( 'Unknown', 'Total' ) ) & 
                 SexID %in% c( 1, 2 ), ] %>%
      setorder( SeriesID, SexID, AgeStart ) 
    
    # return the result
    return( resFilt )
  })

check_pop <- do.call( rbind, myPop )

check_pop[ AgeStart<15,
           .( count = sum( DataValue ) ),
           .( SeriesID,LocID,DataCatalogName,
              DataSourceName,AgeStart,AgeEnd,AgeLabel ) ] %>%
  View
