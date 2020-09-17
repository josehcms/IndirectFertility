### Old

revsurv <- 
  lapply( series, function( x ){
    
    aux_c <- pop_child[ SeriesID == x ]
    aux_f <- pop_females[ SeriesID == x ]
    
    date_ref <- unique( c( aux_f$DateFormtd, aux_c$DateFormtd )  )
    date_ref_dec <- decimal_anydate( date_ref )
    loc <- aux_c$LocID %>% unique
    
    popx5_w = aux_f$pop_w
    ages5_w = aux_f$Age %>% unique
    popx1_c = aux_c$pop_c
    ages1_c = aux_c$Age %>% unique
    
    # 1) get child 0-5 mortality probability for reference period
    #    reference period - 5 and reference period - 10
    if( date_ref_dec < 1955 ){
      ltb_ref <- FetchLifeTableWpp2019( locations = loc,
                                        year = 1955 ,
                                        sex = 'both' )
    } else {
      ltb_ref <- FetchLifeTableWpp2019( locations = loc,
                                        year = date_ref_dec,
                                        sex = 'both' )
    }
    
    if( ( date_ref_dec - 5 ) < 1955 ){
      ltb_ref5 <- FetchLifeTableWpp2019( locations = loc,
                                         year = 1955 ,
                                         sex = 'both' )
    } else {
      ltb_ref5 <- FetchLifeTableWpp2019( locations = loc,
                                         year = ( date_ref_dec - 5 ),
                                         sex = 'both' )
    }
    
    if( ( date_ref_dec - 10 ) < 1955 ){
      ltb_ref10 <- FetchLifeTableWpp2019( locations = loc,
                                          year = 1955 ,
                                          sex = 'both' )
    } else {
      ltb_ref10 <- FetchLifeTableWpp2019( locations = loc,
                                          year = ( date_ref_dec - 10 ),
                                          sex = 'both' )
    }
    
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
    
    # 2) get female q15_45 mortality probability for reference period
    #    reference period - 5 and reference period - 10
    
    if( date_ref_dec < 1955 ){
      ltf_ref <- FetchLifeTableWpp2019( locations = loc,
                                        year = 1955,
                                        sex = 'female' )
    } else{
      ltf_ref <- FetchLifeTableWpp2019( locations = loc,
                                        year = date_ref_dec,
                                        sex = 'female' )
    }
    
    if( ( date_ref_dec - 5 ) < 1955 ){
      ltf_ref5 <-  FetchLifeTableWpp2019( locations = loc,
                                          year = 1955,
                                          sex = 'female' )
    } else{
      ltf_ref5 <- FetchLifeTableWpp2019( locations = loc,
                                         year = ( date_ref_dec - 5 ),
                                         sex = 'female' )
    }
    
    if( ( date_ref_dec - 10 ) < 1955 ){
      ltf_ref10 <-  FetchLifeTableWpp2019( locations = loc,
                                           year = 1955,
                                           sex = 'female' )
    } else{
      ltf_ref10 <- FetchLifeTableWpp2019( locations = loc,
                                          year = ( date_ref_dec - 10 ),
                                          sex = 'female' )
    }
    
    q1  <-  
      ( ltf_ref$lx[ ltf_ref$x == 15 ] - ltf_ref$lx[ ltf_ref$x == 45 ] ) /
      ltf_ref$lx[ ltf_ref$x == 15 ]
    q2  <-  
      ( ltf_ref5$lx[ ltf_ref5$x == 15 ] - ltf_ref5$lx[ ltf_ref5$x == 45 ] ) /
      ltf_ref5$lx[ ltf_ref5$x == 15 ]
    q3  <-  
      ( ltf_ref10$lx[ ltf_ref10$x == 15 ] - ltf_ref10$lx[ ltf_ref10$x == 45 ] ) /
      ltf_ref10$lx[ ltf_ref10$x == 15 ]
    
    q15_45f <- c( q1, q2, q3 )
    
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
    out <- 
      data.table(
        LocID = loc,
        SeriesID = x,
        DateFormtd = date_ref,
        FertRevSurv( ages1_c, popx1_c, ages5_w, popx5_w, lx1_c, lx5_w,
                     asfr, asfr_15prior,
                     q0_5, q15_45f, date_ref )
      )
    
    return( out )
  } )

outRevSurv <- do.call( rbind, revsurv )
data('tfr')

myLocations <- outRevSurv$LocID %>% unique

wpp_tfr <- 
  tfr %>% setDT %>%
  .[ country_code %in% myLocations ] %>%
  melt( measure.vars  = names(tfr)[3:16],
        id.vars       = c( 'country_code', 'name' ),
        value.name    = 'tfr_wpp',
        variable.name = 'period' ) %>%
  .[, period_mid := ( as.numeric( substr( period, 1, 4 ) ) +
                        ( as.numeric( substr( period, 1, 4 ) ) - 
                            as.numeric( substr( period, 6, 9 ) ) ) / 2 ) ] %>%
  .[ , 
     .( LocID = country_code, 
        name, 
        year  = period_mid, 
        TFR   = tfr_wpp ) ]

plot_out <- 
  outRevSurv %>% 
  merge( wpp_tfr[, .( name, LocID ) ] %>% unique,
         by = 'LocID' )

pdf( file = 'figs/outputs_revsurv_latin_america_old.pdf', width = 8, height = 6 )
for( loc in ( plot_out$LocID %>% unique ) ){
  
  aux <- plot_out[ LocID == loc ]
  wpp_aux <- wpp_tfr[ LocID == loc ]
  title_name <- paste0( aux$name %>% unique, ' - ', loc )
  
  plot_this <- 
    ggplot() +
    geom_point( data = aux,
                aes( x = as.numeric( year ), 
                     y = as.numeric( TFR ), color = 'revsurv' ), size = 1.5 ) +
    geom_line( data = wpp_aux,
               aes( x = as.numeric( year ), y = as.numeric( TFR), 
                    color = 'wpp' ),
               size = 1.25 ) +
    scale_y_continuous( breaks = scales::breaks_pretty( 9 ) ) +
    scale_x_continuous( limits = c( 1930, 2030 ), 
                        breaks = seq( 1930, 2030, 10 ) ) +
    scale_color_manual( values = c( 'revsurv' = 'black',
                                    'wpp'     = 'red' ),
                        labels = c( 'revsurv' = 'Reverse Survival\nEstimates',
                                    'wpp'     = 'WPP 2019' ),
                        name = '' ) +
    labs( title = title_name, 
          y = 'TFR', 
          x = 'Year' ) +
    theme_classic() +
    theme(
      legend.position = 'top',
      axis.text = element_text( size = 10, color = 'black' ),
      plot.title = element_text( size = 15, color = 'black' ),
      panel.grid.major = element_line( size = 0.15, color = 'gray81', linetype = 5 )
    ) 
  
  print( plot_this )
  
}
dev.off()

x <- 
  get_recorddata(dataProcessTypeIds = 6, # Register
                 startYear = 1920,
                 endYear = 2020,
                 indicatorTypeIds = 15, # Births by sex
                 isComplete = 0, # Total
                 locIds = 604, # Antigua and Barbuda
                 locAreaTypeIds = 2, # Whole area
                 subGroupIds = 2 ) 
x %>% setDT
x[,.N,.(SeriesID,DataSourceShortName)]

x$DataSourceName %>% unique

x[SeriesID == 99194613031]

## get_seriesdata(): Get information about available details for a given series of data
## 
## get_dataprocess(): Get information about available data-types (DataProcessTypeID)
## 
## get_iitypes():     Get information about available indicators (IndicatorID) and indicatortypeids (IndicatorTypeId)
## get_indicators():     Get information about available indicators (IndicatorID)
## get_indicatortypes(): Get information about available indicators (IndicatorTypeID)
## 
## get_locations():     Get information about available locations (LocID)
## get_locationtypes(): Get information about available location types (LocAreaTypeID)
## 
## get_subgroups():     Get information about available sub-group-types (SubGroupTyp
get_subgroups()

res <- get_recorddata( dataProcessTypeIds = 2,  ## "Census"
                       startYear = 1950,
                       endYear = 1965,
                       indicatorTypeIds = 8,    ## "Population by age and sex"
                       isComplete = 1,          ## 0 = Abridged or 1 = Complete
                       locIds = 740,              ## "Brazil"
                       locAreaTypeIds = 2,      ## "Whole area"
                       subGroupIds = 2 )        ## "Total or All groups"
setDT( res )

res[,.(LocName,SeriesID,SexID,FieldWorkMiddle,AgeLabel,DataValue)] %>% View

res %>% View
