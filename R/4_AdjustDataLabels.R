##################################################
### Title: Adjust data labels and save series
### Author: Jose H C Monteiro da Silva
### Last Update: 2020-11-20
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

### read data #-----------------------------------

### Unesco Labels
data_unesco_base <- 
  fread( 'data/world_unesco_demodata_children_age.csv' ) %>%
  .[ ,
     .( LocID, SeriesID, LocName, DataCatalogID, DataCatalogName, 
        DataCatalogShortName, DataSourceName, 
        DataSourceAuthor, DataSourceYear, 
        DataSourceShortName ) ] %>% 
  unique

### Unesco data for children and WPP data for adult females
outRevSurv_UnescoWPP <- 
  fread( 'outputs/reverse_survival_fertest_world_unesco_popwpp.csv' )

### Unesco data for children and Census demodata for adult females
outRevSurv_UnescoDemodata <- 
  fread( 'outputs/reverse_survival_fertest_world_unesco_all.csv' )
##################################################

### Adjust labels for datasets #------------------
outRevSurv_UnescoDemodata <- 
  outRevSurv_UnescoDemodata %>%
  merge(
    data_unesco_base[ , .( LocID, SeriesID, LocName, 
                           DataCatalogID, DataCatalogName, 
                           DataCatalogShortName, DataSourceName, 
                           DataSourceAuthor, DataSourceYear,
                           DataSourceShortName ) ] %>% unique,
    by = c( 'LocID', 'SeriesID' )
    ) %>%
  .[,.( SeriesID, LocID, LocName, DataCatalogID,
        DataCatalogName, 
        DataCatalogShortName, DataSourceName, 
        DataSourceAuthor, DataSourceYear, 
        DataSourceShortName, 
        DateFormtd,
        year, TFR, births, 
        TypeEst = 'UNESCO-Demodata', Dummy,
        SeriesID_females = SeriesIDfem ) ]

outRevSurv_UnescoWPP <- 
  outRevSurv_UnescoWPP %>%
  merge(
    data_unesco_base[ , .( LocID, SeriesID, LocName, 
                           DataCatalogID, DataCatalogName, 
                           DataCatalogShortName, DataSourceName, 
                           DataSourceAuthor, DataSourceYear,
                           DataSourceShortName ) ] %>% unique,
    by = c( 'LocID', 'SeriesID' )
  ) %>%
  .[,.( SeriesID, LocID, LocName, DataCatalogID,
        DataCatalogName, 
        DataCatalogShortName, DataSourceName, 
        DataSourceAuthor, DataSourceYear, 
        DataSourceShortName, 
        DateFormtd,
        year, TFR, births, 
        TypeEst = 'UNESCO-WPP2019', 
        Dummy,
        SeriesID_females = NA ) ]

##################################################

### Merge and save data #-------------------------

outRevSurvUnesco <- 
  rbind(
    outRevSurv_UnescoWPP,
    outRevSurv_UnescoDemodata
  )

write.table( outRevSurvUnesco,
             file = 'outputs/reverse_survival_fertest_world_unesco.csv',
             row.names = FALSE )
##################################################

