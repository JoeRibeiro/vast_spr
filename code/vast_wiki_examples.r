# COde adapted from:
#https://github.com/James-Thorson-NOAA/VAST/wiki/Combine-acoustic-and-bottom-trawl-data
#https://github.com/James-Thorson-NOAA/VAST/wiki/Plots-using-ggplot

# Lots to do on this script:
# Need to add in covariate_data of other species. Species co-occurences should define the major predictors for these models
# Need to add in covariate_data of limiting environmental parameters. These won't be strong predictors but may define e.g. northernmost extents. Include: various temperature metrics (stick to static rasters, grave/sand/mud, mean, min and max u+v current, minimum temp over whole water column), mean, min and max primary productivity
# Should catch data be a covariate? This is a double-edged sword as you would expect higher commercial catches where it is more abundant (but also higher pressure / mortality). One to explore at the end possibly. I'm wary of using this as the basis of the model as fishing patterns are strongly influenced by relative profitability, not necessarily abundance.
# Not sure settings$FieldConfig is set up correctly
# Year is being passed when we should probably be passing more accurate times like month. Research how this choice should be fed into spatiotemporal models
# No acoustic data, needs downloading and converting to be the same format
# West channel survey missing, needs downloading
# Do we need to be using haul-level data instead of pre-calculated swept areas?


if(T){
  flexfile_survey_list <- c("FR-CGFS","IE-IAMS", "NIGFS","SCOROC","SP-PORC","SP-NORTH", "NS-IBTS", "EVHOE", "SP-ARSA", "IE-IGFS", "SCOWCGFS")
  drctry = 'C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/SDM_course/data/joe_data/swaf_survey_vignette_tmpdir_ran_25_apr_2023'
  first = T
  for(survey in flexfile_survey_list){
    print(any(grepl(survey,list.files(drctry))))
      loaded=readRDS(paste0(drctry,"\\HH_HL_merged_aphia_",survey,".rds"))
      loaded = loaded[,!colnames(loaded) %in% c("SweptAreaDSKM2","SweptAreaBWKM2")] # drop additional columns in BT
      loaded$survey = survey
      if(first){survey_download_raw = loaded}else{survey_download_raw = rbind(survey_download_raw,loaded)}
      first = F
  }; first = T
  loaded = NULL # rm
  
    
  survey_download_filtered = survey_download_raw[,c('Code','HaulID','StatRec','HaulLong','HaulLat','Nr','Year')]
  survey_download_filtered = survey_download_filtered[survey_download_filtered$Code != '',]

  # # Drop infrequently seen species
  # for(spp in unique(survey_download_filtered$Code)){
  #   thisspp = survey_download_filtered[survey_download_filtered$Code==spp,]
  #   if(nrow(thisspp)<50){
  #     survey_download_filtered = survey_download_filtered[survey_download_filtered$Code!=spp,]
  #   }
  # }

  wider = tidyr::pivot_wider(survey_download_filtered,id_cols = c('HaulID','StatRec','HaulLong','HaulLat','Year'), values_from = 'Nr', names_from = 'Code',  values_fn=sum)
  wider[is.na(wider)] = 0
  
  wider$Lat = wider$HaulLat
  wider$Lon = wider$HaulLong
  wider$AreaSwept_km2 = 1
  wider$Vessel = stringr::str_split_fixed(wider$HaulID, ":", 6)[,5]
  wider$Gear = stringr::str_split_fixed(wider$HaulID, ":", 6)[,1]
  dat = wider#[wider$Code=='SPR',]

}

### A quick demonstration of how to extract map quantities and
### plot them externally. Cole Monnahan | May 2021

library(VAST)                           # 3.8.0
library(ggplot2)                        # 2.10.0
library(dplyr)
library(tidyr)
theme_set(theme_bw())

setwd('C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/vast_spr/outputs')

settings <- make_settings(
         n_x = 100,
         Region = "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/vast_spr/region_shapefile/regionofinterest.shp",
         purpose = "index2",
         bias.correct = FALSE,
         fine_scale = TRUE)
## settings$Options$treat_nonencounter_as_zero <- FALSE
#settings$FieldConfig[2,2] <- 0 # turn off spatiotemporal LP2

### Setup the "combined" part of the model
## Associate depth strata (0 = <0.5m, 1 = 0.5-16m, 2 = >16m) with each
## observation (row). This is two columns b/c there are
## observations from at most 2 strata.

# c_iz <- matrix( c(1,2, 2,NA, 3,NA),
#      byrow = TRUE,
#      nrow = 3,
#      ncol = 2)[unique(dat$Gear)] - 1
# unique(c_iz)

## [1,]    0    1  # bottom trawl
## [2,]    1   NA  # acoustic 0.5-16m
## [3,]    2   NA  # acoustic >16m
### Note that NA means to not sum across it. So the bottom trawl
### is the summation over the first two strata, and the two
### acoustic data sets are only from one stratum.

## Run model, with getReportCovariance = TRUE so the Delta method can be used below
fit <- fit_model(
    settings = settings,
    Lat_i = dat$Lat,
    Lon_i = dat$Lon,
    t_i = dat$Year,
    v_i = dat$Gear,
    b_i = dat$SPR,
    a_i = dat$AreaSwept_km2,
    getReportCovariance = TRUE,
    observations_LL = dat[,c('Lat','Lon')])

## Default plots
plot_results(fit)
#plot_results(fit,category_names = c("Acoustic dead zone","ADZ to Effective fishing height","Effective fishing height to surface"))


