# Lots to do on this script:
# I can run it when I use the settings "Modify settings to allow model to run faster for demo" as per the Thorsen demo. But when I apply Chibuzor's settings it breaks.
# Speak to Nicola about how to do this type of modelling correctly.
# It only has two years of data. Try fudging some data to add more years in?
# Need to add in covariate_data of limiting environmental parameters. These won't be strong predictors but may define e.g. northernmost extents. Include: various temperature metrics (stick to static rasters, grave/sand/mud, mean, min and max u+v current, minimum temp over whole water column), mean, min and max primary productivity
# Should catch data be a covariate? This is a double-edged sword as you would expect higher commercial catches where it is more abundant (but also higher pressure / mortality). One to explore at the end possibly. I'm wary of using this as the basis of the model as fishing patterns are strongly influenced by relative profitability, not necessarily abundance.
# Not sure settings$FieldConfig is set up correctly
# Year is being passed when we should probably be passing more accurate times like month. Research how this choice should be fed into spatiotemporal models
# No acoustic data, needs downloading and converting to be the same format
# West channel survey missing, needs downloading
# Do we need to be using haul-level data instead of pre-calculated swept areas?

# Download release number 3.0.0; its useful for reproducibility to use a specific release number
#devtools::install_github("james-thorson-NOAA/VAST")

library(dplyr)
# These groups won't necessarily be honored, but using this approach is useful to gauge how many groups you want the model to end up with
speciesgroups = list(c("SPR","HER"),c("PIL","ANE"),c("WHB","MAC","WHG","HOM"),c("POL","HKE","COD"))
your_species = unlist(speciesgroups)
your_categories = length(speciesgroups)
#your_species = c("SPR","HER","PIL","ANE","WHB","MAC")

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

  # Sum across length classes
  survey_download_filtered <- aggregate(Nr ~ Code + HaulID + StatRec + Year + HaulLat + HaulLong, data = survey_download_filtered, mean) 
  
  
  # # Drop infrequently seen species
  # for(spp in unique(survey_download_filtered$Code)){
  #   thisspp = survey_download_filtered[survey_download_filtered$Code==spp,]
  #   if(nrow(thisspp)<50){
  #     survey_download_filtered = survey_download_filtered[survey_download_filtered$Code!=spp,]
  #   }
  # }

  #wider = tidyr::pivot_wider(survey_download_filtered,id_cols = c('HaulID','StatRec','HaulLong','HaulLat','Year'), values_from = 'Nr', names_from = 'Code',  values_fn=sum)
#  survey_download_filtered[is.na(survey_download_filtered)] = 0
  
  survey_download_filtered$Catch_KG = survey_download_filtered$Nr
  survey_download_filtered$Lat = survey_download_filtered$HaulLat
  survey_download_filtered$Lon = survey_download_filtered$HaulLong
  survey_download_filtered$AreaSwept_km2 = 1
  survey_download_filtered$Vessel = stringr::str_split_fixed(survey_download_filtered$HaulID, ":", 6)[,5]
  survey_download_filtered$Gear = stringr::str_split_fixed(survey_download_filtered$HaulID, ":", 6)[,1]

  spps = unique(survey_download_filtered$Code)

  dat = survey_download_filtered
  dat = dat[dat$Code %in% your_species,]

  # make species numbers consecutive
  for( nn in 1:length(your_species)){
    dat$species_number[dat$Code==your_species[nn]]=nn
  }  
  
  if(T){     # We have no zeros in our data. Try adding them in from the other species presences
    # I don't see why this is neccessary as we have treat_nonencounter_as_zero 
    # This was written To address error: Some years and/or categories have 100% encounters, and this requires either temporal structure of a different link-function
    # Even with treat_nonencounter_as_zero it still seems to increase the chances of the model converging

    # Drop cols
    dat = dat[,c("species_number", "Year"  , "Catch_KG", "AreaSwept_km2",      "Lat"     ,  "Lon", "HaulID")]
  
    for_cols = c('HaulID','species_number')#colnames(dat)[!colnames(dat) %in% c("Nr","Catch_KG")]
    dat2 <- dat[,for_cols] %>% tidyr::complete(!!!syms(for_cols))
  
    # pull over haul details
    dat2 = merge(dat2,dat[,c("Year"  ,  "AreaSwept_km2",      "Lat"     ,  "Lon", "HaulID")], by = 'HaulID', all.x = F, all.y = F, no.dups = TRUE)
    dat2 = dat2[!duplicated(dat2), ] # Not sure why there are duplicates here, I am doing everything possible in the merge to stop that?
    
    # Pull over presence data
    dat2 = merge(dat2,dat[,c("species_number","HaulID","Catch_KG")], by = c("species_number","HaulID"), all.x = T, no.dups = TRUE)
    dat2$Catch_KG[is.na(dat2$Catch_KG)]=0  
  
    dat = dat2
    dat2 <- NULL
  }
  # Drop cols
  dat = dat[,c("species_number", "Year"  , "Catch_KG", "AreaSwept_km2",      "Lat"     ,  "Lon")]
  

}

# Decide where to run and save results
setwd('C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/vast_spr/outputs')

# Load packages
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data
# that are installed automatically with `FishStatsUtils`.
#example = load_example( data_set="five_species_ordination" )
example = list()
example$Region = "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/vast_spr/region_shapefile/regionofinterest.shp"
example$sampling_data = as.matrix(dat)
example$strata.limits = data.frame(STRATA = "All_areas")


# Just run for 2 years
example[["sampling_data"]] = example[["sampling_data"]][example[["sampling_data"]][,2]<2015,]



# Make settings
settings = make_settings(n_x = 50, 
  Region = example$Region, 
  purpose = "ordination",
  strata.limits = example$strata.limits, 
  n_categories = your_categories,
  treat_nonencounter_as_zero = T)

settings$ObsModel <- c(1,0)#lognormal positive and binomial with logit-link for encounter probability

## Other observation model (link functions and distributions) that could be used depending on the data:
#ObsModel <- c(2, 0) ## gamma positive and binomial with logit-link for encounter probability
#ObsModel <- c(2, 1) ## gamma positive and binomial with poisson for encounter probability

###---Model parameters: chibuzor's settings
# OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0) #overdispersion turned off
# settings$FieldConfig <- c("Omega1" = 0, "Epsilon1" = 2, "Omega2" = 0, "Epsilon2" =0)
# settings$RhoConfig <- c("Beta1" = 2, "Beta2" = 0, "Epsilon1" = 2, "Epsilon2" = 0)

###---Model parameters: chibuzor's settings, modified. Runs but doesn't return any cross corr
# OverdispersionConfig <- c("Eta1" = 0, "Eta2" = 0) #overdispersion turned off
# settings$FieldConfig <- c("Omega1" = 0, "Epsilon1" = 2, "Omega2" = 0, "Epsilon2" =0)
# settings$RhoConfig <- c("Beta1" = 3, "Beta2" = 0, "Epsilon1" = 2, "Epsilon2" = 0)


# # Modify settings to allow model to run faster for demo 
settings$FieldConfig['Beta',] = "IID" # specifies that a model component (temporal variation) is correlated following an first-order autoregressive process
settings$FieldConfig['Epsilon',] = 0 # Turns off spatio-temporal variation component
settings$RhoConfig[] = 0 # Set all to a fixed effect



# Initially run with build_model = FALSE to get number of parameters
fit_orig = fit_model(settings = settings, 
                     Lat_i = example$sampling_data[,'Lat'], 
                     Lon_i = example$sampling_data[,'Lon'],
                     t_i = example$sampling_data[,'Year'], 
                     c_i = as.numeric(example$sampling_data[,"species_number"])-1,
                     b_i = example$sampling_data[,'Catch_KG'], 
                     a_i = example$sampling_data[,'AreaSwept_km2'],
                     newtonsteps = 0,
                     getsd = FALSE,
                      build_model = FALSE )

# Extract default constructions
#data_custom = fit_orig$data_list
map_custom = fit_orig$tmb_list$Map
parameters_custom = fit_orig$tmb_list$Parameters
#parameters_custom[["L_beta1_z"]]= c(0,0,0,0,0,0)
#map_custom[["L_beta2_z"]] = as.factor(0)

# Run model
fit = fit_model( settings = settings, 
  Lat_i = example$sampling_data[,'Lat'], 
  Lon_i = example$sampling_data[,'Lon'],
  t_i = example$sampling_data[,'Year'], 
  c_i = as.numeric(example$sampling_data[,"species_number"])-1,
  b_i = example$sampling_data[,'Catch_KG'], 
  a_i = example$sampling_data[,'AreaSwept_km2'],
  newtonsteps = 2,
  getsd = FALSE,
  Map=map_custom)#,
  #Parameters = parameters_custom)


# Plot results
#results = plot( fit,  plot_set = c(1,2,3,6:9,11:21),  category_names = your_species )
results = plot( fit,  plot_set = c(3),  category_names = your_species )

# Plot correlations (showing Omega1 as example)
require(corrplot)
Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
rownames(Cov_omega1) = your_species
colnames(Cov_omega1) = your_species
corrplot( cov2cor(Cov_omega1), method="pie", type="lower")


bmp(file="crosscorrelationsplot.bmp",
width=6, height=4, units="in", res=100)
corrplot.mixed(cov2cor(Cov_omega1) )
dev.off()

