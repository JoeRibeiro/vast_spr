#https://github.com/James-Thorson-NOAA/VAST/wiki/MICE-in-space

# load libraries
library(VAST)

setwd('C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/vast_spr/outputs')


# load data
example = load_example( "GOA_MICE_example" )

# Get settings
settings = make_settings( n_x = 50,
           purpose = "MICE",
           Region = example$Region,
           n_categories = nlevels(example$sampling_data$spp) )
#settings$VamConfig['Timing'] = 0

# Modify defaults
settings$VamConfig['Rank'] = 1  # Reduce to single axis of ratio-dependent interactions
settings$fine_scale = FALSE  # Make it run faster

# Load previous estimates to speed it up
  # Requires loading the previous Kmeans results to get identical results
test_path = file.path(system.file("extdata", package="VAST"),"GOA_MICE_example")
load( file.path(test_path,"saved_estimates.RData") )

# Run model
  # May take many hours, even given informative starting value
Fit = fit_model( settings = settings,
      Lat_i = example$sampling_data[,'Lat'],
      Lon_i = example$sampling_data[,'Lon'],
      b_i = example$sampling_data[,'Catch_KG'],
      a_i = example$sampling_data[,'AreaSwept_km2'],
      v_i = as.numeric(example$sampling_data[,'Vessel']),
      t_i = example$sampling_data[,'Year'],
      c_i = as.numeric(example$sampling_data[,'spp'])-1,
      F_ct = example$F_ct,
      newtonsteps = 0,
      getsd = FALSE,
      startpar = parameter_estimates$par,
      working_dir = test_path )

# Fix issue in auto-generated plot labels
Fit$year_labels = c( 1983, Fit$year_labels )
Fit$years_to_plot = c(1, Fit$years_to_plot+1)

# Plots
plot( Fit )
