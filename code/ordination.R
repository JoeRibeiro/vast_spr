# Download release number 3.0.0; its useful for reproducibility to use a specific release number
#devtools::install_github("james-thorson-NOAA/VAST")


# Decide where to run and save results
setwd('C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/vast_spr/outputs')

# Load packages
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data
# that are installed automatically with `FishStatsUtils`.
example = load_example( data_set="five_species_ordination" )

# Make settings
settings = make_settings( n_x = 50, 
  Region = example$Region, 
  purpose = "ordination",
  strata.limits = example$strata.limits, 
  n_categories = 2 )

# Modify settings to allow model to run faster for demo 
settings$FieldConfig['Beta',] = "IID"
settings$FieldConfig['Epsilon',] = 0
settings$RhoConfig[] = 0

# Run model
fit = fit_model( settings = settings, 
  Lat_i = example$sampling_data[,'Lat'], 
  Lon_i = example$sampling_data[,'Lon'],
  t_i = example$sampling_data[,'Year'], 
  c_i = as.numeric(example$sampling_data[,"species_number"])-1,
  b_i = example$sampling_data[,'Catch_KG'], 
  a_i = example$sampling_data[,'AreaSwept_km2'],
  newtonsteps = 0,
  getsd = FALSE )

# Plot results
results = plot( fit,
  plot_set = c(3,16,17),
  category_names = c("pollock", "cod", "arrowtooth", "snow_crab", "yellowfin") )

# Plot correlations (showing Omega1 as example)
require(corrplot)
Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
corrplot( cov2cor(Cov_omega1), method="pie", type="lower")
corrplot.mixed( cov2cor(Cov_omega1) )