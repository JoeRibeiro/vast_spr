### A quick demonstration of how to extract map quantities and
### plot them externally. Cole Monnahan | May 2021

library(VAST)                           # 3.8.0
library(ggplot2)                        # 2.10.0
library(dplyr)
library(tidyr)
theme_set(theme_bw())

setwd('C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/vast_spr/code')

### Run a simple VAST model for a few years
example <- load_example( data_set="EBS_pollock" )
dat <- subset(example$sampling_data, Year>2008)
years <- unique(dat$Year)
nyrs <- length(years)
settings <- make_settings(n_x = 75,
            Region = example$Region,
            purpose = "index2",
            max_cells = Inf,
            strata.limits = example$strata.limits,
            bias.correct = FALSE )
fit <- fit_model(settings = settings,
       Lat_i = dat$Lat,
       Lon_i = dat$Lon,
       t_i = dat$Year,
       getsd = FALSE,
       b_i = dat$Catch_KG,
       a_i = dat$AreaSwept_km2)

## Remake map list locally for recreating plots
mdl <- make_map_info(Region = settings$Region,
                     spatial_list = fit$spatial_list,
                     Extrapolation_List = fit$extrapolation_list)
## quick dirty AK map
ak_map <- subset(map_data("world"), region=='USA' & subregion=='Alaska')
## Have to duplicate it for each year so can facet below
ak_map <- cbind(ak_map[rep(1:nrow(ak_map), times=nyrs),],
                Year=rep(years, each=nrow(ak_map)))
gmap <- ggplot(ak_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="black", colour = "white") +
  scale_color_viridis_c(option = "magma") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  coord_cartesian(xlim=mdl$Xlim, ylim=mdl$Ylim)


## Below shows to you get the model estimate of density, D_gct,
## for each grid (g), category (c; not used here single
## univariate); and year (t); and link it spatially to a lat/lon
## extrapolation point.  You can do this for any _gct or _gc
## variable in the Report.
names(fit$Report)[grepl('_gc|_gct', x=names(fit$Report))]

D_gt <- fit$Report$D_gct[,1,] # drop the category
dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years)
## tidy way of doing this, reshape2::melt() does
## it cleanly but is deprecated
D_gt <- D_gt %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "cell") %>%
    pivot_longer(-cell, names_to = "Year", values_to='D')
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
g <- gmap +
  geom_point(data=D, aes(Lon, Lat, color=log(as.vector(D)), group=NULL),
             ## These settings are necessary to avoid
             ## overlplotting which is a problem here. May need
             ## to be tweaked further.
             size=.3, stroke=0,shape=16) + facet_wrap('Year')
g

## Or you can do it in base R, building your own palette and
## looping through years as needed.














library(VAST)

## Read in prepared data which comes from a simulated example
## loosely conditioned on EBS pollock.

### !! THIS IS NOT REAL DATA !! ###
data(acoustic_and_trawl, package = "FishStatsUtils" )
dat <- subset(acoustic_and_trawl, Year<2012)
str(dat)       # note Gear reflects different data sets
## Make default settings for index standardization
settings <- make_settings(
         n_x = 100,
         Region = "eastern_bering_sea",
         purpose = "index2",
         bias.correct = FALSE,
         fine_scale = TRUE)
## settings$Options$treat_nonencounter_as_zero <- FALSE
settings$FieldConfig[2,2] <- 0 # turn off spatiotemporal LP2

### Setup the "combined" part of the model
## Associate depth strata (0 = <0.5m, 1 = 0.5-16m, 2 = >16m) with each
## observation (row). This is two columns b/c there are
## observations from at most 2 strata.
c_iz <- matrix( c(1,2, 2,NA, 3,NA),
     byrow = TRUE,
     nrow = 3,
     ncol = 2)[as.numeric(dat$Gear)] - 1
unique(c_iz)
## [1,]    0    1  # bottom trawl
## [2,]    1   NA  # acoustic 0.5-16m
## [3,]    2   NA  # acoustic >16m
### Note that NA means to not sum across it. So the bottom trawl
### is the summation over the first two strata, and the two
### acoustic data sets are only from one stratum.

## Run model, with getReportCovariance = TRUE so the Delta method
## can be used below
fit <- fit_model(
    settings = settings,
    Lat_i = dat$Lat,
    Lon_i = dat$Lon,
    t_i = dat$Year,
    c_i = c_iz,
    b_i = dat$Catch_KG,
    a_i = dat$AreaSwept_km2,
    getReportCovariance = TRUE)

## Default plots
plot(fit,
    category_names = c("Acoustic dead zone","ADZ to Effective fishing height","EFH to surface") )

## Can grab index SEs directly from VAST output in the case of
## the depth strata (VAST categories)
years <- sort(unique(dat$Year))
nyr <- length(years)
SD <- fit$parameter_estimates$SD
tmp <- which(names(SD$value) %in% 'Index_ctl')
est <- fit$Report$Index_ctl[,,1]
index.strata <- data.frame(
             year = rep(years, each = 3)+c(-.05, 0, .05),
             stratum = c('<0.5 m', '0.5-16 m', '>16 m'),
             index = SD$value[tmp],
             se = SD$sd[tmp])


## But need to manually calculate SE for the total biomass index
## and two gear indices. Since it's a sum of the three the
## derivatives are all 1 and so the SE is the sqrt of the sum of
## all of the variances and covariances. This feature is not
## coded into VAST yet so have to do it manually. also note that
## the order of the Index_cyl matrix in vector form is Index_11,
## Index_21, Index_31, Index_12,.. etc. This effects the
## subsetting below
cov.index <- SD$cov[tmp,tmp]
## combined is all three strata summed
index1 <- data.frame( year = years-.05,
          gear = 'total',
          index = apply(est[1:3,], 2, sum),
          se = sqrt(sapply(1:nyr, function(i) {j = 1:3+3*(i-1); sum(cov.index[j,j])})))
## sum the top two to get what the ATS sees
index2 <- data.frame( year = years,
          gear = 'AT',
          index = apply(est[2:3,], 2, sum),
          se = sqrt(sapply(1:nyr, function(i) {j = (1:3+3*(i-1))[-1]; sum(cov.index[j,j])})))
## likewise the BTS is just the first strata
index3 <- data.frame( year = years+.05,
          gear = 'BT',
          index = apply(est[1:2,], 2, sum),
          se = sqrt(sapply(1:nyr, function(i) {j = (1:3+3*(i-1))[-3]; sum(cov.index[j,j])})))
index.gear <- rbind(index1,index2, index3)

if(require(ggplot2)){
  ggplot(index.strata,
    aes(year, index, color = stratum, ymin = index-1.96*se,
                           ymax = index+1.96*se)) + geom_linerange(lwd = 1) +
    geom_line(lwd = 2)
  ggplot(index.gear,
    aes(year, index, color = gear, ymin = pmax(0,index-1.96*se), ymax = index+1.96*se)) +
    geom_linerange(lwd = 1) + geom_line(lwd = 2)
}

## Further applications of the Delta method could be used to get
## other derived quantities in a similar manner.