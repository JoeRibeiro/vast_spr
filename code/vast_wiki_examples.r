### A quick demonstration of how to extract map quantities and
### plot them externally. Cole Monnahan | May 2021

library(VAST)                           # 3.8.0
library(ggplot2)                        # 2.10.0
library(dplyr)
library(tidyr)
theme_set(theme_bw())


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