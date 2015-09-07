# Analysis of map data.
# September 7, 2015 by Jonathan Dobres
# Builds on the work of Sean Lorenz at the Domino Data Blog (http://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/)
# For entertainment and educational purposes only. Not a formal analysis.

# 0. Preamble ----
# ::: a. clear workspace ----
rm(list=ls());

# ::: b. load packages ----
#install.packages(c('ggmap', 'plyr', 'reshape2', 'magrittr', 'XML', 'maptools', 'rgdal'));
library(ggmap);
library(plyr);
library(reshape2);
library(magrittr);
library(XML);
library(maptools);
library(rgdal);

# ::: c. disclaimer annotations for figures ----
disclaimer <- annotate(geom='text', label='Informal analysis for entertainment purposes only\nhttp://jdobr.es', x = Inf, y = -Inf, 
                       alpha = 1, size = 3, hjust = 1, vjust = -0.2, lineheight = 1);
disclaimer.map <- annotate(geom='text', label='Informal analysis for entertainment purposes only\nhttp://jdobr.es', x = -100, y = 37, 
                             alpha = 1, size = 3, hjust = 0.5, vjust = 0.5, lineheight = 1, color = 'red');

# 1. Replicate http://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/ ----
# ::: a. prepare collision data ----
# Note that the geocoding is a little lazy and produces some mistakes, such as failing to geocode Washington state.
data.fars <- read.csv('Data/vehicle-accidents.csv') %>% 
  subset(State != 'USA') %>% 
  mutate(State = as.character(State), lon = geocode(State, source = 'google')[1], lat = geocode(State, source = 'google')[2]);
  
data.mv.collisions <- data.frame(
  state = data.fars$State,
  collisions = data.fars$MV.Number,
  lon = data.fars$lon,
  lat = data.fars$lat
);

# ::: b. get map of United States ----
usa.center <- geocode('United States', source = 'google') %>% as.numeric;
map.usa <- ggmap(get_googlemap(center=usa.center, scale=2, zoom=4));

# ::: c. create plot ----
plot.fars <- map.usa +
  geom_point(aes(x = lon, y = lat, size = collisions), data = data.mv.collisions, col = 'orange', alpha = 0.4, show_guide=F) +
  scale_size_continuous(range = c(2, 20)) +
  disclaimer.map;
ggsave('Output/accident-map-1.jpg', plot.fars, width = 6, height = 6, dpi = 300);

# 2. plot collision data normalized by state's population ----
# ::: a. load population data ----
data.pop <- read.csv('Data/state-populations.csv') %>% 
  subset(grepl('^\\.', Area)) %>% 
  mutate(Area = gsub('\\.', '', Area), pop.2012 = X2012) %>% 
  mutate(pop.2012 = as.numeric(gsub(',', '', pop.2012)));
data.pop <- data.pop[c('Area', 'pop.2012')];

# ::: b. combine with collision data and compute normalized scores ----
data.mv.collisions <- merge(data.mv.collisions, data.pop, by.x = 'state', by.y = 'Area') %>% 
  mutate(accident.rate = collisions / pop.2012, 
         accident.z = (accident.rate - mean(accident.rate)) / sd(accident.rate)
  );

# c. visualize relationships between number of accidents and state population ----
plot.pop <- ggplot(data = data.mv.collisions, aes(x = pop.2012, y = collisions)) +
  geom_point() +
  geom_text(aes(label = state), alpha = 0.5, size = 4) +
  geom_smooth(method = lm, se = F) +
  annotate(geom = 'text', color = 'blue', x = 0, y = 700, hjust = 0, vjust = 1, label = sprintf('R-squared = %0.2f', cor(data.mv.collisions$collisions, data.mv.collisions$pop.2012)^2)) +
  scale_x_continuous(label = function(x) x / 1e6) +
  labs(x = 'State Population (millions)', y = 'Fatal Motor Vehicle Collisions') +
  disclaimer;
ggsave('Output/correlation-plot.png', plot.pop, width = 6, height = 5, dpi= 300)

# ::: d. map plot of accidents normalized by population ----
plot.fars.norm <- map.usa +
  geom_point(aes(x = lon, y = lat, size = accident.z), data = data.mv.collisions, col = 'orange', alpha = 0.4, show_guide=F) +
  scale_size_continuous(range = c(2, 20)) +
  disclaimer.map;
ggsave('Output/accident-map-2.jpg', plot.fars.norm, width = 6, height = 6, dpi = 300);

# ::: e. chloropleth of relative accident rates ----
map.states <- map_data('state');

plot.chloro <- ggplot(data.mv.collisions, aes(map_id = tolower(state))) +
  geom_map(aes(fill = accident.z), map = map.states, color = 'white', lwd = 0.2) +
  expand_limits(x = map.states$long, y = map.states$lat) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid=element_blank()) +
  disclaimer;
ggsave('Output/accident-map-3.png', plot.chloro, width = 7, height = 3.5, dpi = 300);

# ::: f. chloropleth of categorized accident rates ----
rate.quantiles <- quantile(data.mv.collisions$accident.rate, probs = seq(0, 1, 0.2));
rate.quantiles[1] <- rate.quantiles[1] - .01;
data.mv.collisions$Accident.Rate <- cut(data.mv.collisions$accident.rate, rate.quantiles, c('Bottom 20%', 'Lower 20%', 'Mid 20%', 'Upper 20%', 'Top 20%'))
plot.chloro.cat <- ggplot(data.mv.collisions, aes(map_id = tolower(state))) +
  geom_map(aes(fill = Accident.Rate), map = map.states) +
  geom_map(aes(fill = Accident.Rate), map = map.states, color = 'white', lwd = 0.2, show_guide = F) +
  expand_limits(x = map.states$long, y = map.states$lat) +
  scale_fill_manual(values = hsv(seq(0.6, 0, length = 5), 0.7, 0.9)) +
  disclaimer +
  theme_minimal() + theme(axis.text.x = element_blank(), 
                          axis.title.x = element_blank(), 
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_blank(), 
                          axis.title.y = element_blank(), 
                          axis.ticks.y = element_blank(), 
                          panel.grid=element_blank());
ggsave('Output/accident-map-4.png', plot.chloro.cat, width=7, height=3.5, dpi=300)

# 3. simpler plot of the data points themselves -----
plot.points <- data.mv.collisions %>% 
  arrange(desc(accident.z)) %>% 
  mutate(state = factor(as.character(state), as.character(state), ordered = T)) %>% 
  ggplot(data = ., aes(x = state, y = accident.z, color = Accident.Rate)) +
  geom_point() +
  geom_text(aes(label = state, y = accident.z + 0.02, x = as.numeric(state) + 0.4), angle = 45, hjust = 0, vjust = 0, size = 2) +
  scale_x_discrete(limits = arrange(data.mv.collisions, desc(accident.z))$state, expand = c(0.08, 0)) +
  scale_y_continuous(expand = c(0.08, 0)) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        panel.grid.major.x=element_blank(), 
        legend.position=c(1,1), 
        legend.justification = c(1,1)) +
  labs(x = NULL, y = 'Relative Fatal Accident Rate') +
  disclaimer;
ggsave('Output/accident-point-plot.png', plot.points, width = 8, height = 5, dpi = 300)

# 4. Boston marathon map using ggmap ----
# ::: a. prepare marathon data and map tiles ----
data.marathon.raw <- htmlTreeParse('Data/boston-marathon.gpx', useInternalNodes = T);
marathon.elevations <- as.numeric(xpathSApply(data.marathon.raw, path = '//trkpt/ele', xmlValue))
data.marathon <- as.data.frame(t(xpathSApply(data.marathon.raw, path = '//trkpt', xmlAttrs))) %>% 
  mutate(lat = as.numeric(as.character(lat)), 
         lon = as.numeric(as.character(lon)), 
         elevation = marathon.elevations);
marathon.bounds <- with(data.marathon, c(min(lon) - diff(range(lon)) * 0.1, min(lat) - diff(range(lat)) * 0.1, max(lon) + diff(range(lon)) * 0.1, max(lat) + diff(range(lat)) * 0.1));
map.boston <- ggmap(get_map(location=marathon.bounds, zoom=13, maptype = 'toner-lite', source = 'stamen', crop = T, scale = 4));

# ::: b1. prepare boston city limits from SHAPEFILE data ----
# Note: getting to grips with hardcore map data is a lesson in itself.
# I provide nicely extracted mapping coordinates in block b2, below.
# The commented block of code shown below requires libraries 'maptools' and 'rgdal'.
# The shapefile used for Boston can be obtained from: http://www.mass.gov/anf/research-and-tech/it-serv-and-support/application-serv/office-of-geographic-information-massgis/datalayers/townsurvey.html

# data.boston <- readOGR('Data/townssurvey_shp/', layer = 'TOWNSSURVEY_POLY');
# fortify.boston <- fortify(data.boston);
# projection.boston <- proj4string(data.boston);
# coords.boston.raw <- as.matrix(fortify.boston[c('long', 'lat')]);
# coords.boston <- project(coords.boston.raw, proj = projection.boston, inv = T);
# fortify.boston <- mutate(fortify.boston, long = coords.boston[,1], lat = coords.boston[,2], group = as.numeric(as.character(group))) %>% 
#   subset(group >= 1238 & group < 1239)

# ::: b2. load pre-made Boston city limits, extracted from section b1, above ----
fortify.boston <- read.csv('Data/boston-city-limits-simple.csv')

# ::: c. create map ----
plot.boston.marathon <- map.boston +
  geom_path(aes(x = long, y = lat), data = fortify.boston, color = 'blue', lwd = 0.25) +
  geom_path(aes(x = lon, y = lat), data = data.marathon, color = 'red') +
  annotate(x = data.marathon$lon[c(1, nrow(data.marathon))], y = data.marathon$lat[c(1, nrow(data.marathon))], geom = 'point', size = 2, pch = 21, color = 'white', fill = 'red') +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank()
  );
ggsave('Output/boston-marathon-map.jpg', plot.boston.marathon, width = 6, height = 2.5, dpi = 600)
