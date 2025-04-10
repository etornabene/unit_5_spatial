2025-04-10


install.packages("sf")
install.packages("tidyverse")
install.packages("mapdata")
install.packages("marmap")


# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html
USA_crit_hab = st_read(dsn = 'data/North_Atlantic_Right_Whale_Critical_Habitat/',layer = 'North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles
USA_crit_hab
USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) #crs="+proj=longlat +datum=WGS84")

#Load in Canadian RW critical habitat coordinates http://www.dfo-mpo.gc.ca/species-especes/profiles-profils/rightwhaleNA-baleinenoireAN-eng.html
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)

# Turn data frame into sf points, then sf polygon
CAN_crit_hab_sf = CAN_crit_hab %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326) %>% # convert to sf
  dplyr::group_by(habitat, country) %>% 
  dplyr::summarize(do_union=FALSE) %>% # collapses data into multipoint; do_union=FALSE prevents reordering points; check out ?summarise.sf
  st_cast("POLYGON") # converts btwn spatial geometries
print(CAN_crit_hab_sf) # 2 simple features, with habitat and country attributes

# Simply USA_crit_hab data frame to match CAN_crit_hab
plot(USA_crit_hab_sf$geometry[1], axes=TRUE) # GOM habitat
plot(USA_crit_hab_sf$geometry[2], axes=TRUE) # FL / GA habitat
USA_crit_hab_sf$habitat=c("GOM", "SEUS")
USA_crit_hab_sf$country="USA"
USA_crit_hab_sf = USA_crit_hab_sf %>% 
  dplyr::select(country, habitat, geometry) # drops all other variables from shapefile

# Join the USA and Canada critical habitat sf objects
crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)

# set GOM + GSL map limits
lon_bounds = c(-72, -54)
lat_bounds = c(39, 53)

# Coastline data
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

# plot critical habitats and carcass locations
crit_map = ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black") + # add coastline
  geom_sf(data=crit_hab, alpha = 0.5, aes(fill=country)) +
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic() 
crit_map




########################### ais  ###############################

install.packages("lubridat")
library(lubridate)

ais_day = read.csv('data/processed_ais/ais_2017-01-25.csv')
head(ais_day)

# Question 1 : Are there any vessels in the SE habitat (larger than 65ft and faster than 10knots) 

dim(ais_day)

# Coastline data
lat_bounds = c(25, 34)
lon_bounds = c( -82, -76)
world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)
dim(world_map)

#Read in US critical habitat shape-files 
USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat/','North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles


## map all of the data ##

ais_map_pts = ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) + # add coastline
  geom_sf(data=USA_crit_hab, alpha = 0.5, color=NA, fill='purple3') +
  geom_point(data = ais_day, aes(x = LON, y = LAT)) + 
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color="none") +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 
ais_map_pts


### how many AIS intercept with the SE habitat ##

Sys.time()
ships_RW_intersect = ais_day %>%
  st_as_sf(coords=c("LON", "LAT"), crs=4269) %>% # AIS uses NAD83 CRS
  st_intersection(USA_crit_hab %>% dplyr::select(geometry)) 
Sys.time()

law_breakers = ships_RW_intersect %>%
  filter(Length > 20, SOG > 10)
head(law_breakers)
dim(law_breakers)

