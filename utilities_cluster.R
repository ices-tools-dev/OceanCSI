library(sf)

classify_locations_into_clusters <- function(locations,inland_buffer_dist_km=5) {
  # Read coastline
  coastline <- st_read("Input/Country_Europe_Extended.shp")
  
  # Identify invalid geometries
  st_is_valid(coastline)
  
  # Make invalid geometries valid
  coastline <- st_make_valid(coastline)
  
  # Transform projection into UTM33N
  coastline <- sf::st_transform(coastline, crs = 32633)
  
  # Make 1km buffer
  #coastline_Within1km <- sf::st_buffer(coastline, 1000)
  
  # Make 20km buffer
  coastline_Within20km <- sf::st_buffer(coastline, 20000)
  
  # Make negative buffer, default 5km
  # we will exclude points inside this buffer because they are "on land"
  inland_buffer_dist <- inland_buffer_dist_km * -1000
  coastline_inland <- sf::st_buffer(coastline, inland_buffer_dist)
  
  # Transform projection into WGS84
  #coastline <- sf::st_transform(coastline, crs = 4326)
  #coastline_Within1km <- sf::st_transform(coastline_Within1km, crs = 4326)
  coastline_Within20km <- sf::st_transform(coastline_Within20km, crs = 4326)
  coastline_inland <- sf::st_transform(coastline_inland, crs = 4326)
  
  # Make locations spatial keeping original longitude/latitude
  locations <- st_as_sf(locations, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)

  # Classify locations into on land
  #locations$OnLand <- apply(sf::st_intersects(locations, coastline, sparse = TRUE), 1, any)
  
  # Classify stations into within 1km from land
  #stations$Within1km <- apply(sf::st_intersects(locations, coastline_Within1km, sparse = TRUE), 1, any)
  
  # Classify stations into within 20km from land
  locations$Within20km <- apply(st_intersects(locations, coastline_Within20km, sparse = TRUE), 1, any)

  # Classify stations within the 5km from coastline negative buffer
  # we consider these "inland" but they can also be in rivers, lakes
  # e.g. this catches the point 51.2800N 4.3300E which is located right in the middle of Antwerp Harbour  
  locations$inland <- apply(st_intersects(locations, coastline_inland, sparse = TRUE), 1, any)
  
  # rename the "inland" column to include the buffer distance used
  col_name_inland <- paste0("inland",inland_buffer_dist_km,"km")
  names(locations)[names(locations)=="inland"] <- col_name_inland
  
  # Remove spatial column in order to merge station samples
  locations <- sf::st_set_geometry(locations, NULL)
  
  # exclude the "inland" points from the cluster analysis 
  # we will add them back later. They will have NA for ClusterID 
  # both the inland column and the NA value for ClusterID indicate that points are "inland"
  locations_inland <- locations[get(col_name_inland)==TRUE,] 
  locations <- locations[get(col_name_inland)==FALSE,]
  
  # Classify stations using square assignment
  #
  # Stations are defined geographical by position given as longitude and latitude in decimal degrees, but do not contain reliable
  # and consistent station identification. The position of the same station might vary slightly over time. In order to improve the
  # aggregation into to time series, data are aggregated into squares with sides of 1.375 km for coastal stations within 20 km
  # from the coastline (m = 80) and 5.5 km for open water station more than 20 km away from the coastline (m = 20).
  # The procedure does not totally prevent erroneous aggregation of data belonging to stations close to each other or erroneous
  # breakup of time series into fragments due to small shifts in position, but reduces the problem considerably.
  #station$m <- 20
  locations$m <- ifelse(locations$Within20km, 80, 20)
  locations$iY <- round(locations$Latitude..degrees_north.*locations$m)
  locations$latitude_center <- locations$iY/locations$m
  locations$rK <- locations$m/cos(locations$latitude_center*atan(1)/45)
  locations$iX <- round(locations$Longitude..degrees_east.*locations$rK)
  locations$longitude_center <- locations$iX/locations$rK
  comb <- with(locations, paste(iX, iY))
  locations$ClusterID <- match(comb, unique(comb))
  
  locations <- rbindlist(list(locations, locations_inland), fill=T)
  
  return(locations)
}
