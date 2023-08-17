library(sf)

classify_locations_into_searegions <- function(locations) {
  # Read sea regions
  sea_regions <- st_read("Input/EEA_SeaRegion_20180831.shp")
  
  # Identify invalid geometries
  st_is_valid(sea_regions)
  
  # Make invalid geometries valid
  sea_regions <- st_make_valid(sea_regions)
  
  # Make locations spatial keeping original longitude/latitude
  locations <- st_as_sf(locations, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)
  
  # Classify locations into sea regions
  locations$SeaRegionID <- st_intersects(locations, sea_regions) %>% as.numeric()
  
  # Delete locations not classified
  locations <- na.omit(locations)
  
  # Remove spatial column and make into data table
  locations <- st_set_geometry(locations, NULL) %>% as.data.table()
  
  return(locations)
}
