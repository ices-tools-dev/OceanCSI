library(terra)

classify_locations_into_gridcells <- function(locations, gridtype) {
  # Read grid
  if (gridtype=="10x10"){
    grid <- vect("Input_grids/assessment_grid_10_countries.shp")[, "CellCode"]
  }else{
    grid <- vect("Input_grids/assessment_grid_100_20_country.shp")[, "GRIDCODE"]
    names(grid) <- "CellCode"
  }
  
  # Identify invalid geometries
  is.valid(grid)
  
  # Make invalid geometries valid
  grid <- makeValid(grid)
  
  # Transform projection into UTM33N
  grid <- terra::project(grid, "EPSG:4326")
  
  # Make locations spatial keeping original longitude/latitude
  locations <- vect(as.data.frame(locations), geom = c("Longitude", "Latitude"), crs = "EPSG:4326", keepgeom= T)
  
  # Classify stations into the gridcells
  locations <- terra::intersect(grid, locations)
  
  # Remove spatial column in order to merge station samples
  locations <-as.data.table(locations)
  
  return(locations)
}
