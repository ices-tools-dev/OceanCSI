# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "dplyr")
ipak(packages)

# read shapefiles into memory
Country_Europe_Extended <- sf::st_read("Input/Country_Europe_Extended.shp")
SeaRegion <- sf::st_read("Input/SeaRegion.shp")
#plot(Country_Europe_Extended, col = NA, main = NA)
#plot(SeaRegion, col = NA, main = NA)

# check and make geometries valid
#sf::st_is_valid(Country_Europe_Extended)
Country_Europe_Extended <- sf::st_buffer(Country_Europe_Extended, 0.0)
#sf::st_is_valid(SeaRegion)

# transform projection into UTM33N
Country_Europe_Extended_UTM33N <- sf::st_transform(Country_Europe_Extended, crs = 32633)
#plot(Country_Europe_Extended_UTM33N, col = NA, main = NA)

SeaRegion_UTM33N <- sf::st_transform(SeaRegion, crs = 32633)
#plot(SeaRegion_UTM33N, col = NA, main = NA)

# make 1km buffer
Country_Europe_Extended_UTM33N_Within1km <- sf::st_buffer(Country_Europe_Extended_UTM33N, 1000)
#plot(Country_Europe_Extended_UTM33N_Within1km, col = NA, main = NA)

# make 20km buffer
Country_Europe_Extended_UTM33N_Within20km <- sf::st_buffer(Country_Europe_Extended_UTM33N, 20000)
#plot(Country_Europe_Extended_UTM33N_Within20km, col = NA, main = NA)


# read stations ... later we can skip columns not being used - including SeaRegionID which can be clasified in R instead of TSQL
Stations <- read.table("input/OceanCSI_Stations_20180411.txt", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

# make stations spatial keeping original latitude/longitude
Stations <- sf::st_as_sf(Stations, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

# project stations into UTM33N
Stations <- sf::st_transform(Stations, crs = 32633)
Stations$UTM_E <- sf::st_coordinates(Stations)[,1]
Stations$UTM_N <- sf::st_coordinates(Stations)[,2]
#Stations <- sf::st_transform(Stations, crs = 4326)
#Stations$Lon <- sf::st_coordinates(Stations)[,1]
#Stations$Lat <- sf::st_coordinates(Stations)[,2]
#Stations <- sf::st_transform(Stations, crs = 32633)
#Stations <- sf::st_set_geometry(Stations, NULL)

# classify stations into sea regions
Stations$SeaRegion <- sf::st_intersects(Stations, SeaRegion_UTM33N) %>% as.numeric()
table(Stations$SeaRegion)
table(Stations$SeaRegionID)

# classify stations into on land
#sf::st_intersects(Stations, Country_Europe_Extended_UTM33N)

# classify stations into within 1km from land
#sf::st_intersects(Stations, Country_Europe_Extended_UTM33N_Within1km)

# classify stations into within 20km from land
#sf::st_intersects(Stations, Country_Europe_Extended_UTM33N_Within20km)