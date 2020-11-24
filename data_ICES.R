library(data.table)
library(sf)
library(tidyverse)

# ICES Bottle and low resolution CTD data --------------------------------------

# Read stations
dt01 <- fread("Data/ICES_Stations_20201030.txt.gz", na.strings = c("NULL"))

# Make stations spatial keeping original latitude/longitude
dt01 <- st_as_sf(dt01, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

# Transform projection into UTM33N
#dt01 <- st_transform(dt01, crs = 32633)

# Classify stations into sea regions
dt01$SeaRegionID <- st_intersects(dt01, units) %>% as.numeric()

# Remove spatial column
dt01 <- st_set_geometry(dt01, NULL)

dt01[order(SeaRegionID), .N, SeaRegionID]

# Delete stations which haven't been classified into SeaRegions
dt01 <- dt01[!is.na(SeaRegionID)]