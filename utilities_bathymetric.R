library(tidyverse)
library(httr)

# Function to get bathymetric depth from EMODnet bathymetry REST web service
get.bathymetric <- function(x, y, host = "https://rest.emodnet-bathymetry.eu") {
  query = paste0("/depth_sample?geom=POINT(", x, " ", y,")")
  path = httr::modify_url(paste0(host, query))
  r = GET(path)
  # to catch empty responses in a proper way
  if(is.numeric(content(r)$avg)){
    return(paste(content(r)$min, content(r)$max, content(r)$avg, content(r)$stdev, sep = "_"))
  } else {
    return(NA_real_)
  }
}

classify_locations_into_bathymetric <- function(locations) {
  bathymetrics <- map2(locations$Longitude, locations$Latitude, get.bathymetric) %>% unlist
  
  locations$Bathymetric <- bathymetrics
  
  locations <- locations %>%
    separate(Bathymetric, c("BathymetricMin", "BathymetricMax", "BathymetricAvg", "BathymetricStDev"), sep = "_") %>%
    mutate(
      BathymetricMin = -as.numeric(BathymetricMin),
      BathymetricMax = -as.numeric(BathymetricMax),
      BathymetricAvg = -as.numeric(BathymetricAvg),
      BathymetricStDev = -as.numeric(BathymetricStDev),
    )
  
  return(locations)
}
