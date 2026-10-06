library(tidyverse)
library(httr)

# Function to get bathymetric depth from EMODnet bathymetry REST web service
get.bathymetric <- function(x, y, host = "https://rest.emodnet-bathymetry.eu/depth_sample?") {
  query = paste0("geom=POINT(", x, "%20", y,")")
  path = paste0(host, query)
  r = try(GET(path))
  # to catch empty responses in a proper way
  if(is.numeric(content(r)$avg)){
    return(content(r)$avg)
  } else {
    return(NA_real_)
  }
}

get.bathymetric2 <- function(
    lon,
    lat,
    host = "https://rest.emodnet-bathymetry.eu/depth_sample",
    attempts = 5
) {
  geom <- sprintf("POINT(%s %s)", lon, lat)
  
  r <- tryCatch(
    httr::RETRY(
      "GET",
      url = host,
      query = list(geom = geom),
      times = attempts,
      pause_base = 1,
      pause_cap = 20,
      pause_min = 1,
      quiet = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(r)) {
    message("Geen respons voor: ", geom)
    return(NA_real_)
  }
  
  status <- httr::status_code(r)
  
  if (status != 200) {
    message("HTTP ", status, " voor: ", geom)
    return(NA_real_)
  }
  
  result <- tryCatch(
    httr::content(
      r,
      as = "parsed",
      type = "application/json"
    ),
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    message("Respons kon niet worden gelezen voor: ", geom)
    return(NA_real_)
  }
  
  value <- suppressWarnings(
    as.numeric(result$avg)
  )
  
  if (length(value) != 1L || !is.finite(value)) {
    message(
      "Geen geldige avg voor ", geom,
      " | respons: ",
      paste(httr::content(r, as = "text", encoding = "UTF-8"))
    )
    return(NA_real_)
  }
  
  value
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
