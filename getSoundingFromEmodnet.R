
# Script that fetches station depth from EMODnet Bathymetry REST service
# Information about the REST service: https://portal.emodnet-bathymetry.eu/services/#rest
# author: willem.stolte@deltares.nl
# 

require(httr)
require(jsonlite)
require(data.table)
require(tidyverse)
require(leaflet)

# example = "https://rest.emodnet-bathymetry.eu/depth_sample?geom=POINT(3.3233642578125%2055.01953125)"
# r = GET(example)
# status_code(r)
# content(r)
# content(r)$avg


#======== start ===========================================

# function to get depth from EMODnet bathymetry REST service
getDepth <- function(x, y, host = "https://rest.emodnet-bathymetry.eu") {
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


stationFile <- "Input/OceanCSI_Station_20201002.txt"
stations2 <- fread(input = stationFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

# test = stations %>% dplyr::sample_n(200)

#!!!!!!!!!!!!! takes very long time !!!!!!!!! appr 15 hours
depths <- map2(stations2$Longitude, stations2$Latitude, getDepth) %>% unlist

stations2$depth <- depths

stations3 <- stations2 %>% #head(2) %>%
  # drop_na(depth) %>%
  separate(depth, c("minDepth", "maxDepth", "avgDepth", "stdevDepth"), sep = "_") %>%
  mutate(
    minDepth = as.numeric(minDepth),
    maxDepth = as.numeric(maxDepth),
    avgDepth = as.numeric(avgDepth),
    stdevDepth = as.numeric(stdevDepth),
  )

write_delim(stations3, "station_emodnetDepth.csv", delim = ";")

# alternatively, read earlier retrieved data from file.

stations3 <- read_delim("station_emodnetDepth.csv", delim = ";")


#===== analysis ====================================

stations3 <- read_delim("station_emodnetDepth.csv", delim = ";")

## compare EMODnet depths with soundings in the stations table

stations3 %>% 
  mutate(relVarDepth = (maxDepth- minDepth)/avgDepth) %>%
  # sample_n(100000) %>%
  filter(Sounding < 5000) %>%
ggplot() +
  geom_point(aes(Sounding, avgDepth, size = relVarDepth), shape = 21, color = "black", fill = "white", alpha = 0.3) +
  scale_size_continuous(limits = c(0,1))

ggsave("output/depthplot.png", width = 10, height = 10)

stations3 %>% 
  mutate(relVarDepth = (maxDepth- minDepth)/avgDepth) %>%
  # sample_n(100000) %>%
  filter(Sounding < 5000) %>%
  ggplot() +
  geom_point(aes(Sounding, avgDepth, size = relVarDepth), shape = 21, color = "black", fill = "white", alpha = 0.3) +
  scale_size_continuous(limits = c(0,1)) +
  coord_cartesian(xlim = c(0,50), ylim = c(0,50))

ggsave("output/depthplotShallow.png", width = 10, height = 10)

stations3 %>%
  mutate(relVarDepth = (maxDepth- minDepth)/avgDepth) %>%
  ggplot(aes(log10(relVarDepth))) + geom_histogram()

stations3 %>%
  filter((maxDepth - minDepth) > 0.1 * avgDepth) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 0.1)

stations3 %>%
  drop_na(Sounding, avgDepth) %>%
  filter(abs((avgDepth - Sounding)/Sounding) > 1) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 0.1, label = ~paste(Year, avgDepth, Sounding))
  

