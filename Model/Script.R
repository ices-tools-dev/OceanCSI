# ipak function -----------------------------------------------------------
# install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "dplyr")
ipak(packages)

# Country_Europe_Extended -------------------------------------------------

# read shapefile
#Country_Europe_Extended <- sf::st_read("Input/Country_Europe_Extended.shp")

# check geometries is valid
#sf::st_is_valid(Country_Europe_Extended)

# make geometries valid by doing the buffer of nothing trick
#Country_Europe_Extended <- sf::st_buffer(Country_Europe_Extended, 0.0)

# transform projection into UTM33N
#Country_Europe_Extended <- sf::st_transform(Country_Europe_Extended, crs = 32633)

# make 1km buffer
#Country_Europe_Extended_Within1km <- sf::st_buffer(Country_Europe_Extended, 1000)

# make 20km buffer
#Country_Europe_Extended_Within20km <- sf::st_buffer(Country_Europe_Extended, 20000)

# SeaRegion ---------------------------------------------------------------

# read shapefile
#SeaRegion <- sf::st_read("Input/SeaRegion.shp")

# transform projection into UTM33N
#SeaRegion <- sf::st_transform(SeaRegion, crs = 32633)

# Stations ----------------------------------------------------------------

# read stations
Stations <- fread("input/OceanCSI_Stations_20180713.txt", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

# rename ID variable to StationID
names(Stations)[names(Stations) == 'ID'] <- 'StationID'

# make stations spatial keeping original latitude/longitude
#Stations <- sf::st_as_sf(Stations, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

# project stations into UTM33N
#Stations <- sf::st_transform(Stations, crs = 32633)
#Stations$UTM_E <- sf::st_coordinates(Stations)[,1]
#Stations$UTM_N <- sf::st_coordinates(Stations)[,2]
#Stations <- sf::st_transform(Stations, crs = 4326)
#Stations$Lon <- sf::st_coordinates(Stations)[,1]
#Stations$Lat <- sf::st_coordinates(Stations)[,2]
#Stations <- sf::st_transform(Stations, crs = 32633)
#Stations <- sf::st_set_geometry(Stations, NULL)

# classify stations into sea regions
#Stations$SeaRegion <- sf::st_intersects(Stations, SeaRegion) %>% as.numeric()
#table(Stations$SeaRegion)
#table(Stations$SeaRegionID)

# classify stations into on land
#Stations$OnLand <- apply(sf::st_intersects(Stations, Country_Europe_Extended, sparse = TRUE), 1, any)

# classify stations into within 1km from land
#Stations$Within1km <- apply(sf::st_is_within_distance(Stations, Country_Europe_Extended, 1000, sparse = TRUE), 1, any)

# classify stations into within 20km from land
#Stations$Within20km <- apply(sf::st_is_within_distance(Stations, Country_Europe_Extended, 20000, sparse = TRUE), 1, any)

# classify stations - square assignment
#Stations$m <- ifelse(Stations$Within20km, 80, 20)
Stations$m <- 80
Stations$m <- 20
Stations$iY <- round(Stations$Latitude*Stations$m)
Stations$latitude_center <- Stations$iY/Stations$m
Stations$rK <- Stations$m/cos(Stations$latitude_center*atan(1)/45)
Stations$iX <- round(Stations$Longitude*Stations$rK)
Stations$longitude_center <- Stations$iX/Stations$rK
comb <- with(Stations, paste(iX, iY))
Stations$ClusterID <- match(comb, unique(comb))

# Samples -----------------------------------------------------------------

# read samples
Samples <- fread("input/OceanCSI_Samples_20180713.txt", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

# rename ID variable to SampleID
names(Samples)[names(Samples) == 'ID'] <- 'SampleID'

# StationSamples ----------------------------------------------------------

# merge station and samples
setkey(Stations, StationID)
setkey(Samples, StationID, SampleID)
StationSamples <- Stations[Samples]

# Dissolved Inorganic Nitrogen - DIN (Winter) -----------------------------
#   Parameters: [NO3-N] + [NO2-N] + [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea, North and Middel e.g. East of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium
wk <- StationSamples[Depth <= 10 & ifelse(SeaRegionID == 5 | SeaRegionID == 6, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate|Nitrite|Ammonium) & (NitrateQ != 3 & NitrateQ != 4 & NitriteQ != 3 & NitriteQ != 4 & AmmoniumQ != 3 & AmmoniumQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium)]
wk$DIN <- apply(wk[, c("Nitrate", "Nitrite", "Ammonium")], 1, coalesce)

# Calculate station mean --> ClusterID, StationID, Latitude, Longitude, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgDIN, MinDIN, MaxDIN, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgDIN = mean(DIN), MinDIN = min(DIN), MaxDIN = max(DIN), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgDIN, MinMinDIN, MaxMaxDIN, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgDIN = mean(AvgDIN), MinDIN = min(MinDIN), MaxDIN = max(MaxDIN), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

# Nitrate (Winter) -------------------------------------------------------
#   Parameters: [NO3-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea, North and Middel e.g. East of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Nitrate
wk <- StationSamples[Depth <= 10 & ifelse(SeaRegionID == 5 | SeaRegionID == 6, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate) & (NitrateQ != 3 & NitrateQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate)]

# Calculate station mean --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrate, MaxNitrate, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrate = mean(Nitrate), MinNitrate = min(Nitrate), MaxNitrate = max(Nitrate), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrate, MinMinNitrate, MaxMaxNitrate, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrate = mean(AvgNitrate), MinNitrate = min(MinNitrate), MaxNitrate = max(MaxNitrate), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

# Nitrite (Winter) -------------------------------------------------------
#   Parameters: [NO2-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea, North and Middel e.g. East of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Nitrite
wk <- StationSamples[Depth <= 10 & ifelse(SeaRegionID == 5 | SeaRegionID == 6, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrite) & (NitriteQ != 3 & NitriteQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrite)]

# Calculate station mean --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrite, MaxNitrite, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrite = mean(Nitrite), MinNitrite = min(Nitrite), MaxNitrite = max(Nitrite), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrite, MinMinNitrite, MaxMaxNitrite, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrite = mean(AvgNitrite), MinNitrite = min(MinNitrite), MaxNitrite = max(MaxNitrite), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

# Ammonium indicator (Winter) -------------------------------------------------------
#   Parameters: [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea, North and Middel e.g. East of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Ammonium
wk <- StationSamples[Depth <= 10 & ifelse(SeaRegionID == 5 | SeaRegionID == 6, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Ammonium) & (AmmoniumQ != 3 & AmmoniumQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Ammonium)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgAmmonium, MinAmmonium, MaxAmmonium, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgAmmonium = mean(Ammonium), MinAmmonium = min(Ammonium), MaxAmmonium = max(Ammonium), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgAmmonium, MinMinAmmonium, MaxMaxAmmonium, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgAmmonium = mean(AvgAmmonium), MinAmmonium = min(MinAmmonium), MaxAmmonium = max(MaxAmmonium), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

# TN [NTOT] indicator (Winter) --------------------------------------------

# DIP [PO4] indicator (Winter) --------------------------------------------

# TP [PTOT] indicator (Winter) --------------------------------------------

# Chlorophyll a indicator -------------------------------------------------

# DissolvedOxygen indicator -----------------------------------------------



# hierarchical clustering
data <- Stations[which(Stations$SeaRegionID==2), c("UTM_E", "UTM_N"),]
data <- sf::st_set_geometry(data, NULL)
dist <- dist(data)
fit <- hclust(as.dist(dist), method = "complete")
plot(fit)
clusters <- cutree(fit, h = 1000)
plot(data, col = clusters)

data1 <- Stations[which(Stations$SeaRegionID==2), c("Longitude", "Latitude"),]
dist <- sf::st_distance(data1)
fit <- hclust(as.dist(dist), method = "complete")
clusters <- cutree(fit, h = 1000)
plot(data1, col = clusters)

install.packages("geosphere")
library(geosphere)
data2 <- Stations[which(Stations$SeaRegionID==2), c("Longitude", "Latitude"),]
data2 <- sf::st_set_geometry(data2, NULL)
mdist <- distm(data2)
fit <- hclust(as.dist(mdist), method = "complete")
clusters <- cutree(fit, h = 1000)
plot(data2, col = clusters)
data2$Clusters <- cutree(fit, h = 1000)
table(data2$Clusters)

dist <- sf::st_distance(Stations)
fit <- hclust(as.dist(dist), method = "complete")
Stations$clusters <- cutree(fit, h = 1000)
plot(Stations, col = clusters)


# Test --------------------------------------------------------------------

points_in_distance <- function(in_pts, maxdist, ncuts = 10) {
  require(data.table)
  require(sf)
  # convert points to data.table and create a unique identifier
  pts <-  data.table(in_pts)
  pts <- pts[, or_id := 1:dim(in_pts)[1]]
  
  # divide the extent in quadrants in ncuts*ncuts quadrants and assign each
  # point to a quadrant, then create the index over "x" to speed-up
  # the subsetting
  range_x  <- range(pts$x)
  limits_x <-(range_x[1] + (0:ncuts)*(range_x[2] - range_x[1])/ncuts)
  range_y  <- range(pts$y)
  limits_y <- range_y[1] + (0:ncuts)*(range_y[2] - range_y[1])/ncuts
  pts[, `:=`(xcut =  as.integer(cut(x, ncuts, labels = 1:ncuts)),
             ycut = as.integer(cut(y, ncuts, labels = 1:ncuts)))]  %>%
    setkey(x)
  
  results <- list()
  count <- 0
  # start cycling over quadrants
  for (cutx in seq_len(ncuts)) {
    
    # get the points included in a x-slice extended by `maxdist`, and build
    # an index over y to speed-up subsetting in the inner cycle
    min_x_comp    <- ifelse(cutx == 1,
                            limits_x[cutx],
                            (limits_x[cutx] - maxdist))
    max_x_comp    <- ifelse(cutx == ncuts,
                            limits_x[cutx + 1],
                            (limits_x[cutx + 1] + maxdist))
    subpts_x <- pts[x >= min_x_comp & x < max_x_comp] %>%
      setkey(y)
    
    for (cuty in seq_len(ncuts)) {
      count <- count + 1
      
      # subset over subpts_x to find the final set of points needed for the
      # comparisons
      min_y_comp  <- ifelse(cuty == 1,
                            limits_y[cuty],
                            (limits_x[cuty] - maxdist))
      max_y_comp  <- ifelse(cuty == ncuts,
                            limits_x[cuty + 1],
                            (limits_x[cuty + 1] + maxdist))
      subpts_comp <- subpts_x[y >= min_y_comp & y < max_y_comp]
      
      # subset over subpts_comp to get the points included in a x/y chunk,
      # which "neighbours" we want to find. Then buffer them by maxdist.
      subpts_buf <- subpts_comp[ycut == cuty & xcut == cutx] %>%
        sf::st_as_sf() %>% 
        sf::st_buffer(maxdist)
      
      # retransform to sf since data.tables lost the geometric attrributes
      subpts_comp <- sf::st_as_sf(subpts_comp)
      
      # compute the intersection and save results in a element of "results".
      # For each point, save its "or_id" and the "or_ids" of the points within "dist"
      inters <- sf::st_intersects(subpts_buf, subpts_comp)
      
      # save results
      results[[count]] <- data.table(
        id = subpts_buf$or_id,
        int_ids = lapply(inters, FUN = function(x) subpts_comp$or_id[x]))
    }
  }
  data.table::rbindlist(results)
}


pts <- data.frame(x = runif(20000, 0, 100000), y = runif(20000, 0, 100000), id = 1:20000) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE)

maxdist <- 2000

out <- points_in_distance(pts, maxdist = maxdist, ncut = 10)

Atlantic <- fread("Input/Atlantic_time_series_EEA.txt", 
                  sep = "\t", na.strings = "NULL", 
                  stringsAsFactors = FALSE, header = TRUE, skip = "Cruise\t")


table2start <- which(names(Atlantic) == "time_ISO8601")

table1cols <- 1:(table2start-1)
table2cols <- table2start:ncol(Atlantic)

table1rows <- which(Atlantic$Cruise != "")

Atlantic1 <- Atlantic[table1rows, ..table1cols]
Atlantic2 <- Atlantic[, ..table2cols]

table1key <- 1:nrow(Atlantic1)

nrecords <- c(table1rows[-1] - 1, nrow(Atlantic)) - table1rows + 1
Atlantic2$ID <- rep(table1key, nrecords)
Atlantic1$ID <- table1key