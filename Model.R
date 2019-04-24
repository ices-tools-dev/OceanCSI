# ipak function ----------------------------------------------------------------
# install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "dplyr")
ipak(packages)
memory.limit(size = 16000)

# Input files ------------------------------------------------------------------

coastlineFile <- "Input/Country_Europe_Extended.shp"
#coastlineFile <- "Input/EEA_Coastline_20170228.shp"
searegionFile <- "Input/EEA_SeaRegion_20180831.shp"
stationFile <- "Input/OceanCSI_Station_20180829.txt"
sampleFile <- "Input/OceanCSI_Sample_20180829.txt"

# Coastline --------------------------------------------------------------------

# Read shapefile
coastline <- sf::st_read(coastlineFile)

# Check if geometries is valid
#sf::st_is_valid(coastline)

# Make geometries valid by doing the buffer of nothing trick
#coastline <- sf::st_buffer(coastline, 0.0)

# Transform projection into UTM33N
coastline <- sf::st_transform(coastline, crs = 32633)

# Make 1km buffer
#coastline_Within1km <- sf::st_buffer(coastline, 1000)

# Make 20km buffer
coastline_Within20km <- sf::st_buffer(coastline, 20000)
#plot(coastline_Within20km)

# SeaRegions -------------------------------------------------------------------

# Read shapefile
searegions <- sf::st_read(searegionFile)

# Check if geometries is valid
#sf::st_is_valid(searegions)

# Make geometries valid by doing the buffer of nothing trick
#searegions <- sf::st_buffer(searegions, 0.0)

# Transform projection into UTM33N
searegions <- sf::st_transform(searegions, crs = 32633)

# Stations ---------------------------------------------------------------------

# Read stations
stations <- fread(input = stationFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)
# stations <- fread(input = stationFile, sep = "\t", nrows = 100000, na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

# Make stations spatial keeping original latitude/longitude
stations <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

# Project stations into UTM33N
stations <- sf::st_transform(stations, crs = 32633)
stations$UTM_E <- sf::st_coordinates(stations)[,1]
stations$UTM_N <- sf::st_coordinates(stations)[,2]
stations <- sf::st_transform(stations, crs = 4326)
stations$Lon <- sf::st_coordinates(stations)[,1]
stations$Lat <- sf::st_coordinates(stations)[,2]
stations <- sf::st_transform(stations, crs = 32633)

# Classify stations into sea regions - the R way. However the stations have allready been classified when extracting the data from the database
#stations$SeaRegionID_R <- sf::st_intersects(stations, searegions) %>% as.numeric()
#table(stations$SeaRegionID_R)
#table(stations$SeaRegionID)

# Classify stations into on land
#stations$OnLand <- apply(sf::st_intersects(stations, coastline, sparse = TRUE), 1, any)

# Classify stations into within 1km from land
#stations$Within1km <- apply(sf::st_intersects(stations, coastline_Within1km, sparse = TRUE), 1, any)

# Classify stations into within 20km from land
stations$Within20km <- apply(sf::st_intersects(stations, coastline_Within20km, sparse = TRUE), 1, any)

# Remove spatial column in order to merge station samples
stations <- sf::st_set_geometry(stations, NULL)

# Classify stations using square assignment
#
# Stations are defined geographical by position given as longitude and latitude in decimal degrees, but do not contain relaible
# and consistent station identification. The position of the same station migth vary slighly over time. In order to improve the
# aggregation into to time series, data are aggregated into squares with sides of 1.375 km for coastal stations within 20 km
# from the coastline (m = 80) and 5.5 km for open water station more than 20 km away from the coastline (m = 20).
# The procedure does not totally prevent errorneous aggregation of data belonging to stations close to each other or errorneous
# breakup of time series into fragments due to small shifts in position, but reduces the problem considerably.
#station$m <- 20
stations$m <- ifelse(stations$Within20km, 80, 20)
stations$iY <- round(stations$Latitude*stations$m)
stations$latitude_center <- stations$iY/stations$m
stations$rK <- stations$m/cos(stations$latitude_center*atan(1)/45)
stations$iX <- round(stations$Longitude*stations$rK)
stations$longitude_center <- stations$iX/stations$rK
comb <- with(stations, paste(iX, iY))
stations$ClusterID <- match(comb, unique(comb))

# Samples ----------------------------------------------------------------------

# Read samples
samples <- fread(sampleFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

## In order to save the current state of data, run following code
# save(samples, stations, searegionlist, file = "oceancsidata.RData")

# StationSamples ----------------------------------------------------------

## In order to (re)load the data processed before, run following code. Data should have been saved in an earlier session
load("oceancsidata.RData")

# merge stations and samples
setkey(stations, StationID)
setkey(samples, StationID, SampleID)
stationSamples <- stations[samples]

# free memory
rm(stations, samples)

save(stationSamples, searegionlist, file = "oceancsidata2.RData")

# Prepare for plotting
source("plotfunctions.r")

# Nitrate Nitrogen (Winter) -------------------------------------------------------------
#   Parameters: [NO3-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Nitrate
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate) & (NitrateQ != 3 & NitrateQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate)]

# Calculate station mean --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrate, MaxNitrate, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrate = mean(Nitrate), MinNitrate = min(Nitrate), MaxNitrate = max(Nitrate), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrate, MinMinNitrate, MaxMaxNitrate, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrate = mean(AvgNitrate), MinNitrate = min(MinNitrate), MaxNitrate = max(MaxNitrate), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_nitrate.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Nitrate = mean(AvgNitrate), Longitude = mean(AvgLongitude), Latitude = mean(AvgLatitude)), list(ClusterID)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "Longitude", ylat = "Latitude", 
               parameterValue = "Nitrate", 
               invJet = F, 
               limits = c(0,100))
saveEuropeStatusMap(parameter = "Nitrate")

# trend analysis using Kendall test

# ClusterIDs where one of the years is > 2006
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgNitrate"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_nitrate.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Nitrate")
saveEuropeTrendMap("Nitrate")


# Nitrite Nitrogen (Winter) -------------------------------------------------------------
#   Parameters: [NO2-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Nitrite
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrite) & (NitriteQ != 3 & NitriteQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrite)]

# Calculate station mean --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrite, MaxNitrite, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrite = mean(Nitrite), MinNitrite = min(Nitrite), MaxNitrite = max(Nitrite), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrite, MinMinNitrite, MaxMaxNitrite, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrite = mean(AvgNitrite), MinNitrite = min(MinNitrite), MaxNitrite = max(MaxNitrite), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_nitrite.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Nitrite = mean(AvgNitrite)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Nitrite", 
               invJet = F, 
               limits = "auto"
)
saveEuropeStatusMap(parameter = "Nitrite")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgNitrite"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_nitrite.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Nitrite")
saveEuropeTrendMap("Nitrite")

# Ammonium Nitrogen (Winter) ------------------------------------------------------------
#   Parameters: [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Ammonium
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Ammonium) & (AmmoniumQ != 3 & AmmoniumQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Ammonium)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgAmmonium, MinAmmonium, MaxAmmonium, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgAmmonium = mean(Ammonium), MinAmmonium = min(Ammonium), MaxAmmonium = max(Ammonium), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgAmmonium, MinMinAmmonium, MaxMaxAmmonium, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgAmmonium = mean(AvgAmmonium), MinAmmonium = min(MinAmmonium), MaxAmmonium = max(MaxAmmonium), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_ammonium.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Ammonium = mean(AvgAmmonium)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Ammonium", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Ammonium")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgAmmonium"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_ammonium.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Ammonium")
saveEuropeTrendMap("Ammonium")

# Dissolved Inorganic Nitrogen - DIN (Winter) ----------------------------------
#   Parameters: [NO3-N] + [NO2-N] + [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate|Nitrite|Ammonium) & (NitrateQ != 3 & NitrateQ != 4 & NitriteQ != 3 & NitriteQ != 4 & AmmoniumQ != 3 & AmmoniumQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium)]
coalesce <- function(x) if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
wk$DIN <- apply(wk[, c("Nitrate", "Nitrite", "Ammonium")], 1, coalesce)

# Calculate station mean --> ClusterID, StationID, Latitude, Longitude, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgDIN, MinDIN, MaxDIN, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgDIN = mean(DIN), MinDIN = min(DIN), MaxDIN = max(DIN), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgDIN, MinMinDIN, MaxMaxDIN, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgDIN = mean(AvgDIN), MinDIN = min(MinDIN), MaxDIN = max(MaxDIN), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_din.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(DIN = mean(AvgDIN)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "DIN", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "DIN")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgDIN"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_din.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "DIN")
saveEuropeTrendMap("DIN")

# Total Nitrogen (Annual) ------------------------------------------------------
#   Parameters: [N]
#   Depth: <= 10
#   Period: Annual
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, TotalNitrogen
wk <- stationSamples[Depth <= 10 & !is.na(TotalNitrogen) & (TotalNitrogenQ != 3 & TotalNitrogenQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, TotalNitrogen)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgTotalNitrogen, MinTotalNitrogen, MaxTotalNitrogen, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgTotalNitrogen = mean(TotalNitrogen), MinTotalNitrogen = min(TotalNitrogen), MaxTotalNitrogen = max(TotalNitrogen), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgTotalNitrogen, MinMinTotalNitrogen, MaxMaxTotalNitrogen, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalNitrogen = mean(AvgTotalNitrogen), MinTotalNitrogen = min(MinTotalNitrogen), MaxTotalNitrogen = max(MaxTotalNitrogen), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_totalnitrogen.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(TotalNitrogen = mean(AvgTotalNitrogen)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "TotalNitrogen", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "TotalNitrogen")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgTotalNitrogen"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_totalnitrogen.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "TotalNitrogen")
saveEuropeTrendMap("TotalNitrogen")

# Phosphate Phosphorus / Dissolved Inorganic Phophorus - DIP (Winter) ---------------------
#   Parameters: [PO4]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Phosphate
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Phosphate) & (PhosphateQ != 3 & PhosphateQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Phosphate)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgPhosphate, MinPhosphate, MaxPhosphate, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgPhosphate = mean(Phosphate), MinPhosphate = min(Phosphate), MaxPhosphate = max(Phosphate), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgPhosphate, MinMinPhosphate, MaxMaxPhosphate, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgPhosphate = mean(AvgPhosphate), MinPhosphate = min(MinPhosphate), MaxPhosphate = max(MaxPhosphate), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_phosphate.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Phosphate = mean(AvgPhosphate)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Phosphate", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Phosphate")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgPhosphate"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_phosphate.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Phosphate")
saveEuropeTrendMap("Phosphate")

# Total Phosphorus (Annual) ----------------------------------------------------
#   Parameters: [P]
#   Depth: <= 10
#   Period: Annual
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, TotalPhosphorus
wk <- stationSamples[Depth <= 10 & !is.na(TotalPhosphorus) & (TotalPhosphorusQ != 3 & TotalPhosphorusQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, TotalPhosphorus)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgTotalPhosphorus, MinTotalPhosphorus, MaxTotalPhosphorus, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgTotalPhosphorus = mean(TotalPhosphorus), MinTotalPhosphorus = min(TotalPhosphorus), MaxTotalPhosphorus = max(TotalPhosphorus), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgTotalPhosphorus, MinMinTotalPhosphorus, MaxMaxTotalPhosphorus, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalPhosphorus = mean(AvgTotalPhosphorus), MinTotalPhosphorus = min(MinTotalPhosphorus), MaxTotalPhosphorus = max(MaxTotalPhosphorus), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_totalphosphorus.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(TotalPhosphorus = mean(AvgTotalPhosphorus)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "TotalPhosphorus", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "TotalPhosphorus")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgTotalPhosphorus"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_totalphosphorus.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "TotalPhosphorus")
saveEuropeTrendMap("TotalPhosphorus")

# Chlorophyll a (Summer) -------------------------------------------------------
#   Parameters: Chlorophyll a
#   Depth: <= 10
#   Period: Summer
#     June - September for stations within Baltic Sea north of 59 N
#     May - September for all other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Chlorophyll
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Latitude > 59, Month >= 6 & Month <= 9, Month >= 5 & Month <= 9) & !is.na(Chlorophyll) & (ChlorophyllQ != 3 & ChlorophyllQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Chlorophyll)]

# Calculate station mean --> ClusterID, StationID, Latitude, Longitude, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgChlorophyll, MinChlorophyll, MaxChlorophyll, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgChlorophyll = mean(Chlorophyll), MinChlorophyll = min(Chlorophyll), MaxChlorophyll = max(Chlorophyll), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgChlorophyll, MinMinChlorophyll, MaxMaxChlorophyll, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgChlorophyll = mean(AvgChlorophyll), MinChlorophyll = min(MinChlorophyll), MaxChlorophyll = max(MaxChlorophyll), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_chlorophyll.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Chlorophyll = mean(AvgChlorophyll)), list(ClusterID, AvgLongitude, AvgLatitude, SeaRegionID)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Chlorophyll", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Chlorophyll")

# Plot chlorophyll values for all regions separately
regionsToPlot <- unique(wk21$SeaRegionID)
for(ii in seq(1:length(regionsToPlot))){
    plotRegionStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
                       parameterValue = "Chlorophyll", 
                       invJet = F, 
                       limits = "auto",
                       region = regionsToPlot[ii])
  saveEuropeStatusMap(parameter = paste0("Chlorophyll_", regionsToPlot[ii]))
}

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgChlorophyll"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))

fwrite(KendallResult.clustered, "output/trend_chlorophyll.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Chlorophyll")
saveEuropeTrendMap("Chlorophyll")

# DissolvedOxygen (Summer/Autumn) -----------------------------------------------------
#   Parameters: Dissolved Oxygen
#   Depth: <= 10 m above seafloor; Adapted to <= if(sounding < 100) 20 else 50
#   Period: July - October
#   Aggregation Method: mean of lower quartile by station and cluster per year
#   per class (<4, 4-6, >6) trend maps

# Filter stations rows and columns
DO_samples_summer <- stationSamples[(!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
                                      (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4) &
                                      Depth <= Sounding &
                                      case_when(
                                        Sounding < 100 ~ Depth >= Sounding - 20,
                                        Sounding >= 100 ~ Depth >= Sounding - 50) &
                                      Year > 1989 &
                                      Month > 6 & Month < 11,
                                    list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, latitude_center, Sounding, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide)]

# Check number of samples per searegion
# DO_samples_summer %>% group_by(SeaRegionID) %>% summarize(timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfSamples = n())

DO_samples_summer <- DO_samples_summer %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )
  ) %>% as.data.table()

# Calculate 25 percentile per cluster and year
Q25all <- DO_samples_summer[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]

# Calculate mean of lower quartile 
mean25perc <- DO_samples_summer %>% 
  left_join(Q25all) %>% 
  filter(Oxygen <= q25) %>%
  group_by(Year, ClusterID, SeaRegionID, UTM_E, UTM_N) %>%
  summarize(AvgOxygen = mean(Oxygen),
            AvgLatitude = mean(latitude_center),
            AvgLongitude = mean(longitude_center),
            AvgSounding = mean(Sounding),
            AvgDepth = mean(Depth)) %>%
  as.data.table()

# check value distribution
# hist(mean25perc[AvgOxygen < 0, list(AvgOxygen)]$AvgOxygen)

fwrite(mean25perc, "output/status_dissolvedoxygen.csv")

# Check number of clusters selected per searegion
# mean25perc %>% group_by(SeaRegionID) %>% summarize( timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfClusters = n())

# plot average status for last 5 years 
wk21 <- mean25perc[Year > 2012, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", 
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Oxygen")


# trend analysis using Kendall test for each oxygen class
classes <- c("O2_4 mg_l", "4_O2_6 mg_l", "O2_6 mg_l")
prettyClassNames <- c("O2 < 4 mg/l", "4 < O2 < 6 mg/l", "O2 > 6 mg/l")

ID_class <- wk21 %>% mutate(
  class = case_when(
    Oxygen < 4 ~ 1,
    Oxygen >= 4 & Oxygen < 6 ~ 2,
    Oxygen >= 6 ~ 3
  )
) %>% select(ClusterID, class) %>% as.data.table()

# merge wk21 and class list
setkey(mean25perc, ClusterID)
setkey(ID_class, ClusterID)

mean25perc2 <- mean25perc[ID_class]


for(cc in seq(1:length(classes))){
  
yearcriteria <- mean25perc2[Year>2006 & class == cc, unique(ClusterID)]

clusterSelection <- mean25perc[ClusterID %in% yearcriteria][
  , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
    , .(NrYears = .N), by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)][NrYears >=5]
hist(clusterSelection$NrYears)
wk22 <- mean25perc[ClusterID %in% clusterSelection$ClusterID]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgOxygen"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "increasing",
    .$sl > 0.05 ~ "no trend")
  ) %>%
  mutate(trend = as.factor(trend))
KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))


fwrite(KendallResult.clustered, paste0("output/trend_dissolvedoxygen", classes[cc], ".csv"))

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Oxygen")
saveEuropeTrendMap(paste("Oxygen", classes[cc]))

}

