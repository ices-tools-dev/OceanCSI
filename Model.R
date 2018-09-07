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
#stations <- fread(input = stationFile, sep = "\t", nrows = 100000, na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

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

# StationSamples ----------------------------------------------------------

# merge stations and samples
setkey(stations, StationID)
setkey(samples, StationID, SampleID)
stationSamples <- stations[samples]

# free memory
rm(stations, samples)

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

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Nitrate = mean(AvgNitrate)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Nitrate", 
               invJet = F, 
               limits = c(0,100))
ggsave(filename = file.path("output", paste0("Nitrate", "_status", ".png")), height = 8, width = 10)

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

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Nitrite = mean(AvgNitrite)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Nitrite", 
               invJet = F, 
               limits = "auto")
ggsave(filename = file.path("output", paste0("Nitrite", "_status", ".png")), height = 8, width = 10)


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

# DissolvedOxygen (Summer/Autumn) -----------------------------------------------------
#   Parameters: Dissolved Oxygen
#   Depth: <= 10 m above seafloor; Adapted to <= if(sounding < 100) 20 else 50
#   Period: July - October
#   Aggregation Method: mean of lower quartile by station and cluster per year

# Filter stations rows and columns
DO_samples_summer <- stationSamples[!is.na(Oxygen) &
                                      Depth <= Sounding &
                                      case_when(
                                        Sounding < 100 ~ Depth >= Sounding - 20,
                                        Sounding >= 100 ~ Depth >= Sounding - 50) &
                                      Year > 1989 &
                                      Month > 6 & Month < 11,
                                    list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, latitude_center, Sounding, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide)]

# Check number of samples per searegion
DO_samples_summer %>% group_by(SeaRegionID) %>% summarize(timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfSamples = n())

# Calculate 25 percentile per cluster and year
Q25all <- DO_samples_summer[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]

# Calcuate mean of lower quartile 
mean25perc <- DO_samples_summer %>% 
  left_join(Q25all) %>% 
  filter(Oxygen <= q25) %>%
  group_by(Year, ClusterID, SeaRegionID, UTM_E, UTM_N) %>%
  summarize(avgOxygen = mean(Oxygen),
            avgLatitude = mean(latitude_center),
            avgLongitude = mean(longitude_center),
            avgSounding = mean(Sounding),
            avgDepth = mean(Depth))

# Check number of clusters selected per searegion
mean25perc %>% group_by(SeaRegionID) %>% summarize( timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfClusters = n())
