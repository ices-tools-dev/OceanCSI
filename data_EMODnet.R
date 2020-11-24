library(sf)
library(data.table)
library(tidyverse)

# EMODnet Arctic Sea - profiles ------------------------------------------------------
# N.B! Total Nitrogen not available

dt02 <- fread("Data/EMODnet_Eutrophication_Arctic_profiles_all_20200610.txt.gz")
names(dt02)
setnames(dt02,
         c("Cruise", "Station", "Type", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "Sounding", "Instrument",
           "Depth", "DepthQ", "Temperature", "TemperatureQ", "Salinity", "SalinityQ", "Oxygen", "OxygenQ", "Oxygen%", "Oxygen%Q",
           "Phosphate", "PhosphateQ", "TotalPhosphorus", "TotalPhosphorusQ", "Silicate", "SilicateQ", "Nitrate", "NitrateQ", "NitrateNitrite",
           "NitrateNitriteQ", "Nitrite", "NitriteQ", "Ammonium", "AmmoniumQ", "pH", "pHQ",
           "Alkalinity", "AlkalinityQ", "Chlorophyll", "ChlorophyllQ", "SampleQ"))
dt02[, DataSource := 2]
dt02[, TotalNitrogen := NA]
dt02[, TotalNitrogenQ := 9]

# EMODnet Atlantic Sea - profiles ------------------------------------------------------
# Time series also available with 692 stations along the French coast

dt03 <- fread("Data/EMODnet_Eutrophication_Acidification_NAT_all_profiles_all_parameters_all_accesses_v1.0.txt.gz")
names(dt03)
setnames(dt03,
         c("Cruise", "Station", "Type", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "Sounding", "Instrument",
           "Depth", "DepthQ", "Temperature", "TemperatureQ", "Salinity", "SalinityQ", "Oxygen", "OxygenQ", "Oxygen%", "Oxygen%Q",
           "Phosphate", "PhosphateQ", "TotalPhosphorus", "TotalPhosphorusQ", "Silicate", "SilicateQ", "Nitrate", "NitrateQ", "NitrateNitrite",
           "NitrateNitriteQ", "Nitrite", "NitriteQ", "TotalNitrogen", "TotalNitrogenQ", "Ammonium", "AmmoniumQ", "pH", "pHQ",
           "Alkalinity", "AlkalinityQ", "Chlorophyll", "ChlorophyllQ", "SampleQ"))
dt03[, DataSource := 3]

# EMODnet Baltic Sea - profiles --------------------------------------------------------

dt04 <- fread("Data/EMODnet_Eutrophication_BAL_profiles_20200604.txt.gz")
names(dt04)
setnames(dt04,
         c("Cruise", "Station", "Type", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "Sounding", "Instrument",
           "Depth", "DepthQ", "Temperature", "TemperatureQ", "Salinity", "SalinityQ", "Oxygen", "OxygenQ", "Oxygen%", "Oxygen%Q",
           "Phosphate", "PhosphateQ", "TotalPhosphorus", "TotalPhosphorusQ", "Silicate", "SilicateQ", "Nitrate", "NitrateQ", "NitrateNitrite",
           "NitrateNitriteQ", "Nitrite", "NitriteQ", "TotalNitrogen", "TotalNitrogenQ", "Ammonium", "AmmoniumQ", "pH", "pHQ",
           "Alkalinity", "AlkalinityQ", "Chlorophyll", "ChlorophyllQ", "SampleQ"))
dt04[, DataSource := 4]

# EMODnet Black Sea --------------------------------------------------------------------

dt05 <- fread("Data/EMODnet_Eutrophication_BLS_profiles_v02.txt.gz")
names(dt05)
setnames(dt05,
         c("Cruise", "Station", "Type", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "Sounding", "Instrument",
           "Depth", "DepthQ", "Temperature", "TemperatureQ", "Salinity", "SalinityQ", "Oxygen", "OxygenQ", "Oxygen%", "Oxygen%Q",
           "Phosphate", "PhosphateQ", "TotalPhosphorus", "TotalPhosphorusQ", "Silicate", "SilicateQ", "Nitrate", "NitrateQ", "NitrateNitrite",
           "NitrateNitriteQ", "Nitrite", "NitriteQ", "TotalNitrogen", "TotalNitrogenQ", "Ammonium", "AmmoniumQ", "pH", "pHQ",
           "Alkalinity", "AlkalinityQ", "Chlorophyll", "ChlorophyllQ", "SampleQ"))
dt05[, DataSource := 5]

# EMODnet Mediterranean Sea - profiles -------------------------------------------------
# N.B! Time series also available with 9912 stations along the Italian coast

dt06 <- fread("Data/EMODnet_Eutrophication_MED_profiles.txt.gz")
names(dt06)
setnames(dt06,
         c("Cruise", "Station", "Type", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "Sounding", "Instrument",
           "Depth", "DepthQ", "Temperature", "TemperatureQ", "Salinity", "SalinityQ", "Oxygen", "OxygenQ", "Oxygen%", "Oxygen%Q",
           "Phosphate", "PhosphateQ", "TotalPhosphorus", "TotalPhosphorusQ", "Silicate", "SilicateQ", "Nitrate", "NitrateQ", "NitrateNitrite",
           "NitrateNitriteQ", "Nitrite", "NitriteQ", "TotalNitrogen", "TotalNitrogenQ", "Ammonium", "AmmoniumQ", "pH", "pHQ",
           "Alkalinity", "AlkalinityQ", "Chlorophyll", "ChlorophyllQ", "SampleQ"))
dt06[, DataSource := 6]

# EMODnet EMODnet North Sea - profiles -------------------------------------------------

dt07 <- fread("Data/EMODnet_Eutrophication_NS_all_profiles_2020-01-16.txt.gz")
names(dt07)
setnames(dt07,
         c("Cruise", "Station", "Type", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "Sounding", "Instrument",
           "Depth", "DepthQ", "Temperature", "TemperatureQ", "Salinity", "SalinityQ", "Oxygen", "OxygenQ", "Oxygen%", "Oxygen%Q",
           "Phosphate", "PhosphateQ", "TotalPhosphorus", "TotalPhosphorusQ", "Silicate", "SilicateQ", "Nitrate", "NitrateQ", "NitrateNitrite",
           "NitrateNitriteQ", "Nitrite", "NitriteQ", "TotalNitrogen", "TotalNitrogenQ", "Ammonium", "AmmoniumQ", "pH", "pHQ",
           "Alkalinity", "AlkalinityQ", "Chlorophyll", "ChlorophyllQ", "SampleQ"))
dt07[, DataSource := 7]

# Combined data
dt08 <- rbindlist(list(dt02, dt03, dt04, dt05, dt06, dt07), use.names = TRUE)

# Remove original data tables
rm(dt02, dt03, dt04, dt05, dt06, dt07)

# Unique stations by natural key
# N.B! DataSource included as some station apparently are duplicated
# N.B! Sounding and Instrument included for station completeness
uniqueN(dt08, by = c("Cruise", "Station", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "Sounding", "Instrument", "DataSource"))
uniqueN(dt08, by = c("Cruise", "Station", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code", "DataSource"))
uniqueN(dt08, by = c("Cruise", "Station", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code"))

# Assign station ID by natural key
dt08[, StationID := .GRP, by = .(Cruise, Station, DateTime, Longitude, Latitude, Local_CDI_ID, EDMO_code, DataSource)]

# Assign sample ID by natural key
#dt08[, SampleID := .GRP, by = .(Cruise, Station, DateTime, Longitude, Latitude, Local_CDI_ID, EDMO_code, DataSource, Depth)]

# Extract stations
dt09 <- unique(dt08[, .(Cruise, Station, DateTime, Longitude, Latitude, Local_CDI_ID, EDMO_code, Sounding, Instrument, DataSource, StationID)])

# Identify and Remove duplicates
dt10 <- dt09[!duplicated(dt09, by = c("Cruise", "Station", "DateTime", "Longitude", "Latitude", "Local_CDI_ID", "EDMO_code")), ]

# Make stations spatial keeping original latitude/longitude
dt10 <- st_as_sf(dt10, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

# Transform projection into ETRS_1989_LAEA
#dt10 <- st_transform(dt10, crs = 3035)

# Classify stations into sea regions
dt10$SeaRegionID <- st_intersects(dt10, units) %>% as.numeric()

# Remove spatial column
dt10 <- st_set_geometry(dt10, NULL)

dt10[order(SeaRegionID), .N, SeaRegionID]

# Delete stations which haven't been classified into SeaRegions
dt10 <- dt10[!is.na(SeaRegionID)]

# Identify ICES stations not in EMODnet
dt10[, Year := as.integer(substr(DateTime, 1, 4))]
dt10[, Month := as.integer(substr(DateTime, 6, 7))]
dt10[, Day := as.integer(substr(DateTime, 9, 10))]
dt10[, Hour := as.integer(substr(DateTime, 12, 13))]
dt10[, Minute := as.integer(substr(DateTime, 15, 16))]
dt10[, MinuteLow := Minute - 3] # 34 = 31, 2 = -1
dt10[, MinuteHigh := Minute + 3] # 34 = 37, 2 = 5
dt10[, Lon := round(Longitude, 2)]
dt10[, Lat := round(Latitude, 2)]
dt01[, Lon := round(Longitude, 2)]
dt01[, Lat := round(Latitude, 2)]

# Identify ICES stations not in EMODnet
dt01[!dt10, on = .(Year = Year, Month = Month, Day = Day, Hour = Hour, Minute = Minute, Lon = Lon, Lat = Lat)][, .N]
#dt01[!dt10, on = .(Year = Year, Month = Month, Day = Day, Hour = Hour, Minute >= MinuteLow, Minute <= MinuteHigh, Lon = Lon, Lat = Lat)][, .N]
#dt01[!dt10, on = .(Year = Year, Month = Month, Day = Day, Hour = Hour, Minute <= MinuteLow, Minute >= MinuteHigh, Lon = Lon, Lat = Lat)][, .N]

# Identify EMODnet station not in ICES
dt10[!dt01, on = .(Year = Year, Month = Month, Day = Day, Hour = Hour, Minute = Minute, Lon = Lon, Lat = Lat)][, .N]

# Identify EMODnet stations already in ICES
dt10[dt01, nomatch = 0, on = .(Year = Year, Month = Month, Day = Day, Hour = Hour, Minute = Minute, Lon = Lon, Lat = Lat)][, .N]

# Extract ICES / EMODnet samples

# QC
dt01[Sounding == 0 | Sounding >= 9999, .N]

dt10[Sounding == 0 | Sounding >= 9999, .N]

dt08[(Depth < 0 | Depth > 9999) & DepthQ == 1]
dt08[(Temperature < -2 | Temperature > 40) & TemperatureQ == 1, .N]
dt08[(Oxygen < 0 | Oxygen > 15) & OxygenQ == 1, .N]

ggplot(dt08, aes(x = Oxygen, y = Depth)) + geom_point()
