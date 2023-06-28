library(data.table)
library(sf)
library(tidyverse)

# Download WISE6 data from:
# https://www.eea.europa.eu/data-and-maps/data/waterbase-water-quality-icm-2
# or
# https://discodata.eea.europa.eu

# Sea regions ------------------------------------------------------------------
sea_regions <- st_read("Input/EEA_SeaRegion_20180831.shp")

# Identify invalid geometries
st_is_valid(sea_regions)

# Make geometries valid
sea_regions <- st_make_valid(sea_regions)

# Monitoring Sites -------------------------------------------------------------

# Read monitoring sites --> 70815
monitoring_sites <- fread("Data/Waterbase_v2021_1_S_WISE6_SpatialObject_DerivedData.csv.gz")

# N.B! Unfortunately the monitoringSiteIdentifier with longitude and latitude aren't unique
# which forces us to identify and remove duplicates.

# Remove empty monitoringSiteIdentifier, lat or lon --> 55042
monitoring_sites <- monitoring_sites[!monitoringSiteIdentifier == "" & !is.na(lat) & !is.na(lon)]

# Calculate unique monitoring sites --> 54910
uniqueN(monitoring_sites[, .(monitoringSiteIdentifier)])

# Remove duplicates and select fields needed --> 54910
monitoring_sites <- monitoring_sites[!duplicated(monitoring_sites, by = c("monitoringSiteIdentifier")), .(monitoringSiteIdentifier, lon, lat)]

# Make stations spatial keeping original latitude/longitude
monitoring_sites <- st_as_sf(monitoring_sites, coords = c("lon", "lat"), remove = FALSE, crs = 4326)

# Classify monitoring sites into sea regions
monitoring_sites$SeaRegionID <- st_intersects(monitoring_sites, sea_regions) %>% as.numeric()

# Remove spatial column
monitoring_sites <- st_set_geometry(monitoring_sites, NULL)

monitoring_sites[order(SeaRegionID), .N, SeaRegionID]

# Delete monitoring sites which haven't been classified into sea regions --> 35576
monitoring_sites <- monitoring_sites[!is.na(SeaRegionID)]

# Data -------------------------------------------------------------------------

# Read data --> 60011857 observations
data <- fread("Data/Waterbase_v2021_1_T_WISE6_DisaggregatedData.csv.gz")

# Filter data by parameters of interest, water body category of interest, making sure a depth exists and result is confirmed correct --> 250900 observations
data <- data[
    observedPropertyDeterminandLabel %in% c('Water temperature','Salinity','Dissolved oxygen','Phosphate','Total phosphorus','Silicate','Nitrate','Nitrite','Ammonium','Total nitrogen','Hydrogen sulphide','pH','Alkalinity','Chlorophyll a','Secchi depth')
  &
    parameterWaterBodyCategory %in% c('CW','TW')
  &
    !is.na(parameterSampleDepth)
  &
    resultObservationStatus == 'A'
  &
    resultQualityObservedValueBelowLOQ == 0
]

# View parameter units
data[, .N, by = .(observedPropertyDeterminandLabel, resultUom)]

# Dcast into samples --> 39496 samples 
data <- dcast(data, monitoringSiteIdentifier + phenomenonTimeSamplingDate + parameterSampleDepth ~ observedPropertyDeterminandLabel, value.var = "resultObservedValue", fun.aggregate = mean)

# Merge latitude longitude into data
data <- monitoring_sites[data, on = .(monitoringSiteIdentifier)]

data <- data[, .(
  Cruise = monitoringSiteIdentifier,
  Station = paste(monitoringSiteIdentifier,phenomenonTimeSamplingDate),
  Year = as.integer(substr(phenomenonTimeSamplingDate, 1, 4)),
  Month = as.integer(substr(phenomenonTimeSamplingDate, 5, 6)),
  Day = as.integer(substr(phenomenonTimeSamplingDate, 7, 8)),
  Longitude = lon,
  Latitude = lat,
  Depth = parameterSampleDepth, # m
  DepthQ = ifelse(is.na(parameterSampleDepth), 9, 1),
  Temperature = `Water temperature`, # degC
  TemperatureQ = ifelse(is.na(`Water temperature`), 9, 1),
  Salinity = Salinity, # psu
  SalinityQ = ifelse(is.na(Salinity), 9, 1),
  Oxygen = `Dissolved oxygen` * 0.7, # mg/l --> ml/l
  OxygenQ = ifelse(is.na(`Dissolved oxygen`), 9, 1),
  Phosphate = Phosphate * 1000 * 0.032285, # mg{P}/l --> umol/l !!! P or PO4
  PhosphateQ = ifelse(is.na(Phosphate), 9, 1),
  TotalPhosphorus = `Total phosphorus` * 1000 * 0.032285, # mg{P}/l --> umol/l
  TotalPhosphorusQ = ifelse(is.na(`Total phosphorus`), 9, 1),
  Silicate = Silicate * 1000 * 0.035606, # mg{Si}/l --> umol/l !!! Si or SiO4
  SilicateQ = ifelse(is.na(Silicate), 9, 1),
  Nitrate = Nitrate * 1000 * 0.016128, # mg{NO3}/l --> umol/l
  NitrateQ = ifelse(is.na(Nitrate), 9, 1),
  Nitrite = Nitrite * 1000 * 0.021736, # mg{NO2}/l --> umol/l
  NitriteQ = ifelse(is.na(Nitrite), 9, 1),
  Ammonium = Ammonium * 1000 * 0.055437, # mg{NH4}/l --> umol/l
  AmmoniumQ = ifelse(is.na(Ammonium), 9, 1),
  TotalNitrogen = `Total nitrogen` * 1000 * 0.071394, # mg{N}/l --> umol/l
  TotalNitrogenQ = ifelse(is.na(`Total nitrogen`), 9, 1),
  HydrogenSulphide = NA,
  HydrogenSulphideQ = 9,
  pH = pH,
  pHQ = ifelse(is.na(pH), 9, 1),
  Alkalinity = NA,
  AlkalinityQ = 9,
  Chlorophyll = `Chlorophyll a`,
  ChlorophyllQ = ifelse(is.na(`Chlorophyll a`), 9, 1)
  )]

# Replace NA for NaN 
data <- data[, lapply(.SD, function(x) replace(x, is.nan(x), NA))]

# Extract stations
stations <- unique(data[, .(Cruise, Station, Year, Month, Day, Longitude, Latitude)])












