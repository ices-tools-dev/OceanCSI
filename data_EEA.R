library(data.table)

source("utilities_searegion.R")

# Download WISE6 data from:
# https://www.eea.europa.eu/data-and-maps/data/waterbase-water-quality-icm-2
# or
# https://discodata.eea.europa.eu

# Read monitoring_sites --> 71843
monitoring_sites <- fread("Input/Waterbase_v2022_1_S_WISE6_SpatialObject_DerivedData.csv.gz")

# N.B! Unfortunately the monitoringSiteIdentifier with longitude and latitude aren't unique
# which forces us to identify and remove duplicates.

# Remove empty monitoringSiteIdentifier, lat or lon --> 55943
monitoring_sites <- monitoring_sites[!monitoringSiteIdentifier == "" & !is.na(lat) & !is.na(lon)]

# Calculate unique monitoring_sites --> 55820
uniqueN(monitoring_sites[, .(monitoringSiteIdentifier)])

# Remove duplicates and select fields needed --> 55820
monitoring_sites <- monitoring_sites[!duplicated(monitoring_sites, by = c("monitoringSiteIdentifier")), .(monitoringSiteIdentifier, lon, lat)]

# Read observations --> 61793906 observations
observations <- fread("Data/Waterbase_v2022_1_T_WISE6_DisaggregatedData.csv.gz")

# Filter observations by parameters of interest, water body category of interest, making sure a depth exists and result is confirmed correct --> 196202 observations
observations <- observations[
    observedPropertyDeterminandLabel %in% c('Water temperature','Salinity','Dissolved oxygen','Phosphate','Total phosphorus','Nitrate','Nitrite','Ammonium','Total nitrogen','Chlorophyll a')
  &
    parameterWaterBodyCategory %in% c('CW','TW')
  &
    !is.na(parameterSampleDepth)
  &
    resultObservationStatus == 'A'
  &
    resultQualityObservedValueBelowLOQ == 0
]

# View observations parameter units
observations[, .N, by = .(observedPropertyDeterminandLabel, resultUom)]

# Dcast into samples --> 40801 samples 
samples <- dcast(observations, monitoringSiteIdentifier + phenomenonTimeSamplingDate + parameterSampleDepth ~ observedPropertyDeterminandLabel, value.var = "resultObservedValue", fun.aggregate = mean)

# Merge stations i.e. monitoring sites (latitude longitude) into samples
stationSamples <- monitoring_sites[samples, on = .(monitoringSiteIdentifier)]

# Replace NA for NaN 
stationSamples <- stationSamples[, lapply(.SD, function(x) replace(x, is.nan(x), NA))]

stationSamples <- stationSamples[, .(
  Cruise = monitoringSiteIdentifier,
  Station = paste(monitoringSiteIdentifier, phenomenonTimeSamplingDate),
  Year = as.integer(substr(phenomenonTimeSamplingDate, 1, 4)),
  Month = as.integer(substr(phenomenonTimeSamplingDate, 5, 6)),
  Day = as.integer(substr(phenomenonTimeSamplingDate, 7, 8)),
  Longitude..degrees_east. = lon,
  Latitude..degrees_north. = lat,
  DataSourceID = 10,
  Depth..m. = parameterSampleDepth, # m
  QV.ODV.Depth..m. =  ifelse(is.na(parameterSampleDepth), "1", "0"),
  Temperature..degC. = `Water temperature`, # degC
  QV.ODV.Temperature..degC. = ifelse(is.na(`Water temperature`), "1", "0"),
  Practical.Salinity..dmnless. = Salinity, # psu
  QV.ODV.Practical.Salinity..dmnless. = ifelse(is.na(Salinity), "1", "0"),
  Dissolved.Oxygen..ml.l. = `Dissolved oxygen` * 0.7, # mg/l --> ml/l
  QV.ODV.Dissolved.Oxygen..ml.l. = ifelse(is.na(`Dissolved oxygen`), "1", "0"),
  Phosphate.Phosphorus..PO4.P...umol.l. = Phosphate * 1000 * 0.032285, # mg{P}/l --> umol/l !!! Assume PO4-P and not PO4
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = ifelse(is.na(Phosphate), "1", "0"),
  Total.Phosphorus..P...umol.l. = `Total phosphorus` * 1000 * 0.032285, # mg{P}/l --> umol/l
  QV.ODV.Total.Phosphorus..P...umol.l. = ifelse(is.na(`Total phosphorus`), "1", "0"),
  Nitrate.Nitrogen..NO3.N...umol.l. = Nitrate * 1000 * 0.016128, # mg{NO3}/l --> umol/l !!! Assume NO3 and not NO3-N
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Nitrate), "1", "0"),
  Nitrite.Nitrogen..NO2.N...umol.l. = Nitrite * 1000 * 0.021736, # mg{NO2}/l --> umol/l !!! Assume NO2 and not NO2-N
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = ifelse(is.na(Nitrite), "1", "0"),
  Ammonium.Nitrogen..NH4.N...umol.l. = Ammonium * 1000 * 0.055437, # mg{NH4}/l --> umol/l !!! Assume NH4 and not NH4-N
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = ifelse(is.na(Ammonium), "1", "0"),
  Total.Nitrogen..N...umol.l. = `Total nitrogen` * 1000 * 0.071394, # mg{N}/l --> umol/l
  QV.ODV.Total.Nitrogen..N...umol.l. = ifelse(is.na(`Total nitrogen`), "1", "0"),
  Chlorophyll.a..ug.l. = `Chlorophyll a`, # ug/l
  QV.ODV.Chlorophyll.a..ug.l. = ifelse(is.na(`Chlorophyll a`), "1", "0")
  )]

# Free memory
rm(monitoring_sites, observations, samples)

# Extract unique locations i.e. longitude/latitude pairs --> 843 positions
locations <- unique(stationSamples[, .(Longitude..degrees_east., Latitude..degrees_north.)])

# Classify locations into sea regions
locations <- classify_locations_into_searegions(locations)

# Merge locations incl. sea regions back into station samples - getting rid of station samples not classified --> 40801 station samples
stationSamples <- locations[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.), nomatch = 0]

# Output station samples
fwrite(stationSamples, file.path("Data", "StationSamples_EEA.csv.gz"))

# Free memory
rm(locations, stationSamples, classify_locations_into_searegions)
