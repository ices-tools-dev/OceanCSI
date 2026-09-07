library(data.table)

source("utilities_plot.R")

assessmentYear <- 2025

#load(file.path("Output", "StationSamples.RData"))
stationSamples <- fread(file.path("Data", "StationSamples.csv.gz"))

# Station Samples for the Oxygen Indicator
stationSamplesOxygen <- stationSamples[!is.na(Oxygen) | !is.na(HydrogenSulphide), .(
  DataSourceID,
  SeaRegionID,
  ClusterID,  
  Latitude,
  Longitude,
  Year,
  Month,
  Sounding,
  Bathymetric, 
  Depth,
  DepthQ,
  Oxygen,
  OxygenQ,
  HydrogenSulphide,
  HydrogenSulphideQ
)]
fwrite(stationSamplesOxygen, file.path("Data", "StationSamplesOxygen.csv.gz"))

# Station Samples for the other indicators
stationSamples <- stationSamples[, .(
  DataSourceID,
  SeaRegionID,
  ClusterID, 
  Cruise,
  Station,  
  Latitude = Latitude..degrees_north.,
  Longitude = Longitude..degrees_east.,
  Year,
  Month,
  Sounding = Bot..Depth..m.,
  Bathymetric = BathymetricAvg, 
  Depth = Depth..m.,
  DepthQ = QV.ODV.Depth..m.,
  Temperature = Temperature..degC.,
  TemperatureQ = QV.ODV.Temperature..degC.,
  Salinity = Practical.Salinity..dmnless.,
  SalinityQ = QV.ODV.Practical.Salinity..dmnless.,
  Phosphate = Phosphate.Phosphorus..PO4.P...umol.l.,
  PhosphateQ = QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l.,
  TotalPhosphorus = Total.Phosphorus..P...umol.l.,
  TotalPhosphorusQ = QV.ODV.Total.Phosphorus..P...umol.l.,
  Nitrate = Nitrate.Nitrogen..NO3.N...umol.l.,
  NitrateQ = QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l.,
  Nitrite = Nitrite.Nitrogen..NO2.N...umol.l.,
  NitriteQ = QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l.,
  Ammonium = Ammonium.Nitrogen..NH4.N...umol.l.,
  AmmoniumQ = QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l.,
  TotalNitrogen = Total.Nitrogen..N...umol.l.,
  TotalNitrogenQ = QV.ODV.Total.Nitrogen..N...umol.l.,
  Chlorophyll = Chlorophyll.a..ug.l.,
  ChlorophyllQ = QV.ODV.Chlorophyll.a..ug.l.
)]

# Station Samples Summary
# To Do - Make a summary output per indicator taking the indicator criteria into account 
#stationSamplesDataSourceSummary <- stationSamples[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = c(10,12,14,16,18,20,22,24,26,28,30,32), .(DataSourceID)]
#fwrite(stationSamplesDataSourceSummary, file.path("Data", "StationSamplesDataSourceSummary.csv"))

#stationSamplesDataSourceSeaRegionSummary <- stationSamples[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = c(10,12,14,16,18,20,22,24,26,28,30,32), .(DataSourceID, SeaRegionID)]
#fwrite(stationSamplesDataSourceSeaRegionSummary, file.path("Data", "StationSamplesDataSourceSeaRegionSummary.csv"))

#locationsDataSourceSeaRegion <- unique(stationSamples[, .(DataSourceID, SeaRegionID, Longitude, Latitude)])

#locationsDataSourceSummary <- locationsDataSourceSeaRegion[, .N, (DataSourceID)]


library(ggplot2)

locationsSeaRegionICES <- unique(stationSamples[DataSourceID <= 3, .(SeaRegionID, Longitude, Latitude)])
ggplot(locationsSeaRegionICES, mapping = aes(Longitude, Latitude)) + geom_point(aes(colour = factor(SeaRegionID)))

locationsSeaRegionEMODNET <- unique(stationSamples[DataSourceID >= 4 & DataSourceID <= 9, .(SeaRegionID, Longitude, Latitude)])
ggplot(locationsSeaRegionEMODNET, mapping = aes(Longitude, Latitude)) + geom_point(aes(colour = factor(SeaRegionID)))

locationsSeaRegionEEA <- unique(stationSamples[DataSourceID == 10, .(SeaRegionID, Longitude, Latitude)])
ggplot(locationsSeaRegionEEA, mapping = aes(Longitude, Latitude)) + geom_point(aes(colour = factor(SeaRegionID)))

# Nitrate Nitrogen (Winter) -------------------------------------------------------------
#   Parameters: [NO3-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate) & (NitrateQ != 4 & NitrateQ != 8), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate)]

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrate, MaxNitrate, CountSamples
wk0 <- wk[, .(AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrate = mean(Nitrate), MinNitrate = min(Nitrate), MaxNitrate = max(Nitrate), SampleCount = .N), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth)]

# Calculate station mean --> SeaRegionID, ClusterID, Latitude, Longitude, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrate, MinMinNitrate, MaxMaxNitrate, SumCountSamples
wk1 <- wk0[, .(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrate = mean(AvgNitrate), MinNitrate = min(AvgNitrate), MaxNitrate = max(AvgNitrate), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Latitude, Longitude, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgNitrate, MinMinMinNitrate, MaxMaxMaxNitrate, SumCountSamples
wk2 <- wk1[, .(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrate = mean(AvgNitrate), MinNitrate = min(MinNitrate), MaxNitrate = max(MaxNitrate), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Year)]

# Write output
fwrite(wk2, file.path("Output", "Nitrate_status.csv"))

# Plot average status for last 5 years 
wk21 <- wk2[Year > assessmentYear - 5 & Year <= assessmentYear, list(Nitrate = mean(AvgNitrate), Longitude = mean(AvgLongitude), Latitude = mean(AvgLatitude)), list(ClusterID)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "Longitude", ylat = "Latitude", 
               parameterValue = "Nitrate", 
               invJet = F, 
               limits = c(0,100))
saveEuropeStatusMap(parameter = "Nitrate")

# trend analysis using Kendall test

# ClusterIDs where one of the years is > 2006
yearcriteria <- wk2[Year > 2006, unique(ClusterID)]
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

fwrite(KendallResult.clustered, file.path("Output", "Nitrate_trend.csv"))

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
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrite) & (NitriteQ != 4 & NitriteQ != 8), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrite)]

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrate, MaxNitrate, CountSamples
wk0 <- wk[, .(AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrite = mean(Nitrite), MinNitrite = min(Nitrite), MaxNitrite = max(Nitrite), SampleCount = .N), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth)]

# Calculate station mean --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrate, MinMinNitrite, MaxMaxNitrite, SumCountSamples
wk1 <- wk0[, .(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrite = mean(AvgNitrite), MinNitrite = min(MinNitrite), MaxNitrite = max(MaxNitrite), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Latitude, Longitude, Year)]

# Calculate cluster mean --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgNitrite, MinMinMinNitrite, MaxMaxMaxNitrite, SumCountSamples
wk2 <- wk1[, .(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrite = mean(AvgNitrite), MinNitrite = min(MinNitrite), MaxNitrite = max(MaxNitrite), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Year)]

# Write output
fwrite(wk2, file.path("Output", "Nitrite_status.csv"))

# Plot average status for last 5 years 
wk21 <- wk2[Year > assessmentYear - 5 & Year <= assessmentYear, list(Nitrite = mean(AvgNitrite)), list(ClusterID, AvgLongitude, AvgLatitude)]
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

fwrite(KendallResult.clustered, file.path("Output", "Nitrite_trend.csv"))

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Nitrite")
saveEuropeTrendMap("Nitrite")

# Ammonium Nitrogen (Winter) ------------------------------------------------------------
#   Parameters: [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> SeaRegionID, ClusterID, Year, Depth, Temperature, Salinity, Ammonium
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Ammonium) & (AmmoniumQ != 4 & AmmoniumQ != 8), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Ammonium)]

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, AvgTemperature, AvgSalinity, AvgAmmonium, MinAmmonium, MaxAmmonium, CountSamples
wk0 <- wk[, .(AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgAmmonium = mean(Ammonium), MinAmmonium = min(Ammonium), MaxAmmonium = max(Ammonium), SampleCount = .N), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth)]

# Calculate station annual average --> SeaRegionID, ClusterID, StationID, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgSalinity, AvgAvgAmmonium, MinMinAmmonium, MaxMaxAmmonium, CountSamples
wk1 <- wk0[, .(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgAmmonium = mean(AvgAmmonium), MinAmmonium = min(MinAmmonium), MaxAmmonium = max(MaxAmmonium), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> SeaRegionID, ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgAmmonium, MinMinMinAmmonium, MaxMaxMaxAmmonium, SumCountSamples
wk2 <- wk1[, .(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgAmmonium = mean(AvgAmmonium), MinAmmonium = min(MinAmmonium), MaxAmmonium = max(MaxAmmonium), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Year)]

fwrite(wk2, file.path("Output", "Ammonium_status.csv"))

# plot average status for last 5 years 
wk21 <- wk2[Year > assessmentYear - 5 & Year <= assessmentYear, list(Ammonium = mean(AvgAmmonium)), list(ClusterID, AvgLongitude, AvgLatitude)]
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

fwrite(KendallResult.clustered, file.path("Output", "Ammonium_trend.csv"))

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Ammonium")
saveEuropeTrendMap("Ammonium")

# Dissolved Inorganic Nitrogen - DIN (Winter) ----------------------------------
#   Parameters: [NO3-N] + [NO2-N] + [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

#Assessment should be done for both EEA grid types:
# 10x10
# 100+20
gridtype<-"10x10"

# Filter stations rows and columns --> SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate|Nitrite|Ammonium) & (NitrateQ != 4 & NitrateQ != 8 & NitriteQ != 4 & NitriteQ != 8 & AmmoniumQ != 4 & AmmoniumQ != 8), .(SeaRegionID, ClusterID, Cruise, Station, Latitude, Longitude, Year, Month, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium)]
coalesce <- function(x) if (all(is.na(x))| is.na(x[1])) NA else sum(x, na.rm = TRUE)
wk$DIN <- apply(wk[, c("Nitrate", "Nitrite", "Ammonium")], 1, coalesce)
wk <- wk[!is.na(DIN)]

# Filter outliers applying quantile limits by SeaRegionID; do not linclude the limit value
wk <- wk[, `:=`(limit_min = quantile(DIN, 0.001),
                limit_max = quantile(DIN, 0.999)),
         by = SeaRegionID]
wk <- wk[DIN > limit_min & DIN < limit_max]

# Calculate means per SeaRegionID, Cruise, Station, month, and year.
wk <- wk[, .(
  Latitude = mean(Latitude),
  Longitude = mean(Longitude),
  DIN = mean(DIN),
  logDIN = mean(log(DIN))
), by = .(SeaRegionID, Cruise, Station, Year, Month)]

# Classify locations into EEA gridcells
source("utilities_grid.R")
locations <- unique(wk[, .(Cruise, Station, Longitude, Latitude)])
locations <- classify_locations_into_gridcells(locations,gridtype)

wk <- locations[wk, on = .(Cruise, Station, Longitude, Latitude), nomatch = 0]

wk[, Year := factor(Year, levels = sort(unique(Year)))]
wk[, Month := factor(Month, levels = sort(unique(Month)))]

# Divide the data in three datasets, each one will be treated differently.
# Min 2 years min 2 month --> emmeans by year and month --- LEVEL 0
wk0 <- wk[, n := uniqueN(Month), by = .(SeaRegionID, CellCode, Year)][n != 1]
wk0 <- wk0[, n := uniqueN(Year), by = .(SeaRegionID, CellCode)][n != 1][, n := NULL]

# Min 2 years x months --> emmeans only by year
wk <- wk[!wk0, on = names(wk0)] # Same as an anti_join --- LEVEL 1
wk1 <- wk[, n := uniqueN(Year), by = .(SeaRegionID, CellCode)][n > 1][, n := NULL]

# Only 1 year --> arithmetic mean --- LEVEL 2
wk2 <- wk[!wk1, on = names(wk1)] # Same as an anti_join

# Apply linear model-emmeans or arithmetic means depending on the data
source("utilities_average.R")
wk0_emmeans <- linear_model_average(df=wk0, level=0, indicator="DIN")
wk1_emmeans <- linear_model_average(df=wk1, level=1, indicator="DIN")
wk2_means <- linear_model_average(df=wk2, level=2, indicator="DIN")

wk <- rbindlist(list(wk0_emmeans, wk1_emmeans, wk2_means), use.names = T, fill = T)

setnames(wk, "avg", "logValue")
setcolorder(wk, c("CellCode", "Year", "SE", "df", "lower.CL", "upper.CL", "Value", "logValue", "type"))

if (gridtype=="10x10"){
  fwrite(wk, file.path("Output", "DIN_status_10x10.csv"))
}else{
  fwrite(wk, file.path("Output", "DIN_status.csv"))
}

# plot average status for last 6 years
wk21 <- wk[Year > assessmentYear - 6 & Year <= assessmentYear]
wk21 <- wk21[, .(log_mean = mean(logValue, na.rm = T), sd= sd(logValue,na.rm = T),value_mean=mean(Value,na.rm=T)), by = CellCode]

plotStatusGridMaps(data = wk21, parameter = "DIN", gridtype=gridtype)
saveEuropeStatusMap(parameter = "DIN",gridtype=gridtype)


# trend analysis using linear model and Kendall test
# Divide the data in two periods
wk31<- wk[Year < 2000]
period<-"PRE2000"

#To calculate the trend, we need at least 5 years of data
requirements <- wk31[, .(NrYears= .N), by = .(CellCode)][NrYears >=5]
wk31 <- wk31[CellCode %in% requirements[[1]]]
wk31[, NrYears := .N, by = .(CellCode)]

minY_pre<-min(wk31$Year,na.rm = T)
maxY_pre<-max(wk31$Year,na.rm = T)

nrYears_pre <- unique(wk31[, .(CellCode,NrYears)])


general_trends_pre<-linear_model_trends(df=wk31,indicator="din",nrYears=nrYears_pre)
general_trends_pre$minY <- minY_pre
general_trends_pre$maxY <- maxY_pre

if (gridtype=="10x10"){
  fwrite(general_trends_pre, file.path("Output", "DIN_trend_10x10_PRE2000.csv"))
}else{
  fwrite(general_trends_pre, file.path("Output", "DIN_trend_PRE2000.csv"))
}

# Repeat process for second period

# Divide the data in two periods
wk32<- wk[Year >= 2000]
period<-"POST2000"

#To calculate the trend, we need at least 5 years of data
requirements <- wk32[, .(NrYears= .N), by = .(CellCode)][NrYears >=5]
wk32 <- wk32[CellCode %in% requirements[[1]]]
wk32[, NrYears := .N, by = .(CellCode)]

minY_post<-min(wk32$Year,na.rm = T)
maxY_post<-max(wk32$Year,na.rm = T)

nrYears_post <- unique(wk32[, .(CellCode,NrYears)])


general_trends_post<-linear_model_trends(df=wk32,indicator="din",nrYears=nrYears_post)
general_trends_post$minY <- minY_post
general_trends_post$maxY <- maxY_post

if (gridtype=="10x10"){
  fwrite(general_trends_post, file.path("Output", "DIN_trend_10x10_POST2000.csv"))
}else{
  fwrite(general_trends_post, file.path("Output", "DIN_trend_POST2000.csv"))
}


# Plot trends of both periods
plot_data_pre<-merge(st_as_sf(grid),general_trends_pre, by = "CellCode", all.x = TRUE)
plot_data_pre<-plot_data_pre%>%
  dplyr::mutate(trend = factor(trend))

plot_data_post<-merge(st_as_sf(grid),general_trends_post, by = "CellCode", all.x = TRUE)
plot_data_post<-plot_data_post%>%
  dplyr::mutate(trend = factor(trend))

plotTrendsGridMaps(pre=plot_data_pre, post=plot_data_post, indicator="DIN")

saveEuropeTrendMap(indicator = "DIN", gridtype=gridtype)


# Total Nitrogen (Annual) ------------------------------------------------------
#   Parameters: [N]
#   Depth: <= 10
#   Period: Annual
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> SeaRegionID, ClusterID, StationID, Year, Depth, Temperature, Salinity, TotalNitrogen
wk <- stationSamples[Depth <= 10 & !is.na(TotalNitrogen) & (TotalNitrogenQ != 4 & TotalNitrogenQ != 8), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, Temperature, Salinity, TotalNitrogen)]

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, AvgTemperature, AvgSalinity, AvgTotalNitrogen, MinTotalNitrogen, MaxTotalNitrogen, CountSamples
wk0 <- wk[, .(AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgTotalNitrogen = mean(TotalNitrogen), MinTotalNitrogen = min(TotalNitrogen), MaxTotalNitrogen = max(TotalNitrogen), SampleCount = .N), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth)]

# Calculate station annual average --> SeaRegionID, ClusterID, Latitude, Longitude, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgSalinity, AvgAvgTotalNitrogen, MinMinTotalNitrogen, MaxMaxTotalNitrogen, CountSamples
wk1 <- wk0[, .(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalNitrogen = mean(AvgTotalNitrogen), MinTotalNitrogen = min(MinTotalNitrogen), MaxTotalNitrogen = max(MaxTotalNitrogen), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> SeaRegionID, ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgTotalNitrogen, MinMinMinTotalNitrogen, MaxMaxMaxTotalNitrogen, SumCountSamples
wk2 <- wk1[, .(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalNitrogen = mean(AvgTotalNitrogen), MinTotalNitrogen = min(MinTotalNitrogen), MaxTotalNitrogen = max(MaxTotalNitrogen), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Year)]

fwrite(wk2, file.path("Output", "TotalNitrogen_status.csv"))

# plot average status for last 5 years 
wk21 <- wk2[Year > assessmentYear - 5 & Year <= assessmentYear, list(TotalNitrogen = mean(AvgTotalNitrogen)), list(ClusterID, AvgLongitude, AvgLatitude)]
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

fwrite(KendallResult.clustered, file.path("Output", "TotalNitrogen_trend.csv"))

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "TotalNitrogen")
saveEuropeTrendMap("TotalNitrogen")

# Phosphate Phosphorus / Dissolved Inorganic Phophorus - DIP (Winter) ---------------------
#   Parameters: [PO4]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

#Assessment should be done for both EEA grid types:
# 10x10
# 100+20
gridtype<-"10x10"

# Filter stations rows and columns --> SeaRegionID, ClusterID, Year, Depth, Temperature, Salinity, Phosphate
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Phosphate) & (PhosphateQ != 4 & PhosphateQ != 8), .(SeaRegionID, ClusterID, Cruise, Station, Latitude, Longitude, Year, Month, Depth, Temperature, Salinity, Phosphate)]

# Filter outliers applying quantile limits by SeaRegionID; do not linclude the limit value
wk <- wk[, `:=`(limit_min = quantile(Phosphate, 0.001),
                limit_max = quantile(Phosphate, 0.999)),
         by = SeaRegionID]
wk <- wk[Phosphate > limit_min & Phosphate < limit_max]

# Calculate means per SeaRegionID, Cruise, Station, month, and year.
wk <- wk[, .(
  Latitude = mean(Latitude),
  Longitude = mean(Longitude),
  Phosphate = mean(Phosphate),
  minPhosphate = min(Phosphate),
  maxPhosphate = max(Phosphate),
  logPhosphate = mean(log(Phosphate))
), by = .(SeaRegionID, Cruise, Station, Year, Month)]

# Classify locations into EEA gridcells
source("utilities_grid.R")
locations <- unique(wk[, .(Cruise, Station, Longitude, Latitude)])
locations <- classify_locations_into_gridcells(locations,gridtype)

wk <- locations[wk, on = .(Cruise, Station, Longitude, Latitude), nomatch = 0]

wk[, Year := factor(Year, levels = sort(unique(Year)))]
wk[, Month := factor(Month, levels = sort(unique(Month)))]

# Divide the data in three datasets, each one will be treated differently.
# Min 2 years min 2 month --> emmeans by year and month --- LEVEL 0
wk0 <- wk[, n := uniqueN(Month), by = .(SeaRegionID, CellCode, Year)][n != 1]
wk0 <- wk0[, n := uniqueN(Year), by = .(SeaRegionID, CellCode)][n != 1][, n := NULL]

# Min 2 years x months --> emmeans only by year
wk <- wk[!wk0, on = names(wk0)] # Same as an anti_join --- LEVEL 1
wk1 <- wk[, n := uniqueN(Year), by = .(SeaRegionID, CellCode)][n > 1][, n := NULL]

# Only 1 year --> arithmetic mean --- LEVEL 2
wk2 <- wk[!wk1, on = names(wk1)] # Same as an anti_join

# Apply linear model-emmeans or arithmetic means depending on the data
source("utilities_average.R")
wk0_emmeans <- linear_model_average(df=wk0, level=0, indicator="DIP")
wk1_emmeans <- linear_model_average(df=wk1, level=1, indicator="DIP")
wk2_means <- linear_model_average(df=wk2, level=2, indicator="DIP")

wk <- rbindlist(list(wk0_emmeans, wk1_emmeans, wk2_means), use.names = T, fill = T)


setnames(wk, "avg", "logValue")
setcolorder(wk, c("CellCode", "Year", "SE", "df", "lower.CL", "upper.CL", "Value", "logValue", "type"))

if (gridtype=="10x10"){
  fwrite(wk, file.path("Output", "DIP_status_10x10.csv"))
}else{
  fwrite(wk, file.path("Output", "DIP_status.csv"))
}

# plot average status for last 6 years
wk21 <- wk[Year > assessmentYear - 6 & Year <= assessmentYear]
wk21 <- wk21[, .(log_mean = mean(logValue, na.rm = T), sd= sd(logValue,na.rm = T),value_mean=mean(Value,na.rm=T)), by = CellCode]

plotStatusGridMaps(data = wk21, parameter = "DIP", gridtype = gridtype)
saveEuropeStatusMap(parameter = "DIP", gridtype = gridtype)

# trend analysis using linear model and Kendall test
# Divide the data in two periods
wk31<- wk[Year < 2000]
period<-"PRE2000"

#To calculate the trend, we need at least 5 years of data
requirements <- wk31[, .(NrYears= .N), by = .(CellCode)][NrYears >=5]
wk31 <- wk31[CellCode %in% requirements[[1]]]
wk31[, NrYears := .N, by = .(CellCode)]

minY_pre<-min(wk31$Year,na.rm = T)
maxY_pre<-max(wk31$Year,na.rm = T)

nrYears_pre <- unique(wk31[, .(CellCode,NrYears)])


general_trends_pre<-linear_model_trends(df=wk31,indicator="DIP",nrYears=nrYears_pre)
general_trends_pre$minY <- minY_pre
general_trends_pre$maxY <- maxY_pre

if (gridtype=="10x10"){
  fwrite(general_trends_pre, file.path("Output", "DIP_trend_10x10_PRE2000.csv"))
}else{
  fwrite(general_trends_pre, file.path("Output", "DIP_trend_PRE2000.csv"))
}

# Repeat process for second period

# Divide the data in two periods
wk32<- wk[Year >= 2000]
period<-"POST2000"

#To calculate the trend, we need at least 5 years of data
requirements <- wk32[, .(NrYears= .N), by = .(CellCode)][NrYears >=5]
wk32 <- wk32[CellCode %in% requirements[[1]]]
wk32[, NrYears := .N, by = .(CellCode)]

minY_post<-min(wk32$Year,na.rm = T)
maxY_post<-max(wk32$Year,na.rm = T)

nrYears_post <- unique(wk32[, .(CellCode,NrYears)])


general_trends_post<-linear_model_trends(df=wk32,indicator="DIP",nrYears=nrYears_post)
general_trends_post$minY <- minY_post
general_trends_post$maxY <- maxY_post

if (gridtype=="10x10"){
  fwrite(general_trends_post, file.path("Output", "DIP_trend_10x10_POST2000.csv"))
}else{
  fwrite(general_trends_post, file.path("Output", "DIP_trend_POST2000.csv"))
}


# Plot trends of both periods
plot_data_pre<-merge(st_as_sf(grid),general_trends_pre, by = "CellCode", all.x = TRUE)
plot_data_pre<-plot_data_pre%>%
  dplyr::mutate(trend = factor(trend))

plot_data_post<-merge(st_as_sf(grid),general_trends_post, by = "CellCode", all.x = TRUE)
plot_data_post<-plot_data_post%>%
  dplyr::mutate(trend = factor(trend))

plotTrendsGridMaps(pre=plot_data_pre, post=plot_data_post, indicator="DIP")

saveEuropeTrendMap(indicator = "DIP", gridtype=gridtype)


# Total Phosphorus (Annual) ----------------------------------------------------
#   Parameters: [P]
#   Depth: <= 10
#   Period: Annual
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> SeaRegionID, ClusterID, Year, Depth, Temperature, Salinity, TotalPhosphorus
wk <- stationSamples[Depth <= 10 & !is.na(TotalPhosphorus) & (TotalPhosphorusQ != 4 & TotalPhosphorusQ != 8), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, Temperature, Salinity, TotalPhosphorus)]

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, AvgTemperature, AvgSalinity, AvgTotalPhosphorus, MinTotalPhosphorus, MaxTotalPhosphorus, CountSamples
wk0 <- wk[, .(AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgTotalPhosphorus = mean(TotalPhosphorus), MinTotalPhosphorus = min(TotalPhosphorus), MaxTotalPhosphorus = max(TotalPhosphorus), SampleCount = .N), .(SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth)]

# Calculate station annual average --> SeaRegionID, ClusterID, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgSalinity, AvgAvgTotalPhosphorus, MinMinTotalPhosphorus, MaxMaxTotalPhosphorus, SumCountSamples
wk1 <- wk0[, .(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalPhosphorus = mean(AvgTotalPhosphorus), MinTotalPhosphorus = min(MinTotalPhosphorus), MaxTotalPhosphorus = max(MaxTotalPhosphorus), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> SeaRegionID, ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgTotalPhosphorus, MinMinMinTotalPhosphorus, MaxMaxMaxTotalPhosphorus, SumSumCountSamples
wk2 <- wk1[, .(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalPhosphorus = mean(AvgTotalPhosphorus), MinTotalPhosphorus = min(MinTotalPhosphorus), MaxTotalPhosphorus = max(MaxTotalPhosphorus), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Year)]

fwrite(wk2, file.path("Output", "TotalPhosphorus_status.csv"))

# plot average status for last 5 years 
wk21 <- wk2[Year > assessmentYear - 5 & Year <= assessmentYear, list(TotalPhosphorus = mean(AvgTotalPhosphorus)), list(ClusterID, AvgLongitude, AvgLatitude)]
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

fwrite(KendallResult.clustered, file.path("Output", "TotalPhosphorus_trend.csv"))

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "TotalPhosphorus")
saveEuropeTrendMap("TotalPhosphorus")

# Chlorophyll a (Summer) -------------------------------------------------------
#   Parameters: Chlorophyll a
#   Depth: <= 10
#   Period: Summer
#     June - September for stations within Baltic Sea north of 59 N
#     May - September for all other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year


#Assessment should be done for both EEA grid types:
# 10x10
# 100+20
gridtype<-"10x10"

# Filter stations rows and columns --> SeaRegionID, ClusterID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Chlorophyll
wk <- stationSamples[Depth <= 10 & ifelse(SeaRegionID == 1 & Latitude > 59, Month >= 6 & Month <= 9, Month >= 5 & Month <= 9) & !is.na(Chlorophyll) & (ChlorophyllQ != 4 & ChlorophyllQ != 8), .(SeaRegionID, ClusterID, Cruise, Station, Latitude, Longitude, Year, Month, Depth, Temperature, Salinity, Chlorophyll)]

# Quantile limits applied to all seasonal dataset, when filtering outliers, do not include the limit value
wk <- wk[, `:=`(limit_min = quantile(Chlorophyll, 0.001),
                    limit_max = quantile(Chlorophyll, 0.999)),
             by = SeaRegionID]

wk <- wk[Chlorophyll > limit_min & Chlorophyll < limit_max]

# Calculate means per clusterID, month, and year.
data <- data[, .(
  Latitude = mean(Latitude),
  Longitude = mean(Longitude),
  Chlorophyll = mean(Chlorophyll),
  minChlorophyll = min(Chlorophyll),
  maxChlorophyll = max(Chlorophyll),
  logChlorophyll = mean(log(Chlorophyll))
), by = .(SeaRegionID, Cruise, Station, Year, Month)]


# Classify locations into EEA gridcells
source("utilities_grid.R")
locations <- unique(wk[, .(Cruise, Station, Longitude, Latitude)])
locations <- classify_locations_into_gridcells(locations,gridtype)

wk <- locations[wk, on = .(Cruise, Station, Longitude, Latitude), nomatch = 0]

wk[, Year := factor(Year, levels = sort(unique(Year)))]
wk[, Month := factor(Month, levels = sort(unique(Month)))]

# Divide the data in three datasets, each one will be treated differently.
# Min 2 years min 2 month --> emmeans by year and month --- LEVEL 0
wk0 <- wk[, n := uniqueN(Month), by = .(SeaRegionID, CellCode, Year)][n != 1]
wk0 <- wk0[, n := uniqueN(Year), by = .(SeaRegionID, CellCode)][n != 1][, n := NULL]

# Min 2 years x months --> emmeans only by year
wk <- wk[!wk0, on = names(wk0)] # Same as an anti_join --- LEVEL 1
wk1 <- wk[, n := uniqueN(Year), by = .(SeaRegionID, CellCode)][n > 1][, n := NULL]

# Only 1 year --> arithmetic mean --- LEVEL 2
wk2 <- wk[!wk1, on = names(wk1)] # Same as an anti_join

# Apply linear model-emmeans or arithmetic means depending on the data
source("utilities_average.R")
wk0_emmeans <- linear_model_average(df=wk0, level=0, indicator="CHL")
wk1_emmeans <- linear_model_average(df=wk1, level=1, indicator="CHL")
wk2_means <- linear_model_average(df=wk2, level=2, indicator="CHL")

wk <- rbindlist(list(wk0_emmeans, wk1_emmeans, wk2_means), use.names = T, fill = T)


setnames(wk, "avg", "logValue")
setcolorder(wk, c("CellCode", "Year", "SE", "df", "lower.CL", "upper.CL", "Value", "logValue", "type"))

if (gridtype=="10x10"){
  fwrite(wk, file.path("Output", "Chlorophyll_status_10x10.csv"))
}else{
  fwrite(wk, file.path("Output", "Chlorophyll_status.csv"))
}
# plot average status for last 6 years
wk21 <- wk[Year > assessmentYear - 6 & Year <= assessmentYear]
wk21 <- wk21[, .(log_mean = mean(logValue, na.rm = T), sd= sd(logValue,na.rm = T),value_mean=mean(Value,na.rm=T)), by = CellCode]


plotStatusGridMaps(data = wk21, parameter = "CHL", gridtype = gridtype)
saveEuropeStatusMap(parameter = "CHL", gridtype = gridtype)

# trend analysis using linear model and Kendall test
# Divide the data in two periods
wk31<- wk[Year < 2000]
period<-"PRE2000"

#To calculate the trend, we need at least 5 years of data
requirements <- wk31[, .(NrYears= .N), by = .(CellCode)][NrYears >=5]
wk31 <- wk31[CellCode %in% requirements[[1]]]
wk31[, NrYears := .N, by = .(CellCode)]

minY_pre<-min(wk31$Year,na.rm = T)
maxY_pre<-max(wk31$Year,na.rm = T)

nrYears_pre <- unique(wk31[, .(CellCode,NrYears)])


general_trends_pre<-linear_model_trends(df=wk31,indicator="CHL",nrYears=nrYears_pre)
general_trends_pre$minY <- minY_pre
general_trends_pre$maxY <- maxY_pre

if (gridtype=="10x10"){
  fwrite(general_trends_pre, file.path("Output", "Chlorophyll_trend_10x10_PRE2000.csv"))
}else{
  fwrite(general_trends_pre, file.path("Output", "Chlorophyll_trend_PRE2000.csv"))
}


# Repeat process for second period

# Divide the data in two periods
wk32<- wk[Year >= 2000]
period<-"POST2000"

#To calculate the trend, we need at least 5 years of data
requirements <- wk32[, .(NrYears= .N), by = .(CellCode)][NrYears >=5]
wk32 <- wk32[CellCode %in% requirements[[1]]]
wk32[, NrYears := .N, by = .(CellCode)]

minY_post<-min(wk32$Year,na.rm = T)
maxY_post<-max(wk32$Year,na.rm = T)

nrYears_post <- unique(wk32[, .(CellCode,NrYears)])


general_trends_post<-linear_model_trends(df=wk32,indicator="CHL",nrYears=nrYears_post)
general_trends_post$minY <- minY_post
general_trends_post$maxY <- maxY_post

if (gridtype=="10x10"){
  fwrite(general_trends_post, file.path("Output", "Chlorophyll_trend_10x10_POST2000.csv"))
}else{
  fwrite(general_trends_post, file.path("Output", "Chlorophyll_trend_POST2000.csv"))
}


# Plot trends of both periods
plot_data_pre<-merge(st_as_sf(grid),general_trends_pre, by = "CellCode", all.x = TRUE)
plot_data_pre<-plot_data_pre%>%
  dplyr::mutate(trend = factor(trend))

plot_data_post<-merge(st_as_sf(grid),general_trends_post, by = "CellCode", all.x = TRUE)
plot_data_post<-plot_data_post%>%
  dplyr::mutate(trend = factor(trend))

plotTrendsGridMaps(pre=plot_data_pre, post=plot_data_post, indicator="CHL")

saveEuropeTrendMap(indicator = "CHL", gridtype=gridtype)

# DissolvedOxygen (Summer/Autumn) -----------------------------------------------------
#   Parameters: Dissolved Oxygen
#   Depth: <= 10 m above seafloor; Adapted to <= if(sounding < 100) 20 else 50
#   Period: July - October
#   Aggregation Method: mean of lower quartile by station and cluster per year
#   per class (<4, 4-6, >6) trend maps

# Filter stations rows and columns --> DataSourceID, SeaRegionID, ClusterID, Latitude, Longitude, Sounding, Bathymetric, Year, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide
wk <- stationSamples[(Depth <= Bathymetric & case_when(Bathymetric < 100 ~ Depth >= Bathymetric - 20, Bathymetric >= 100 ~ Depth >= Bathymetric - 50)) & (Month >= 7 & Month <= 10) & (!is.na(Oxygen) & OxygenQ != 4 & OxygenQ != 8) | (!is.na(HydrogenSulphide) & HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4), .(DataSourceID, SeaRegionID, ClusterID, Latitude, Longitude, Sounding, Bathymetric, Year, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide)]

# Calculate Oxygen Hydrogen Sulphide in mg/l
wk[, OxygenHydrogenSulphide := ifelse(!is.na(Oxygen), Oxygen / 0.7, # convert ml/l to mg/l
                      ifelse(!is.na(HydrogenSulphide), -HydrogenSulphide * 0.022391 / 0.7, NA))] # convert umol/l to ml/l to mg/l

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, Depth, AvgTemperature, AvgSalinity, AvgOxygenHydrogenSulphide, MinOxygenHydrogenSulphide, MaxOxygenHydrogenSulphide, CountSamples
wk0 <- wk[, .(AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgOxygenHydrogenSulphide = mean(OxygenHydrogenSulphide), MinOxygenHydrogenSulphide = min(OxygenHydrogenSulphide), MaxOxygenHydrogenSulphide = max(OxygenHydrogenSulphide), SampleCount = .N), .(SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, Depth)]

# Calculate station mean --> SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgOxygenHydrogenSulphide, MinMinOxygenHydrogenSulphide, MaxMaxOxygenHydrogenSulphide, SumCountSamples
#wk1 <- wk0[, .(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgOxygenHydrogenSulphide = mean(AvgOxygenHydrogenSulphide), MinOxygenHydrogenSulphide = min(MinOxygenHydrogenSulphide), MaxOxygenHydrogenSulphide = max(MaxOxygenHydrogenSulphide), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgOxygenHydrogenSulphide, MinMinMinOxygenHydrogenSulphide, MaxMaxMaxOxygenHydrogenSulphide, SumSumCountSamples
#wk2 <- wk1[, .(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgOxygenHydrogenSulphide = mean(AvgOxygenHydrogenSulphide), MinOxygenHydrogenSulphide = min(MinOxygenHydrogenSulphide), MaxOxygenHydrogenSulphide = max(MaxOxygenHydrogenSulphide), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Year)]

# Calculate cluster 25 percentile --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year
Q25all <- wk0[, .(q25 = quantile(.SD, 0.25, na.rm = TRUE)), .(SeaRegionID, ClusterID, Year)]

# Calculate mean of lower quartile 
mean25perc <- wk0 %>% 
  left_join(Q25all) %>% 
  filter(AvgOxygenHydrogenSulphide <= q25) %>%
  group_by(SeaRegionID, ClusterID, Year) %>%
  summarize(AvgOxygen = mean(AvgOxygenHydrogenSulphide),
            AvgLatitude = mean(Latitude),
            AvgLongitude = mean(Longitude),
            AvgBathymetric = mean(Bathymetric),
            AvgDepth = mean(Depth)) %>%
  as.data.table()

fwrite(mean25perc, file.path("Output", "Oxygen_status.csv"))

# Plot average status for last 5 years 
wk21 <- mean25perc[Year > assessmentYear - 5 & Year <= assessmentYear, .(Oxygen = mean(AvgOxygen), AvgLongitude = mean(AvgLongitude), AvgLatitude = mean(AvgLatitude)), .(ClusterID)]

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

