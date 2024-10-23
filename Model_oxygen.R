library(data.table)
library(readr)
library(dplyr)
require(leaflet)

source("utilities_plot.R")

assessmentYear <- 2022
getDTthreads()
setDTthreads(4)
rm(stationSamples)

stationSamples <- fread(file.path("Data", "1980-2023_StationSamplesOxygen.csv.gz"))

stationSamples %>%
  sample_n(10000) %>%
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 1)



# Station Samples Summary
# To Do - Make a summary output per indicator taking the indicator criteria into account 
stationSamplesSummary <- stationSamples[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = c(10,12,14), .(DataSourceID)]


# DissolvedOxygen (Summer/Autumn) -----------------------------------------------------
#   Parameters: Dissolved Oxygen
#   Depth: <= 10 m above seafloor; Adapted to <= if(sounding < 100) 20 else 50
#   Period: July - October
#   Aggregation Method: mean of lower quartile by station and cluster per year
#   per class (<4, 4-6, >6) trend maps

# Filter stations rows and columns --> DataSourceID, SeaRegionID, ClusterID, Latitude, Longitude, Sounding, Bathymetric, Year, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide
wk <- stationSamples[
  (Depth <= Bathymetric & case_when(
    Bathymetric < 100 ~ Depth >= Bathymetric - 20, 
    Bathymetric >= 100 ~ Depth >= Bathymetric - 50)
  ) & (
    Month >= 7 & Month <= 10
  ) & (
    !is.na(Oxygen) & OxygenQ != 4 & OxygenQ != 8
  ) | (
    !is.na(HydrogenSulphide) & HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4
  ),
  .(DataSourceID, 
    SeaRegionID, 
    ClusterID, 
    Latitude, 
    Longitude, 
    Sounding, 
    Bathymetric, 
    Year, 
    Depth, 
    Oxygen, 
    HydrogenSulphide)
  ]

# Calculate Oxygen Hydrogen Sulphide in mg/l
wk[, OxygenHydrogenSulphide := ifelse(!is.na(Oxygen), Oxygen / 0.7, # convert ml/l to mg/l
                      ifelse(!is.na(HydrogenSulphide), -HydrogenSulphide * 0.022391 / 0.7, NA))] # convert umol/l to ml/l to mg/l

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, Depth, AvgTemperature, AvgSalinity, AvgOxygenHydrogenSulphide, MinOxygenHydrogenSulphide, MaxOxygenHydrogenSulphide, CountSamples
wk0 <- wk[, .(AvgOxygenHydrogenSulphide = mean(OxygenHydrogenSulphide), MinOxygenHydrogenSulphide = min(OxygenHydrogenSulphide), MaxOxygenHydrogenSulphide = max(OxygenHydrogenSulphide), SampleCount = .N), .(SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, Depth)]

# Calculate station mean --> SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgOxygenHydrogenSulphide, MinMinOxygenHydrogenSulphide, MaxMaxOxygenHydrogenSulphide, SumCountSamples
#wk1 <- wk0[, .(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgOxygenHydrogenSulphide = mean(AvgOxygenHydrogenSulphide), MinOxygenHydrogenSulphide = min(MinOxygenHydrogenSulphide), MaxOxygenHydrogenSulphide = max(MaxOxygenHydrogenSulphide), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgOxygenHydrogenSulphide, MinMinMinOxygenHydrogenSulphide, MaxMaxMaxOxygenHydrogenSulphide, SumSumCountSamples
#wk2 <- wk1[, .(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgOxygenHydrogenSulphide = mean(AvgOxygenHydrogenSulphide), MinOxygenHydrogenSulphide = min(MinOxygenHydrogenSulphide), MaxOxygenHydrogenSulphide = max(MaxOxygenHydrogenSulphide), SampleCount = sum(SampleCount)), .(SeaRegionID, ClusterID, Year)]

prefix = "perc25_"

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

fwrite(mean25perc, file.path("Output", paste0(prefix, "Oxygen_status.csv")))

# Plot average status for last 5 years 
wk21 <- mean25perc[Year > assessmentYear - 5 & Year <= assessmentYear, .(Oxygen = mean(AvgOxygen), AvgLongitude = mean(AvgLongitude), AvgLatitude = mean(AvgLatitude)), .(ClusterID)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", 
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = paste0(prefix, "Oxygen"))

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
try(hist(clusterSelection$NrYears))
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


fwrite(KendallResult.clustered, paste0("output/", prefix, "trend_dissolvedoxygen", classes[cc], ".csv"))

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Oxygen")
saveEuropeTrendMap(paste(prefix, "Oxygen", classes[cc]))

}

#=== 5 percentiles =======================================

prefix = "perc05_"

# Calculate cluster 5 percentile --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year
# Q05all <- wk0[, .(q05 = quantile(.SD, 0.05, na.rm = TRUE)), .(SeaRegionID, ClusterID, Year)]


# # Calculate mean of lower quartile 
mean05perc <- wk0 %>% ungroup() %>%
  left_join(Q05all) %>% ungroup() %>%
  filter(AvgOxygenHydrogenSulphide <= q05) %>%
  group_by(SeaRegionID, ClusterID, Year) %>%
  summarize(AvgOxygenOrig = mean(AvgOxygenHydrogenSulphide),
            AvgLatitude = mean(Latitude),
            AvgLongitude = mean(Longitude),
            AvgBathymetric = mean(Bathymetric),
            AvgDepth = mean(Depth),
            AvgOxygen = unique(q05)) %>%  # = 5% quantile per year and ClusterID
  as.data.table()


fwrite(mean05perc, file.path("Output", paste0(prefix, "Oxygen_status.csv")))

# Plot average status for last 5 years 
wk21 <- mean05perc[
  Year > assessmentYear - 5 & Year <= assessmentYear, .(
    Oxygen = mean(AvgOxygen), 
    AvgLongitude = mean(AvgLongitude), 
    AvgLatitude = mean(AvgLatitude)), 
  .(ClusterID)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", 
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = paste0(prefix, "Oxygen"))

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
setkey(mean05perc, ClusterID)
setkey(ID_class, ClusterID)

mean05perc2 <- mean05perc[ID_class]


for(cc in seq(1:length(classes))){
  
  yearcriteria <- mean05perc2[
    Year>2006 & class == cc, unique(ClusterID)
    ]
  
  clusterSelection <- mean05perc[
    ClusterID %in% yearcriteria
  ][,
    list(NrClustersPerYear = .N, 
         AvgLatitude = mean(AvgLatitude), 
         AvgLongitude = mean(AvgLongitude)
    ), 
    by = .(ClusterID, Year, SeaRegionID)][
      , 
      .(NrYears = .N), 
      by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)][
        NrYears >=5
      ]
  
  try(hist(clusterSelection$NrYears))
  wk22 <- mean05perc[ClusterID %in% clusterSelection$ClusterID]
  if(nrow(wk22 != 0)){
    l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
    timeserieslist <- lapply(
      l, function(x) xts::xts(x[,"Oxygen"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
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
    
    
    fwrite(KendallResult.clustered, paste0("output/", prefix, "trend_dissolvedoxygen", classes[cc], ".csv"))
    
    plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = paste0(prefix, "Oxygen"))
    saveEuropeTrendMap(paste(prefix, "Oxygen", classes[cc]))
  }
}



