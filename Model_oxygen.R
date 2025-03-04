library(data.table)
library(readr)
library(dplyr)
require(leaflet)


source("utilities_plot.R")
source("utilities_bathymetric.R")

assessmentYear <- 2024
setDTthreads(4)

renewBathymetry = FALSE

if(renewBathymetry){ # TAKES VERY LONG TIME (DAYS)
  
  stationSamples <- fread(file.path("Data", "1980-2023_StationSamplesOxygen.csv.gz"))

  stationSamples <- stationSamples %>%
    # sample_n(200) %>% # for testing
    mutate(
      Bathymetric2 = case_when(
        !is.na(Bathymetric) ~ Bathymetric,
        is.na(Bathymetric) ~ -unlist(map2(Longitude, Latitude, get.bathymetric)
        )
      )
    )
  save(stationSamples, file = "Data/stationSamples_correctedBathymetry")
}

load(file = "Data/stationSamples_correctedBathymetry")


range(stationSamples$Oxygen, na.rm = T)
stationSamples <- stationSamples[Oxygen != -999 & Oxygen != 999,,]

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
  (Depth <= Bathymetric &                               # above sea floor
     case_when(
       Bathymetric < 100 ~ Depth >= Bathymetric - 20,   # max 20 m above sea floor
       Bathymetric >= 100 ~ Depth >= Bathymetric - 50)  # max 50 m above sea floor
  ) & (
    Month >= 7 & Month <= 10                            # summer months
  ) & (
    !is.na(Oxygen) & OxygenQ != 4 & OxygenQ != 8        # one of O2 or H2S is not NA
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
  ] %>%
  # filter(
  #   !(Oxygen >= 15 & Oxygen < 0) 
  # ) %>%
  as.data.table()

summary(wk)
# minimum oxygen = -7. Could be right if it implies H2S
# maximum oxygen = 79. Can NOT be right

View(wk[Oxygen > 10] %>% arrange(-Oxygen))

# High O2 in very shallow places (couple of meters)

wk[Oxygen > 10] %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(label = ~paste(Bathymetric, Depth, Oxygen))

# Bathymetric is NA in some of the points. 

graphics::hist(stationSamples$Oxygen, breaks = 1000)
graphics::hist(wk$Oxygen, breaks = 1000)
graphics::hist(wk$HydrogenSulphide, breaks = 1000)

range(wk$Oxygen, na.rm = T)
range(wk$HydrogenSulphide, na.rm = T)

# Calculate Oxygen Hydrogen Sulphide in mg/l
wk <- wk[, OxygenHydrogenSulphide := ifelse(
  !is.na(Oxygen) & Oxygen != 0, 
  Oxygen / 0.7, # convert ml/l to mg/l
  ifelse(
    !is.na(HydrogenSulphide) | Oxygen == 0, 
    -HydrogenSulphide * 0.022391 / 0.7,  # convert umol/l to ml/l to mg/l 
    NA
  )
)
]

wk %>% 
  sample_n(1000) %>%
  arrange(Oxygen, HydrogenSulphide) %>%
  mutate(x = row_number()) %>%
  ggplot() +
  geom_point(aes(x = x, y = Oxygen)) +
  geom_point(aes(x = x, y = HydrogenSulphide))

## Make map with percentiles (over whole population)

length(which(is.na(wk$OxygenHydrogenSulphide)))

qpal <- colorQuantile("YlGnBu", wk$OxygenHydrogenSulphide, probs = c(0, 0.05, 0.25, 1))

wk %>%
  # sample_n(100000) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 4, 
    stroke = F,
    fillOpacity = 1, 
    fillColor = ~qpal(OxygenHydrogenSulphide)) %>%
  leaflet::addLegend(
    "bottomright", 
    pal = qpal, 
    values = ~OxygenHydrogenSulphide,
    title = "scale",
    labFormat = labelFormat(),
    opacity = 1
  )

# Calculate depth mean --> SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, Depth, AvgTemperature, AvgSalinity, AvgOxygenHydrogenSulphide, MinOxygenHydrogenSulphide, MaxOxygenHydrogenSulphide, CountSamples
wk0 <- wk[
  , .(
    AvgOxygenHydrogenSulphide = mean(OxygenHydrogenSulphide)#, 
    # MinOxygenHydrogenSulphide = min(OxygenHydrogenSulphide), 
    # MaxOxygenHydrogenSulphide = max(OxygenHydrogenSulphide), 
    # SampleCount = .N
  ), .(
    SeaRegionID, 
    ClusterID, 
    Latitude, 
    Longitude, 
    Bathymetric, 
    Year, 
    Depth
  )
]

# Calculate station mean --> SeaRegionID, ClusterID, Latitude, Longitude, Bathymetric, Year, MinDepth, MaxDepth, AvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgOxygenHydrogenSulphide, MinMinOxygenHydrogenSulphide, MaxMaxOxygenHydrogenSulphide, SumCountSamples
wk1 <- wk0[
  , 
  .(MinDepth = min(Depth), 
    MaxDepth = max(Depth), 
    # AvgTemperature = mean(AvgTemperature), 
    # AvgSalinity = mean(AvgSalinity), 
    AvgOxygenHydrogenSulphide = mean(AvgOxygenHydrogenSulphide)#, 
    # MinOxygenHydrogenSulphide = min(MinOxygenHydrogenSulphide), 
    # MaxOxygenHydrogenSulphide = max(MaxOxygenHydrogenSulphide), 
    # SampleCount = sum(SampleCount)
    ), 
  .(
    SeaRegionID, 
    ClusterID, 
    Latitude, 
    Longitude, 
    Bathymetric, 
    Year)
  ]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgAvgSalinity, AvgAvgAvgOxygenHydrogenSulphide, MinMinMinOxygenHydrogenSulphide, MaxMaxMaxOxygenHydrogenSulphide, SumSumCountSamples
wk2 <- wk1[
  , 
  .(
    AvgLatitude = mean(Latitude), 
    AvgLongitude = mean(Longitude), 
    MinDepth = min(MinDepth), 
    MaxDepth = max(MaxDepth), 
    # AvgTemperature = mean(AvgTemperature), 
    # AvgSalinity = mean(AvgSalinity), 
    AvgOxygenHydrogenSulphide = mean(AvgOxygenHydrogenSulphide)#, 
    # MinOxygenHydrogenSulphide = min(MinOxygenHydrogenSulphide), 
    # MaxOxygenHydrogenSulphide = max(MaxOxygenHydrogenSulphide), 
    # SampleCount = sum(SampleCount)
  ), 
  .(
    SeaRegionID, 
    ClusterID, 
    Year
  )
]

# prefix = "perc25_"
# 
# # Calculate cluster 25 percentile --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year
# Q25all <- wk0[
#   , .(
#     q25 = quantile(.SD, 0.25, na.rm = TRUE)
#   ), .(
#     SeaRegionID, ClusterID, Year)]
# 
# # Calculate mean of lower quartile 
# mean25perc <- wk0 %>% 
#   left_join(Q25all) %>% 
#   filter(AvgOxygenHydrogenSulphide <= q25) %>%
#   group_by(SeaRegionID, ClusterID, Year) %>%
#   summarize(AvgOxygen = mean(AvgOxygenHydrogenSulphide),
#             AvgLatitude = mean(Latitude),
#             AvgLongitude = mean(Longitude),
#             AvgBathymetric = mean(Bathymetric),
#             AvgDepth = mean(Depth)) %>%
#   as.data.table()
# 
# fwrite(mean25perc, file.path("Output", paste0(prefix, "Oxygen_status.csv")))
# 
# # Plot average status for last 5 years 
# wk21 <- mean25perc[
#   Year > assessmentYear - 5 & Year <= assessmentYear, 
#   .(
#     Oxygen = mean(AvgOxygen), 
#     AvgLongitude = mean(AvgLongitude), 
#     AvgLatitude = mean(AvgLatitude)
#   ), 
#   .(ClusterID)]
# 
# plotStatusMaps(
#   bboxEurope, 
#   data = wk21, 
#   xlong = "AvgLongitude", 
#   ylat = "AvgLatitude", 
#   parameterValue = "Oxygen", 
#   invJet = T, 
#   limits = "auto"
# )
# 
# saveEuropeStatusMap(parameter = paste0(prefix, "Oxygen"))
# 
# # trend analysis using Kendall test for each oxygen class
# classes <- c("O2_4 mg_l", "4_O2_6 mg_l", "O2_6 mg_l")
# prettyClassNames <- c("O2 < 4 mg/l", "4 < O2 < 6 mg/l", "O2 > 6 mg/l")
# 
# ID_class <- wk21 %>% mutate(
#   class = case_when(
#     Oxygen < 4 ~ 1,
#     Oxygen >= 4 & Oxygen < 6 ~ 2,
#     Oxygen >= 6 ~ 3
#   )
# ) %>% 
#   select(
#     ClusterID, 
#     class
#   ) %>% 
#   as.data.table()
# 
# # merge wk21 and class list
# setkey(mean25perc, ClusterID)
# setkey(ID_class, ClusterID)
# mean25perc2 <- mean25perc[ID_class]
# 
# for(cc in seq(1:length(classes))){
#   
# yearcriteria <- mean25perc2[
#   Year>2006 & class == cc, 
#   unique(ClusterID)
# ]
# 
# clusterSelection <- mean25perc[
#   ClusterID %in% 
#     yearcriteria][
#       , list(
#         NrClustersPerYear = .N, 
#         AvgLatitude = mean(AvgLatitude), 
#         AvgLongitude = mean(AvgLongitude)
#       ), 
#       by = .(ClusterID, Year, SeaRegionID)][
#         , .(NrYears = .N), 
#         by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)][
#           NrYears >=5]
# 
# try(hist(clusterSelection$NrYears))
# 
# wk22 <- mean25perc[
#   ClusterID %in% clusterSelection$ClusterID
# ]
# l <- wk22 %>% 
#   as.data.frame() %>% 
#   split(.$ClusterID)
# 
# timeserieslist <- lapply(
#   l, 
#   function(x) 
#     xts::xts(
#       x[
#         ,"AvgOxygen"
#       ], 
#       order.by = as.Date(as.character(x[,"Year"]),format = "%Y")
#     )
# )
# 
# KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
# df.KendallResult <- as.data.frame(
#   matrix(
#     unlist(
#       list.flatten(
#         KendallResult)
#     ), 
#     ncol  = 5, 
#     byrow = T
#   )
# )
# names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
# df.KendallResult$ClusterID <- as.integer(names(KendallResult))
# KendallResult.clustered <- df.KendallResult %>% 
#   left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
#   filter(!is.na(S)) %>%
#   mutate(trend = case_when(
#     .$sl <= 0.05 & .$S < 0 ~ "decreasing",
#     .$sl <= 0.05 & .$S > 0 ~ "increasing",
#     .$sl > 0.05 ~ "no trend")
#   ) %>%
#   mutate(trend = as.factor(trend))
# 
# KendallResult.clustered$trend <- factor(
#   KendallResult.clustered$trend, 
#   levels =  c("no trend", "decreasing", "increasing")
# )
# 
# 
# fwrite(KendallResult.clustered, paste0("output/", prefix, "trend_dissolvedoxygen", classes[cc], ".csv"))
# 
# plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Oxygen")
# saveEuropeTrendMap(paste(prefix, "Oxygen", classes[cc]))
# 
# }

#=== percentiles =======================================

# Previously calculated for 25 percentile, now testing 5 percentile. 
# names of output files adjust automatically

percentile = "05" # change if necessary

prefix = paste0("perc", percentile, "_", assessmentYear, "_")

Quartile_all <- wk0[
  , 
  .(
    q = quantile(
      .SD,
      as.numeric(percentile)/100, na.rm = TRUE
      )
  ), 
  .(SeaRegionID, ClusterID, Year)
  ]

Quartile_all2 <- wk0 %>%
  group_by(SeaRegionID, ClusterID, Year) %>%
  summarise(
    q = quantile(
      x = AvgOxygenHydrogenSulphide, 
      probs = 0.01, 
      na.rm = TRUE
    ),
    q2 = median(AvgOxygenHydrogenSulphide, na.rm = T),
    n = n(),
    min = min(AvgOxygenHydrogenSulphide),
    max = max(AvgOxygenHydrogenSulphide),
    .groups = "drop"
  )

hist(Quartile_all$q)
hist(Quartile_all2$q)

#check
Quartile_all %>%
  left_join(
    Quartile_all2, 
    by = c(
      SeaRegionID = "SeaRegionID", 
      ClusterID = "ClusterID", 
      Year = "Year")) %>% View
  ggplot(aes(q.x, q.y)) +
  geom_point()

# # Calculate mean of lower quartile 
mean_percentile <- wk0 %>% ungroup() %>%
  left_join(Quartile_all) %>% ungroup() %>%
  filter(AvgOxygenHydrogenSulphide <= q) %>%
  group_by(SeaRegionID, ClusterID, Year) %>%
  summarize(AvgOxygenOrig = mean(AvgOxygenHydrogenSulphide),
            AvgLatitude = mean(Latitude),
            AvgLongitude = mean(Longitude),
            AvgBathymetric = mean(Bathymetric),
            AvgDepth = mean(Depth),
            AvgOxygen = unique(q)) %>%  # = 5% quantile per year and ClusterID
  as.data.table()


fwrite(mean_percentile, file.path("Output", paste0(prefix, "Oxygen_status.csv")))

# Plot average status for last 5 years 
wk21 <- mean_percentile[
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
setkey(mean_percentile, ClusterID)
setkey(ID_class, ClusterID)

mean_percentile2 <- mean_percentile[ID_class]


for(cc in seq(1:length(classes))){
  
  yearcriteria <- mean_percentile2[
    Year>2006 & class == cc, unique(ClusterID)
    ]
  
  clusterSelection <- mean_percentile[
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
  wk22 <- mean_percentile[
    ClusterID %in% clusterSelection$ClusterID
  ]
  if(nrow(wk22 != 0)){
    l <- wk22 %>% 
      as.data.frame() %>% 
      split(.$ClusterID) 
    
    timeserieslist <- lapply(
      l, 
      function(x) {
        xts::xts(
          x[
            ,"AvgOxygen"
          ], order.by = as.Date(
            as.character(
              x[
                ,"Year"
              ]
            ),
            format = "%Y"
          )
        )
      }
    )
    KendallResult <- lapply(
      timeserieslist, 
      function(x) MannKendall(x)
    )
    df.KendallResult <- as.data.frame(
      matrix(
        unlist(
          list.flatten(
            KendallResult
          )
        ), 
        ncol  = 5, 
        byrow = T
      )
    )
    
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
    
    plotKendallClasses(
      plotdata = KendallResult.clustered, 
      parameterValue = paste0(prefix, "Oxygen")
    )
    saveEuropeTrendMap(paste(prefix, "Oxygen", classes[cc]))
  }
}
