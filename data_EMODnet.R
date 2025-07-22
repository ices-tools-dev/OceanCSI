library(data.table)

source("utilities_searegion.R")

qflag <- function(qflag) {
  ifelse(qflag %in% c("1", "2", "5", "6", "Q"), "0",
         ifelse(qflag %in% c("0", "7", "8", "9", "A", "B"), "1",
                ifelse(qflag == "3", "4",
                       ifelse(qflag == "4", "8", "1"))))
}

# EMODnet data are delivered by EMODnet chemistry ------------------------------

# Read EMODnet Arctic Sea profiles --> 10,053,920 (2022) --> 12,294,777 (2024) --> 11,867,253 (2025) station samples
stationSamplesArctic <- fread(input = "Input/EMD_eutrophication_Arctic_profiles_2025_EEA.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE, colClasses = list(character = c(1,2,3,4,7,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,41), numeric = c(5,6,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)))
stationSamplesArctic <- stationSamplesArctic[yyyy.mm.ddThh.mm.ss.sss >= "1980", .(
  Cruise,
  Station,
  Type,
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  LOCAL_CDI_ID,
  EDMO_code,
  Instrument...gear.type,
  DataSourceID = 4,
  Depth..m.,
  QV.ODV.Depth..m. = QV.SEADATANET %>% qflag(),
  Temperature..degC. = ITS.90.water.temperature..degrees.C.,
  QV.ODV.Temperature..degC. = QV.SEADATANET.1 %>% qflag(),
  Practical.Salinity..dmnless. = Water.body.salinity..per.mille.,
  QV.ODV.Practical.Salinity..dmnless. = QV.SEADATANET.2 %>% qflag(),
  Dissolved.Oxygen..ml.l. = Water.body.dissolved.oxygen.concentration..umol.l. * 0.022391,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.SEADATANET.3 %>% qflag(),
  Phosphate.Phosphorus..PO4.P...umol.l. = Water.body.phosphate..umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.SEADATANET.7 %>% qflag(),
  Total.Phosphorus..P...umol.l. = Water.body.total.phosphorus..umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.SEADATANET.11 %>% qflag(),
  Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), Water.body.nitrate.plus.nitrite_origin..umol.l., Water.body.nitrate..umol.l.),
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), QV.SEADATANET.5, QV.SEADATANET.4) %>% qflag(),
  Nitrite.Nitrogen..NO2.N...umol.l. = Water.body.nitrite..umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.SEADATANET.6 %>% qflag(),
  Ammonium.Nitrogen..NH4.N...umol.l. = Water.body.ammonium..umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.SEADATANET.8 %>% qflag(),
  Total.Nitrogen..N...umol.l. = Water.body.total.nitrogen..umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.SEADATANET.10 %>% qflag(),
  Chlorophyll.a..ug.l. = Water.body.chlorophyll.a..mg.m.3.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.SEADATANET.9 %>% qflag()
)]

# Read EMODnet Atlantic Sea profiles --> 9,638,644 (2022) --> 9,985,414 (2024) --> 14,467,929 (2025) station samples
stationSamplesAtlantic <- fread(input = "Input/EMD_eutrophication_Atlantic_profiles_2025_EEA.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE, colClasses = list(character = c(1,2,3,4,7,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,41), numeric = c(5,6,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)))
stationSamplesAtlantic <- stationSamplesAtlantic[yyyy.mm.ddThh.mm.ss.sss >= "1980", .(
  Cruise,
  Station,
  Type,
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  LOCAL_CDI_ID,
  EDMO_code,
  Instrument...gear.type,
  DataSourceID = 5,
  Depth..m.,
  QV.ODV.Depth..m. = QV.SEADATANET %>% qflag(),
  Temperature..degC. = ITS.90.water.temperature..degrees.C.,
  QV.ODV.Temperature..degC. = QV.SEADATANET.1 %>% qflag(),
  Practical.Salinity..dmnless. = Water.body.salinity..per.mille.,
  QV.ODV.Practical.Salinity..dmnless. = QV.SEADATANET.2 %>% qflag(),
  Dissolved.Oxygen..ml.l. = Water.body.dissolved.oxygen.concentration..umol.l. * 0.022391,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.SEADATANET.3 %>% qflag(),
  Phosphate.Phosphorus..PO4.P...umol.l. = Water.body.phosphate..umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.SEADATANET.8 %>% qflag(),
  Total.Phosphorus..P...umol.l. = Water.body.total.phosphorus..umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.SEADATANET.11 %>% qflag(),
  Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), Water.body.nitrate.plus.nitrite_origin..umol.l., Water.body.nitrate..umol.l.),
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), QV.SEADATANET.5, QV.SEADATANET.4) %>% qflag(),
  Nitrite.Nitrogen..NO2.N...umol.l. = Water.body.nitrite..umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.SEADATANET.6 %>% qflag(),
  Ammonium.Nitrogen..NH4.N...umol.l. = Water.body.ammonium..umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.SEADATANET.7 %>% qflag(),
  Total.Nitrogen..N...umol.l. = Water.body.total.nitrogen..umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.SEADATANET.10 %>% qflag(),
  Chlorophyll.a..ug.l. = Water.body.chlorophyll.a..mg.m.3.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.SEADATANET.9 %>% qflag()
)]

# Read EMODnet Baltic Sea profiles --> 4,478,965 (2022) --> 4,812,445 (2024) --> 4,871,042 (2025) station samples
stationSamplesBaltic <- fread(input = "Input/EMD_eutrophication_Baltic_profiles_2025_EEA.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE, colClasses = list(character = c(1,2,3,4,7,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,41), numeric = c(5,6,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)))
stationSamplesBaltic <- stationSamplesBaltic[yyyy.mm.ddThh.mm.ss.sss >= "1980", .(
  Cruise,
  Station,
  Type,
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  LOCAL_CDI_ID,
  EDMO_code,
  Instrument...gear.type,
  DataSourceID = 6,
  Depth..m.,
  QV.ODV.Depth..m. = QV.SEADATANET %>% qflag(),
  Temperature..degC. = ITS.90.water.temperature..degrees.C.,
  QV.ODV.Temperature..degC. = QV.SEADATANET.1 %>% qflag(),
  Practical.Salinity..dmnless. = Water.body.salinity..per.mille.,
  QV.ODV.Practical.Salinity..dmnless. = QV.SEADATANET.2 %>% qflag(),
  Dissolved.Oxygen..ml.l. = Water.body.dissolved.oxygen.concentration..umol.l. * 0.022391,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.SEADATANET.3 %>% qflag(),
  Phosphate.Phosphorus..PO4.P...umol.l. = Water.body.phosphate..umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.SEADATANET.7 %>% qflag(),
  Total.Phosphorus..P...umol.l. = Water.body.total.phosphorus..umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.SEADATANET.11 %>% qflag(),
  Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), Water.body.nitrate.plus.nitrite_origin..umol.l., Water.body.nitrate..umol.l.),
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), QV.SEADATANET.5, QV.SEADATANET.4) %>% qflag(),
  Nitrite.Nitrogen..NO2.N...umol.l. = Water.body.nitrite..umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.SEADATANET.6 %>% qflag(),
  Ammonium.Nitrogen..NH4.N...umol.l. = Water.body.ammonium..umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.SEADATANET.8 %>% qflag(),
  Total.Nitrogen..N...umol.l. = Water.body.total.nitrogen..umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.SEADATANET.10 %>% qflag(),
  Chlorophyll.a..ug.l. = Water.body.chlorophyll.a..mg.m.3.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.SEADATANET.9 %>% qflag()
)]

# Read EMODnet Black Sea profiles --> 3,500,795 (2022) --> 3,647,167 (2024) --> 3,044,344 (2025) station samples
stationSamplesBlack <- fread(input = "Input/EMD_eutrophication_BlackSea_profiles_2025_EEA.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE, colClasses = list(character = c(1,2,3,4,7,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,41), numeric = c(5,6,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)))
stationSamplesBlack <- stationSamplesBlack[yyyy.mm.ddThh.mm.ss.sss >= "1980", .(
  Cruise,
  Station,
  Type,
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  LOCAL_CDI_ID,
  EDMO_code,
  Instrument...gear.type,
  DataSourceID = 7,
  Depth..m.,
  QV.ODV.Depth..m. = QV.SEADATANET %>% qflag(),
  Temperature..degC. = ITS.90.water.temperature..degrees.C.,
  QV.ODV.Temperature..degC. = QV.SEADATANET.1 %>% qflag(),
  Practical.Salinity..dmnless. = Water.body.salinity..per.mille.,
  QV.ODV.Practical.Salinity..dmnless. = QV.SEADATANET.2 %>% qflag(),
  Dissolved.Oxygen..ml.l. = Water.body.dissolved.oxygen.concentration..umol.l. * 0.022391,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.SEADATANET.3 %>% qflag(),
  Phosphate.Phosphorus..PO4.P...umol.l. = Water.body.phosphate..umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.SEADATANET.4 %>% qflag(),
  Total.Phosphorus..P...umol.l. = Water.body.total.phosphorus..umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.SEADATANET.5 %>% qflag(),
  Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), Water.body.nitrate.plus.nitrite_origin..umol.l., Water.body.nitrate..umol.l.),
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), QV.SEADATANET.7, QV.SEADATANET.6) %>% qflag(),
  Nitrite.Nitrogen..NO2.N...umol.l. = Water.body.nitrite..umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.SEADATANET.8 %>% qflag(),
  Ammonium.Nitrogen..NH4.N...umol.l. = Water.body.ammonium..umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.SEADATANET.10 %>% qflag(),
  Total.Nitrogen..N...umol.l. = Water.body.total.nitrogen..umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.SEADATANET.9 %>% qflag(),
  Chlorophyll.a..ug.l. = Water.body.chlorophyll.a..mg.m.3.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.SEADATANET.11 %>% qflag()
)]

# Read EMODnet Mediterranean Sea profiles --> 23,885,401 (2022) --> 26,290,782 (2024) --> 26,557,146 (2025) station samples
stationSamplesMediterranean <- fread(input = "Input/EMD_eutrophication_Med_profiles_2025_EEA.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE, colClasses = list(character = c(1,2,3,4,7,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,41), numeric = c(5,6,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)))
stationSamplesMediterranean <- stationSamplesMediterranean[yyyy.mm.ddThh.mm.ss.sss >= "1980", .(
  Cruise,
  Station,
  Type,
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  LOCAL_CDI_ID,
  EDMO_code,
  Instrument...gear.type,
  DataSourceID = 8,
  Depth..m.,
  QV.ODV.Depth..m. = QV.SEADATANET %>% qflag(),
  Temperature..degC. = ITS.90.water.temperature..degrees.C.,
  QV.ODV.Temperature..degC. = QV.SEADATANET.1 %>% qflag(),
  Practical.Salinity..dmnless. = Water.body.salinity..per.mille.,
  QV.ODV.Practical.Salinity..dmnless. = QV.SEADATANET.2 %>% qflag(),
  Dissolved.Oxygen..ml.l. = Water.body.dissolved.oxygen.concentration..umol.l. * 0.022391,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.SEADATANET.3 %>% qflag(),
  Phosphate.Phosphorus..PO4.P...umol.l. = Water.body.phosphate..umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.SEADATANET.7 %>% qflag(),
  Total.Phosphorus..P...umol.l. = Water.body.total.phosphorus..umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.SEADATANET.11 %>% qflag(),
  Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), Water.body.nitrate.plus.nitrite_origin..umol.l., Water.body.nitrate..umol.l.),
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), QV.SEADATANET.5, QV.SEADATANET.4) %>% qflag(),
  Nitrite.Nitrogen..NO2.N...umol.l. = Water.body.nitrite..umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.SEADATANET.6 %>% qflag(),
  Ammonium.Nitrogen..NH4.N...umol.l. = Water.body.ammonium..umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.SEADATANET.8 %>% qflag(),
  Total.Nitrogen..N...umol.l. = Water.body.total.nitrogen..umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.SEADATANET.10 %>% qflag(),
  Chlorophyll.a..ug.l. = Water.body.chlorophyll.a..mg.m.3.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.SEADATANET.9 %>% qflag()
)]

# Read EMODnet EMODnet North Sea profiles --> 17,036,665 (2024) --> 10,494,408 (2025) station samples
stationSamplesNorth <- fread(input = "Input/EMD_eutrophication_North_profiles_2025_EEA.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE, colClasses = list(character = c(1,2,3,4,7,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,41), numeric = c(5,6,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)))
stationSamplesNorth <- stationSamplesNorth[yyyy.mm.ddThh.mm.ss.sss >= "1980", .(
  Cruise,
  Station,
  Type,
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  LOCAL_CDI_ID,
  EDMO_code,
  Instrument...gear.type,
  DataSourceID = 9,
  Depth..m.,
  QV.ODV.Depth..m. = QV.SEADATANET %>% qflag(),
  Temperature..degC. = ITS.90.water.temperature..degrees.C.,
  QV.ODV.Temperature..degC. = QV.SEADATANET.1 %>% qflag(),
  Practical.Salinity..dmnless. = Water.body.salinity..per.mille.,
  QV.ODV.Practical.Salinity..dmnless. = QV.SEADATANET.2 %>% qflag(),
  Dissolved.Oxygen..ml.l. = Water.body.dissolved.oxygen.concentration..umol.l. * 0.022391,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.SEADATANET.3 %>% qflag(),
  Phosphate.Phosphorus..PO4.P...umol.l. = Water.body.phosphate..umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.SEADATANET.7 %>% qflag(),
  Total.Phosphorus..P...umol.l. = Water.body.total.phosphorus..umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.SEADATANET.11 %>% qflag(),
  Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), Water.body.dissolved.oxygen.concentration..umol.l., Water.body.nitrate..umol.l.),
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = ifelse(is.na(Water.body.nitrate..umol.l.), QV.SEADATANET.5, QV.SEADATANET.4) %>% qflag(),
  Nitrite.Nitrogen..NO2.N...umol.l. = Water.body.nitrite..umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.SEADATANET.6 %>% qflag(),
  Ammonium.Nitrogen..NH4.N...umol.l. = Water.body.ammonium..umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.SEADATANET.8 %>% qflag(),
  Total.Nitrogen..N...umol.l. = Water.body.total.nitrogen..umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.SEADATANET.10 %>% qflag(),
  Chlorophyll.a..ug.l. = Water.body.chlorophyll.a..mg.m.3.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.SEADATANET.9 %>% qflag()
)]

# Combined data --> 51,557,725 (2022) --> 74,067,250 (2024) --> 71,302,122 (2025) station samples
stationSamples <- rbindlist(list(stationSamplesArctic, stationSamplesAtlantic, stationSamplesBaltic, stationSamplesBlack, stationSamplesMediterranean, stationSamplesNorth), use.names = TRUE)

# Free memory
rm(stationSamplesArctic, stationSamplesAtlantic, stationSamplesBaltic, stationSamplesBlack, stationSamplesMediterranean, stationSamplesNorth)

# Extract unique locations i.e. longitude/latitude pairs --> 730,899 (2022) --> 769,972 (2024) --> 2,254,817 (2025) locations
locations <- unique(stationSamples[, .(Longitude..degrees_east., Latitude..degrees_north.)])

# Classify locations into sea regions --> 1,669,394 (2025) locations
locations <- classify_locations_into_searegions(locations)

# Merge locations incl. sea regions back into station samples - getting rid of station samples not classified --> 37,969,394 (2022) --> 42,066,628 (2024) --> 52,144,608 (2025) station samples
stationSamples <- locations[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.), nomatch = 0]

# Output station samples
fwrite(stationSamples, file.path("Data", "StationSamples_EMODNET.csv.gz"))

# Free memory
rm(locations, stationSamples, classify_locations_into_searegions, qflag)

# Identify and Remove duplicates
#uniqueN(stationSamples[, .(Cruise, Station, yyyy.mm.ddThh.mm.ss.sss, Longitude..degrees_east., Latitude..degrees_north., LOCAL_CDI_ID, EDMO_code, Depth..m.)])
#uniqueN(stationSamples[, .(Cruise, Station, yyyy.mm.ddThh.mm.ss.sss, Longitude..degrees_east., Latitude..degrees_north., LOCAL_CDI_ID, Depth..m.)])
#uniqueN(stationSamples[, .(Cruise, Station, yyyy.mm.ddThh.mm.ss.sss, Longitude..degrees_east., Latitude..degrees_north., Depth..m.)])
#uniqueN(stationSamples[, .(yyyy.mm.ddThh.mm.ss.sss, Longitude..degrees_east., Latitude..degrees_north., Depth..m.)])

#stationSamples <- stationSamples[!duplicated(stationSamples, by = c("Cruise", "Station", "yyyy.mm.ddThh.mm.ss.sss", "Longitude..degrees_east.", "Latitude..degrees_north.", "LOCAL_CDI_ID", "EDMO_code", "Depth..m."))]
