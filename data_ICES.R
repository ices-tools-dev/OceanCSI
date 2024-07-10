library(data.table)

source("utilities_searegion.R")

# Download ICES data from ICES Data Portal at https://data.ices.dk -------------

# Read and Merge station samples -----------------------------------------------

# ICES Bottle and low resolution CTD data --> 21,641,076 --> 15,389,705 station samples
stationSamplesBOT <- fread(input = "Input/ICES_StationSamples_BOT_2024-07-10.csv.gz", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesBOT <- stationSamplesBOT[as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)) >= 1980, .(
  Cruise,
  Station,
  Type = "B",
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID = 1,
  Depth..m. = Depth..ADEPZZ01_ULAA...m.,
  QV.ODV.Depth..m. = QV.ODV.Depth..ADEPZZ01_ULAA.,
  Temperature..degC. = Temperature..TEMPPR01_UPAA...degC.,
  QV.ODV.Temperature..degC. = QV.ODV.Temperature..TEMPPR01_UPAA.,
  Practical.Salinity..dmnless. = Salinity..PSALPR01_UUUU...dmnless.,
  QV.ODV.Practical.Salinity..dmnless. = QV.ODV.Salinity..PSALPR01_UUUU.,
  Dissolved.Oxygen..ml.l. = Oxygen..DOXYZZXX_UMLL...ml.l.,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.ODV.Oxygen..DOXYZZXX_UMLL.,
  Phosphate.Phosphorus..PO4.P...umol.l. = Phosphate..PHOSZZXX_UPOX...umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.ODV.Phosphate..PHOSZZXX_UPOX.,
  Total.Phosphorus..P...umol.l. = Total.Phosphorus..TPHSZZXX_UPOX...umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.ODV.Total.Phosphorus..TPHSZZXX_UPOX.,
  Nitrate.Nitrogen..NO3.N...umol.l. = Nitrate..NTRAZZXX_UPOX...umol.l.,
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = QV.ODV.Nitrate..NTRAZZXX_UPOX.,
  Nitrite.Nitrogen..NO2.N...umol.l. = Nitrite..NTRIZZXX_UPOX...umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.ODV.Nitrite..NTRIZZXX_UPOX.,
  Ammonium.Nitrogen..NH4.N...umol.l. = Ammonium..AMONZZXX_UPOX...umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.ODV.Ammonium..AMONZZXX_UPOX.,
  Total.Nitrogen..N...umol.l. = Total.Nitrogen..NTOTZZXX_UPOX...umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.ODV.Total.Nitrogen..NTOTZZXX_UPOX.,
  Hydrogen.Sulphide..H2S.S...umol.l. = Hydrogen.Sulphide..H2SXZZXX_UPOX...umol.l.,
  QV.ODV.Hydrogen.Sulphide..H2S.S...umol.l. = QV.ODV.Hydrogen.Sulphide..H2SXZZXX_UPOX.,
  Chlorophyll.a..ug.l. = Chlorophyll.a..CPHLZZXX_UGPL...ug.l.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.ODV.Chlorophyll.a..CPHLZZXX_UGPL.
  )]

# ICES high resolution CTD data --> 153,897,705 -->  station samples
stationSamplesCTD <- fread(input = "Input/ICES_StationSamples_CTD_2024-07-10.csv.gz", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesCTD <- stationSamplesCTD[as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)) >= 1980, .(
  Cruise,
  Station,
  Type = "C",
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID = 2,
  Depth..m. = Depth..ADEPZZ01_ULAA...m.,
  QV.ODV.Depth..m. = QV.ODV.Depth..ADEPZZ01_ULAA.,
  Temperature..degC. = Temperature..TEMPPR01_UPAA...degC.,
  QV.ODV.Temperature..degC. = QV.ODV.Temperature..TEMPPR01_UPAA.,
  Practical.Salinity..dmnless. = Salinity..PSALPR01_UUUU...dmnless.,
  QV.ODV.Practical.Salinity..dmnless. = QV.ODV.Salinity..PSALPR01_UUUU.,
  Dissolved.Oxygen..ml.l. = Oxygen..DOXYZZXX_UMLL...ml.l.,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.ODV.Oxygen..DOXYZZXX_UMLL.,
  Phosphate.Phosphorus..PO4.P...umol.l. = Phosphate..PHOSZZXX_UPOX...umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.ODV.Phosphate..PHOSZZXX_UPOX.,
  Nitrate.Nitrogen..NO3.N...umol.l. = Nitrate..NTRAZZXX_UPOX...umol.l.,
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = QV.ODV.Nitrate..NTRAZZXX_UPOX.,
  Chlorophyll.a..ug.l. = Chlorophyll.a..CPHLZZXX_UGPL...ug.l.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.ODV.Chlorophyll.a..CPHLZZXX_UGPL.
)]

# ICES Pump data --> 2,997,668 --> 2,997,668 station samples
stationSamplesPMP <- fread(input = "Input/ICES_StationSamples_PMP_2024-07-10.csv.gz", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesPMP <- stationSamplesPMP[as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)) >= 1980, .(
  Cruise,
  Station,
  Type = "P",
  Year = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 1, 4)),
  Month = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 6, 7)),
  Day = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 9, 10)),
  Hour = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 12, 13)),
  Minute = as.integer(substr(yyyy.mm.ddThh.mm.ss.sss, 15, 16)),
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID = 3,
  Depth..m. = Depth..ADEPZZ01_ULAA...m.,
  QV.ODV.Depth..m. = QV.ODV.Depth..ADEPZZ01_ULAA.,
  Temperature..degC. = Temperature..TEMPPR01_UPAA...degC.,
  QV.ODV.Temperature..degC. = QV.ODV.Temperature..TEMPPR01_UPAA.,
  Practical.Salinity..dmnless. = Salinity..PSALPR01_UUUU...dmnless.,
  QV.ODV.Practical.Salinity..dmnless. = QV.ODV.Salinity..PSALPR01_UUUU.,
  Dissolved.Oxygen..ml.l. = Oxygen..DOXYZZXX_UMLL...ml.l.,
  QV.ODV.Dissolved.Oxygen..ml.l. = QV.ODV.Oxygen..DOXYZZXX_UMLL.,
  Phosphate.Phosphorus..PO4.P...umol.l. = Phosphate..PHOSZZXX_UPOX...umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l. = QV.ODV.Phosphate..PHOSZZXX_UPOX.,
  Total.Phosphorus..P...umol.l. = Total.Phosphorus..TPHSZZXX_UPOX...umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l. = QV.ODV.Total.Phosphorus..TPHSZZXX_UPOX.,
  Nitrate.Nitrogen..NO3.N...umol.l. = Nitrate..NTRAZZXX_UPOX...umol.l.,
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l. = QV.ODV.Nitrate..NTRAZZXX_UPOX.,
  Nitrite.Nitrogen..NO2.N...umol.l. = Nitrite..NTRIZZXX_UPOX...umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l. = QV.ODV.Nitrite..NTRIZZXX_UPOX.,
  Ammonium.Nitrogen..NH4.N...umol.l. = Ammonium..AMONZZXX_UPOX...umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l. = QV.ODV.Ammonium..AMONZZXX_UPOX.,
  Total.Nitrogen..N...umol.l. = Total.Nitrogen..NTOTZZXX_UPOX...umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l. = QV.ODV.Total.Nitrogen..NTOTZZXX_UPOX.,
  Chlorophyll.a..ug.l. = Chlorophyll.a..CPHLZZXX_UGPL...ug.l.,
  QV.ODV.Chlorophyll.a..ug.l. = QV.ODV.Chlorophyll.a..CPHLZZXX_UGPL.
)]

# Combined data tables --> ? station samples
stationSamples <- rbindlist(list(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP), use.names = TRUE, fill = TRUE)

# Free memory
rm(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP)

# Extract unique locations i.e. longitude/latitude pairs --> ?  locations
locations <- unique(stationSamples[, .(Longitude..degrees_east., Latitude..degrees_north.)])

# Classify locations into sea regions --> 2,897,755 locations
locations <- classify_locations_into_searegions(locations)

# Merge locations incl. sea regions back into station samples - getting rid of station samples not classified --> 46,409,020 station samples
stationSamples <- locations[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.), nomatch = 0]

# Output station samples
#fwrite(stationSamples[Type == 'B'], file.path("Data", "StationSamples_ICES_BOT.csv.gz"))
#fwrite(stationSamples[Type == 'C'], file.path("Data", "StationSamples_ICES_CTD.csv.gz"))
#fwrite(stationSamples[Type == 'P'], file.path("Data", "StationSamples_ICES_PMP.csv.gz"))
fwrite(stationSamples, file.path("Data", "StationSamples_ICES.csv.gz"))

# Free memory
rm(locations, stationSamples, classify_locations_into_searegions)

# To Do - Could possible create a merged dataset reduced to standard depths as done in IOF/WOD
