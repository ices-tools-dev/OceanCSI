library(data.table)

source("utilities_searegion.R")

# Download ICES data from ICES Data Portal at https://data.ices.dk -------------

# Read and Merge station samples -----------------------------------------------

# ICES Bottle and low resolution CTD data --> 16,204,690 station samples
stationSamplesBOT <- fread(input = "Input/ICES_StationSamples_BOT_2023-06-09.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesBOT <- stationSamplesBOT[Year >= 1980, .(
  Cruise,
  Station,
  Type = "B",
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID = 1,
  Depth..m.,
  QV.ODV.Depth..m.,
  Temperature..degC.,
  QV.ODV.Temperature..degC.,
  Practical.Salinity..dmnless.,
  QV.ODV.Practical.Salinity..dmnless.,
  Dissolved.Oxygen..ml.l.,
  QV.ODV.Dissolved.Oxygen..ml.l.,
  Phosphate.Phosphorus..PO4.P...umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l.,
  Total.Phosphorus..P...umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l.,
  Nitrate.Nitrogen..NO3.N...umol.l.,
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l.,
  Nitrite.Nitrogen..NO2.N...umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l.,
  Ammonium.Nitrogen..NH4.N...umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l.,
  Total.Nitrogen..N...umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l.,
  Hydrogen.Sulphide..H2S.S...umol.l.,
  QV.ODV.Hydrogen.Sulphide..H2S.S...umol.l.,
  Chlorophyll.a..ug.l.,
  QV.ODV.Chlorophyll.a..ug.l.
  )]

# ICES high resolution CTD data --> 123,421,195 station samples
stationSamplesCTD <- fread(input = "Input/ICES_StationSamples_CTD_2023-06-09.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesCTD <- stationSamplesCTD[Year >= 1980, .(
  Cruise,
  Station,
  Type = "C",
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID = 2,
  Depth..m.,
  QV.ODV.Depth..m.,
  Temperature..degC.,
  QV.ODV.Temperature..degC.,
  Practical.Salinity..dmnless.,
  QV.ODV.Practical.Salinity..dmnless.,
  Dissolved.Oxygen..ml.l.,
  QV.ODV.Dissolved.Oxygen..ml.l.
)]

# ICES Pump data --> 3,000,751 station samples
stationSamplesPMP <- fread(input = "Input/ICES_StationSamples_PMP_2023-06-09.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesPMP <- stationSamplesPMP[Year >= 1980, .(
  Cruise,
  Station,
  Type = "P",
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID = 3,
  Depth..m.,
  QV.ODV.Depth..m.,
  Temperature..degC.,
  QV.ODV.Temperature..degC.,
  Practical.Salinity..dmnless.,
  QV.ODV.Practical.Salinity..dmnless.,
  Dissolved.Oxygen..ml.l.,
  QV.ODV.Dissolved.Oxygen..ml.l.,
  Phosphate.Phosphorus..PO4.P...umol.l.,
  QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l.,
  Total.Phosphorus..P...umol.l.,
  QV.ODV.Total.Phosphorus..P...umol.l.,
  Nitrate.Nitrogen..NO3.N...umol.l.,
  QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l.,
  Nitrite.Nitrogen..NO2.N...umol.l.,
  QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l.,
  Ammonium.Nitrogen..NH4.N...umol.l.,
  QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l.,
  Total.Nitrogen..N...umol.l.,
  QV.ODV.Total.Nitrogen..N...umol.l.,
  Chlorophyll.a..ug.l.,
  QV.ODV.Chlorophyll.a..ug.l.
)]

# Combined data tables --> 142,626,636 station samples
stationSamples <- rbindlist(list(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP), use.names = TRUE, fill = TRUE)

# Free memory
rm(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP)

# Extract unique locations i.e. longitude/latitude pairs --> 3,303,893  locations
locations <- unique(stationSamples[, .(Longitude..degrees_east., Latitude..degrees_north.)])

# Classify locations into sea regions --> 2,890,431 locations
locations <- classify_locations_into_searegions(locations)

# Merge locations incl. sea regions back into station samples - getting rid of station samples not classified --> 44,893,836 station samples
stationSamples <- locations[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.), nomatch = 0]

# Output station samples
#fwrite(stationSamples[Type == 'B'], file.path("Data", "StationSamples_ICES_BOT.csv.gz"))
#fwrite(stationSamples[Type == 'C'], file.path("Data", "StationSamples_ICES_CTD.csv.gz"))
#fwrite(stationSamples[Type == 'P'], file.path("Data", "StationSamples_ICES_PMP.csv.gz"))
fwrite(stationSamples, file.path("Data", "StationSamples_ICES.csv.gz"))

# Free memory
rm(locations, stationSamples, classify_locations_into_searegions)

# To Do - Could possible create a merged dataset reduced to standard depths as done in IOF/WOD
