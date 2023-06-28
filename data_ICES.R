library(data.table)
library(sf)
library(tidyverse)

# Download ICES data from ICES Data Portal at https://data.ices.dk ------------------

# Read and Merge station samples -----------------------------------------------

# ICES Bottle and low resolution CTD data --> 22,456,075 --> 16,204,690 station samples
stationSamplesBOT <- fread(input = "Data/ICES_StationSamples_BOT_2023-06-09.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesBOT[, Type := "B"]
stationSamplesBOT[, DataSourceID := 1]
stationSamplesBOT <- stationSamplesBOT[Year >= 1980, .(
  Cruise,
  Station,
  Type,
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID,
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

# ICES CTD data --> 127,425,271 station samples
stationSamplesCTD <- fread(input = "Data/ICES_StationSamples_CTD_2023-06-09.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesCTD[, Type := "C"]
stationSamplesCTD[, DataSourceID := 2]
stationSamplesCTD <- stationSamplesCTD[Year >= 1980, .(
  Cruise,
  Station,
  Type,
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID,
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
stationSamplesPMP <- fread(input = "Data/ICES_StationSamples_PMP_2023-06-09.txt.gz", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesPMP[, Type := "P"]
stationSamplesPMP[, DataSourceID := 3]
stationSamplesPMP <- stationSamplesPMP[Year >= 1980, .(
  Cruise,
  Station,
  Type,
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Longitude..degrees_east.,
  Latitude..degrees_north.,
  Bot..Depth..m.,
  DataSourceID,
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

# Combined data tables --> 152,882,097 station samples
stationSamples <- rbindlist(list(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP), use.names = TRUE, fill = TRUE)

# Remove original data tables
rm(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP)

# Assign station and sample IDs ------------------------------------------------

# Unique stations by natural key --> 5,099,899 staions
#uniqueN(stationSamples, by = c("Cruise", "Station", "Type", "Year", "Month", "Day", "Hour", "Minute", "Longitude..degrees_east.", "Latitude..degrees_north.", "Bot..Depth..m.", "DataSourceID"))

# Assign station ID by natural key
#stationSamples[, StationID := .GRP, by = .(Cruise, Station, Type, Year, Month, Day, Hour, Minute, Longitude..degrees_east., Latitude..degrees_north., Bot..Depth..m., DataSourceID)]

# Assign sample ID by natural key
#stationSamples[, SampleID := .GRP, by = .(Cruise, Station, Type, Year, Month, Day, Hour, Minute, Longitude..degrees_east., Latitude..degrees_north., Bot..Depth..m., DataSourceID, Depth..m.)]

# Classify station samples into sea regions ------------------------------------

# Read sea regions
sea_regions <- st_read("Input/EEA_SeaRegion_20180831.shp")

# Identify invalid geometries
st_is_valid(sea_regions)

# Make invalid geometries valid
sea_regions <- st_make_valid(sea_regions)

# Extract unique positions i.e. longitude/latitude pairs --> 3,561,725 positions
positions <- unique(stationSamples[, .(Longitude..degrees_east., Latitude..degrees_north.)])

# Make positions spatial keeping original longitude/latitude
positions <- st_as_sf(positions, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)

# Classify positions into sea regions
positions$SeaRegionID <- st_intersects(positions, sea_regions) %>% as.numeric()

# Delete positions not classified --> 2,987,000 positions
positions <- na.omit(positions)

# Remove spatial column and make into data table
positions <- st_set_geometry(positions, NULL) %>% as.data.table()

# Merge positions i.e. sea regions back into station samples - getting rid of station samples not classified into assessment units --> 48,593,364 station samples
stationSamples <- positions[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.), nomatch = 0]

# Remove sea regions and positions
rm(sea_regions, positions)

# Output station samples mapped to assessment units for contracting parties to check i.e. acceptance level 1
#fwrite(stationSamples[Type == 'B'], file.path("Output", "ICES_StationSamples_BOT.csv"))
#fwrite(stationSamples[Type == 'C'], file.path("Output", "ICES_StationSamples_CTD.csv"))
#fwrite(stationSamples[Type == 'P'], file.path("Output", "ICES_StationSamples_PMP.csv"))
fwrite(stationSamples, file.path("Output", "ICES_StationSamples.csv"))

# To Do
# Could possible create a merged dataset reduced to standard depths as done in IOF/WOD
