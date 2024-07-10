library(data.table)

stationSamples_ICES <- fread(input = "Data/StationSamples_ICES.csv.gz")
stationSamples_EMODNET <- fread(input = "Data/StationSamples_EMODNET.csv.gz")
stationSamples_EEA <- fread(input = "Data/StationSamples_EEA.csv.gz")

# Combined data tables --> 88,516,449 station samples
stationSamples <- rbindlist(list(stationSamples_ICES, stationSamples_EMODNET, stationSamples_EEA), use.names = TRUE, fill = TRUE)

# Free memory
rm(stationSamples_ICES, stationSamples_EMODNET, stationSamples_EEA)

# Extract unique locations i.e. longitude/latitude pairs --> 3,637,548 locations
locations <- unique(stationSamples[, .(Longitude..degrees_east., Latitude..degrees_north.)])

# Classify locations into clusters
source("utilities_cluster.R")
locations <- classify_locations_into_clusters(locations)

# Merge locations incl. clusters back into station samples
stationSamples <- locations[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.), nomatch = 0]

# Extract unique locations used by the dissolved oxygen indicator
locations <- unique(stationSamples[(Month >= 7 & Month <= 10) & ((!is.na(Dissolved.Oxygen..ml.l.) & QV.ODV.Dissolved.Oxygen..ml.l. != 3 & QV.ODV.Dissolved.Oxygen..ml.l. != 4) | (!is.na(Hydrogen.Sulphide..H2S.S...umol.l.) & QV.ODV.Hydrogen.Sulphide..H2S.S...umol.l. != 3 & QV.ODV.Hydrogen.Sulphide..H2S.S...umol.l. != 4)), .(Longitude..degrees_east., Latitude..degrees_north.)])

# Classify locations used by the dissolved oxygen indicator into bathymetric
source("utilities_bathymetric.R")
locations <- classify_locations_into_bathymetric(locations)

# Merge locations incl. bathymetric back into station samples
stationSamples <- locations[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.)]

# Output station samples
#save(stationSamples, file = file.path("Data", "StationSamples.RData"))
fwrite(stationSamples, file.path("Data", "StationSamples.csv.gz"))

# Free memory
rm(locations, stationSamples, classify_locations_into_clusters, classify_locations_into_bathymetric)
