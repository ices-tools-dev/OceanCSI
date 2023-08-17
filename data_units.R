library(sf)
library(tidyverse)
library(ggplot2)

# MSFD Marine regions and subregions
# https://dd.eionet.europa.eu/vocabulary/msfd/regions/view
# https://github.com/ices-taf/eea-topic-centre-wise6-dataflow/issues/2
unitsFile <- "Input/MSFD_Publication_20181129_incl_WISE6_units.gdb"

st_layers(unitsFile)

# MSFD Marine regions and subregions
# North East Atlantic Ocean (ATL) modified (cut against other units instead of coast with overlaps)
# Unwanted 'region' removed
units <- st_read(unitsFile, layer = "EuropeSeas_20181129_WISE6_units")
ggplot() + geom_sf(data = units) + coord_sf()

# Rename columns
units %>%
  rename(Code = Id, Region = name)

# Identify invalid geometries
st_is_valid(units)

# Make geometries valid by doing the buffer of nothing trick
units <- sf::st_buffer(units, 0.0)

# Transform projection into ETRS_1989_LAEA
#units <- st_transform(units, crs = 3035)

# Write to database
# st_write(
#   units,
#   dsn = "MSSQL:server=SQL08;database=OceanCSI_19802019;trusted_connection=yes;",
#   layer = "SeaRegion1",
#   layer_options = c("LAUNDER=NO", "GEOM_NAME=GEOM")
# )