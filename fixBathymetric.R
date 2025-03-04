
source("utilities_bathymetric.R")

assessmentYear <- 2024
setDTthreads(4)

stationSamples <- fread(file.path("Data", "1980-2023_StationSamplesOxygen.csv.gz"))

missingBathymetries = stationSamples %>%
  mutate(missingBathymetry = is.na(Bathymetric)) %>%
  group_by(Latitude, Longitude) %>%
  summarize(
    count = n(),
    missing = sum(missingBathymetry),
    .groups = "drop"
    ) %>%
  mutate(
    percentmissing = 100*(count - missing)/count
    ) %>%  # only 0 or 100 % occurs
  filter(
    percentmissing < 100
    ) # which in practice is all 

# Get bathymetries for missing values of Bathymetric
additionalBathymetries <- missingBathymetries %>%
  select(Latitude, Longitude) %>%
  mutate(
    newBathymetric = -unlist(
      map2(
        Longitude, 
        Latitude, 
        get.bathymetric
      )
    )
  )
         
# integrate in original dataset

stationSamples_with_new_bathymetric <- stationSamples %>%
  left_join(additionalBathymetries) %>%
  mutate(
    Bathymetric = case_when(
    !is.na(Bathymetric) ~ Bathymetric,
    is.na(Bathymetric) ~ newBathymetric
  ))

save(stationSamples_with_new_bathymetric, file = "Data/stationSamples_correctedBathymetry")

stationSamples_with_new_bathymetric %>%
  mutate(missingBathymetry = is.na(Bathymetric)) %>%
  group_by(Latitude, Longitude) %>%
  summarize(
    count = n(),
    missing = sum(missingBathymetry),
    .groups = "drop"
  ) %>%
  mutate(
    percentmissing = 100*(count - missing)/count
  ) %>%  # only 0 or 100 % occurs
  filter(
    percentmissing < 100
  ) # which in practice is all 
