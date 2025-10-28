
source("utilities_bathymetric.R")
library(data.table)
library(tidyverse)
library(leaflet)

assessmentYear <- 2025
setDTthreads(4)

# stationSamples <- fread(file.path("Data", "1980-2023_StationSamplesOxygen.csv.gz"))

stationSamples <- fread(file.path("Data2025", "Data", "StationSamplesOxygen.csv.gz"))

# onderstaand klopt niet. 

stationSamples %>%
  filter(is.na(Bathymetric)) %>% 
  nrow() / nrow(stationSamples) * 100

stationSamples %>%
  filter(is.na(Sounding)) %>% 
  nrow() / nrow(stationSamples) * 100

stationSamples %>%
  filter(is.na(Bathymetric) & is.na(Sounding)) %>% 
  nrow() / nrow(stationSamples) * 100

stationSamples  %>%
  sample_n(100000) %>%
  mutate(new_bath = case_when(
    !is.na(Sounding) ~ Sounding,
    is.na(Sounding) ~ Bathymetric
  )) %>%
  ggplot(aes(new_bath, new_bath - Depth)) +
  geom_point(aes(color = Year))

# 23515755 missing bathymetries (almost half)



missingBathymetries = stationSamples %>%
  mutate(new_bath = case_when(
    !is.na(Sounding) ~ Sounding,
    is.na(Sounding) ~ Bathymetric
  )) %>%
  mutate(missingBathymetry = is.na(new_bath)) %>%
  group_by(Latitude, Longitude) %>%
  summarize(
    mean_bath = mean(new_bath, na.rm = T),
    sd_bath = sd(new_bath, na.rm = T),
    count = n(),
    missing = sum(missingBathymetry),
    .groups = "drop"
  ) %>%
  mutate(
    percentmissing = 100*(missing/count)
  )

hist(missingBathymetries$percentmissing)

fixing_needed = FALSE

if(fixing_needed){
  
  # needs fixing
  
  needs_fixing = missingBathymetries %>%
    filter(!(!is.na(mean_bath) & sd_bath == 0) | missing != 0)
  
  # Get bathymetries for missing values of Bathymetric
  ## DEZE STAP KAN ERG LANG DUREN. 
  # break up in smaller bits for better control
  
  n = 10000
  # bathList = list()
  for(ii in 37:ceiling(nrow(needs_fixing)/n)){
    
    # ii = 2
    
    start = (ii-1) * n + 1
    end   = (ii) * n
    
    toFix <- needs_fixing %>%
      slice(start:end)
    
    print(ii)
    
    additionalBathymetries <- toFix %>%
      select(Latitude, Longitude) %>%
      mutate(
        newBathymetric = -unlist(
          map2( 
            Longitude, 
            Latitude, 
            get.bathymetric,
            .progress = TRUE
          )
        )
      ) 
    
    bathList[[ii]] <- additionalBathymetries
    save(bathList, file = "Data2025/bathList.Rdata")
    
  }
  
  bathdf <- bathList %>% 
    bind_rows()
  
  
  save(
    bathdf, 
    file = 
      file.path(
        paste0("Data", assessmentYear), 
        paste0(
          "additionalBathymetries_", assessmentYear, "Rdata"
        )
      )
  )
}

load(
  file = 
    file.path(
      paste0("Data", assessmentYear), 
      paste0(
        "additionalBathymetries_", assessmentYear, "Rdata"
      )
    )
)


bathdf %>%
  group_by(Latitude, Longitude) %>%
  summarize(bath = mean(newBathymetric)) %>%
  ungroup() %>% 
  count(is.na(bath))

# integrate in original dataset

# check if all missing bathymetries are now completed
completedBathymetries <- missingBathymetries %>%
  left_join(bathdf) %>%
  mutate(
    newBathymetric2 = case_when(
      is.na(newBathymetric) ~ mean_bath,
      !is.na(newBathymetric) ~ newBathymetric
    )) %>%
  select(
    Latitude,
    Longitude,
    newBathymetric2
  )

completedBathymetries %>%
  count(is.na(newBathymetric2))


stationSamples_with_new_bathymetric <- stationSamples %>%
  left_join(completedBathymetries)

# Check

stationSamples_with_new_bathymetric %>%
  group_by(Latitude, Longitude) %>%
  summarize(bath = mean(newBathymetric2)) %>%
  ungroup() %>%
  count(is.na(bath))

# ALL GOOD! 


stationSamples_with_new_bathymetric %>%
  sample_n(100000) %>%
  ggplot(aes(Sounding, newBathymetric2)) +
  geom_point()
# not a perfect agreement between Sounding and EMODnet Bathymetry

stationSamples_with_new_bathymetric %>%
  sample_n(100000) %>%
  ggplot(aes(Bathymetric, newBathymetric2)) +
  geom_point()
# good agreement between EMODnet bathymetry and Bathymetric



save(stationSamples_with_new_bathymetric, file = "Data/stationSamples_correctedBathymetry.Rdata")

