# Requirements ---------------
# Libraries
require(data.table)
require(tidyverse)
require(ggmap)
require(mapproj)
require(ggrepel)
require(ggspatial)
require(viridis)
require(RColorBrewer)
require(sf)
require(scatterpie)
require(rworldmap)
require(rnaturalearth)
require(rnaturalearthdata)
require(ggnewscale)

# Scripts and data (raw)
# rm(list = ls())

if(!dir.exists("output_2024")){dir.create("output_2024")} 

ETC_ClassNames <- c("DO<2", "DO 2-4", "DO 4-6", "DO>6")
colorClassificion = read_csv("input/colorClassificationTable.csv")

# Shapefiles
searegionFile <- "Input/EEA_SeaRegion_20180831.shp"
searegions <- sf::st_read(searegionFile)
grid100File <- "Input/europe_100km.shp"
grid100 <- sf::st_read(grid100File)

# create bounding box for plotting on European scale
bboxEurope <- st_bbox(searegions)
bboxGrid <- st_bbox(grid100)
xxlim = c(bboxEurope[1], bboxEurope[3])
yylim = c(bboxEurope[2], bboxEurope[4])

# Map data
data("countriesLow")
world <- fortify(countriesLow) 
rm(countriesLow)
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf') %>% sf::st_make_valid()
#ID	Code	Description						envDomain	zoneType	spZoneType	Shape_Length	Shape_Area
#1	BAL	Baltic							            water	marineRegion	MSFDregion		860.717834064261	61.1764774372317
#2	BLA	Black Sea - sea of Azov					water	marineRegion	MSFDregion_part		46.8036300233879	4.64210461222669
#3	BLM	Black Sea - sea of Marmara				water	marineRegion	MSFDregion_part		16.6122037987906	1.2499968958581
#4	AMA	Macaronesia						                                              water	marineRegion	MSFDsubregion		129.858146721819	403.800809039279
#5	MAD	Adriatic Sea						water	marineRegion	MSFDsubregion		100.416051967312	15.3706242561274
#6	MAL	Aegean-Levantine Sea					water	marineRegion	MSFDsubregion		222.518364756116	74.7396360823146
#7	ABI	Bay of Biscay and the Iberian Coast			water	marineRegion	MSFDsubregion		105.591710393974	88.0366900404256
#8	ANS	Greater North Sea, incl. Kattegat + English Channel	water	marineRegion	MSFDsubregion		464.95134457305		95.2014264637056
#9	MIC	Ionian Sea and the Central Mediterranean Sea		water	marineRegion	MSFDsubregion		98.7583461236747	76.5746705808825
#10	MWE	Western Mediterranean Sea				water	marineRegion	MSFDsubregion		147.767877527397	88.633689073479
#11	BAR	Barents Sea						water	marineRegion	nonMSFDsea		741.733713175722	656.236971936288
#12	ICE	Iceland Sea						water	marineRegion	nonMSFDsea		152.984613720431	142.344800339171
#13	NOR	Norwegian Sea						water	marineRegion	nonMSFDsea		577.843816458465	220.728020578548
#14	WHI	White Sea						water	marineRegion	nonMSFDsea		52.7603238847586	17.8195239452825
#15	ACS	Celtic Seas						water	marineRegion	MSFDsubregion		358.016500849447	136.003803526643
#16	ACSo	Celtic Seas - overlapping submissions to UNCLOS 	water	marineRegion	MSFDsubregion_part	26.2033727812915	22.9006291957611
#17	ATL	North East Atlantic Ocean				water	marineRegion	MSFDregion_part		1094.91157549377	1541.80056043198
#18	BLK	Black Sea						water	marineRegion	MSFDregion_part		77.0331675675044	46.8110900817822

#Load latest  percentile calculations
Q05_2023 <- read.csv(file = paste0("Output/perc05_2024_Oxygen_status.csv"))
range(Q05_2023$Year)

Q05_2023 %>% ggplot(aes(x = q)) + geom_histogram()

# Convert to gridcells
Q05_2023_grd <- Q05_2023 %>%
  filter(Year >= 2012 & Year < 2024) %>%
  sf::st_as_sf(coords = c('AvgLongitude', 'AvgLatitude'), crs = 4326) %>%
  st_transform(crs = 3035) %>%                                                # This is the new coordinate code used
  mutate(Y = st_coordinates(.)[,2],                                         # We then extract them and assign them
         X = st_coordinates(.)[,1]) %>%
  st_join(grid100, join = st_intersects) %>%
  drop_na(CellCode) %>%
  rename("pointGeometry" = geometry) %>%
  as.data.frame()

# # Calculate mean of lower quartile   # DEPRECATED AS PER 2024. NOW, THE PERCENTILE IS USED DIRECTLY
# cell_mean25perc_BS_grd <- DO_samples_summer_all_BS_grd %>% 
#   left_join(Q25all_BS, by = c("Year", "SeaRegionID", "ClusterID")) %>% 
#   filter(Oxygen <= q25) %>%
#   filter(Year > 2011) %>%
#   dplyr::select(Year, avgDepth, SeaRegionID, ClusterID, Depth, Oxygen, X, Y, CellCode) %>%
#   group_by(CellCode) %>%
#   summarise(oxymean = mean(Oxygen),
#             oxySD = sd(Oxygen),
#             oxymin = min(Oxygen),
#             oxymax = max(Oxygen),
#             oxyn = n(),
#             oxySE = oxySD/sqrt(oxyn)) %>%
#   as.data.table() 

cell_mean05perc_BS_grd <- Q05_2023_grd %>%
  select(
    Year, 
    # avgDepth = "AvgDepth", 
    SeaRegionID, 
    ClusterID, 
    # Depth = "AvgBathymetric", 
    Oxygen = "q", 
    X, 
    Y, 
    CellCode
  ) %>%
  group_by(CellCode) %>%
  summarise(oxymean = mean(Oxygen, na.rm = T),
            oxySD = sd(Oxygen, na.rm = T),
              oxymin = min(Oxygen, na.rm = T),
              oxymax = max(Oxygen, na.rm = T),
              oxyn = n(),
              oxySE = oxySD/sqrt(oxyn)) %>%
    as.data.table()

# Join with grid files and make it a sf 
oxy_grid_bs <- left_join(grid100, cell_mean05perc_BS_grd, by = "CellCode")

oxy_grid_bs_class <- oxy_grid_bs %>% 
  drop_na(oxymean) %>%
  mutate(
    class = case_when(
      oxymean < 2 ~ "oxy < 2",
      oxymean >= 2 & oxymean < 4 ~ "oxy 2-4",
      oxymean >= 4 & oxymean < 6 ~ "oxy 4-6",
      oxymean >= 6 ~ "oxy > 6"
    ),
    Classification = case_when(
      oxymean < 2            ~ ETC_ClassNames[1],
      oxymean >= 2 & oxymean < 4   ~ ETC_ClassNames[2],
      oxymean >= 4 & oxymean < 6   ~ ETC_ClassNames[3],
      oxymean >= 6           ~ ETC_ClassNames[4]
    ),
    `n observations` = case_when(
     oxyn <= 5 ~ "<=5",
     oxyn > 5  ~ ">5"
    )
  ) %>%
  left_join(colorClassificion)


oxy_grid_bs_class$class2 <- factor(oxy_grid_bs_class$Classification, levels = ETC_ClassNames)

# Coordinates for piecharts
searegions_center <- searegions %>%
  st_set_crs(4326) %>%
  st_transform(crs = 3035) %>%
  st_centroid(geometry) %>%
  rename(center = geometry) %>%
  filter(SubRegion != "Sea of Azov") %>%
  filter(SubRegion != "Sea of Marmara") %>%
  filter(SubRegion != "Macaronesia") 

searegions_center1 <- searegions_center %>%
  mutate(Lon = sf::st_coordinates(searegions_center$center)[,1]) %>%
  mutate(Lat = sf::st_coordinates(searegions_center$center)[,2]) %>%
  as.data.frame()

searegions_center2 <- searegions_center1 %>%
  group_by(Region) %>%
  mutate(meanLon = mean(Lon),
            meanLat = mean(Lat)) %>%
  distinct(Region, .keep_all =T) %>%
  ungroup() %>%
  select(Region, meanLon, meanLat)

eu_center <- searegions_center1 %>%
  summarise(meanLon = mean(Lon),
         meanLat = mean(Lat)) %>%
  mutate(Region = "Europe") %>%
  select(Region, meanLon, meanLat)

searegions3 <- searegions %>%
  st_set_crs(4326) %>%
  st_transform(crs = 3035) 

oxy_searegion <- st_join(oxy_grid_bs, searegions3, join = st_intersects, largest = T)

ggplot(oxy_searegion, aes(fill = oxymean)) +
  geom_sf()

searegions_center_eu2 <- rbind(searegions_center2, eu_center)

searegions_center_eu <- searegions_center_eu2 %>%
  mutate(meanLon = case_when(
    Region == "Baltic Sea" ~ meanLon + 400000,
    Region == "North-east Atlantic Ocean" ~ meanLon + 200000,
    Region == "Mediterranean Sea" ~ meanLon - 850000,
    Region == "Black Sea" ~ meanLon - 250000,
    TRUE ~ meanLon
  )) %>%
  mutate(meanLat = case_when(
    Region == "Baltic Sea" ~ meanLat + 500000,
    Region == "North-east Atlantic Ocean" ~ meanLat + 1500000,
    Region == "Mediterranean Sea" ~ meanLat - 500000,
    Region == "Black Sea" ~ meanLat + 600000,
    TRUE ~ meanLat
  ))

# Calculate the piechart per region
oxy_searegion_pie <- oxy_searegion %>%
  st_drop_geometry() %>%
  drop_na(oxymean) %>%
  #mutate(oxymean=replace_na(oxymean, 0)) %>%
  mutate(
    Classification = case_when(
      oxymean < 2                  ~ ETC_ClassNames[1],
      oxymean >= 2 & oxymean < 4   ~ ETC_ClassNames[2],
      oxymean >= 4 & oxymean < 6   ~ ETC_ClassNames[3],
      oxymean >= 6                 ~ ETC_ClassNames[4]
    )#,
    # class = case_when(
    #   oxymean > 0 & oxymean < 2 ~ "oxy < 2",
    #   oxymean >= 2 & oxymean < 4 ~ "oxy 2-4",
    #   oxymean >= 4 & oxymean < 6 ~ "oxy 4-6",
    #   oxymean >= 6 ~ "oxy > 6"
    # )
    ) %>%
  group_by(Region, Classification) %>%
  summarise(class_n = n(),
            oxymean2 = mean(oxymean)) %>%
  ungroup()  %>%
  group_by(Region) %>%
  mutate(class_perc = (class_n/sum(class_n))*100) %>%
  as_data_frame() %>%
  left_join(searegions_center_eu, by = "Region") %>%
  drop_na(Region) %>%
  select(Region, meanLon, meanLat, Classification, class_perc) %>%
  pivot_wider(names_from = Classification, values_from = class_perc) %>%
  select(
    Region, 
    meanLon, 
    meanLat, 
    `oxy < 2` = `DO<2`, 
    `oxy 2-4` = `DO 2-4`, 
    `oxy 4-6` = `DO 4-6`, 
    `oxy > 6` = `DO>6`
    ) %>%
  mutate_all(~replace(., is.na(.), 0))

# Calculate the piechart values for all of Europe
oxy_sea_eu_n <- oxy_searegion %>%
  st_drop_geometry() %>%
  drop_na(oxymean) %>%
  mutate(
    class = case_when(
      oxymean > 0 & oxymean < 2 ~ "oxy < 2",
      oxymean >= 2 & oxymean < 4 ~ "oxy 2-4",
      oxymean >= 4 & oxymean < 6 ~ "oxy 4-6",
      oxymean >= 6 ~ "oxy > 6"
    )) %>%
  group_by(class) %>%
  summarise(class_n = n(),
            oxymean2 = mean(oxymean)) %>%
  mutate(class_perc = (class_n/sum(class_n))*100) %>%
  as_data_frame() %>%
  mutate(Region = "Europe") %>%
  select(Region, class, class_n, oxymean2, class_perc) %>%
  left_join(searegions_center_eu, by = "Region") %>%
  drop_na(Region) %>%
  select(Region, meanLon, meanLat, class, class_perc) %>%
  pivot_wider(names_from = class, values_from = class_perc) %>%
  select(Region, meanLon, meanLat, `oxy < 2`, `oxy 2-4`, `oxy 4-6`, `oxy > 6`) %>%
  mutate_all(~replace(., is.na(.), 0))

#oxy_bar_all <- rbind(oxy_searegion_bar2, oxy_sea_eu2)

#oxy_bar_all1 <- oxy_bar_all %>%
#  pivot_longer(cols = c(4:7), names_to = "class", values_to = "class_perc")

# Figure 0 ---------------------
# Create map with piecharts without NA's
colorscale3a <-
  colorRampPalette(rev(c("#39568CFF", "#29AF7FFF", 
                         "#FDE725FF", "#7F0000")))

colorscaleSea <- colorRampPalette(rev(c("#BDD7E7", "#EFF3FF", "#6BAED6", "#2171B5")))

colorscaleSeaALL <-  colorRampPalette(rev(c("#39568CFF", "#29AF7FFF", 
                                            "#FDE725FF", "#7F0000",
                                            "#BDD7E7", "#EFF3FF", "#6BAED6", "#2171B5")))
searegions2 <- searegions %>% 
  st_set_crs(4326) %>%
  filter(SubRegion != "Macaronesia") 

# Low sampled grids are filtered
oxy_grid_bs_class2a <- oxy_grid_bs_class %>%
  filter(oxyn >= 6)

oxy_grid_bs_class2b <- oxy_grid_bs_class %>%
  filter(oxyn < 6)

#Figure 0
fig0 <- ggplot() +
  #geom_sf(data = oxy_grid_bs, colour = NA, fill = NA) +
  geom_sf(data = searegions3, aes(fill = Region), colour = NA, size = 1, show.legend = F) +
  scale_fill_manual(values = colorscaleSea(n=4), labels = NULL) +
  new_scale_fill() +
  geom_sf(data = oxy_grid_bs_class2a, aes(fill = class), alpha = 1, colour = "grey") +
  geom_sf(data = oxy_grid_bs_class2b, aes(fill = class), alpha = 0.6, colour = "grey") +
  scale_fill_manual(values = colorscale3a(n=4), labels = c(expression("< 2 mg/L"), "> 2 and < 4 mg/L", "> 4 and < 6 mg/L", "> 6 mg/L")) +
  scale_alpha(range = c(0.00, 1.00), guide = 'none') +
  #geom_sf(data = searegions2, fill = NA, colour = "red") +
  geom_sf(data = worldmap) +
  geom_scatterpie(data=oxy_searegion_pie, aes(x=meanLon, y=meanLat, group=Region, r=250000),
                  cols=c("oxy < 2", "oxy 2-4", "oxy 4-6", "oxy > 6")) +
  scale_fill_manual(values = colorscale3a(n=4), 
                    labels = c(expression("< 2 mg/L"), "> 2 and < 4 mg/L", "> 4 and < 6 mg/L", "> 6 mg/L")) +
  geom_scatterpie(data=oxy_sea_eu_n %>% mutate(newLat = meanLat+350000), aes(x=meanLon, y=newLat, r=450000),
                  cols=c("oxy < 2", "oxy 2-4", "oxy 4-6", "oxy > 6")) +
  scale_fill_manual(values = colorscale3a(n=4), 
                    labels = c(expression("< 2 mg/L"), "> 2 and < 4 mg/L", "> 4 and < 6 mg/L", "> 6 mg/L")) +
  coord_sf(xlim = c(2200000, 7000000), ylim = c(820000, 5500000), expand = F) +
  ggtitle("Oxygen classes", subtitle = "Per EEA Searegion") +
  guides(fill = guide_legend(title = expression(paste("Oxygen class")))) + 
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(0.92, 0.84), legend.background = element_rect(fill = "white", color = "black"))

fig0




 ggsave(
   fig0, 
   filename = file.path("output_2024", paste0("Fig0_Oxy_classes_map_bs.png")),
   height = 8, 
   width = 8
 )

write_csv(
  oxy_grid_bs_class %>%
    select(CellCode,
           Classification,
           `n observations`,
           `Colour code`
    ), 
  file = paste0("output_2024/oxy_grid_classes_2023.csv"
                )
  )

write_csv(
  oxy_searegion_pie %>%
    select(
      Region,
      `oxy > 6`,
      `oxy 4-6`,
      `oxy 2-4`,
      `oxy < 2`
    ), 
  file = paste0("output_2024/oxy_searegion_piechart_2023.csv"
                )
  )

write_csv(
  oxy_sea_eu_n %>%
    select(
      Region,
      `oxy > 6`,
      `oxy 4-6`,
      `oxy 2-4`,
      `oxy < 2`
    ), 
  file = paste0("output_2024/oxy_piechart_all_2023.csv"
                )
  )


