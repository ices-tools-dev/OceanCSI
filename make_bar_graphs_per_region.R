
searegionFile <- "Input/EEA_SeaRegion_20180831.shp"
searegionMapping <- sf::st_read(searegionFile, quiet = T) %>%
  st_drop_geometry() %>%
  select(ID, Region, SubRegion)

perc = "05"

timeperiods = c("1980-1999", "2000-2024")

for(timeperiod in timeperiods){

classes <- c("O2_4 mg_l", "4_O2_6 mg_l", "O2_6 mg_l")
prettyClassNames <- c("O2 < 4 mg/l", "4 < O2 < 6 mg/l", "O2 > 6 mg/l")

filenames <- list.files("Output", pattern = paste0(perc, ".+", timeperiod, ".+", "dissolvedoxygen"), full.names = T)

sort(classes)

data = lapply(
  filenames,
  \(filename)read_csv(filename, show_col_types = F)
)
names(data) <- sort(classes)

data_df <- data %>% bind_rows(.id = "class") %>%
  filter(SeaRegionID %in% c(1,2)) %>%
  mutate(class = factor(class, levels = classes)) %>%
  mutate(trend = factor(trend, levels = rev(c("increasing", "no trend", "decreasing")))) %>%
  mutate(searegion = case_match(
    SeaRegionID,
    1 ~ "Baltic Sea",
    2 ~ "North-east Atlantic Ocean"
  ))

ggplot(data_df, aes(x = class)) +
  geom_bar(aes(fill = trend)) +
  scale_fill_manual(
    values = c(
      "decreasing" = "red",
      "increasing" = 'green',
      "no trend" = "darkblue"
    )
  ) +
  facet_wrap("searegion")
ggsave(paste0("output/", "barchart_", perc, "_", timeperiod, ".png"), width = 8, height = 4)

print("")

data_df %>%
  group_by(
    searegion, class, trend) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(trend = as.character(trend)) %>%
  pivot_wider(id_cols = c(searegion, class), names_from = "trend", values_from = "n") %>%
  write_csv(paste0("output/", "barchartdata_", perc, "_", timeperiod, ".csv"))
}
