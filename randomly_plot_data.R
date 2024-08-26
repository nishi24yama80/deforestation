pacman::p_load(tidyverse, arrow, reshape2, ggnetwork, network)

forestry_df <- read_parquet("data/dev/faostat_forestry_trade_long2.parquet")
trade_df <- read_parquet("data/dev/faostat_forestry_trade_long.parquet")

# all
forestry_df %>% 
  filter(area == "World") %>% 
  ggplot(data=., aes(x=year, y=value, color=item)) +
  geom_line() +
  facet_wrap(facets=~element, ncol = 2, scales = "free")
ggsave(file="../fig/world_time_item_all.png", width=20, height=20, dpi=300)

forestry_df %>% 
  filter(area == "World" & str_detect(item, "roundwood")) %>% 
  ggplot(data=., aes(x=year, y=value, color=item)) +
  geom_line() +
  facet_wrap(facets=~element, ncol = 2, scales = "free")
ggsave(file="../fig/world_time_item_roundwood.png", width=20, height=20, dpi=300)

forestry_df %>% 
  select(area, year, item, element, value) %>% 
  filter(area == "World" & str_detect(item, "roundwood")) %>% 
  pivot_wider(names_from = element, values_from = value) %>% 
  rename(
    export_value = "Export Value",
    import_value = "Import Value",
    export_q = "Export Quantity",
    import_q = "Import Quantity"
  ) %>% 
  ggplot(data=., aes(x=export_value, y=import_value)) + 
  geom_point() +
  geom_abline(intercept = 0, slope=1, color="blue", alpha = 0.4) +
  geom_smooth(method = "lm") +
  expand_limits(x = 0, y = 0) 
  

forestry_df %>% 
  filter(area == "World" & year == 2020 & element == "Production") %>% 
  arrange(desc(value)) %>% 
  View()

forestry_df %>% 
  filter(area == "World" & year == 2020 & element == "Import Value") %>% 
  arrange(desc(value)) %>% 
  View()

trade_df %>% 
  distinct(item) %>% 
  View()

trade_df %>%
  filter(element == "Export Quantity") %>% 
  group_by(item) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  arrange(desc(value)) %>% 
  View()

forestry_df %>% 
  filter(area %in% c("Indonesia", "Philippines", "Malaysia") & item == "Roundwood" & element == "Export Quantity") %>% 
  ggplot(data=., aes(x=year, y=value, color=area)) +
  geom_line()

forestry_df %>% 
  filter(area %in% c("Indonesia", "Malaysia", "Philippines", "China") & item == "Roundwood") %>% 
  ggplot(data=., aes(x=year, y=value, color=area)) +
  geom_line() +
  facet_wrap(facets = ~element, scales="free_y")

forestry_df %>% 
  filter(area %in% c("Africa", "Asia", "Europe", "Northern America", "Southern America", "Oceania") & item == "Roundwood") %>% 
  ggplot(data=., aes(x=year, y=value, color=area)) +
  geom_line() +
  facet_wrap(facets=~element, scales="free_y")

forestry_df %>% 
  filter(area %in% c("Africa", "Asia", "Europe", "Northern America", "Southern America", "Oceania") & item == "Roundwood" & element == "Export Quantity") %>% 
  ggplot(data=., aes(x=year, y=value, color=area)) +
  geom_line()

forestry_df %>% 
  filter(area == "Indonesia" & item == "Roundwood") %>% 
  ggplot(data=., aes(x=year, y=value)) +
  geom_line() +
  facet_wrap(facets = ~element, scales="free_y")

forestry_df %>% 
  filter(area == "Indonesia" & item == "Roundwood") %>% 
  ggplot(data=., aes(x=year, y=value)) +
  geom_line() +
  facet_wrap(facets = ~element, scales="free_y")

forestry_df %>% 
  filter(area == "World" & item == "Roundwood") %>% 
  ggplot(data = ., aes(x=year, y=value)) +
  geom_line() +
  facet_wrap(facets = ~element, scales="free_y")

forestry_df %>% 
  filter(area == "World" & year == 2010) %>% 
  ggplot(data=., aes(x=value)) +
  geom_histogram()

forestry_df %>% 
  filter(year == 2020& item == "Roundwood" & element == "Export Quantity") %>% 
  arrange(desc(value)) %>% 
  View()

forestry_df %>% 
  filter(year == 2021 & item == "Forest products (export/import)" & element == "Export Value") %>% 
  arrange(desc(value)) %>% 
  View()

forestry_df %>% 
  filter(year == 2021 & area == "World" & element == "Export Value") %>% 
  arrange(desc(value)) %>% 
  View()


item_name <- "Roundwood"

item_list <- forestry_df %>% 
  distinct(item) %>% 
  pull()

forestry_df %>% 
  distinct(item)

paste("fig/", item_list[1])

areas <- unique(forestry_df$area)

for (item_name in item_list) {
  dir_name <- str_remove(item_name, " \\(export/import\\)")
  dir.create(paste("fig/", dir_name))
  for (area_name in areas) {
    tryCatch({
      p <- forestry_df %>% 
        filter(area == area_name & item == item_name) %>% 
        ggplot(data = ., aes(x=year, y=value)) +
        geom_line() +
        facet_wrap(facets = ~element, scales="free_y") +
        labs(title=area_name, subtitle = item_name)
      ggsave(filename = paste0("fig/", dir_name, "/", area_name, ".png"), plot = p)
      print(area_name) 
    }, error = function(e) {
      print(area_name) 
      print("error")
    }
    )  
  }
  print(dir_name)
}

regions <- c("World", "Europe","Southern Europe", "Americas", "Northern America", "South America", "Western Europe", "Eastern Europe", "Northern Europe", "Asia", "South-eastern Asia", "Eastern Asia")

regions2 <- c("Southern Europe", "Russia", "Northern America", "South America", "Western Europe", "Eastern Europe", "Northern Europe", "Asia", "South-eastern Asia", "Eastern Asia", "Africa")

regions3 <- c("Southern Europe", "Northern America", "South America", "Western Europe", "Eastern Europe", "Northern Europe", "Asia", "South-eastern Asia", "Eastern Asia", "Africa")


for (item_name in item_list) {
  dir_name <- str_remove(item_name, " \\(export/import\\)")
  dir_name <- str_remove(dir_name, ", ")
  p <- forestry_df %>% 
    filter(area_code %% 100 == 0) %>% 
    mutate(
      continent = case_when(
        area_code >= 5100 & area_code < 5200 ~ "Africa",
        area_code >= 5200 & area_code < 5300 ~ "America",
        area_code >= 5300 & area_code < 5400 ~ "Asia",
        area_code >= 5400 & area_code < 5500 ~ "Europe",
        area_code >= 5500 & area_code < 5600 ~ "Oceania"
      )
    )%>% 
    filter(area_code > 5000 & area_code <= 5501 & item == item_name & element == "Export Value") %>% 
    ggplot(data=., aes(x=year, y=value, color=area, shape = continent)) +
    geom_line() +
    geom_point() +
    scale_shape() +
    labs(title=item_name, subtitle = ~element)
  ggsave(filename = paste0("fig/continent/", dir_name, ".png"), plot = p)
}

for (item_name in item_list) {
  dir_name <- str_remove(item_name, " \\(export/import\\)")
  dir_name <- str_remove(dir_name, ", ")
  p <- forestry_df %>% 
    filter(area_code %% 100 != 0) %>% 
    mutate(
      continent = case_when(
        area_code >= 5100 & area_code < 5200 ~ "Africa",
        area_code >= 5200 & area_code < 5300 ~ "America",
        area_code >= 5300 & area_code < 5400 ~ "Asia",
        area_code >= 5400 & area_code < 5500 ~ "Europe",
        area_code >= 5500 & area_code < 5600 ~ "Oceania"
      )
    )%>% 
    filter(area_code > 5000 & area_code <= 5501 & item == item_name & element == "Export Value") %>% 
    ggplot(data=., aes(x=year, y=value, color=area, shape = continent)) +
    geom_line() +
    geom_point() +
    scale_shape() +
    labs(title=item_name, subtitle = ~element)
  ggsave(filename = paste0("fig/continent2/", dir_name, ".png"), plot = p)
}

trade_df %>% 
  filter(reporter_country == "Japan" & partner_country == "Malaysia") %>% 
  select(reporter_country, partner_country, year, item, element, value) %>% 
  View()

forestry_df %>% 
  filter(year == 2020 & area_code > 5000 & element == "Export Value") %>% 
  arrange(desc(area_code)) %>%
  distinct(area, area_code) %>% 
  View()
# network