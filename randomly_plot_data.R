pacman::p_load(tidyverse, arrow, reshape2, ggnetwork, network)

forestry_df <- read_parquet("data/dev/faostat_forestry_long.parquet")
trade_df <- read_parquet("data/dev/faostat_forestry_trade_long.parquet")

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

areas <- unique(forestry_df$area)
item_name <- "Roundwood"

for (area_name in areas) {
  tryCatch({
    p <- forestry_df %>% 
      filter(area == area_name & item == item_name) %>% 
      ggplot(data = ., aes(x=year, y=value)) +
      geom_line() +
      facet_wrap(facets = ~element, scales="free_y") +
      labs(title=area_name, subtitle = item_name)
    ggsave(filename = paste0("fig/", item_name, "/", area_name, ".png"), plot = p)
    print(area_name) 
  }, error = function(e) {
    print(area_name) 
    print("error")
  }
  )
}

# network