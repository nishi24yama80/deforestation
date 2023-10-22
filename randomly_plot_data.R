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

trade_net <- 