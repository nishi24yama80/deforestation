pacman::p_load(tidyverse, arrow, reshape2, ggnetwork, network)

forestry_df <- read.csv("data/raw/faostat/Forestry_E_All_Data/Forestry_E_All_Data.csv")

forestry_df <- forestry_df %>% 
  rename(
    area_code = "Area.Code",
    area_code_m49 = "Area.Code..M49.",
    item_code = "Item.Code",
    element_code = "Element.Code"
  )

forestry_long_df <- forestry_df %>% 
  select(area_code, area_code_m49, Area, Item, item_code, Element, element_code, names(forestry_df)[nchar(names(forestry_df))==5]) %>% 
  pivot_longer(cols = names(forestry_df)[nchar(names(forestry_df))==5],
               names_to = "year",
               values_to = "value") %>% 
  mutate(year = as.numeric(str_replace(year, "^Y", "")))

write_parquet(forestry_long_df, "data/dev/faostat_forestry_long.parquet")
