pacman::p_load(tidyverse, arrow, reshape2)

# Faostat Forestry data
forestry_df <- read.csv("data/raw/faostat/Forestry_E_All_Data/Forestry_E_All_Data.csv")

forestry_df <- forestry_df %>% 
  rename(
    area_code = "Area.Code",
    area_code_m49 = "Area.Code..M49.",
    item_code = "Item.Code",
    element_code = "Element.Code"
  )

colnames(forestry_df) <- tolower(colnames(forestry_df))

forestry_long_df <- forestry_df %>% 
  select(area_code, area_code_m49, area, item, item_code, element, element_code, names(forestry_df)[nchar(names(forestry_df))==5]) %>% 
  pivot_longer(cols = names(forestry_df)[nchar(names(forestry_df))==5],
               names_to = "year",
               values_to = "value") %>% 
  mutate(year = as.numeric(str_replace(year, "^y", "")))

write_parquet(forestry_long_df, "data/dev/faostat_forestry_long.parquet")


# Faostat faostat trade flows data
forestry_trade_df <- read.csv("data/raw/faostat/Forestry_Trade_Flows_E_All_Data/Forestry_Trade_Flows_E_All_Data.csv")

forestry_trade_df <- forestry_trade_df %>% 
  rename(
    reporter_country = Reporter.Countries,
    reporter_country_code = Reporter.Country.Code,
    partner_country = Partner.Countries,
    partner_country_code = Partner.Country.Code,
    item_code = Item.Code,
    element_code = Element.Code
  )

colnames(forestry_trade_df) <- tolower(colnames(forestry_trade_df))


forestry_trade_long_df <- forestry_trade_df %>% 
  select(reporter_country, reporter_country_code, partner_country, partner_country_code, item, item_code, element, element_code, 
         names(forestry_trade_df)[nchar(names(forestry_trade_df))==5]) %>% 
  pivot_longer(cols = names(forestry_trade_df)[nchar(names(forestry_trade_df))==5],
               names_to = "year",
               values_to = "value") %>% 
  mutate(year = as.numeric(str_replace(year, "^y", "")))

write_parquet(forestry_trade_long_df, "data/dev/faostat_forestry_trade_long.parquet")




forestry_df <- read_parquet("data/dev/faostat_forestry_long.parquet")
trade_df <- read_parquet("data/dev/faostat_forestry_trade_long.parquet")
item_df <- read.csv("data/raw/faostat/Forestry_E_All_Data/forestry_production_trade_item_group.csv")

trade_items <- trade_df %>% 
  filter(element == "Export Value") %>% 
  group_by(item) %>% 
  arrange(desc(value)) %>% 
  View()

trade_items <- trade_df %>% 
  distinct(item) %>%
  pull()

forestry_df %>% 
  filter(year == 2020 & area == "World" & element == "Export Value") %>% 
  group_by(item) %>% 
  arrange(desc(value)) %>% 
  filter(item %in% trade_items) %>% 
  select(item) %>% 
  pull()

forestry_df2 <- forestry_df %>% 
  filter(item %in% trade_items) 

write_parquet(forestry_df2, "data/dev/faostat_forestry_trade_long2.parquet")
