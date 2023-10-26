pacman::p_load(tidyverse, arrow, openxlsx, estimatr, modelsummary)
trade_df <- read_parquet("data/dev/trade_gdp_dist.parquet")
forestry_df <- read_parquet("data/dev/faostat_forestry_trade_long2.parquet")
country_df <- read.csv("data/raw/faostat/Forestry_Trade_Flows_E_All_Data/trade_country_code.csv")
geo_df <- read.csv("data/raw/geo_cepii.csv")

View(geo_df)
forestry_df2 <- 
  forestry_df %>% 
  left_join(country_df, by=c("area_code"="Reporter.Country.Code"))


forestry_df2 %>% 
  filter(item == "Forest products (export/import)" & element == "Export Value" & year == 2018 & !is.na(ISO3.Code)) %>% 
  arrange(desc(value)) %>% 
  select(area, value) %>% 
  View()

trade_df %>% 
  filter(reporter_country=="United States of America" & year == 2018 & item == "Forest products (export/import)" & element == "Export Value") %>% 
  arrange(desc(value)) %>% 
  select(partner_country, value, seadistance) %>% 
  View()
