pacman::p_load(tidyverse, arrow, broom, estimatr, modelsummary, fixest)

trade_df <- read_parquet("data/dev/trade_gdp_dist.parquet")
forest_df <- read_parquet("data/dev/faostat_forestry_long.parquet")
country_df <- read.csv("data/raw/faostat/Forestry_E_All_Data/forestry_country_code.csv")
gdp_df <- read.csv("data/raw/gdp.csv")

trade_df <- trade_df %>% 
  filter(reporter_country_code < 5000 & partner_country_code < 5000 & ! reporter_country_code %in% c(252, 254) & ! partner_country_code %in% c(252, 254)) %>% 
  left_join(country_df, by = c("area" = "Country"))


forest_df <- forest_df %>% 
  filter(area_code < 5000 & ! area_code %in% c(252, 254)) %>% 
  left_join(country_df %>% select(Country, ISO3.Code), by = c("area" = "Country")) %>% 
  left_join(gdp_df, by=c("ISO3.Code" = "Code", "year"="Year")) %>% 
  mutate(gdp = GDP..constant.2015.US..)

gdp_df2 <- gdp_df %>% 
  mutate(gdp = GDP..constant.2015.US..) %>% 
  left_join(country_df, by=c("Code" = "ISO3.Code")) 

trade_item_list <- trade_df %>% 
  filter(year == 2018) %>% 
  distinct(item) %>% 
  pull()

forest_item_list <- forest_df %>% 
  filter(year == 2018) %>% 
  distinct(item) %>% 
  pull()

####Roundwood as intermediate input goods####
item_list2 <- c()

roundwood_df <- forest_df %>% 
  filter(year > 1989) %>% 
  mutate(
    category = case_when(
      (item == "Industrial roundwood, coniferous" & element == "Production") ~ "conif_prod",
      (item == "Industrial roundwood, non-coniferous" & element == "Production") ~ "nonconif_prod",
      (item == "Industrial roundwood, coniferous (export/import)" & element == "Export Quantity") ~ "conif_exp",
      (item == "Industrial roundwood, non-coniferous non-tropical (export/import)" & element == "Export Quantity") ~ "nonconif_nontrop_exp",
      (item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Export Quantity") ~ "nonconif_trop_exp",
      (item == "Industrial roundwood, coniferous (export/import)" & element == "Import Quantity") ~ "conif_imp",
      (item == "Industrial roundwood, non-coniferous non-tropical (export/import)" & element == "Import Quantity") ~ "nonconif_nontrop_imp",
      (item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Import Quantity") ~ "nonconif_trop_imp"
    ) 
  ) %>% 
  mutate(value = replace_na(value, 0)) %>% 
  select(area, year, category, value2) %>% 
  filter(!is.na(category)) %>% 
  pivot_wider(names_from = category, values_from = value2) %>% 
  mutate(
    conif_domestic = conif_prod - conif_exp,
    nonconif_domestic = conif_prod - nonconif_trop_exp - nonconif_nontrop_exp
  )

final_prod_df <- forest_df %>% 
  filter(year > 1989) %>% 
  mutate(category = case_when(
    (item == "Sawnwood" & element == "Production") ~ "sawn",
    (item == "Sawnwood, non-coniferous all" & element == "Production") ~ "sawn_nonconif",
    (item == "Sawnwood, coniferous" & element == "Production") ~ "sawn_conif",
    (item == "Plywood" & element == "Production") ~ "plywood",
    (item == "Wood pulp" & element == "Production") ~ "pulp",
    (item == "Wood chips and particles" & element == "Production") ~ "chip"
  )) %>% 
  mutate(value2 = if_else(is.na(value), 0, value)) %>% 
  select(area, year, category, value2) %>% 
  filter(!is.na(category)) %>% 
  pivot_wider(names_from = category, values_from = value2)

io_df <- roundwood_df %>% 
  left_join(final_prod_df, by = c("area", "year")) %>% 
  left_join(gdp_df2 %>% select(Country, Year, gdp), by = c("area" = "Country", "year" = "Year")) 

io_df2 <- io_df %>% 
  group_by(area) %>% 
  arrange(year) %>% 
  mutate(
    lag1_gdp = lag(gdp),
    lag10_gdp = lag(gdp, 10)
    ) %>% 
  filter(year > 2000)

reg1 <- feols(sawn ~ conif_domestic + conif_imp + nonconif_domestic + nonconif_trop_imp + nonconif_nontrop_imp + lag1_gdp | area + as.factor(year), data = io_df2)
reg2 <- feols(plywood ~ conif_domestic + conif_imp + nonconif_domestic + nonconif_trop_imp + nonconif_nontrop_imp + lag1_gdp | area + as.factor(year), data = io_df2)
reg3 <- feols(pulp ~ conif_domestic + conif_imp + nonconif_domestic + nonconif_trop_imp + nonconif_nontrop_imp + lag1_gdp | area + as.factor(year), data = io_df2)
reg4 <- feols(chip ~ conif_domestic + conif_imp + nonconif_domestic + nonconif_trop_imp + nonconif_nontrop_imp + lag1_gdp | area + as.factor(year), data = io_df2)

etable(reg1, reg2, reg3, reg4)

ggplot(data = io_df2, aes(x = lag1_gdp, y = nonconif_trop_exp)) +
  geom_point()

forest_df %>% 
  filter((item == "Industrial roundwood, coniferous" & element == "Production") | (item == "Industrial roundwood, coniferous (export/import)" & element == "Export Quantity")) %>% 
  group_by(item, year) %>% 
  summarise(sum_value = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = sum_value, color = item)) +
  geom_line()

forest_df %>% 
  filter(
    (item == "Other industrial roundwood, coniferous (production)"  & element == "Production") | 
      (item == "Industrial roundwood, coniferous (export/import)" & element == "Export Quantity")) %>% 
  group_by(year) %>% 
  summarize(value_sum = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = value_sum)) +
  geom_line()
