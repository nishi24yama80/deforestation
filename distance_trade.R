pacman::p_load(tidyverse, arrow, openxlsx, estimatr, modelsummary)
trade_df <- read_parquet("data/dev/faostat_forestry_trade_long.parquet")
country_df <- read.csv("data/raw/faostat/Forestry_Trade_Flows_E_All_Data/trade_country_code.csv")
distance_df <- read.xlsx("data/raw/CERDI-seadistance.xlsx")
gdp_df <- read.csv("data/raw/gdp.csv")

country_df2 <- country_df %>% 
  select(Reporter.Country.Code, ISO3.Code)

gdp_df2 <- gdp_df %>% 
  rename(c(gdp = "GDP..constant.2015.US..")) %>% 
  mutate(gdp = gdp / 1000)

trade_df2 <- 
  trade_df %>% 
  left_join(country_df2, by=c("reporter_country_code"="Reporter.Country.Code")) %>% 
  left_join(country_df2, by=c("partner_country_code"="Reporter.Country.Code")) %>% 
  left_join(distance_df, by=c("ISO3.Code.x"="iso1", "ISO3.Code.y"="iso2"))%>% 
  left_join(gdp_df2, by=c("ISO3.Code.x"="Code", "year"="Year")) %>% 
  left_join(gdp_df2, by=c("ISO3.Code.y"="Code", "year"="Year")) 

write_parquet(trade_df2, "data/dev/trade_gdp_dist.parquet")

trade_df2 %>% 
  filter(year==2015, element=="Export Quantity") %>% 
  ggplot(data=., aes(x=seadistance, y=value)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(facets=~item, scales = "free_y") +
  ylim(0, 100000) 

trade_df2 %>% 
  filter(year==2015, reporter_country == "Malaysia", element=="Export Quantity") %>% 
  ggplot(data=., aes(x=seadistance, y=value)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(facets=~item, scales = "free_y")

trade_df2 %>% 
  filter(year==2015, reporter_country == "Malaysia", item=="Forest products (export/import)", element=="Export Value") %>%
  filter(value < 500000) %>% 
  ggplot(data=., aes(x=seadistance, y=value)) +
  geom_point() +
  geom_smooth(method="lm")

trade_ex_df <- trade_df2 %>% 
  filter(str_detect(element, "Export Value")) %>% 
  rename(c(exporter_gdp = "gdp.x", importer_gdp = "gdp.y"))

trade_ex_df %>% 
  distinct(item) %>% 
  pull()

trade_ex_df %>% 
  filter(year == 2018) %>%
  group_by(item) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  arrange(desc(value)) %>% 
  select(item) %>% 
  pull()

lm_list = list()
for (i in 1:length(items)) {
  item <- items[i]
  lm_dist <- lm_robust(log(value+0.001) ~ log(seadistance+0.001) + log(exporter_gdp+0.001) + log(importer_gdp+0.001), data = trade_ex_df[trade_ex_df["item"]==item, ])
  lm_list[[i]] <- lm_dist
}

msummary(lm_list, stars = TRUE, fmt="%.2e", output="latex", gof_map = "nobs")
dist_results <- msummary(lm_list, stars = TRUE, fmt="%.2e", output="data.frame", gof_map = "nobs")

write.csv(dist_results, "data/dev/reg_expval_dist.csv")
