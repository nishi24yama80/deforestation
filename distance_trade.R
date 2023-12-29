pacman::p_load(tidyverse, arrow, openxlsx, estimatr, modelsummary, fixest)
trade_df <- read_parquet("data/dev/faostat_forestry_trade_long.parquet")
country_df <- read.csv("data/raw/faostat/Forestry_Trade_Flows_E_All_Data/trade_country_code.csv")
seadistance_df <- read.xlsx("data/raw/CERDI-seadistance.xlsx")
distance_df <- read.xlsx("data/raw/dist_cepii.xlsx")
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
  left_join(seadistance_df %>% select(iso1, iso2, seadistance), by=c("ISO3.Code.x"="iso1", "ISO3.Code.y"="iso2"))%>% 
  left_join(distance_df %>% select(iso_d, iso_o, distcap, distw, distwces), by=c("ISO3.Code.x"="iso_o", "ISO3.Code.y"="iso_d")) %>% 
  left_join(gdp_df2, by=c("ISO3.Code.x"="Code", "year"="Year")) %>% 
  left_join(gdp_df2, by=c("ISO3.Code.y"="Code", "year"="Year")) 

trade_df2 <- trade_df2 %>% 
  mutate(distw = as.numeric(distw), distwces = as.numeric(distwces), distcap = as.numeric(distcap))

write_parquet(trade_df2, "data/dev/trade_gdp_dist.parquet")


trade_df2 <- read_parquet("data/dev/trade_gdp_dist.parquet")

trade_df2 %>% 
  filter(year==2015, element=="Export Quantity") %>% 
  ggplot(data=., aes(x=seadistance, y=value)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap(facets=~item, scales = "free_y") +
  labs(x="distance", y="export value") +
  ylim(0, 100000) 
ggsave(file="fig/distance_export.png", width=40, height=40, dpi=300)

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

items <- trade_ex_df %>% 
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
  lm_dist <- feols(log(value+0.00001) ~ log(seadistance+0.00001) | as.factor(year) + as.factor(reporter_country) + as.factor(partner_country), data = trade_ex_df[trade_ex_df["item"]==item, ])
  lm_list[[i]] <- lm_dist
}

msummary(lm_list, stars = TRUE, fmt="%.2e", output="latex", gof_map = "nobs")
dist_results <- msummary(lm_list, stars = TRUE, fmt="%.2e", output="data.frame", gof_map = "nobs")

msummary(lm_list, stars = TRUE, fmt="%.2e", gof_map = "nobs")

write.csv(dist_results, "data/dev/reg_expval_dist.csv")
