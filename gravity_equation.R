pacman::p_load(tidyverse, arrow, broom, estimatr, modelsummary, fixest)

trade_df <- read_parquet("data/dev/trade_gdp_dist.parquet")
forest_df <- read_parquet("data/dev/faostat_forestry_long.parquet")
gdp_df <- read.csv("data/raw/gdp.csv")

trade_df <- trade_df %>% 
  filter(reporter_country_code < 5000 & partner_country_code < 5000 & ! reporter_country_code %in% c(252, 254) & ! partner_country_code %in% c(252, 254)) 

# Gravity Equation
item_list <- trade_df %>% 
  filter(year == 2018) %>% 
  distinct(item) %>% 
  pull()

####Gravity Eq using whole sample####
lm_geq_all1 <- feols(
  log(value) ~ log(seadistance) | reporter_country + partner_country + as.factor(year),
  data = trade_df %>% 
    filter(item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Export Value")
)

lm_geq_all2 <- feols(
  log(value) ~ log(seadistance) | reporter_country^as.factor(year) + partner_country^as.factor(year),
  data = trade_df %>% 
    filter(item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Export Value")
)

lm_geq_all3 <- feols(
  log(value) ~ log(distw) | reporter_country + partner_country + as.factor(year),
  data = trade_df %>% 
    filter(item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Export Value")
)

lm_geq_all4 <- feols(
  log(value) ~ log(distw) | reporter_country^as.factor(year) + partner_country^as.factor(year),
  data = trade_df %>% 
    filter(item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Export Value")
)

etable(lm_geq_all1, lm_geq_all2, lm_geq_all3, lm_geq_all4)

####Plot Fixed Effects####
lm_geq_2016 <- feols(
  log(value) ~ log(distw) | reporter_country + partner_country,
  data = trade_df %>% 
    filter(item == "Forest products (export/import)" & element == "Export Value") %>% 
    filter(year == 2016)
)

fixed_effects <- fixef(lm_geq_2016)

exporter_fe_df <- data.frame(fixed_effects$reporter_country) %>% 
  rownames_to_column(var = "country")

importer_fe_df <- data.frame(fixed_effects$partner_country)%>% 
  rownames_to_column(var = "country")

exporter_fe_df2 <- gdp_df %>% 
  filter(Year == 2015) %>% 
  select(Entity, GDP..constant.2015.US..) %>% 
  left_join(exporter_fe_df, by = c("Entity" = "country"))

importer_fe_df2 <- gdp_df %>% 
  filter(Year == 2015) %>% 
  select(Entity, GDP..constant.2015.US..) %>% 
  left_join(importer_fe_df, by = c("Entity" = "country"))

ggplot(data = exporter_fe_df2, aes(x = fixed_effects.reporter_country, y = log(GDP..constant.2015.US..))) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Exporter Country FE", y = "Lagged Exporter Country GDP", title = "Location size and gravity FE")

ggplot(data = importer_fe_df2, aes(x = fixed_effects.partner_country, y = log(GDP..constant.2015.US..))) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(x = "Importer Country FE", y = "Lagged Importer Country GDP", title = "Location size and gravity FE")
  

####Scatter Distance vs Trade Flows####
trade_df %>% 
  filter(year == 2018) %>% 
  distinct(distw) %>% 
  ggplot(data = ., aes(x = distw)) +
  geom_histogram()

trade_df %>% 
  filter(year == 2018) %>% 
  distinct(distw) %>% 
  ggplot(data = ., aes(x = log(distw))) +
  geom_histogram()

trade_df %>% 
  filter(year == 2018) %>% 
  filter(item == item_name & element == "Export Value") %>% 
  ggplot(data = ., aes(x = log(distw), y = log(value))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

for (item_name in item_list) {
  p <- trade_df %>% 
    filter(year == 2018) %>% 
    filter(item == item_name & element == "Export Value") %>% 
    ggplot(data = ., aes(x = log(distw), y = log(value))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess")  +
    labs(x = "Log Distance", y = "Log Export Value") +
    ggtitle(paste0(item_name, ": trade flows")) +
    theme_minimal()
  file_name <- paste0("fig/gravity_eq/scatter_dist_value/", gsub(",|\\(|\\)|[[:blank:]]|/", "_", item_name), ".png")
  ggsave(file = file_name, plot = p)
  print(item_name)
}

####Gravity Coef by Year####

for (item_name in item_list) {
  reg_results <- trade_df %>% 
    filter(item == item_name & element == "Export Value") %>% 
    group_by(year) %>% 
    do(tidy(feols(log(value) ~ log(distw) | reporter_country + partner_country, data = .))) %>% 
    ungroup()
  
  p <- reg_results %>% 
    filter(term == "log(distw)") %>% 
    ggplot(data = ., aes(x = factor(year), y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
    labs(x = "Year", y = "Coefficient Estimate") +
    ggtitle(paste0(item_name, ": Gravity coefficient over time")) +
    ylim(-3.7, 0.1) +
    theme_minimal()
  file_name <- paste0("fig/gravity_eq/", gsub(",|\\(|\\)|[[:blank:]]|/", "_", item_name), ".png")
  ggsave(file = file_name, plot = p)
  print(item_name)
}
