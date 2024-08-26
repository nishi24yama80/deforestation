## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
pacman::p_load(tidyverse, arrow, broom, estimatr, modelsummary, fixest, synthdid, AER)

## ---------------------------------------------------------------------------------------------------------------------------------
trade_df <- read_parquet("data/dev/trade_gdp_dist.parquet")
forest_df <- read_parquet("data/dev/faostat_forestry_long.parquet")

#' 
#' # Background
#' - Prohibition of export of logs
#' - The WTO reports that Myanmar's 2014 log export ban covered logs, as well as boule-cut logs, baulk-squared timber, and confiscated timber (https://www.wto.org/english/tratop_e/tpr_e/s405_e.pdf).
#' - The WTO previously reported the name of the relevant legislation (Notification No. 26/2013) in 2014 (https://www.wto.org/english/tratop_e/tpr_e/s293_e.pdf)

## ---------------------------------------------------------------------------------------------------------------------------------
trade_df <- trade_df %>% 
  filter(reporter_country_code < 5000 & partner_country_code < 5000 & ! reporter_country_code %in% c(252, 254) & ! partner_country_code %in% c(252, 254)) 

forest_df <- forest_df %>% 
  filter(area_code < 5000 & ! area_code %in% c(252, 254))


item_list <- trade_df %>% 
  filter(year == 2018) %>% 
  distinct(item) %>% 
  pull()

roundwood_item_list <- trade_df %>% 
  filter(grepl("roundwood", item)) %>% 
  distinct(item) %>% 
  pull()

####Synthetic DID####
forest_df

####Shift-Share Variable####
#t_0 <- 2014
#shock_leading_country <- "Myanmar"

t_0 <-2007
shock_leading_country <- "Russian Federation"

roundwood_item_list <- trade_df %>% 
  filter(grepl("roundwood", item)) %>% 
  distinct(item) %>% 
  pull()

shift_share_all_df <- trade_df %>% 
  distinct(reporter_country)
item_val_list <- c("ss_nonconif_nontrop", "ss_conif", "ss_nonconif_trop")
old_names <- c("shift_share", "shift_share", "shift_share")

item_name <- roundwood_item_list[1]

exp_shift_df <- trade_df %>% 
  filter(reporter_country == shock_leading_country & item == item_name & element == "Export Quantity") %>% 
  arrange(partner_country, year) %>% 
  group_by(partner_country) %>% 
  mutate(exp_m_diff = value - lag(value)) %>% 
  ungroup()

exp_share_df <- trade_df %>% 
  filter(reporter_country == shock_leading_country & item == item_name & element == "Export Quantity") %>% 
  filter(year == t_0) %>%
  mutate(share = value / sum(value, na.rm = TRUE)) %>% 
  select(partner_country, share)


forest_lag_df <- forest_df %>% 
  filter(item %in% c("Industrial roundwood") ) %>%
  mutate(
    element = case_when(
      element == "Production" ~ "production",
      element == "Import Quantity" ~ "imp_quant",
      element == "Export Quantity" ~ "exp_quant",
      element == "Import Value" ~ "imp_val",
      element == "Export Value" ~ "exp_val"
    )
  ) %>% 
  select(area, year, item, element, value) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element, values_from = value) %>% 
  group_by(area) %>% 
  mutate(
    prod_diff = production - lag(production),
    imp_quant_diff = imp_quant - lag(imp_quant),
    exp_quant_diff = exp_quant - lag(exp_quant),
    imp_val_diff = imp_val - lag(imp_val),
    exp_val_diff = exp_val - lag(exp_val))

lag_df <- exp_shift_df %>% 
  filter(year == 2008) %>% 
  right_join(forest_lag_df %>% filter(year == 2008), by=c("partner_country" = "area")) %>% 
  mutate(exp_m_diff2 = if_else(is.na(exp_m_diff), 0, exp_m_diff)) %>% 
  mutate(prod_diff2 = replace_na(prod_diff, 0)) %>% 
  mutate(imp_quant_diff2 = replace_na(imp_quant_diff, 0)) %>% 
  mutate(exp_quant_diff2 = replace_na(exp_quant_diff, 0)) %>% 
  mutate(imp_val_diff2 = replace_na(imp_val_diff, 0)) %>% 
  mutate(exp_val_diff2 = replace_na(exp_val_diff, 0)) %>% 
  left_join(exp_share_df, by="partner_country") %>% 
  mutate(share2 = if_else(is.na(share), 0, share))

lm_1 <- lm(prod_diff2 ~ exp_m_diff2, data = lag_df)
lm_2 <- lm(imp_quant_diff2 ~ exp_m_diff2, data = lag_df)
lm_3 <- lm(imp_val_diff2 ~ exp_m_diff2, data = lag_df)
lm_4 <- lm(exp_quant_diff2 ~ exp_m_diff2, data = lag_df)
lm_5 <- lm(exp_val_diff2 ~ exp_m_diff2, data = lag_df)

lag_df %>% select(prod_diff2, exp_m_diff2, share) %>% View()

modelsummary(list("prod" = lm_1, "imp quant" = lm_2,"imp val" = lm_3, "exp quant" = lm_4, "exp val" = lm_5), stars = TRUE)

lm_1 <- ivreg(prod_diff2 ~ exp_m_diff2 | share, data = lag_df)

summary(lm_1)

lm_1 <- lm(prod_diff2 ~ share2, data = lag_df)
lm_2 <- lm(imp_quant_diff2 ~ share2, data = lag_df)
lm_3 <- lm(imp_val_diff2 ~ share2, data = lag_df)
lm_4 <- lm(exp_quant_diff2 ~ share2, data = lag_df)
lm_5 <- lm(exp_val_diff2 ~ share2, data = lag_df)

lm_1 <- lm(prod_diff ~ exp_m_diff2, data = lag_df)
lm_2 <- lm(imp_quant_diff ~ exp_m_diff2, data = lag_df)
lm_3 <- lm(imp_val_diff ~ exp_m_diff2, data = lag_df)
lm_4 <- lm(exp_quant_diff ~ exp_m_diff2, data = lag_df)
lm_5 <- lm(exp_val_diff ~ exp_m_diff2, data = lag_df)



lm_1 <- lm(prod_diff2 ~ exp_m_diff2 + distcap, data = lag_df)
lm_2 <- lm(imp_quant_diff2 ~ exp_m_diff2 + gdp.y + distcap, data = lag_df)
lm_3 <- lm(imp_val_diff2 ~ exp_m_diff2 + gdp.y + distcap, data = lag_df)
lm_4 <- lm(exp_quant_diff2 ~ exp_m_diff2 + gdp.y + distcap, data = lag_df)
lm_5 <- lm(exp_val_diff2 ~ exp_m_diff2 + gdp.y + distcap, data = lag_df)

modelsummary(list("prod" = lm_1, "imp quant" = lm_2,"imp val" = lm_3, "exp quant" = lm_4, "exp val" = lm_5), stars = TRUE)


lm_1 <- lm(prod_diff2 ~ share2 + gdp.y + distcap, data = lag_df)
lm_2 <- lm(imp_quant_diff2 ~ share2 + gdp.y + distcap, data = lag_df)
lm_3 <- lm(imp_val_diff2 ~ share2 + gdp.y + distcap, data = lag_df)
lm_4 <- lm(exp_quant_diff2 ~ share2 + gdp.y + distcap, data = lag_df)
lm_5 <- lm(exp_val_diff2 ~ share2 + gdp.y + distcap, data = lag_df)

modelsummary(list("prod" = lm_1, "imp quant" = lm_2,"imp val" = lm_3, "exp quant" = lm_4, "exp val" = lm_5), stars = TRUE)



lag_df %>% 
  select(partner_country, exp_m_diff, exp_m_diff2, prod_diff2) %>% 
  View()
  


ggplot(data = lag_df, aes(x = exp_m_diff, y = prod_diff)) +
  geom_point() +
  xlim(-5000, 5000)

lm_1 <- lm_robust(prod_diff ~ exp_m_diff * distcap + gdp.y, data = lag_df)
lm_2 <- lm_robust(imp_quant_diff ~ exp_m_diff * distcap + gdp.y, data = lag_df)
lm_3 <- lm_robust(exp_quant_diff ~ exp_m_diff * distcap + gdp.y, data = lag_df)

modelsummary(list(lm_1, lm_2, lm_3), stars = TRUE)

trade_df %>% 
  filter(reporter_country == "Gabon" & grepl("roundwood", item) & element == "Export Quantity") %>% 
  group_by(item, year) %>%
  summarize(total_exp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = total_exp, color = item)) +
  geom_line() 

forest_df %>% 
  filter(grepl("roundwood", item) & element == "Export Quantity") %>% 
  group_by(item, year) %>% 
  summarize(total_exp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = total_exp, color = item)) +
  geom_line() 

trade_df %>% 
  filter(reporter_country == "Russian Federation" & grepl("roundwood", item) & element == "Export Quantity") %>% 
  group_by(item, year) %>%
  summarize(total_exp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = total_exp, color = item)) +
  geom_line() 

exp_shift_df %>% 
  filter(year == 2008) %>% 
  mutate(exp_m_diff2 = replace_na(exp_m_diff, 0)) %>% 
  ggplot(data = ., aes(x = exp_m_diff2)) +
  geom_histogram() 

exp_shift_df %>% 
  filter(year == 2008) %>% 
  ggplot(data = ., aes(x = exp_m_diff)) +
  geom_histogram()

exp_shift_df %>% 
  filter(year == 2008) %>% 
  View()
exp_shift_df %>% 
  filter(year == 2008) %>% 
  filter(exp_m_diff < 0) %>% 
  summarize(n())

trade_df %>% 
  filter(reporter_country == shock_leading_country & item == item_name & element == "Export Value") %>% 
  filter(year == 2006) %>% 
  mutate(value = replace_na(value, 0)) %>% 
  ggplot(data = ., aes(x = value)) +
  geom_histogram(bins = 100)

trade_df %>% 
  filter(reporter_country == shock_leading_country & item == item_name & element == "Export Value") %>% 
  filter(year == 2006) %>% 
  filter(value > 0) %>% 
  select(partner_country, value) %>% 
  arrange(desc(value)) %>% 
  View()

