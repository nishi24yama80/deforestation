## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
pacman::p_load(tidyverse, arrow, broom, estimatr, modelsummary, fixest)

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

####Shift-Share Variable####
t_0 <- 2006
shock_leading_country <- "Russian Federation"

t_0 <-2013
shock_leading_country <- "Myanmar"

roundwood_item_list <- trade_df %>% 
  filter(grepl("roundwood", item)) %>% 
  distinct(item) %>% 
  pull()

shift_share_all_df <- trade_df %>% 
  distinct(reporter_country)
item_val_list <- c("ss_nonconif_nontrop", "ss_conif", "ss_nonconif_trop")
old_names <- c("shift_share", "shift_share", "shift_share")
for (i in 1:3){
  item_name <- roundwood_item_list[i]
  item_val <- item_val_list[i]
  exp_share_df <- trade_df %>% 
    filter(year == t_0 & item == item_name & element == "Export Value") %>% 
    group_by(reporter_country) %>% 
    summarize(sum_exp = sum(value, na.rm = TRUE)) %>% 
    select(reporter_country, sum_exp) %>% 
    left_join(filter(trade_df, year == t_0 & item == item_name & element == "Export Value"), by="reporter_country") %>% 
    mutate(exp_share = value / sum_exp) %>% 
    select(reporter_country, partner_country, year, exp_share) 
  
  exp_shift_df <- trade_df %>% 
    filter(reporter_country == shock_leading_country & item == item_name & element == "Export Value") %>% 
    arrange(partner_country, year) %>% 
    group_by(partner_country) %>% 
    mutate(exp_m_diff = value - lag(value)) %>% 
    ungroup()
  
  shift_share_df <- exp_shift_df %>% 
    filter(year == t_0 + 2) %>% 
    select(partner_country, exp_m_diff) %>% 
    right_join(exp_share_df, by = "partner_country") %>% 
    mutate(ss_pair = exp_share * exp_m_diff) %>% 
    group_by(reporter_country) %>% 
    summarize(shift_share = - sum(ss_pair, na.rm = TRUE)) 
  names(shift_share_df) <- c("reporter_country", item_val)
  
  shift_share_all_df <- shift_share_all_df %>% 
    left_join(shift_share_df, by = "reporter_country")
  print(item_name)
  print(item_val)
}

####Regression####
trade_ss_df <- trade_df %>% 
  filter(item %in% roundwood_item_list) %>%
  filter(year > 2000) %>% 
  group_by(reporter_country, year, item, element) %>% 
  summarise(total = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    element = case_when(
      element == "Import Quantity" ~ "imp_quant",
      element == "Export Quantity" ~ "exp_quant",
      element == "Import Value" ~ "imp_val",
      element == "Export Value" ~ "exp_val"
    )
  ) %>% 
  select(reporter_country, year, item, element, total) %>% 
  mutate(
    item = case_when(
      item == "Industrial roundwood, non-coniferous non-tropical (export/import)" ~ "nonconif_nontrop",
      item == "Industrial roundwood, coniferous (export/import)" ~ "conif",
      item == "Industrial roundwood, non-coniferous tropical (export/import)" ~ "nonconif_trop"
    )
  ) %>% 
  group_by(reporter_country, item, element) %>% 
  mutate(diff_from_t_0 = total - total[year == t_0]) %>% 
  ungroup() %>% 
  mutate(item_element = paste0(item, "_", element)) %>% 
  select(reporter_country, year, item_element, diff_from_t_0) %>% 
  pivot_wider(names_from = item_element, values_from = diff_from_t_0) %>% 
  right_join(shift_share_all_df, by="reporter_country")

forest_ss_df <- forest_df %>% 
  filter(item %in% c("Industrial roundwood, coniferous", "Industrial roundwood, non-coniferous") & element == "Production") %>%
  filter(year > 2000) %>% 
  group_by(area, year, item, element) %>% 
  summarise(total = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(area, year, item, element, total) %>% 
  mutate(
    item = case_when(
      item == "Industrial roundwood, non-coniferous" ~ "nonconif",
      item == "Industrial roundwood, coniferous" ~ "conif",
    )
  ) %>% 
  group_by(area, item) %>% 
  mutate(diff_from_t_0 = total - total[year == t_0]) %>% 
  ungroup() %>% 
  mutate(item_element = paste0(item, "_", element)) %>% 
  select(area, year, item_element, diff_from_t_0) %>% 
  pivot_wider(names_from = item_element, values_from = diff_from_t_0)%>% 
  right_join(shift_share_all_df, by=c("area" = "reporter_country"))

var_name_vec <- colnames(trade_ss_df)[3:14]
title_name_vec <- c("Coniferous", "Coniferous","Coniferous","Coniferous", 
                    "Non-coniferous non-tropical", "Non-coniferous non-tropical", "Non-coniferous non-tropical", "Non-coniferous non-tropical", 
                    "Non-coniferous tropical", "Non-coniferous tropical", "Non-coniferous tropical", "Non-coniferous tropical")
for (i in 1:11) {
  var_name <- var_name_vec[i]
  title_name <- title_name_vec[i]
  formula_str <- paste0(var_name, " ~ ss_conif + ss_nonconif_nontrop + ss_nonconif_trop")
  formula_f <- as.formula(formula_str)
  
  reg_results <- trade_ss_df %>%
    filter(reporter_country != shock_leading_country) %>% 
    filter(year > 2000) %>% 
    group_by(year) %>%
    do(tidy(lm(formula_f, data = .),conf.int=T, level=0.95)) %>%
    ungroup()
  
  p <- reg_results %>% 
    filter(term == "ss_conif") %>% 
    ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
    labs(x = "Year", y = "Coefficient Estimate") +
    ggtitle(title_name) +
    theme_minimal() +
    geom_vline(xintercept = t_0, alpha = 0.5)
  file_name <- paste0("fig/competitor/Russia/", var_name, ".png")
  ggsave(filename = file_name, plot = p, width = 10, height = 8, dpi = 150)
  print(var_name)
}


var_name_vec <- colnames(forest_ss_df)[3:4]
title_name_vec <- c("Coniferous", "Non-coniferous")

for (i in 1:2) {
  var_name <- var_name_vec[i]
  title_name <- title_name_vec[i]
  formula_str <- paste0(var_name, " ~ ss_conif + ss_nonconif_nontrop + ss_nonconif_trop")
  formula_f <- as.formula(formula_str)
  
  reg_results <- forest_ss_df %>%
    filter(area != shock_leading_country) %>% 
    filter(year > 2000) %>% 
    group_by(year) %>%
    do(tidy(lm(formula_f, data = .),conf.int=T, level=0.95)) %>%
    ungroup()
  
  p <- reg_results %>% 
    filter(term == "ss_conif") %>% 
    ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_point(position = position_dodge(width = 0.3), size = 3) +
    geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
    labs(x = "Year", y = "Coefficient Estimate") +
    ggtitle(title_name) +
    theme_minimal() +
    geom_vline(xintercept = t_0, alpha = 0.5)
  file_name <- paste0("fig/competitor/Russia/", var_name, ".png")
  ggsave(filename = file_name, plot = p, width = 10, height = 8, dpi = 150)
  print(var_name)
}

