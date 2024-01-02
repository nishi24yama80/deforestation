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
t_0 <- 2007
shock_leading_country <- "Russian Federation"

# t_0 <-2007
# shock_leading_country <- "Russian Federation"

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
    filter(year == 2015) %>% 
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
  group_by(item) %>% 
  pivot_wider(names_from = c(item, element), values_from = total) %>% 
  group_by(reporter_country) %>% 
  mutate(
    nonconif_nontrop_imp_quant_diff = nonconif_nontrop_imp_quant - lag(nonconif_nontrop_imp_quant),
    nonconif_nontrop_exp_quant_diff = nonconif_nontrop_exp_quant - lag(nonconif_nontrop_exp_quant),
    nonconif_nontrop_imp_val_diff = nonconif_nontrop_imp_val - lag(nonconif_nontrop_imp_val),
    nonconif_nontrop_exp_val_diff = nonconif_nontrop_exp_val - lag(nonconif_nontrop_exp_val),
    conif_imp_quant_diff = conif_imp_quant - lag(conif_imp_quant),
    conif_exp_quant_diff = conif_exp_quant - lag(conif_exp_quant),
    conif_imp_val_diff = conif_imp_val - lag(conif_imp_val),
    conif_exp_val_diff = conif_exp_val - lag(conif_exp_val),
    nonconif_trop_imp_quant_diff = nonconif_trop_imp_quant - lag(nonconif_trop_imp_quant),
    nonconif_trop_exp_quant_diff = nonconif_trop_exp_quant - lag(nonconif_trop_exp_quant),
    nonconif_trop_imp_val_diff = nonconif_trop_imp_val - lag(nonconif_trop_imp_val),
    nonconif_trop_exp_val_diff = nonconif_trop_exp_val - lag(nonconif_trop_exp_val)) %>% 
  right_join(shift_share_all_df, by="reporter_country")

ggplot(data = trade_ss_df %>% filter(year == 2007), aes(x = ss_nonconif_trop)) +
  geom_histogram()

reg_results <- trade_ss_df %>%
  filter(reporter_country != shock_leading_country) %>% 
  filter(year > 2005) %>% 
  group_by(year) %>%
  do(tidy(lm(conif_exp_val ~ ss_conif + ss_nonconif_nontrop + ss_nonconif_trop, data = .),conf.int=T, level=0.95)) %>%
  ungroup()

reg_results %>% 
  filter(term == "ss_conif") %>% 
  ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Year", y = "Coefficient Estimate") +
  ggtitle("Regression Coefficients of Export Value on Exposure to Myanmar's Supply Shock") +
  theme_minimal()

#' 
#' # Distribution of Shift-Share (Exposure) Value
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df4 %>% 
  filter(year == 2015) %>% 
  ggplot(data = ., aes(x=ss2)) +
  geom_histogram() +
  labs(x = "Exposure Value", y = "Count", title = "Distribution of Exposure Value")

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results <- trade_ss_df4 %>%
  filter(year > 2014) %>% 
  group_by(year) %>%
  do(tidy(lm(exp_val ~ ss2, data = .),conf.int=T, level=0.95)) %>%
  ungroup()

#' 
#' # Effect on Export Value
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results %>% 
  filter(term == "ss2") %>% 
  ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Year", y = "Coefficient Estimate") +
  ggtitle("Regression Coefficients of Export Value on Exposure to Myanmar's Supply Shock") +
  theme_minimal()

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results <- trade_ss_df4 %>%
  filter(year > 2014) %>% 
  group_by(year) %>%
  do(tidy(lm(exp_quant ~ ss2, data = .),conf.int=T, level=0.95)) %>%
  ungroup()

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
forest_df2 <- forest_df %>% 
  filter(area_code < 5000 & ! area_code %in% c(252, 254))

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df4 <- forest_df2 %>% 
  filter(item %in% c("Industrial roundwood, non-coniferous tropical (export/import)") ) %>%
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
    imp_quant_diff = imp_quant - lag(imp_quant),
    exp_quant_diff = exp_quant - lag(exp_quant),
    imp_val_diff = imp_val - lag(imp_val),
    exp_val_diff = exp_val - lag(exp_val)) %>% 
  right_join(trade_ss_df3, by=c("area" = "reporter_country"))

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results <- trade_ss_df4 %>%
  filter(year > 2014) %>% 
  group_by(year) %>%
  do(tidy(lm(exp_val ~ ss2, data = .),conf.int=T, level=0.95)) %>%
  ungroup()

#' 
#' # Effect on Export Value
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results %>% 
  filter(term == "ss2") %>% 
  ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Year", y = "Coefficient Estimate") +
  ggtitle("Regression Coefficients of Export Value on Exposure to Myanmar's Supply Shock") +
  theme_minimal()



## ---------------------------------------------------------------------------------------------------------------------------------
forest_df %>% 
  filter(area == "Myanmar"  & item %in% c("Roundwood", "Sawnwood", "Wood-based panels") & element %in% c("Export Value", "Export Quantity")) %>% 
  mutate(element2 = case_when(
    element == "Export Value" ~ "exp_val",
    element == "Export Quantity" ~ "exp_quant"
  )) %>% 
  select(year, item, element2, value) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element2, values_from = value) %>% 
  mutate(exp_price = exp_val / exp_quant) %>% 
  pivot_longer(cols=c(-year, -item), names_to = "element", values_to = "value") %>% 
  mutate(
    element = case_when(
      element == "exp_price" ~ "Export Price (Value / Quantity)",
      element == "exp_val" ~ "Export Value",
      element == "exp_quant" ~ "Export Quantity"
    )
  )%>% 
  ggplot(data = ., aes(x=year, y=value, color=item)) +
  geom_line() +
  labs(title="Change in Export of Myanmar Forestry Goods", x="Year", y="Value")  +
  geom_vline(xintercept = 2014, alpha=0.3) +
  facet_wrap(facets=~element, scales="free_y", ncol = 1)

forest_df %>% 
  filter(area == "Russian Federation"  & item == "Roundwood" & element %in% c("Export Value", "Export Quantity")) %>% 
  mutate(element2 = case_when(
    element == "Export Value" ~ "exp_val",
    element == "Export Quantity" ~ "exp_quant"
  )) %>% 
  select(year, item, element2, value) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element2, values_from = value) %>% 
  mutate(exp_price = exp_val / exp_quant) %>% 
  pivot_longer(cols=c(-year, -item), names_to = "element", values_to = "value") %>% 
  mutate(
    element = case_when(
      element == "exp_price" ~ "Export Price (Value / Quantity)",
      element == "exp_val" ~ "Export Value",
      element == "exp_quant" ~ "Export Quantity"
    )
  )%>% 
  ggplot(data = ., aes(x=year, y=value)) +
  geom_line() +
  labs(title="Change in Export of Myanmar Forestry Goods", x="Year", y="Value")  +
  geom_vline(xintercept = 2007, alpha=0.3) +
  facet_wrap(facets = ~element, ncol = 1, scales = "free_y")

#' 
#' # Myanmar 2014
## ---------------------------------------------------------------------------------------------------------------------------------
forest_df %>% 
  filter(area == "Myanmar"  & item %in% c("Roundwood", "Sawnwood", "Wood-based panels") & element %in% c("Import Value", "Import Quantity")) %>% 
  mutate(element2 = case_when(
    element == "Import Value" ~ "imp_val",
    element == "Import Quantity" ~ "imp_quant"
  )) %>% 
  select(year, item, element2, value) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element2, values_from = value) %>% 
  mutate(imp_price = imp_val / imp_quant) %>% 
  pivot_longer(cols=c(-year, -item), names_to = "element", values_to = "value") %>% 
  mutate(
    element = case_when(
      element == "imp_price" ~ "Import Price (Value / Quantity)",
      element == "imp_val" ~ "Import Value",
      element == "imp_quant" ~ "Import Quantity"
    )
  )%>% 
  ggplot(data = ., aes(x=year, y=value, color=item)) +
  geom_line() +
  labs(title="Change in Import of Myanmar Forestry Goods", x="Year", y="Value")  +
  geom_vline(xintercept = 2014, alpha=0.3) +
  facet_wrap(facets=~element, scales="free_y", ncol = 1)

country_list <- forest_df %>% 
  filter(item == "Roundwood" & element == "Export Value" & year == c(2014, 2016)) %>% 
  arrange(desc(value)) %>% 
  slice(1:15) %>% 
  select(area) %>% 
  pull()

forest_df %>% 
  filter(item == "Roundwood" & element == "Export Value" & area %in% country_list) %>% 
  ggplot(data = ., aes(x = year, y = value, color = area)) +
  geom_line()
#' 
#' # Forestry Data of Myanmar
## ---------------------------------------------------------------------------------------------------------------------------------
forest_df %>% 
  filter(area == "Myanmar" & item %in% c("Roundwood", "Sawnwood", "Wood-based panels")) %>% 
  ggplot(data = ., aes(x=year, y=value, color=item)) +
  geom_line() +
  labs(title="Export Value of Forestry Goods", x="Year", y="Value") +
  facet_wrap(facets=~element, scales="free_y")+
  geom_vline(xintercept = 2014, alpha=0.3) 

t

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
item_list <- trade_df %>% 
  filter(reporter_country == "Myanmar" & element == "Export Value" & year == 2013) %>% 
  group_by(item) %>% 
  summarize(val_mean = mean(value, na.rm = TRUE)) %>% 
  arrange(desc(val_mean)) %>% 
  select(item) %>% 
  slice(1:10) %>% 
  pull()

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
item_list <- c("Industrial roundwood, non-coniferous tropical (export/import)", "Industrial roundwood, non-coniferous non-tropical (export/import)", "Industrial roundwood, coniferous (export/import)", "Sawnwood, non-coniferous all", "Sawnwood, coniferous")

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
country_list <- trade_df %>% 
  filter(reporter_country == "Myanmar" & item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Export Value" & year == 2013 & partner_country_code < 4000 & ! partner_country_code %in% c(252, 254)) %>% 
  arrange(desc(value)) %>% 
  select(partner_country) %>% 
  slice(1:10) %>% 
  pull()

#' 
#' # Forestry Data of China
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_df %>% 
  filter(reporter_country == "Myanmar" & item %in% item_list & element == "Export Value" & partner_country %in% country_list) %>% 
  ggplot(data=., aes(x=year, y=value, color=partner_country)) +
  geom_line() +
  facet_wrap(facets = ~item, scales="free_y", ncol = 2)+
  geom_vline(xintercept = 2014, alpha=0.3) +
  labs(title="Change in Export of Myanmar Forestry Goods by Importer Country", x="Year", y="Export Value") 

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
country_list <- trade_df %>% 
  filter(reporter_country == "China" & item == "Industrial roundwood, non-coniferous non-tropical (export/import)" & element == "Import Value" & year %in% c(2014, 2017)) %>% 
  arrange(desc(value)) %>% 
  select(partner_country) %>% 
  slice(1:15) %>% 
  pull()

#' 
#' # Forestry Data of China 2
## ---------------------------------------------------------------------------------------------------------------------------------
trade_df %>% 
  filter(reporter_country == "China" & item == "Industrial roundwood, non-coniferous non-tropical (export/import)" & element == "Import Value" & partner_country %in% country_list) %>% 
  ggplot(data=., aes(x=year, y=value, color=partner_country)) +
  geom_line() +
  geom_vline(xintercept = 2014, alpha=0.3) +
  labs(title="Change in Chinese Import of Forestry Goods by Exporter Country", x="Year", y="Import Value") 

trade_df %>% 
  filter(reporter_country == "China" & grepl("roundwood", item) & element == "Import Value") %>% 
  group_by(item, year) %>% 
  summarize(sum_imp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = sum_imp, color = item)) +
  geom_line()

trade_df %>% 
  filter(reporter_country == "India" & grepl("roundwood", item) & element == "Import Value") %>% 
  group_by(item, year) %>% 
  summarize(sum_imp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = sum_imp, color = item)) +
  geom_line()

trade_df %>% 
  filter(reporter_country == "Myanmar" & partner_country == "China" & element == "Export Value") %>% 
  group_by(item, year) %>% 
  summarize(exp_item = sum(value, na.rm = TRUE)) %>% 
  ggplot(data=., aes(x=year, y=exp_item, color=item)) +
  geom_line() +
  geom_vline(xintercept = 2014, alpha=0.3) +
  labs(title="Change in Chinese Import of Forestry Goods by Exporter Country", x="Year", y="Import Value") 

trade_df %>% 
  filter(reporter_country == "Russian Federation" & grepl("roundwood", item) & element == "Export Value") %>% 
  group_by(item, year) %>% 
  summarize(sum_exp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = sum_exp, color = item)) +
  geom_vline(xintercept = 2007, alpha=0.3) +
  geom_vline(xintercept = 2008, alpha=0.3) +
  geom_vline(xintercept = 2012, alpha=0.3) +
  geom_line()

trade_df %>% 
  filter(grepl("roundwood", item) & element == "Export Value") %>% 
  group_by(item, year) %>% 
  summarize(sum_exp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = sum_exp, color = item)) +
  geom_vline(xintercept = 2007, alpha=0.3) +
  geom_vline(xintercept = 2008, alpha=0.3) +
  geom_vline(xintercept = 2012, alpha=0.3) +
  geom_line()

trade_df %>% 
  filter(reporter_country == "Russian Federation" & grepl("roundwood", item)) %>% 
  mutate(element2 = case_when(
    element == "Export Value" ~ "exp_val",
    element == "Export Quantity" ~ "exp_quant"
  )) %>% 
  select(year, item, element2, value) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element2, values_from = value) %>% 
  mutate(exp_price = exp_val / exp_quant) %>% 
  pivot_longer(cols=c(-year, -item), names_to = "element", values_to = "value") %>% 
  filter(element2 == "exp_price") %>% 
  group_by(item, year) %>% 
  summarize(mean_price = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = mean_price)) +
  geom_line() +
  facet_wrap(facets = ~item)

trade_df %>% 
  filter(reporter_country == "Russian Federation" & grepl("roundwood", item) & element == "Export Quantity") %>% 
  group_by(item, year) %>%
  summarize(total_exp = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = total_exp, color = item)) +
  geom_line() 

#' 
#' # Forestry Data of China 3
## ---------------------------------------------------------------------------------------------------------------------------------
forest_df %>% 
  filter(area == "China" & item %in% c("Roundwood", "Sawnwood", "Wood-based panels")) %>% 
  ggplot(data = ., aes(x=year, y=value, color=item)) +
  geom_line() +
  labs(title="Chinese Forestry Goods", x="Year", y="Value") +
  geom_vline(xintercept = 2014, alpha=0.3) +
  facet_wrap(facets=~element, scales="free_y")

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
item1 <- "Industrial roundwood, non-coniferous non-tropical (export/import)"
item2 <- "Industrial roundwood, non-coniferous tropical (export/import)"

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_df2 <- trade_df %>% 
  filter(reporter_country_code < 5000 & partner_country_code < 5000 & ! reporter_country_code %in% c(252, 254) & ! partner_country_code %in% c(252, 254))

#' 
#' # Complementarity
#' ## Shift-Share 
#' 
#' $y_{i,t} = \beta_0 + \beta_1 \text{Exposure}_{i,t} + \varepsilon_{i,t}$
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df <- trade_df2 %>% 
  filter(year == 2014 & item == item2 & element == "Export Value") %>% 
  group_by(reporter_country) %>% 
  summarize(sum_exp = sum(value, na.rm = TRUE)) %>% 
  select(reporter_country, sum_exp) %>% 
  left_join(filter(trade_df2, year == 2014 & item == item2 & element == "Export Value"), by="reporter_country") %>% 
  mutate(exp_share_2013 = value / sum_exp) %>% 
  select(reporter_country, partner_country, year, exp_share_2013) 

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
m_exp_shift_df <- trade_df2 %>% 
  filter(reporter_country == "Myanmar" & item == item2 & element == "Export Value") %>% 
  group_by(year, partner_country) %>% 
  summarize(exp_m_all = sum(value, na.rm = TRUE)) %>% 
  arrange(partner_country, year) %>% 
  group_by(partner_country) %>% 
  mutate(exp_m_diff = exp_m_all - lag(exp_m_all)) %>% 
  ungroup()

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df2 <- m_exp_shift_df %>% 
  filter(year == 2015) %>% 
  select(partner_country, exp_m_diff) %>% 
  right_join(trade_ss_df, by = "partner_country")

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df3 <- trade_ss_df2 %>% 
  mutate(ss_indiv = exp_share_2013 * exp_m_diff) %>% 
  group_by(reporter_country) %>% 
  summarize(ss = - sum(ss_indiv, na.rm = TRUE))

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
forest_df2 <- forest_df %>% 
  filter(area_code < 5000 & ! area_code %in% c(252, 254))

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df4 <- forest_df2 %>% 
  filter(item %in% c("Industrial roundwood, non-coniferous tropical (export/import)") ) %>%
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
    imp_quant_diff = imp_quant - lag(imp_quant),
    exp_quant_diff = exp_quant - lag(exp_quant),
    imp_val_diff = imp_val - lag(imp_val),
    exp_val_diff = exp_val - lag(exp_val)) %>% 
  right_join(trade_ss_df3, by=c("area" = "reporter_country"))

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df4 <- trade_ss_df4 %>% 
  mutate(ss2 = -ss)

#' 
#' # Distribution of Shift-Share (Exposure) Value
## ---------------------------------------------------------------------------------------------------------------------------------
trade_ss_df4 %>% 
  filter(year == 2015) %>% 
  ggplot(data = ., aes(x=ss2)) +
  geom_histogram() +
  labs(x = "Exposure Value", y = "Count", title = "Distribution of Exposure Value")

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results <- trade_ss_df4 %>%
  filter(year > 2014) %>% 
  group_by(year) %>%
  do(tidy(lm(exp_val ~ ss2, data = .),conf.int=T, level=0.95)) %>%
  ungroup()

#' 
#' # Effect on Export Value
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results %>% 
  filter(term == "ss2") %>% 
  ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Year", y = "Coefficient Estimate") +
  ggtitle("Regression Coefficients of Export Value on Exposure to Myanmar's Supply Shock") +
  theme_minimal()

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results <- trade_ss_df4 %>%
  filter(year > 2014) %>% 
  group_by(year) %>%
  do(tidy(lm(exp_quant ~ ss2, data = .),conf.int=T, level=0.95)) %>%
  ungroup()

#' 
#' # Effect on Export Quantity
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results %>% 
  filter(term == "ss2") %>% 
  ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Year", y = "Coefficient Estimate") +
  ggtitle("Regression Coefficients of Export Quantity on Exposure to Myanmar's Supply Shock") +
  theme_minimal()

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results <- trade_ss_df4 %>%
  filter(year > 2014) %>% 
  filter(exp_quant != 0) %>% 
  mutate(exp_price = exp_val / exp_quant) %>% 
  group_by(year) %>%
  do(tidy(lm(exp_price ~ ss2, data = .),conf.int=T, level=0.95)) %>%
  ungroup()

#' 
#' # Effect on Export Price
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
reg_results %>% 
  filter(term == "ss2") %>% 
  ggplot(., aes(x = factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Year", y = "Coefficient Estimate") +
  ggtitle("Regression Coefficients of Export Price on Exposure to Myanmar's Supply Shock") +
  theme_minimal()

#' 
#' # Appendix
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
ss_df <- trade_df2 %>% 
  distinct(partner_country)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# initial export value from Myanmar to i
ss_df <- trade_df2 %>% 
  filter(reporter_country == "Myanmar" & year == 2013 & item == item2 & element == "Export Value") %>% 
  select(partner_country, exp_m_i_2013 = value) %>% 
  right_join(., ss_df, by="partner_country")

#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# sum of initial exp val from all to i (sum of i's imp)
ss_df <- trade_df2 %>% 
  filter(year == 2013 & item == item2 & element == "Export Value") %>% 
  group_by(partner_country) %>% 
  summarize(sum_imp_2013 = sum(value, na.rm = TRUE)) %>% 
  right_join(., ss_df, by="partner_country")

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
sum_exp_m_2014 <- 479293	
sum_exp_m_2015 <- 57858

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
ss_df <- ss_df %>% 
  mutate(
    ss_2014 = exp_m_i_2013 / sum_imp_2013 * sum_exp_m_2014,
    ss_2015 = exp_m_i_2013 / sum_imp_2013 * sum_exp_m_2015
  ) %>% 
  replace_na(list(ss_2014 = 0, ss_2015 = 0))

#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
trade_df2 %>% 
  filter(reporter_country == "Myanmar" & item == item2 & element == "Export Value") %>% 
  group_by(year) %>% 
  summarize(value = sum(value, na.rm = TRUE)) %>% 
  ggplot(data = ., aes(x = year, y = value)) +
  geom_line()

#' 
