pacman::p_load(tidyverse, arrow, broom, estimatr, modelsummary, fixest)

trade_df <- read_parquet("data/dev/trade_gdp_dist.parquet")
forest_df <- read_parquet("data/dev/faostat_forestry_long.parquet")
country_df <- read.csv("data/raw/faostat/Forestry_E_All_Data/forestry_country_code.csv")
gdp_df <- read.csv("data/raw/gdp.csv")

trade_df <- trade_df %>% 
  filter(reporter_country_code < 5000 & partner_country_code < 5000 & ! reporter_country_code %in% c(252, 254) & ! partner_country_code %in% c(252, 254)) %>% 
  left_join(country_df, by = c("reporter_country" = "Country"))


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

####Decomposition####
decompose_var <- function(item_name, element_name) {
  decomposition_df <- trade_df %>% 
    filter(item == item_name & element == element_name) %>% 
    filter(year == 2018) %>% 
    mutate(value2 = if_else(is.na(value), 0, value)) %>% 
    group_by(reporter_country) %>% 
    summarise(
      x_it = sum(value2, na.rm = TRUE),
      n_it = n(),
      xbar_it = mean(value2, na.rm = TRUE)
    ) %>% 
    filter(x_it != 0)  
  lm_decomp_all_1 <- lm_robust(log(n_it) ~ 0 + log(x_it), data = decomposition_df) 
  lm_decomp_all_2 <- lm_robust(log(xbar_it) ~ 0 + log(x_it), data = decomposition_df) 
  return(list(lm_decomp_all_1, lm_decomp_all_2))
}

item_vec <- c("Industrial roundwood, coniferous (export/import)", "Industrial roundwood, non-coniferous non-tropical (export/import)", "Industrial roundwood, non-coniferous tropical (export/import)")

all_regs <- list()
for (item_name in item_vec) {
  reg_results <- decompose_var(item_name, "Export Value")
  all_regs <- c(all_regs, reg_results)
}

modelsummary(all_regs, gof_map = c("nobs", "r.squared"), stars = c('*' = .1, '**' = .05, '***' = .01), output = "latex")

all_regs <- list()
for (item_name in item_vec) {
  reg_results <- decompose_var(item_name, "Import Value")
  all_regs <- c(all_regs, reg_results)
}
modelsummary(all_regs, gof_map = c("nobs", "r.squared"), stars = c('*' = .1, '**' = .05, '***' = .01), output = "latex")


item_vec <- c("Plywood", "Sawnwood, coniferous", "Sawnwood, non-coniferous all", "Wood chips and particles")

all_regs <- list()
for (item_name in item_vec) {
  reg_results <- decompose_var(item_name, "Export Value")
  all_regs <- c(all_regs, reg_results)
}

modelsummary(all_regs, stars = TRUE, gof_map = c("nobs", "r.squared"))

all_regs <- list()
for (item_name in item_vec) {
  reg_results <- decompose_var(item_name, "Import Value")
  all_regs <- c(all_regs, reg_results)
}
modelsummary(all_regs, stars = TRUE, gof_map = c("nobs", "r.squared"))

reg_results <- decomposition_df %>% 
  group_by(year) %>% 
  do(tidy(lm_robust(log(n_it) ~ 0 + log(x_it), data = .))) %>% 
  ungroup()

reg_results %>% 
  filter(term == "log(x_it)") %>% 
  ggplot(data = ., aes(x = factor(year), y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.3)) +
  labs(x = "Year", y = "Coefficient Estimate") +
  ggtitle(paste0(item_name, ": Gravity coefficient over time")) +
  theme_minimal()

file_name <- paste0("fig/gravity_eq/", gsub(",|\\(|\\)|[[:blank:]]|/", "_", item_name), ".png")
ggsave(file = file_name, plot = p)
print(item_name)

ggplot(data = decomposition_df, aes(x = n_it, y = x_it)) +
  geom_point()

ggplot(data = decomposition_df, aes(x = n_it, y = xbar_it)) +
  geom_point()

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
  mutate(value2 = replace_na(value, 0)) %>% 
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

etable(reg1, reg2, reg3, reg4, tex = TRUE, title = "Production of Forestry Goods and Usage of Logs")

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

####Myanmar 2014####
item_vec <- c("Industrial roundwood, coniferous (export/import)", "Industrial roundwood, non-coniferous non-tropical (export/import)", "Industrial roundwood, non-coniferous tropical (export/import)", )
forest_df %>% 
  filter(area == "Myanmar"  & item %in% item_vec & element %in% c("Export Value", "Export Quantity")) %>% 
  filter(year > 1990) %>% 
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
      element == "exp_val" ~ "Export Value (1000USD)",
      element == "exp_quant" ~ "Export Quantity (m3)"
    )
  )%>% 
  ggplot(data = ., aes(x=year, y=value, color=item)) +
  geom_line() +
  labs(title="Change in Export of Myanmar Forestry Goods", x="Year", y="Value")  +
  geom_vline(xintercept = 2014, alpha=0.3) +
  facet_wrap(facets=~element, scales="free_y", ncol = 1)


forest_df %>% 
  filter(area == "Russian Federation"  & item %in% item_vec & element %in% c("Export Value", "Export Quantity")) %>% 
  filter(year > 1990) %>% 
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
      element == "exp_val" ~ "Export Value (1000USD)",
      element == "exp_quant" ~ "Export Quantity (m3)"
    )
  )%>% 
  ggplot(data = ., aes(x=year, y=value, color=item)) +
  geom_line() +
  labs(title="Change in Export of Myanmar Forestry Goods", x="Year", y="Value")  +
  geom_vline(xintercept = 2007, alpha=0.3) +
  facet_wrap(facets=~element, scales="free_y", ncol = 1)


trade_df %>% 
  filter(partner_country == "Russian Federation" & item %in% item_vec & element %in% c("Import Value", "Import Quantity")) %>% 
  group_by(year, item, element) %>% 
  summarize(total = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(element2 = case_when(
    element == "Import Value" ~ "imp_val",
    element == "Import Quantity" ~ "imp_quant"
  )) %>% 
  select(year, item, element2, total) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element2, values_from = total) %>% 
  mutate(imp_price = imp_val / imp_quant) %>% 
  pivot_longer(cols=c(-year, -item), names_to = "element", values_to = "total") %>% 
  mutate(
    element = case_when(
      element == "imp_price" ~ "Import Price (Value / Quantity)",
      element == "imp_val" ~ "Import Value (1000USD)",
      element == "imp_quant" ~ "Import Quantity (m3)"
    )
  )%>% 
  ggplot(data = ., aes(x=year, y=total, color=item)) +
  geom_line() +
  labs(title="Change in Import of Russian Forestry Goods", x="Year", y="Value")  +
  geom_vline(xintercept = 2007, alpha=0.3) +
  facet_wrap(facets=~element, scales="free_y", ncol = 1)

####Shock####
country_name <- "Russian Federation"
country_name2 <- "Russia"
start_year <- 2000
shock_year <- 2007

for (item_i in 1:3) {
  for (element_i in 1:3) {
    item_name <- item_vec[item_i]
    element_name <- element_vec[element_i]
    element_name2 <- element_vec2[element_i]
    p <- trade_df %>% 
      filter(item %in% item_vec & element %in% c("Import Value", "Import Quantity")) %>% 
      filter(year > start_year) %>% 
      filter(year < 2015) %>% 
      group_by(partner_country, year, item, element) %>% 
      summarize(total = sum(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(element2 = case_when(
        element == "Import Value" ~ "imp_val",
        element == "Import Quantity" ~ "imp_quant"
      )) %>% 
      select(partner_country, year, item, element2, total) %>% 
      group_by(item) %>% 
      pivot_wider(names_from = element2, values_from = total) %>% 
      mutate(imp_price = imp_val / imp_quant) %>% 
      pivot_longer(cols=c(-partner_country, -year, -item), names_to = "element", values_to = "total") %>% 
      mutate(
        element = case_when(
          element == "imp_price" ~ "Import Price (1000USD / m3)",
          element == "imp_val" ~ "Import Value (1000USD)",
          element == "imp_quant" ~ "Import Quantity (m3)"
        )
      )%>% 
      ggplot() +
      geom_line(data = . %>% filter(element == element_name & item == item_name), aes(x = year, y = total, group = partner_country),
                color = "gray", alpha = 0.5) +
      geom_line(data = . %>% filter(partner_country == country_name & element == element_name & item == item_name), aes(x = year, y = total),
                color = "blue") +
      geom_vline(xintercept = shock_year, alpha = 0.3) +
      labs(x = "Year", y = element_name) +
      theme_minimal()
    file_name <- paste0("fig/shock/", country_name2, "/", gsub(",|\\(|\\)|[[:blank:]]|/", "_", item_name), "_", element_name2, ".png")
    ggsave(filename = file_name, width = 10, height = 8, dpi = 150,  plot = p)
    print(file_name)
  }
}

item_name <- item_vec[3]
element_name <- element_vec[1]
element_name2 <- element_vec2[1]
p <- trade_df %>% 
  filter(item %in% item_vec & element %in% c("Import Value", "Import Quantity")) %>% 
  filter(year > start_year) %>% 
  filter(year < 2015) %>% 
  group_by(partner_country, year, item, element) %>% 
  summarize(total = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(element2 = case_when(
    element == "Import Value" ~ "imp_val",
    element == "Import Quantity" ~ "imp_quant"
  )) %>% 
  select(partner_country, year, item, element2, total) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element2, values_from = total) %>% 
  mutate(imp_price = imp_val / imp_quant) %>% 
  pivot_longer(cols=c(-partner_country, -year, -item), names_to = "element", values_to = "total") %>% 
  mutate(
    element = case_when(
      element == "imp_price" ~ "Import Price (1000USD / m3)",
      element == "imp_val" ~ "Import Value (1000USD)",
      element == "imp_quant" ~ "Import Quantity (m3)"
    )
  )%>% 
  ggplot() +
  geom_line(data = . %>% filter(element == element_name & item == item_name), aes(x = year, y = total, group = partner_country),
            color = "gray", alpha = 0.5) +
  geom_line(data = . %>% filter(partner_country == country_name & element == element_name & item == item_name), aes(x = year, y = total),
            color = "blue") +
  geom_vline(xintercept = shock_year, alpha = 0.3) +
  ylim(0, 3) +
  labs(x = "Year", y = element_name) +
  theme_minimal()

file_name <- paste0("fig/shock/", country_name2, "/", gsub(",|\\(|\\)|[[:blank:]]|/", "_", item_name), "_", element_name2, ".png")
ggsave(filename = file_name,width = 10, height = 8, dpi = 150,  plot = p)
print(file_name)


# Myanmar 2014
country_name <- "Myanmar"
country_name2 <- "Myanmar"
start_year <- 2010
shock_year <- 2014

for (item_i in 1:3) {
  for (element_i in 1:3) {
    item_name <- item_vec[item_i]
    element_name <- element_vec[element_i]
    element_name2 <- element_vec2[element_i]
    p <- trade_df %>% 
      filter(item %in% item_vec & element %in% c("Import Value", "Import Quantity")) %>% 
      filter(year > start_year) %>% 
      group_by(partner_country, year, item, element) %>% 
      summarize(total = sum(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(element2 = case_when(
        element == "Import Value" ~ "imp_val",
        element == "Import Quantity" ~ "imp_quant"
      )) %>% 
      select(partner_country, year, item, element2, total) %>% 
      group_by(item) %>% 
      pivot_wider(names_from = element2, values_from = total) %>% 
      mutate(imp_price = imp_val / imp_quant) %>% 
      pivot_longer(cols=c(-partner_country, -year, -item), names_to = "element", values_to = "total") %>% 
      mutate(
        element = case_when(
          element == "imp_price" ~ "Import Price (1000USD / m3)",
          element == "imp_val" ~ "Import Value (1000USD)",
          element == "imp_quant" ~ "Import Quantity (m3)"
        )
      )%>% 
      ggplot() +
      geom_line(data = . %>% filter(element == element_name & item == item_name), aes(x = year, y = total, group = partner_country),
                color = "gray", alpha = 0.5) +
      geom_line(data = . %>% filter(partner_country == country_name & element == element_name & item == item_name), aes(x = year, y = total),
                color = "blue") +
      geom_vline(xintercept = shock_year, alpha = 0.3) +
      labs(x = "Year", y = element_name) +
      theme_minimal()
    file_name <- paste0("fig/shock/", country_name2, "/", gsub(",|\\(|\\)|[[:blank:]]|/", "_", item_name), "_", element_name2, ".png")
    ggsave(filename = file_name,width = 10, height = 8, dpi = 150,  plot = p)
    print(file_name)
  }
}


item_name <- item_vec[1]
element_name <- element_vec[3]
element_name2 <- element_vec2[3]
p <- trade_df %>% 
  filter(item %in% item_vec & element %in% c("Import Value", "Import Quantity")) %>% 
  filter(year > start_year) %>% 
  group_by(partner_country, year, item, element) %>% 
  summarize(total = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(element2 = case_when(
    element == "Import Value" ~ "imp_val",
    element == "Import Quantity" ~ "imp_quant"
  )) %>% 
  select(partner_country, year, item, element2, total) %>% 
  group_by(item) %>% 
  pivot_wider(names_from = element2, values_from = total) %>% 
  mutate(imp_price = imp_val / imp_quant) %>% 
  pivot_longer(cols=c(-partner_country, -year, -item), names_to = "element", values_to = "total") %>% 
  mutate(
    element = case_when(
      element == "imp_price" ~ "Import Price (1000USD / m3)",
      element == "imp_val" ~ "Import Value (1000USD)",
      element == "imp_quant" ~ "Import Quantity (m3)"
    )
  )%>% 
  ggplot() +
  geom_line(data = . %>% filter(element == element_name & item == item_name), aes(x = year, y = total, group = partner_country),
            color = "gray", alpha = 0.5) +
  geom_line(data = . %>% filter(partner_country == country_name & element == element_name & item == item_name), aes(x = year, y = total),
            color = "blue") +
  geom_vline(xintercept = shock_year, alpha = 0.3) +
  ylim(0, 20000000) +
  labs(x = "Year", y = element_name) +
  theme_minimal()
file_name <- paste0("fig/shock/", country_name2, "/", gsub(",|\\(|\\)|[[:blank:]]|/", "_", item_name), "_", element_name2, ".png")
ggsave(filename = file_name, width = 10, height = 8, dpi = 150, plot = p)
print(file_name)
