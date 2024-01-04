pacman::p_load(tidyverse, gsynth, panelView, arrow, openxlsx)


## ---------------------------------------------------------------------------------------------------------------------------------
trade_df <- read_parquet("data/dev/trade_gdp_dist.parquet")
forest_df <- read_parquet("data/dev/faostat_forestry_long.parquet")
country_df <- read.csv("data/raw/faostat/Forestry_E_All_Data/forestry_country_code.csv")
gdp_df <- read.csv("data/raw/gdp.csv")
penn_df <- read.xlsx("data/raw/penn_world_table.xlsx")

trade_df <- trade_df %>% 
  filter(reporter_country_code < 5000 & partner_country_code < 5000 & ! reporter_country_code %in% c(252, 254) & ! partner_country_code %in% c(252, 254)) %>% 
  left_join(country_df, by = c("partner_country" = "Country"))

forest_df <- forest_df %>% 
  filter(area_code < 5000 & ! area_code %in% c(252, 254)) %>% 
  left_join(penn_df %>% select(countrycode, year, rgdpna, rgdpo), by=c("ISO3.Code" = "countrycode", "year"="year")) 

item_list <- trade_df %>% 
  filter(year == 2018) %>% 
  distinct(item) %>% 
  pull()

roundwood_item_list <- trade_df %>% 
  filter(grepl("roundwood", item)) %>% 
  distinct(item) %>% 
  pull()

forest_item_list <- forest_df %>% 
  filter(year == 2018) %>% 
  distinct(item) %>% 
  pull()

item_vec <- c("Industrial roundwood, coniferous (export/import)", "Industrial roundwood, non-coniferous non-tropical (export/import)", "Industrial roundwood, non-coniferous tropical (export/import)")

####gsynth####
####gsynth: production####
shock_item_name <- item_vec[3]
outcome_item_name <- "Industrial roundwood, coniferous"
outcome_element_name <- "Production"
shock_year <- 2015
#shock_leading_country <- "Russian Federation"
#shock_leading_country2 <- "Russia"
shock_leading_country <- "Myanmar"
shock_leading_country2 <- "Myanmar"

treatment_df <- trade_df %>% 
  distinct(partner_country)

treatment_df <- trade_df %>% 
  filter(reporter_country == shock_leading_country & year == shock_year & item == shock_item_name & element == "Export Quantity") %>% 
  mutate(m_dest = if_else(value > 0, 1, 0)) %>% 
  select(partner_country, m_dest, value) %>% 
  right_join(treatment_df, by = "partner_country") %>% 
  mutate(m_dest2 = if_else(is.na(m_dest), 0, m_dest)) %>% 
  select(partner_country, m_dest2)


did_df3 <- forest_df %>% 
  filter(year > 1990) %>% 
  filter(item == outcome_item_name & element == outcome_element_name) %>%
  mutate(lag_gdp = lag(rgdpo)) %>% 
  mutate(imp_value = value) %>% 
  select(area, year, imp_value, lag_gdp) %>% 
  left_join(treatment_df, by = c("area" = "partner_country")) %>% 
  mutate(m_dest2 = if_else(is.na(m_dest2), 0, m_dest2)) %>% 
  mutate(D = if_else(year < shock_year, 0, m_dest2)) %>% 
  filter(!is.na(imp_value)) %>% 
  filter(!is.na(lag_gdp))

country_list <- did_df3 %>% 
  filter(year < shock_year) %>% 
  group_by(area) %>% 
  summarize(nonna = sum(!is.na(imp_value))) %>% 
  filter(nonna >= 7) %>% 
  select(area) %>% 
  pull()

did_df4 <- did_df3 %>% 
  filter(area %in% country_list) %>% 
  filter(area != shock_leading_country)

#panelview(imp_value ~ D, data = did_df4,  index = c("area","year"), pre.post = TRUE) 
#panelview(imp_value ~ D, data = did_df4,  index = c("area","year"), type = "outcome") 


out_nogdp <- gsynth(imp_value ~ D, data = did_df4, 
              index = c("area","year"), force = "two-way", 
              CV = TRUE, r = c(0, 5), se = TRUE, 
              inference = "parametric", nboots = 1000, 
              parallel = TRUE)

#plot(out_nogdp)


file_name <- paste0("fig/syntheticDID/", shock_leading_country2, "/ATT/", outcome_element_name, "_", gsub(",|\\(|\\)|[[:blank:]]|/", "_", outcome_item_name), ".png")
png(filename = file_name)
plot(out_nogdp, xlab = "Elapsed Year from Supply Shock", ylab = "Coefficient", main = paste0(outcome_item_name))
dev.off()


file_name <- paste0("fig/syntheticDID/", shock_leading_country2, "/CF/", outcome_element_name, "_", gsub(",|\\(|\\)|[[:blank:]]|/", "_", outcome_item_name), ".png")
png(filename = file_name)
plot(out_nogdp, type = "ct", raw = "none", xlab = "Elapsed Year from Supply Shock", ylab = "Outcome Value", main = paste0(outcome_item_name))
dev.off()




####gsynth: trade####

item_vec <- c("Industrial roundwood, coniferous (export/import)", "Industrial roundwood, non-coniferous non-tropical (export/import)", "Industrial roundwood, non-coniferous tropical (export/import)")
element_vec <- c("Export Value", "Export Quantity", "Import Value", "Import Quantity")
item_vec2 <- c("Coniferous", "Non-coniferous non-tropical", "Non-coniferous tropical")
element_vec2 <- c("exp_val", "exp_quant", "imp_val", "imp_quant")

shock_item_name <- item_vec[3]
outcome_item_name <- "Industrial roundwood, coniferous"
outcome_element_name <- "Production"
shock_year <- 2015
#shock_leading_country <- "Russian Federation"
#shock_leading_country2 <- "Russia"
shock_leading_country <- "Myanmar"
shock_leading_country2 <- "Myanmar"

for (item_i in 1:3){
  for (element_i in 1:4) {
    outcome_item_name <- item_vec[item_i]
    outcome_element_name <- element_vec[element_i]
    outcome_item_name2 <- item_vec2[item_i]
    outcome_element_name2 <- element_vec2[element_i]
    treatment_df <- trade_df %>% 
      distinct(partner_country)
    
    treatment_df <- trade_df %>% 
      filter(reporter_country == shock_leading_country & year == shock_year & item == shock_item_name & element == "Export Quantity") %>% 
      mutate(m_dest = if_else(value > 0, 1, 0)) %>% 
      select(partner_country, m_dest, value) %>% 
      right_join(treatment_df, by = "partner_country") %>% 
      mutate(m_dest2 = if_else(is.na(m_dest), 0, m_dest)) %>% 
      select(partner_country, m_dest2)
    
    
    did_df3 <- forest_df %>% 
      filter(year > 1990) %>% 
      filter(item == outcome_item_name & element == outcome_element_name) %>%
      mutate(lag_gdp = lag(rgdpo)) %>% 
      mutate(imp_value = value) %>% 
      select(area, year, imp_value, lag_gdp) %>% 
      left_join(treatment_df, by = c("area" = "partner_country")) %>% 
      mutate(m_dest2 = if_else(is.na(m_dest2), 0, m_dest2)) %>% 
      mutate(D = if_else(year < shock_year, 0, m_dest2)) %>% 
      filter(!is.na(imp_value)) %>% 
      filter(!is.na(lag_gdp))
    
    country_list <- did_df3 %>% 
      filter(year < shock_year) %>% 
      group_by(area) %>% 
      summarize(nonna = sum(!is.na(imp_value))) %>% 
      filter(nonna >= 7) %>% 
      select(area) %>% 
      pull()
    
    did_df4 <- did_df3 %>% 
      filter(area %in% country_list) %>% 
      filter(area != shock_leading_country)
    
    #panelview(imp_value ~ D, data = did_df4,  index = c("area","year"), pre.post = TRUE) 
    #panelview(imp_value ~ D, data = did_df4,  index = c("area","year"), type = "outcome") 
    
    
    out_nogdp <- gsynth(imp_value ~ D, data = did_df4, 
                        index = c("area","year"), force = "two-way", 
                        CV = TRUE, r = c(0, 5), se = TRUE, 
                        inference = "parametric", nboots = 1000, 
                        parallel = TRUE)
    
    #plot(out_nogdp)
    
    
    file_name <- paste0("fig/syntheticDID/", shock_leading_country2, "/ATT/", outcome_element_name2, "_", gsub(",|\\(|\\)|[[:blank:]]|/", "_", outcome_item_name2), ".png")
    png(filename = file_name)
    plot(out_nogdp, xlab = "Elapsed Year from Supply Shock", ylab = "Coefficient", main = paste0(outcome_item_name2))
    dev.off()
    
    
    file_name <- paste0("fig/syntheticDID/", shock_leading_country2, "/CF/", outcome_element_name2, "_", gsub(",|\\(|\\)|[[:blank:]]|/", "_", outcome_item_name2), ".png")
    png(filename = file_name)
    plot(out_nogdp, type = "ct", raw = "none", xlab = "Elapsed Year from Supply Shock", ylab = "Outcome Value", main = paste0(outcome_item_name2))
    dev.off()
    print(file_name)
  }
}




shock_item_name <- item_vec[1]
outcome_item_name <- "Industrial roundwood, coniferous"
outcome_element_name <- "Production"
shock_year <- 2007
shock_leading_country <- "Russian Federation"
shock_leading_country2 <- "Russia"
#shock_leading_country <- "Myanmar"
#shock_leading_country2 <- "Myanmar"

for (item_i in 1:3){
  for (element_i in 1:4) {
    outcome_item_name <- item_vec[item_i]
    outcome_element_name <- element_vec[element_i]
    outcome_item_name2 <- item_vec2[item_i]
    outcome_element_name2 <- element_vec2[element_i]
    treatment_df <- trade_df %>% 
      distinct(partner_country)
    
    treatment_df <- trade_df %>% 
      filter(reporter_country == shock_leading_country & year == shock_year & item == shock_item_name & element == "Export Quantity") %>% 
      mutate(m_dest = if_else(value > 0, 1, 0)) %>% 
      select(partner_country, m_dest, value) %>% 
      right_join(treatment_df, by = "partner_country") %>% 
      mutate(m_dest2 = if_else(is.na(m_dest), 0, m_dest)) %>% 
      select(partner_country, m_dest2)
    
    
    did_df3 <- forest_df %>% 
      filter(year > 1990) %>% 
      filter(item == outcome_item_name & element == outcome_element_name) %>%
      mutate(lag_gdp = lag(rgdpo)) %>% 
      mutate(imp_value = value) %>% 
      select(area, year, imp_value, lag_gdp) %>% 
      left_join(treatment_df, by = c("area" = "partner_country")) %>% 
      mutate(m_dest2 = if_else(is.na(m_dest2), 0, m_dest2)) %>% 
      mutate(D = if_else(year < shock_year, 0, m_dest2)) %>% 
      filter(!is.na(imp_value)) %>% 
      filter(!is.na(lag_gdp))
    
    country_list <- did_df3 %>% 
      filter(year < shock_year) %>% 
      group_by(area) %>% 
      summarize(nonna = sum(!is.na(imp_value))) %>% 
      filter(nonna >= 7) %>% 
      select(area) %>% 
      pull()
    
    did_df4 <- did_df3 %>% 
      filter(area %in% country_list) %>% 
      filter(area != shock_leading_country)
    
    #panelview(imp_value ~ D, data = did_df4,  index = c("area","year"), pre.post = TRUE) 
    #panelview(imp_value ~ D, data = did_df4,  index = c("area","year"), type = "outcome") 
    
    
    out_nogdp <- gsynth(imp_value ~ D, data = did_df4, 
                        index = c("area","year"), force = "two-way", 
                        CV = TRUE, r = c(0, 5), se = TRUE, 
                        inference = "parametric", nboots = 1000, 
                        parallel = TRUE)
    
    #plot(out_nogdp)
    
    
    file_name <- paste0("fig/syntheticDID/", shock_leading_country2, "/ATT/", outcome_element_name2, "_", gsub(",|\\(|\\)|[[:blank:]]|/", "_", outcome_item_name2), ".png")
    png(filename = file_name)
    plot(out_nogdp, xlab = "Elapsed Year from Supply Shock", ylab = "Coefficient", main = paste0(outcome_item_name2))
    dev.off()
    
    
    file_name <- paste0("fig/syntheticDID/", shock_leading_country2, "/CF/", outcome_element_name2, "_", gsub(",|\\(|\\)|[[:blank:]]|/", "_", outcome_item_name2), ".png")
    png(filename = file_name)
    plot(out_nogdp, type = "ct", raw = "none", xlab = "Elapsed Year from Supply Shock", ylab = "Outcome Value", main = paste0(outcome_item_name2))
    dev.off()
    print(file_name)
  }
}

##
out_nogdp_unit <- gsynth(imp_value ~ D, data = did_df4, 
                    index = c("area","year"), force = "unit", 
                    CV = TRUE, r = c(0, 5), se = TRUE, 
                    inference = "parametric", nboots = 1000, 
                    parallel = TRUE)

plot(out_nogdp_unit)

out_nogdp_none <- gsynth(imp_value ~ D, data = did_df4, 
                         index = c("area","year"), force = "none", 
                         CV = TRUE, r = c(0, 5), se = TRUE, 
                         inference = "parametric", nboots = 1000, 
                         parallel = TRUE)

plot(out_nogdp_none)

out_gdp <- gsynth(imp_value ~ D + lag_gdp, data = did_df4, 
                    index = c("area","year"), force = "two-way", 
                    CV = TRUE, r = c(0, 5), se = TRUE, 
                    inference = "parametric", nboots = 1000, 
                    parallel = TRUE)

plot(out_gdp)


out_gdp <- gsynth(imp_value ~ D + lag_gdp, data = did_df4, 
                  index = c("area","year"), force = "two-way", 
                  CV = TRUE, r = c(0, 5), se = TRUE, 
                  inference = "parametric", nboots = 1000, 
                  parallel = TRUE)

plot(out_gdp)

cumu2$est.catt

plot(out_gdp, type = "gap", xlab = "Period", main = "My GSynth Plot")

print(out_gdp)
plot(out_gdp, type = "ct")

####Else####
1931500 - 1.96 * 1421820

treatment_df <- trade_df %>% 
  distinct(partner_country, Country)

treatment_df <- trade_df %>% 
  filter(reporter_country == "Russian Federation" & year == 2007 & item == item_vec[1] & element == "Export Quantity") %>% 
  mutate(m_dest = if_else(value > 0, 1, 0)) %>% 
  select(partner_country, m_dest, value) %>% 
  right_join(treatment_df, by = "partner_country") %>% 
  mutate(m_dest2 = if_else(is.na(m_dest), 0, m_dest))

y_df <- forest_df %>% 
  left_join(country_df %>% select(Country, ISO3.Code), by = c("area" = "Country")) %>% 
  left_join(gdp_df, by=c("ISO3.Code" = "Code", "year"="Year")) %>% 
  mutate(gdp = GDP..constant.2015.US..) %>% 
  filter(year > 1990) %>% 
  filter(item == item_vec[3] & element == "Import Value") %>% 
  select(area, year, value, gdp) %>% 
  pivot_wider(names_from = year, values_from = c(value, gdp))

did_df2 <- y_df %>% 
  left_join(treatment_df, by = c("area" = "partner_country")) %>% 
  mutate(m_dest2 = if_else(is.na(m_dest), 0, m_dest2)) %>%
  filter(!is.na(gdp_2000)) %>% 
  mutate(
    value_2015 = if_else(is.na(value_2015), 0, value_2015),
    value_2014 = if_else(is.na(value_2014), 0, value_2014),
    value_2008 = if_else(is.na(value_2008), 0, value_2008),
    value_2007 = if_else(is.na(value_2007), 0, value_2007)
  )

####drdid_panel####
y0 <- did_df[did_df$year == 2014, "value2"]

y1 <- did_df[did_df$year == 2015, "value2"]

d <- did_df[did_df$year == 2014, "m_dest2"]

drdid_reg <- drdid_panel(
  y1 = did_df2$val_2015, y0 = did_df2$val_2014, D = did_df2$m_dest2, covariates = NULL
)

drdid_panel(
  y1 = did_df2$value_2008, y0 = did_df2$value_2007, D = did_df2$m_dest2, covariates = did_df2$gdp_2000
)

did_df <- did_df2 %>% 
  select(value_2014, value_2015, m_dest2) %>% 
  pivot_longer(cols = -m_dest2) %>% 
  mutate(post = if_else(name == "value_2015", 1, 0))

lm1 <- lm_robust(value ~ m_dest2 + post + m_dest2 * post, data = did_df)
summary(lm1)

did_df2 %>% 
  select(val_2014, val_2015, m_dest2) %>% 
  View()

eval_lalonde_cps <- eval_lalonde_cps %>% 
  slice(1:200)

drdid_panel(y1 = eval_lalonde_cps$re78, y0 = eval_lalonde_cps$re75,
            D = eval_lalonde_cps$experimental,
            covariates = NULL)

summary(drdid_reg)



eval_lalonde_cps <- subset(nsw, nsw$treated == 0 | nsw$sample == 2)
# Further reduce sample to speed example
set.seed(123)
unit_random <- sample(1:nrow(eval_lalonde_cps), 5000)
eval_lalonde_cps <- eval_lalonde_cps[unit_random,]
# Select some covariates
covX = as.matrix(cbind(eval_lalonde_cps$age, eval_lalonde_cps$educ,
                       eval_lalonde_cps$black, eval_lalonde_cps$married,
                       eval_lalonde_cps$nodegree, eval_lalonde_cps$hisp,
                       eval_lalonde_cps$re74))

# Implement traditional DR locally efficient DiD with panel data
drdid_panel(y1 = eval_lalonde_cps$re78, y0 = eval_lalonde_cps$re75,
            D = eval_lalonde_cps$experimental,
            covariates = covX)

####DRDID####
data(nsw_long)
eval_lalonde_cps <- subset(nsw_long, nsw_long$treated == 0 | nsw_long$sample == 2)
df <- data.frame(eval_lalonde_cps)
out <- drdid(yname = "re", tname = "year", idname = "id", dname = "experimental",
             xformla= ~ age + educ + black + married + nodegree + hisp + re74,
             data = eval_lalonde_cps, panel = TRUE)

out <- drdid(yname = "re", tname = "year", idname = "id", dname = "experimental",
             data = eval_lalonde_cps, panel = TRUE)
summary(out)


