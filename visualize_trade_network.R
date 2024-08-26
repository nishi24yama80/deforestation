pacman::p_load(tidyverse, ggnetwork, network, ggmap, arrow, ggraph, igraph)


trade_df <- read_parquet("data/dev/trade_gdp_dist.parquet")
geo_df <- read.csv("data/raw/geo_cepii.csv")

geo_df2 <- geo_df %>% 
  select(iso3, lat, lon) %>% 
  distinct(iso3, .keep_all = TRUE)

trade_df2 <- trade_df %>% 
  mutate(ISO3.Code.x = if_else(ISO3.Code.x=="F351", "CHN", ISO3.Code.x)) %>% 
  mutate(ISO3.Code.y = if_else(ISO3.Code.y=="F351", "CHN", ISO3.Code.y)) %>% 
  left_join(geo_df2, by=c("ISO3.Code.x" = "iso3")) %>% 
  left_join(geo_df2, by=c("ISO3.Code.y" = "iso3")) 


trade_2010_df <- trade_df2 %>% 
  filter(year == 2010 & item == "Industrial roundwood, coniferous (export/import)" & element == "Export Value" & reporter_country_code < 5000 & partner_country_code < 5000) %>% 
  filter(!is.na(ISO3.Code.x) & !is.na(ISO3.Code.y)) %>% 
  filter(value > 10000) %>% 
  arrange(desc(value)) 

#trade_net <- network::network(trade_2010_df[, c("ISO3.Code.x", "ISO3.Code.y")], directed = TRUE)
trade_net <- network::network(trade_2010_df[, c("reporter_country", "partner_country")], directed = TRUE)
set.edge.attribute(trade_net, "value", trade_2010_df$value)

ggplot(trade_net, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(linewidth=value, color = vertex.names), 
    curvature = 0.1,
    arrow = arrow(length = unit(1, "lines"), type = "closed")
    ) +
  geom_nodelabel(aes(label = vertex.names)) +
  theme_void() + theme(legend.position = "none")



# データ準備
trade_2010_df <- trade_df2 %>%
  filter(year == 2010 & element == "Export Value" & partner_country_code < 5000) %>%
  filter(!is.na(ISO3.Code.x) & !is.na(ISO3.Code.y)) %>%
  arrange(desc(value))

write.csv(trade_2010_df, "~/Downloads/trade_2010_df.csv")

# ネットワークオブジェクトを作成
trade_net <- graph_from_data_frame(trade_2010_df[, c("reporter_country", "partner_country")], directed = TRUE)

# エッジに取引価値の属性を追加
E(trade_net)$value <- trade_2010_df$value

# ggraphでネットワーク図をプロット
ggraph(trade_net, layout = "fr") +  # layoutはfruchterman-reingoldを使用
  geom_edge_link(aes(width = value), arrow = arrow(length = unit(4, 'mm'), type = "closed"), end_cap = circle(3, 'mm')) +
  geom_node_point(size = 5, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = 30) +  # max.overlaps を増加
  theme_void() + 
  theme(legend.position = "none")


trade_2010_df <- trade_df2 %>% 
  filter(year == 2010 & item == "Industrial roundwood, coniferous (export/import)" & element == "Export Value" & reporter_country == "Myanmar" & partner_country_code < 5000) %>% 
  filter(!is.na(ISO3.Code.x) & !is.na(ISO3.Code.y)) %>% 
  arrange(desc(value)) 

#trade_net <- network::network(trade_2010_df[, c("ISO3.Code.x", "ISO3.Code.y")], directed = TRUE)
trade_net <- network::network(trade_2010_df[, c("reporter_country", "partner_country")], directed = TRUE)
set.edge.attribute(trade_net, "value", trade_2010_df$value)

ggplot(trade_net, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(linewidth=value, color = vertex.names), 
    curvature = 0.1,
    arrow = arrow(length = unit(1, "lines"), type = "closed")
  ) +
  geom_nodelabel(aes(label = vertex.names)) +
  theme_void() + theme(legend.position = "none")

trade_2010_df <- trade_df2 %>% 
  filter(year == 2014 & item == "Industrial roundwood, non-coniferous tropical (export/import)" & element == "Export Value" & partner_country == "China" & partner_country_code < 5000) %>% 
  filter(!is.na(ISO3.Code.x) & !is.na(ISO3.Code.y)) %>% 
  filter(value > 5000) %>% 
  arrange(desc(value)) 

#trade_net <- network::network(trade_2010_df[, c("ISO3.Code.x", "ISO3.Code.y")], directed = TRUE)
trade_net <- network::network(trade_2010_df[, c("reporter_country", "partner_country")], directed = TRUE)
set.edge.attribute(trade_net, "value", trade_2010_df$value)

ggplot(trade_net, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(linewidth=value, color = vertex.names), 
    curvature = 0.1,
    arrow = arrow(length = unit(1, "lines"), type = "closed")
  ) +
  geom_nodelabel(aes(label = vertex.names)) +
  theme_void() +
  labs(title = "2014 Trade Flow to China")

trade_country <- trade_2010_df %>% 
  distinct(reporter_country, lat.x, lon.x, .keep_all = TRUE) %>% 
  select(reporter_country, lat.x, lon.x)

lat <- trade_country$lat.x
names(lat) <- trade_country$reporter_country
lon <- trade_country$lon.x
names(lon) <- trade_country$reporter_country

geo <- cbind(lon[network.vertex.names(trade_net)], lat[network.vertex.names(trade_net)])

g <- ggnetwork(trade_net, layout = geo, scale = FALSE) %>% 
  rename(lon = x, lat = y)
g <- g %>% 
  filter(!is.na(value))

# map background
map <- ggmap::get_map(c(left = min(geo[, 1]), bottom = min(geo[, 2]), right = max(geo[, 1]), top = max(geo[, 2])))
# map + network plot
ggmap::ggmap(map) +
  geom_point(data = g, aes(lon, lat)) +
  geom_edges(data = g, aes(lon, lat, xend = xend, yend = yend)) +
  geom_nodelabel(data = g, aes(label = vertex.names))




