library(tidyverse)
library(ggdendro)
library(dendextend)
library(cluster)
library(factoextra)
library(here)

data <- readr::read_csv(here::here("data", "data_parsed.csv")) %>%
  dplyr::mutate(dplyr::across(
    c(region, sex),
    .fns = as_factor
  )) %>% dplyr::select(sex, region, ba_ca:fe_ca)
  
cl_data <- data %>% select(ba_ca:fe_ca) %>% scale()

wards <- cl_data %>%
  # dist() %>% 
  agnes(method = "ward")

ggdendrogram(wards, rotate = TRUE) +
  theme_bw()

profiling <- cutree(wards, 2)  
tab <- tibble(data, profiling) %>% 
  select(sex, region, profiling)
tab %>% 
  group_by(profiling) %>% 
  count(region)

den <- dendro_data(wards)

ggplot() +
  geom_segment(data = segment(den), 
               aes(x = x, y = y, xend = xend, yend = yend)
  ) +
  geom_text(data = label(den), 
            aes(x = x, y = y, label = label, hjust = 0), 
            size = 3
  ) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2, 0)) + 
  theme_bw()

test <- agnes(cl_data, method = "ward") %>% as.dendrogram()
test %>% set("branches_k_color", k=4) %>% plot()

gap_stat <- clusGap(scale(cl_data), FUNcluster = hcut, K.max = 10, B = 100)
fviz_gap_stat(gap_stat)
