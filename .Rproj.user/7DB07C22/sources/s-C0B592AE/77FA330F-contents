# 0. load libraries -------------------------------------------------------

library(tidyverse)

# 1. load data ------------------------------------------------------------

data <- readr::read_csv(here::here("data", "data_parsed.csv")) %>%
  dplyr::mutate(dplyr::across(
    c(region, sex),
    .fns = as_factor
  ))

# 2. visualize data -------------------------------------------------------

fish_table <- data %>% 
  dplyr::select(length_cm, region, sex) %>% 
  dplyr::group_by(region, sex) %>% 
  dplyr::summarize("Length (cm)" = mean(length_cm), 
            S.D. = sd(length_cm), 
            N = dplyr::n()) %>% 
  dplyr::mutate(dplyr::across(
    .cols = c("Length (cm)", S.D.),
    .fns = ~ round(.x, digits = 1)
  )) %>% 
  dplyr::rename(Region = region, Sex = sex)

saveRDS(fish_table, here::here("figures", "fish_table.RDS"))
