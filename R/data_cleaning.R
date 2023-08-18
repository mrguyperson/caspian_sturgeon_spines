
# # load libraries ----------------------------------------------------------

# library(tidyverse)
# library(here)

# # load data --------------------------------------------------------------

# path <- here("data", "spine_chem.csv")

# data <- read_csv(path)

# clean data --------------------------------------------------------------
clean_raw_data <- function(raw_data){
  raw_data %>%
    mutate(across(c(Ba:Fe), .fns = ~ .x / Ca * 1e3, .names = "{.col}_Ca"),
          Sr_Ba = Sr / Ba,
          Sex = case_when(Sex == 1 ~ "female",
                          TRUE ~ "male"),
          Region = case_when(Region == 1 ~ "south",
                              TRUE ~ "north")) %>% 
    rename_with(tolower) %>% 
    rename(length_cm = lengthcm, weight_kg = weightkg)
}