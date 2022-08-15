library(tidyverse)
library(tidymodels)
library(discrim)

anova_res <- readRDS(here::here("data", "anova_output_data.RDS"))

sig_elements <- anova_res %>%
  mutate(
    element = stringr::str_replace(element, "/", " "),
    element = stringr::str_to_lower(element),
    element = stringr::str_replace(element, " ", "_")
  ) %>%
  dplyr::filter((term == "region") & (p.adj < 0.05)) %>%
  dplyr::pull(element)


data <- readr::read_csv(here::here("data", "data_parsed.csv")) %>%
  dplyr::mutate(dplyr::across(
    c(region, sex),
    .fns = as_factor
  ))  %>%
  dplyr::select(region, sig_elements)




set.seed(321) # Remember to always set your seed. Any integer will work

data_split <- initial_split(data, prop = 0.8, 
                             strata = region)

data_training <- data_split %>% training()

data_test <- data_split %>% testing()

dfa_recipe <- recipe(region ~ ., data = data_training) %>% 
  step_log(all_predictors(), offset = 1e-6) %>%
  step_normalize(all_predictors())

dfa_prep <- dfa_recipe %>% prep()

lda_model <- discrim_regularized(frac_common_cov = 1)

lda_wf <- workflow() %>% 
  add_model(lda_model) %>% 
  add_recipe(dfa_recipe)

last_fit_lda <- lda_wf %>% 
  last_fit(split = data_split)

last_fit_lda %>% 
  collect_metrics() %>% 
  saveRDS(here::here("data", "lda.RDS"))



