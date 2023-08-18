set_folds <- function(data, v = 4, repeats = 1000){
  set.seed(123)
  vfold_cv(data, v = v, repeats = repeats)
}

set_fda_recipe <- function(data){
  recipe(region ~ ., data = data) %>% 
    update_role(sex, new_role = "id") %>%
    step_corr()
}

set_fda_wf <- function(model, recipe){
  workflow() %>% 
    add_model(model) %>% 
    add_recipe(recipe)
}

get_fda_res <- function(wf, folds){
  wf %>%
    fit_resamples(
      resamples = folds,
      control = control_resamples(save_pred = TRUE)
    )
}


# test_func <- function(num, data, wf){
#   set.seed(num)
#   wf %>% 
#     last_fit(split = initial_split(data, 
#                               strata = region))
# }
# # num <- sample(1:1e6, 1)
# map_dfr(1:60, ~test_func(.x, anova_data, fda_workflow)) %>%
#   unnest(.metrics) %>%
#   group_by(.metric, .estimator, .config) %>%
#   summarize(mean = mean(.estimate),
#             n = n(),
#             std_err = sd(.estimate)/sqrt(n),
#             .groups = "drop") %>%
#   select(.metric, .estimator, mean, n, std_err, .config)
# # # last_fit_lda %>% 
# #   collect_metrics() 
# tic()
# fda_res %>%
#   unnest(.metrics) %>%
#   group_by(.metric, .estimator, .config) %>%
#   summarize(mean = mean(.estimate),
#             n = n(),
#             std_err = sd(.estimate)/sqrt(n),
#             .groups = "drop") %>%
#   select(.metric, .estimator, mean, n, std_err, .config)
# toc()
# tic()
# fda_res %>% collect_metrics
# toc()


# data_split <- initial_split(anova_data, prop = 0.8, 
#                              strata = region)

# data_training <- data_split %>% training()

# data_test <- data_split %>% testing()

# # boots <- bootstraps(anova_data, times = 10)

# boots <- vfold_cv(anova_data, v=5, repeats = 10)

# knn_recipe <- recipe(region ~ ., data = anova_data) %>% 
#   update_role(sex, new_role = "id") %>%
#   step_zv() %>%
#   step_log(all_predictors(), offset = 1e-6) %>%
#   step_normalize(all_predictors())

# knn_prep <- knn_recipe %>% prep()

# knn_model <- nearest_neighbor(mode = "classification")

# knn_wf <- workflow() %>% 
#   add_recipe(knn_recipe) %>%
#   add_model(knn_model)

# knn_res <- knn_wf %>%
#   fit_resamples(
#     resamples = boots,
#     control = control_resamples(save_pred = TRUE)
#   )

# knn_res %>% collect_metrics()

# print("KNN")
# last_fit_knn <- knn_wf %>% 
#   last_fit(split = data_split)

# last_fit_knn %>% 
#   collect_metrics()
#####

# data <- read_csv(here("data", "test.csv"),
#                   na = c("", "977", "777")) %>%
#   mutate(across(.cols = c(Q01:Child_age_group_2, sta:hinc), .fns = as.factor))

# data <- data %>% dplyr::select(-c(Q01:Q10,Q12:13)) %>% na.omit()

# split <- initial_split(data, strata = sta)

# train <- training(split)

# boots <- bootstraps(train, repeats=5)

# forest_recipe <- recipe(Q11 ~ ., data = train) %>% 
#   update_role(c(RecordNo), new_role = "id")
#   # step_unknown(all_nominal_predictors(), new_level = "unknown") %>%
#   # step_naomit(all_outcomes()) %>%
#   # step_naomit(all_predictors())

# forest_prep <- forest_recipe %>% prep()

# # forest_prep %>% juice() %>% dplyr::select(Q01) %>% count(Q01)

# forest_model <- rand_forest() %>%
#   set_mode("classification") %>%
#   set_engine("ranger")

# forest_wf <- workflow() %>% 
#   add_recipe(forest_recipe) %>%
#   add_model(forest_model)

# forest_res <- forest_wf %>%
#   fit_resamples(
#     resamples = boots,
#     control = control_resamples(save_pred = TRUE)
#   )

# forest_res %>% collect_metrics()

# print("RF")
# last_fit_forest <- forest_wf %>% 
#   last_fit(split = data_split)

# last_fit_forest %>% 
#   collect_metrics()



# #####

# svm_recipe <- recipe(region ~ ., data = anova_data) %>% 
#   update_role(sex, new_role = "id") %>%
#   step_zv() %>%
#   step_log(all_predictors(), offset = 1e-6) %>%
#   step_normalize(all_predictors()) %>%
#   step_corr()


# svm_model <- svm_rbf() %>%
#   set_mode("classification") 

# svm_wf <- workflow() %>% 
#   add_recipe(svm_recipe) %>%
#   add_model(svm_model)

# svm_res <- svm_wf %>%
#   fit_resamples(
#     resamples = boots,
#     control = control_resamples(save_pred = TRUE)
#   )


# print("svm")
# svm_res %>% collect_metrics()


# last_fit_svm <- svm_wf %>% 
#   last_fit(split = data_split)

# last_fit_svm %>% 
#   collect_metrics()


# anova_data %>%
#   dplyr::select(c(region, sig_reg_elems)) %>%
#   group_by(region) %>%
#   summarize(across(cols = cu_ca:sr_ca, .fns = mean)) %>%
#   # pivot_wider(names_from = "region", values_from = cu_ca:sr_ca)
#   pivot_longer(cols = cu_ca:sr_ca) %>%
#   mutate(delta_from_south = value - value[region == "south"]) %>%
#   filter(region == "north")
