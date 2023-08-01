# library(tidyverse)
# library(tidymodels)
# library(tidytext)
# library(vegan)
# library(here)

# path <- here("data", "spine_chem.csv")

# read_csv(path)

parse_data <- function(cleaned_data){
  cleaned_data %>%
    mutate(across(c(Ba:Fe), .fns = ~ .x / Ca * 1000, .names = "{.col}_Ca"),
          Sr_Ba = Sr / Ba,
          Sex = case_when(Sex == 1 ~ "female",
                          TRUE ~ "male"),
          Region = case_when(Region == 1 ~ "south",
                              TRUE ~ "north"))
}

select_pca_data <- function(data_parsed){
  data_parsed %>%
    select(-c(Ca:Fe), -c(lengthCm, weightKg), -Sr_Ba)
}

# pca_recipe <- recipe(~., data = pca_data) %>%
#   update_role(Code, Region, Sex, new_role = "id") %>%
#   step_normalize(all_predictors()) %>%
#   step_pca(all_predictors())

# pca_prep <- prep(pca_recipe)

# tidied_pca <- tidy(pca_prep, 2)

# tidied_pca %>%
#   filter(component %in% paste0("PC", 1:5)) %>%
#   mutate(component = fct_inorder(component)) %>%
#   ggplot(aes(value, terms, fill = terms)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~component, nrow = 1) +
#   labs(y = NULL)

# tidied_pca %>%
#   filter(component %in% paste0("PC", 1:4)) %>%
#   group_by(component) %>%
#   top_n(8, abs(value)) %>%
#   ungroup() %>%
#   mutate(terms = reorder_within(terms, abs(value), component)) %>%
#   ggplot(aes(abs(value), terms, fill = value > 0)) +
#   geom_col() +
#   facet_wrap(~component, scales = "free_y") +
#   scale_y_reordered() +
#   labs(
#     x = "Absolute value of contribution",
#     y = NULL, fill = "Positive?"
#   )


# juice(pca_prep) %>%
#   ggplot(aes(PC1, PC2, shape = Region)) +
#   geom_point(alpha = 0.7, size = 3) +
#   # geom_text(check_overlap = TRUE, hjust = "inward", family = "IBMPlexSans") +
#   # labs(color = NULL) + 
#   stat_ellipse() +
#   theme_classic() +
#   theme(
#     axis.line.x = element_line(colour = "grey50"),
#     axis.line.y = element_line(colour = "grey50")
#   )



# data_parsed %>% t_test(Sr_Ba ~ Region,
#   order = c("north", "south"),
#   alternative = "two_sided"
# )

# data_parsed %>% t_test(Sr_Ba ~ Sex,
#                     order = c("male", "female"),
#                     alternative = "two_sided"
# )

# pca_data %>%
#   pivot_longer(Ba_Ca:Fe_Ca, values_to = "ratio", names_to = "element") %>%
#   group_nest(element) %>%
#   mutate(
#     t_test_region = map(data, ~ t_test(
#       x = .,
#       formula = ratio ~ Region,
#       order = c("north", "south"),
#       alternative = "two_sided"
#     )),
#     t_test_sex = map(data, ~ t_test(
#       x = .,
#       formula = ratio ~ Sex,
#       order = c("male", "female"),
#       alternative = "two_sided"
#     ))
#   ) %>% 
#   unnest(t_test_region)

# sig_elem <- pca_data %>%
#   pivot_longer(Ba_Ca:Fe_Ca, values_to = "ratio", names_to = "element") %>%
#   group_nest(element) %>% 
#   mutate(fit = map(data, ~lm(ratio ~ Region + Sex, data = .)),
#          anova = map(fit, anova),
#          tidy = map(anova, tidy)) %>% 
#   unnest(tidy) %>% 
#   filter(p.value <= 0.05 & term == "Region") %>% 
#   select(element)

# pca_juiced <- juice(pca_prep)

# metadata <- pca_juiced %>% 
#   select(-c(PC1:PC5, Code)) %>% 
#   mutate(region_sex = paste0(Region, Sex))
# pca_res <- pca_juiced %>% 
#   select(PC1:PC5)

# pca_dist <- as.dist(as.matrix(dist(pca_res)))
# set.seed(1)
# adonis2(pca_dist ~ Region * Sex, data = metadata, method = "eu", parallel = 16, permutations = 1e5)



# anova(betadisper(pca_dist, metadata$region_sex))
# permutest(betadisper(pca_dist, metadata$region_sex), pairwise = TRUE)