
# # 0. libraries ------------------------------------------------------------

# library(tidyverse)
# library(tidymodels)
# library(tidytext)
# library(vegan)
# library(here)

# # 1. read in data' --------------------------------------------------------

# data <- read_csv(here("data", "data_parsed.csv"))

# anova_res <- readRDS(here::here("data", "anova_output_data.RDS"))

# 2. prep data ------------------------------------------------------------

# sig_elements <- anova_res %>%
#   mutate(
#     element = stringr::str_replace(element, "/", " "),
#     element = stringr::str_to_lower(element),
#     element = stringr::str_replace(element, " ", "_")
#   ) %>%
#   dplyr::filter((term == "region") & (p.adj < 0.05)) %>%
#   dplyr::pull(element)



get_pca_data <- function(anova_data_clean_names, sig_elements){
  # data_parsed %>%
  #   dplyr::select(code, sex, region, all_of(elements))
  anova_data_clean_names %>%
    dplyr::filter(element %in% sig_elements) %>%
    # dplyr::mutate(id = 1:nrow(.)) %>%
    tidyr::pivot_wider(names_from = "element", values_from = "value", values_fn = list) %>%
    unnest(everything())
}

make_pca_recipe <- function(pca_data){
  recipe(~., data = pca_data) %>%
    update_role(region, sex, new_role = "id") %>%
    # step_log(all_predictors(), offset = 1e-6) %>%
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors())
}

# pca_prep <- prep(pca_recipe)

# pca_juiced <- juice(pca_prep)

# 3. PCA analysis ---------------------------------------------------------

get_centroids <- function(pca_juiced){
  pca_juiced %>%
    dplyr::group_by(region) %>%
    dplyr::summarize(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
}
make_pca_figure <- function(pca_juiced, centroids, pc_vars){
  pca_juiced %>%
    ggplot(aes(
      x = PC1,
      y = PC2,
      color = region,
      fill = region,
      shape = region
    )) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.2) +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.2) +
    stat_ellipse(
      geom = "polygon",
      alpha = 0,
      show.legend = FALSE
    ) +
    geom_point(alpha = 1, size = 3, show.legend = FALSE) +
    geom_point(
      data = centroids,
      aes(x = PC1, y = PC2, fill = region),
      size = 5,
      shape = 22,
      color = "black",
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.5) +
    scale_color_manual(
      name = NULL,
      breaks = c("north", "south"),
      values = c("dodgerblue", "#FC8A82"),
      labels = c("North", "South")
    ) +
    scale_fill_manual(
      name = NULL,
      breaks = c("north", "south"),
      values = c("dodgerblue", "#FC8A82"),
      labels = c("North", "South")
    ) +
    theme_classic(base_size = 25) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = element_text(family = "Arial"),
      axis.text = element_text(color = "black")
    ) +
    labs(
      x = glue::glue("PC1 ({pc_vars[[1]]}%)"),
      y = glue::glue("PC2 ({pc_vars[[2]]}%)")
    )
}
# ggsave(here::here("figures", "PCA.tiff"), dpi = 1200, height = 129, width = 193.5, units = "mm")
# ggsave(here::here("figures", "PCA.jpg"), dpi = 600, height = 129, width = 193.5, units = "mm")

# # save for use in the manuscript.Rmd
# saveRDS(pca_prep, here::here("data", "pca_prep.RDS"))

# 4. PERMANOVA ------------------------------------------------------------

# metadata <- pca_juiced %>%
#   dplyr::select(-c(PC1:PC5, code)) %>%
#   mutate(region_sex = paste0(region, sex))
# pca_res <- pca_juiced %>%
#   dplyr::select(PC1:PC5)

# pca_dist <- as.dist(as.matrix(dist(pca_res)))
# set.seed(1)
# adon_tab <- adonis2(pca_dist ~ region * sex, data = pca_juiced, method = "eu", permutations = 1e5)

# saveRDS(adon_tab, here::here("figures", "adonis.RDS"))


# #
# #
# # anova(betadisper(pca_dist, metadata$region))
# # permutest(betadisper(pca_dist, metadata$region), pairwise = TRUE)
# # TukeyHSD(betadisper(pca_dist, metadata))
# #
# # library(pairwiseAdonis)
# # pairwise.adonis2(pca_dist ~ region * sex, data = pca_juiced, method = "eu")

# tidied_pca <- tidy(pca_prep, 3)
# tidied_pca %>%
#   filter(component %in% paste0("PC", 1:5)) %>%
#   mutate(component = fct_inorder(component)) %>%
#   ggplot(aes(value, terms, fill = terms)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~component, nrow = 1) +
#   labs(y = NULL)

# library(tidytext)
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
# sdev <- pca_prep$steps[[3]]$res$sdev

# percent_variation <- sdev^2 / sum(sdev^2)
# var_df <- data.frame(
#   PC = paste0("PC", 1:length(sdev)),
#   var_explained = percent_variation,
#   stringsAsFactors = FALSE
# )
# var_df %>%
#   mutate(PC = fct_inorder(PC)) %>%
#   ggplot(aes(x = PC, y = var_explained)) +
#   geom_col()
make_components_contr_fig <- function(pca_prep, sig_elements){
  tidy(pca_prep, 2) %>%
      filter(component %in% paste0("PC", 1:4)) %>%
    group_by(component) %>%
    top_n(length(sig_elements), abs(value)) %>%
    ungroup() %>%
    mutate(terms = reorder_within(terms, abs(value), component),
          label = factor(ifelse(value > 0, "positive", "negative"), levels = c("positive", "negative"))) %>%
    ggplot() +
    geom_col_pattern(
      aes(abs(value), 
      terms, 
      fill = label,
      pattern = label
      ),
          color = "black",
          size = 1,
          show.legend = FALSE) +
    scale_fill_manual(values = c("white", "#FFFFFF")) +
    facet_wrap(~component, scales = "free_y") +
    scale_y_reordered() +
    labs(
      x = "Absolute value of contribution",
      y = NULL, 
      # fill = "Positive?"
    ) +
    theme_classic(base_size = 25)
}

make_pca_contr_df <- function(pca_prep){

  sdev <- pca_prep$steps[[2]]$res$sdev

  percent_variation <- sdev^2 / sum(sdev^2)

  var_df <- data.frame(PC=paste0("PC",1:length(sdev)),
                     var_explained=percent_variation,
                     stringsAsFactors = FALSE)


}

get_pc_variation <- function(pca_prep){
  df <- make_pca_contr_df(pca_prep)
  pcs <- c("PC1", "PC2", "PC3", "PC4")
  pc_vars <- df %>%
    dplyr::filter(PC %in% pcs) %>%
    dplyr::pull(var_explained) %>%
    signif(3)
  pc_vars * 100
}

make_full_pca_fig <- function(pca_figure, components_contr_fig){
  fig <- ggarrange(pca_figure, components_contr_fig, 
              labels = c("A)", "B)"),
              font.label = list(size = 25, family = "Arial"),
              nrow = 1,
              ncol=2)
  filename <- here::here("submission", "pca.png")
  ggsave(plot = fig, filename = filename,
          units = "in",
          width = 20,
          height = 10)
  filename
}

get_adonis_table <- function(pca_distance, pca_juiced){

  tab <- adonis2(pca_distance ~ region * sex, 
                  data = pca_juiced, 
                  method = "eu", 
                  permutations = 1e5)
  colnames(tab) <- c("df", "SS", "R^2^", "F statistic", "p-value")
  rownames(tab) <- c("region", "sex", "region * sex", "residual", "total")
  tab
}
