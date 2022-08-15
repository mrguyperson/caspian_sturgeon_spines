
# 0. load libraries -------------------------------------------------------

library(tidyverse)
library(here)
#library(ggsignif)
library(ggpubr)
library(rstatix)

# 1. load data ------------------------------------------------------------

data <- readr::read_csv(here::here("data", "data_parsed.csv")) %>%
  dplyr::mutate(dplyr::across(
    c(region, sex),
    .fns = as_factor
  ))

# 2. select data ----------------------------------------------------------

anova_data <- data %>%
  dplyr::select(region, sex, ba_ca:fe_ca)

# log transform to improve normality
log_trans <- anova_data %>%
  dplyr::mutate(across(ba_ca:fe_ca, ~ log(.x + 1e-10)))

# 3. functions ------------------------------------------------------------

get_anova_tukey_table <- function(element, df) {
  element <- rlang::as_name(enquo(element))
  formula <- formula(paste(element, "~", "region*sex"))
  aov(formula, data = df) %>%
    TukeyHSD() %>%
    broom::tidy() %>%
    dplyr::mutate(element = element)
}

# 4. analysis -------------------------------------------------------------

elements <- log_trans %>%
  dplyr::select(-sex, -region) %>%
  names()

anova_output <- purrr::map_dfr(elements, get_anova_tukey_table, df = log_trans)

# 5. make a table --------------------------------------------------------

# make a nice-looking table of all the two-way ANOVA results with Tukey's test adjusted pvalues
anova_table <- anova_output %>%
  dplyr::mutate(sort_col = case_when(
    term == "region" ~ 1,
    term == "sex" ~ 2,
    TRUE ~ 3
  )) %>%
  dplyr::arrange(sort_col, contrast, element) %>%
  dplyr::select(-c(null.value:conf.high, sort_col)) %>%
  select(term, contrast, element, adj.p.value) %>%
  # dplyr::mutate(
  #   adj.p.value = round(adj.p.value, digits = 3),
  #   adj.p.value = dplyr::if_else(
  #     adj.p.value == 0,
  #     "< 0.001",
  #     as.character(adj.p.value)
  #   )
  # ) %>%
  dplyr::mutate(adj.p.value = signif(adj.p.value, 2)) %>%
  # change the text of the columns to look nice
  dplyr::mutate(
    element = stringr::str_replace(element, "_", " "),
    element = stringr::str_to_title(element),
    element = stringr::str_replace(element, " ", "/"),
    contrast = stringr::str_replace(contrast, "-", " vs. "),
    term = stringr::str_replace(term, ":", " * "),
    contrast = stringr::str_replace_all(contrast, ":", ": ")
  ) %>%
  dplyr::rename(
    Variable = term,
    Comparison = contrast,
    "Elemental ratio" = element,
    "Adjusted p-value" = adj.p.value
  )

saveRDS(anova_table, here::here("figures", "anova_table.RDS"))

# 6. make a bar chart -----------------------------------------------------

# update the element ratio names in the log transformed data
log_trans_cleaned <- log_trans %>%
  tidyr::pivot_longer(ba_ca:fe_ca, names_to = "element") %>%
  dplyr::mutate(
    element = stringr::str_replace(element, "_", " "),
    element = stringr::str_to_title(element),
    element = stringr::str_replace(element, " ", "/")
  )

# do the same to the untransformed data; we will use this one for the bar chart
anova_data_cleaned <- anova_data %>%
  tidyr::pivot_longer(ba_ca:fe_ca, names_to = "element") %>%
  dplyr::mutate(
    element = stringr::str_replace(element, "_", " "),
    element = stringr::str_to_title(element),
    element = stringr::str_replace(element, " ", "/")
  )

# adding p-values to the figure is a pain because of the free y axis
# can use rstatix package to calculate where the coordinates should be given a free y
# the package adds a lot of extra info to the data frame, so calculating the vale ~ region * sex model
# and then adding coordinates to that is not possible. it needs to be one variable

# get the base dataframe using the simple anova model with untransformed values
stat.test <- anova_data_cleaned %>%
  dplyr::group_by(element) %>%
  rstatix::tukey_hsd(value ~ region)

# get the dataframe for the full model using log transformed data
stat.test.full.log <- log_trans_cleaned %>%
  dplyr::group_by(element) %>%
  rstatix::tukey_hsd(value ~ region * sex)

# trim the full model df down
stat.test.region.log <- stat.test.full.log %>%
  dplyr::filter(term == "region")

# replace the p.adj values from the simple one with those of the full model
stat.test$p.adj <- stat.test.region.log$p.adj
stat.test$p.adj.signif <- stat.test.region.log$p.adj.signif

# add the xy coordinates to be graphed
stat.test.xy <- stat.test %>%
  rstatix::add_xy_position(x = "region", fun = "mean_sd", scales = "free") %>%
  # even with new p values, it still thinks some values aren't significant
  # simple hack to get everything to plot is to just filter the data by p values
  dplyr::filter(p.adj < 0.05) %>%
  dplyr::mutate(p.adj = 0.05)

# make the plot using ggpubr rather than base ggplot2
ggpubr::ggbarplot(anova_data_cleaned, x = "region", y = "value", add = "mean_sd", facet.by = "element") +
  ggplot2::facet_wrap(~element, scale = "free_y", ncol = 2) +
  ggpubr::stat_pvalue_manual(stat.test.xy,
    hide.ns = FALSE, # if you hide this, then some values won't show up because they aren't significant in the simple model
    tip.length = 0,
    linetype = 0,
    label = "* p < {p.adj}",
    size = 2.5
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.0, 0.25))) +
  ggplot2::scale_fill_grey() + ggplot2::theme_bw() +
  ggplot2::ylab("element-to-Ca ratio x 1,000") +
  ggplot2::xlab(NULL) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    text = element_text(family = "Arial"),
    axis.text = element_text(color = "black")
  )

ggplot2::ggsave(here::here("figures", "bar_chart.tiff"), dpi = 1200, width = 129, height = 193.5, units = "mm")
ggplot2::ggsave(here::here("figures", "bar_chart.jpg"), dpi = 600, width = 129, height = 193.5, units = "mm")

# saving this for calculations in the manuscript.Rmd file
saveRDS(stat.test.full.log, here::here("data", "anova_output_data.RDS"))
