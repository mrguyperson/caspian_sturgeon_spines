mw_u_test <- function(fish_data, variable){

    variable <- rlang::as_name(enquo(variable))
    model <- formula(glue::glue("{variable} ~ region"))
    fish_data %>%
        # filter(sex == "male") %>%
        dplyr::select(region, all_of(c(
            # sex,
            # region, 
            variable
        ))) %>%
        # group_nest(sex) %>%
        wilcox_test(formula = model)
}

shapiro_test_elements <- function(data) {

    data %>%
        pivot_longer(cols = ba_ca:fe_ca, names_to = "ratio") %>%
        group_nest(ratio) %>%
        mutate(
            model = map(data, ~aov(value ~ sex * region, data = .x)),
            rstd = map(model, rstandard),
            shapiro = map(rstd, ~shapiro.test(.x)$p.value)
            ) %>%
        unnest(shapiro) %>%
        filter(shapiro < 0.05) %>%
        dplyr::select(-data)
}




get_SRH_table <- function(element, df) {
    scheirerRayHare(formula(glue::glue("{element}~region+sex+region:sex")), data = df, verbose = FALSE) %>%
        broom::tidy() %>%
        dplyr::mutate(element = element) %>%
        dplyr::filter(term != "Residuals") %>%
        dplyr::select(element, term, H, p.value)
}

get_SRH_output <- function(elements, data){
    # combos <- expand_grid(elements = element_list, factors = factor_list)
    purrr::map_dfr(elements, get_SRH_table, df = data)
        # rename(Comparison = factor) %>%
        # mutate(Comparison = fifelse(
        #     Comparison == "sex", "female vs. male", "north vs. south"
        # ))
}

get_signif_nonparam_elements <- function(srh_output){
    srh_output %>%
        dplyr::filter(p.value < 0.05, term == "region:sex") %>%
        dplyr::pull(element)
}

dunn_test_for_elements <- function(element, df){
    model <- formula(glue::glue("{element} ~ region_sex"))
    rstatix::dunn_test(model, data = df)
}

do_all_dunn_tests <- function(sig_elements, df){
    df_w_interaction <- df %>%
        dplyr::mutate(region_sex = glue::glue("{region}_{sex}"))
    map_dfr(sig_elements, ~dunn_test_for_elements(.x, df_w_interaction)) %>%
        dplyr::select(-c(p.adj.signif, n1, n2, p)) %>%
        rename(element = .y.)
}

format_srh_table <- function(srh_fancy_names){

    srh_fancy_names %>%
        mutate(
            term = fcase(
                term == "region", "north vs. south",
                term == "sex", "male vs. female",
                default = "region * sex"
            ),
            H = signif(H, 2),
            p.value = signif(p.value, 2),
            p.value = fcase(
                p.value >= 0.05, as.character(p.value),
                p.value < 0.01, "< 0.01*",
                p.value < 0.05, as.character(glue::glue("{p.value}*"))
            )
        ) %>%
        rename(
            Element = element,
            Comparison = term,
            `H statistic` = H,
            `p-value` = p.value
        ) %>%
        group_by(Element) %>%
        mutate(
            Element = replace(Element, duplicated(Element), "")
        )
}


make_srh_bar_chart <- function(anova_data_clean_names, srh_fancy_names){
    sig_stats <- anova_data_clean_names %>%
        group_by(element) %>%
        dunn_test(value ~ region) %>%
        rstatix::add_xy_position(x = "region", fun = "median_mad", scales = "free")
    
    srh_sig <- srh_fancy_names %>%
        dplyr::filter(term == "region") %>%
        arrange(element) %>%
        add_significance()
    
    sig_stats$p <- srh_sig$p.value
    sig_stats$p.adj.signif <- srh_sig$p.value.signif

    sig_stats <- sig_stats %>%
        mutate(p = fifelse(
            p < 0.05, 
            0.05, 
            p
        ))
    
    plot <- ggbarplot(anova_data_clean_names, x = "region", y = "value", add = "median_mad", facet.by = "element") +
    ggplot2::facet_wrap(~element, scale = "free_y", ncol = 2) +
    ggpubr::stat_pvalue_manual(sig_stats,
      hide.ns = TRUE, # if you hide this, then some values won't show up because they aren't significant in the simple model
      tip.length = 0,
      linetype = 0,
      label = "* p < {p}",
      size = 6
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.0, 0.25))) +
    ggplot2::scale_fill_grey()+
    ggplot2::ylab("element-to-Ca ratio x 1,000") +
    ggplot2::xlab(NULL) +
    theme_bw(base_size = 25) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = element_text(family = "Arial"),
      axis.text = element_text(color = "black")
    )

    filename <- here::here("submission", "bar_chart.png")
    ggsave(filename = filename, plot = plot,
        units = "in",
        width = 10,
        height = 20)
    filename

}


