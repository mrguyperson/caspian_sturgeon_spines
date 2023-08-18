extract_all_files <- function(zip_files, destination){
    walk(zip_files, ~ unzip(zipfile = .x, overwrite= TRUE, exdir = destination))
    # list.files(destination)
}

extract_single_file <- function(zip_file, desired_files, destination){
    unzip(zip_file, files = desired_files, overwrite= TRUE, exdir = destination)
}

grab_shp_text <- function(target){
    target[stringr::str_detect(target, pattern = ".shp")]
}

raster_to_df <- function(raster){
    as.data.frame(raster, xy=TRUE) %>% 
        rename(bathymetry = gebco_2023_n90.0_s0.0_w0.0_e90.0) %>%
        filter(bathymetry <=0) %>% # get rid of land values
        mutate(bathymetry = abs(bathymetry))
}



get_sig_elements <- function(srh){
    srh %>% 
        filter(term == "region", p.value < 0.05) %>%
        pull(element) %>%
        sort()
}

region_diff_in_sig_elem <- function(anova_data_clean_names, sig_elements){
    # anova_data %>%
    #     dplyr::select(c(region, sig_elements)) %>%
    #     dplyr::group_by(region) %>%
    #     dplyr::summarize(dplyr::across(cols = cu_ca:sr_ca, .fns = mean)) %>%
    #     # pivot_wider(names_from = "region", values_from = cu_ca:sr_ca)
    #     tidyr::pivot_longer(cols = cu_ca:sr_ca, names_to = "element") %>%
    #     dplyr::mutate(delta_from_south = value - value[region == "south"]) %>%
    #     dplyr::filter(region == "north")
    anova_data_clean_names %>% 
        dplyr::filter(element %in% sig_elements) %>%
        dplyr::group_by(element, region) %>%
        dplyr::summarize(value = mean(value)) %>%
        dplyr::mutate(delta_from_south = value - value[region == "south"]) %>%
        dplyr::filter(region == "north") %>%
        ungroup()
}



sentence_format_for_list <- function(list){
    if (length(list) == 2) {
        paste(list, collapse = " and ")
    } else if (length(list) > 3) {
        longest_part <- list[1:length(list) - 1]
        last_part <- list[length(list)]
        longest_formatted <- paste(longest_part, collapse = ", ")
        glue::glue("{longest_formatted}, and {last_part}")
    } else if (length(list) == 1) {
        list
    } else {
        "EMPTY LIST! Please check!"
    }
}

get_raw_elem_names <- function(raw_data){
    raw_data %>% 
        dplyr::select(Ca:Fe) %>% 
        names() %>% 
        sort()
}

get_fda_metric <- function(fda_metrics, metric){
    fda_metrics %>% 
        dplyr::filter(.metric == metric) %>% 
        dplyr::pull(mean)

}

get_mean_elem_comparison <- function(diff_in_sig_elem, comparison){
    diff_in_sig_elem %>% 
        dplyr::filter({{comparison}} > 0) %>% 
        dplyr::pull(element)

}

select_fish <- function(r, s, fish_data){
  fish_data %>% 
    dplyr::filter(grepl(r, region), sex == s) %>%
    nrow()
} 

fix_element_names <- function(data){
    data %>%
        mutate(
            element = stringr::str_replace(element, "_", " "),
            element = stringr::str_to_title(element),
            element = stringr::str_replace(element, " ", "/")
      )

}

get_rel_path <- function(path){
    # glue::glue("../{file.path(basename(dirname(path)), basename(path))}")
    stringr::str_remove(path, glue::glue("{dirname(getwd())}/"))
}

in_text_pval <- function(pval){

    data.table::fifelse(
        pval >= 0.05,
        "p > 0.05",
        "p < 0.05"
    )
}

remove_outliers <- function(data, variable){
    raw <- data %>%
        dplyr::pull({{variable}})

    iqr <- IQR(raw)
    quants <- quantile(raw, probs=c(.25, .75), na.rm = FALSE)

    lower <- quants[[1]] - 1.5 * iqr
    upper <- quants[[2]] + 1.5 * iqr

    data %>%
        dplyr::filter({{variable}} >= lower, {{variable}} <= upper)
}

plus_minus <- function(avg, var){
    glue::glue("{avg} ± {var}")
}
