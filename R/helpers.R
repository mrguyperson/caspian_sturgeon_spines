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

trans_anova %>%
    pivot_longer(cols = ba_ca:fe_ca, names_to = "ratio") %>%
    group_nest(ratio) %>%
    mutate(ks = map(data, ~ks.test(.x$value, 'pnorm')$p.value)) %>%
    unnest(ks) %>%
    filter(ks < 0.05)
anova_data <- anova_data %>%
    mutate(region_sex = factor(glue::glue("{region}_{sex}")))

kruskal.test(ba_ca ~ sex, data = anova_data)

dunnTest(ba_ca ~ region_sex, data = anova_data)
