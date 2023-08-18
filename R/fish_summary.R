# 0. load libraries -------------------------------------------------------

# library(tidyverse)

# 1. load data ------------------------------------------------------------

get_fish_data <- function(data_parsed){
  data_parsed %>%
    dplyr::mutate(dplyr::across(
      c(region, sex),
      .fns = as_factor
    ))
}

get_fish_supplement_data <- function(path){
  read_csv(path) %>%
    rename_with(tolower) %>%
    rename(sampling_location = `lat,long`,
            area = grep("area", names(.), value = TRUE)) %>%
    get_fish_data() %>%
    mutate(
      region = data.table::fifelse(
        region == "1",
        "south",
        "north"
      ),
      sex = data.table::fifelse(
        sex == "1",
        "female",
        "male"
      )
    )

}
# 2. visualize data -------------------------------------------------------

make_fish_table <- function(data_parsed){

  sampling_area <- get_fish_data(data_parsed) %>%
    dplyr::select(code, area) %>%
    # rename(area = area_col) %>%
    dplyr::mutate(sampling_area = fcase(
      grepl("mianqale", area), "southeast",
      grepl("torkaman", area), "southeast",
      grepl("river delta", area), "north",
      grepl("as above", area), "north",
      default = "southwest"
    ))

  data_parsed %>% 
    left_join(sampling_area, by = c("code","area")) %>%
    dplyr::select(length_cm, region, sampling_area, age, sex) %>% 
    dplyr::group_by(
      region, 
      sampling_area, 
      sex
      ) %>% 
    dplyr::summarize("Total length (cm)" = median(length_cm), 
              `M.A.D. length` = mad(length_cm), 
              # `Age (years)` = mean(age),
              # `S.D. age` = sd(age),
              N = dplyr::n(),
              .groups = "drop") %>% 
    dplyr::mutate(dplyr::across(
      .cols = c("Total length (cm)", 
                "M.A.D. length",
                # "Age (years)", 
                # "S.D. age"
                ),
      .fns = ~ round(.x, digits = 1)
    )) %>% 
    group_by(region) %>%
    dplyr::mutate(
      region = as.character(region),
      region = replace(region, duplicated(region), "")) %>%
    dplyr::group_by(sampling_area) %>%
    dplyr::mutate(sampling_area = replace(sampling_area, duplicated(sampling_area), "")) %>%
    dplyr::rename(
      Region = region, 
      Sex = sex, 
      `Sampling area` = sampling_area) %>%
    dplyr::ungroup()

}
# saveRDS(fish_table, here::here("figures", "fish_table.RDS"))
get_regional_summary <- function(fish_data, var){
  fish_data %>%
    as.data.table() %>%
    .[, .(median = median(get(var)), mad = mad(get(var))), by = "region"]
}

