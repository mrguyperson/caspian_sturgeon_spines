# Load packages required to define the pipeline:
library(targets)
library(here)
# library(ggpubr)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "tictoc",
    "viridis",
    "patchwork",
    "microbenchmark",
    "ggmap",
    "ggpubr",
    "tidymodels",
    "tidytext",
    "vegan",
    "discrim",
    "rnaturalearth",
    "cowplot",
    "ggspatial",
    "dendextend",
    "ggdendro",
    "cluster",
    "factoextra",
    "ggsignif",
    "rstatix",
    "Hmisc",
    "sf"
    ),
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
source(here("R", "data_cleaning.R"))
source(here("R", "data_analysis.R"))
source(here("R", "anova.R"))
source(here("R", "fish_summary.R"))
source(here("R", "pca.R"))
source(here("R", "helpers.R"))
source(here("R", "map.R"))
# source("other_functions.R") # Source other scripts as needed. # nolint

zip_file_names <- c(
      "downloads/ne_10m_rivers_lake_centerlines.zip",
      "downloads/ne_10m_rivers_europe.zip",
      "downloads/ne_10m_lakes.zip",
      "downloads/ne_10m_lakes_europe.zip",
      "downloads/gebco_2023_geotiff.zip"
    )

# Replace the target list below with your own:
list(
  ##### data prep
  tar_target(
    name = raw_data_path,
    command = here("data", "spine_chem.csv"),
    format = "file"
  ),
  tar_target(
    name = data_parsed,
    command = clean_raw_data(raw_data_path)
  ),
  # tar_target(
  #   name = data_parsed,
  #   command = parse_data(cleaned_data)
  # ),
  # tar_target(
  #   name = pca_data,
  #   command = select_pca_data(data_parsed)
  # ),
  ##### ANOVA
  tar_target(
    name = anova_data,
    command = make_anova_data(data_parsed)
  ),
  tar_target(
    name = trans_anova,
    command = log_trans_anova_data(anova_data)
  ),
  tar_target(
    name = elements,
    command = select_elements(anova_data)
  ),
  tar_target(
    name = anova_output,
    command = get_anova_output(elements, trans_anova)
  ),
  # anova figs and tables
  tar_target(
    name = anova_table,
    command = get_anova_table(anova_output)
  ),
  tar_target(
    name = trans_anova_clean_names,
    command = clean_anova_table_names(trans_anova)
  ),
  tar_target(
    name = anova_data_clean_names,
    command = clean_anova_table_names(anova_data)
  ),
  tar_target(
    name = tukey,
    command = do_tukey_test(anova_data_clean_names,
                            formula = "value ~ region")
  ),
  tar_target(
    name = tukey_full,
    command = do_tukey_test(anova_data_clean_names,
                            formula = "value ~ region * sex")
  ),
  tar_target(
    name = anova_bar_chart,
    command = make_anova_bar_chart(
      tukey,
      tukey_full,
      anova_data_clean_names
    ),
    packages = c("ggpubr")
  ),
  ##### Fish summary
  tar_target(
    name = fish_data,
    command = get_fish_data(data_parsed)
  ),
  tar_target(
    name = fish_table,
    command = male_fish_table(fish_data)
  ),
  ##### PCA
  tar_target(
    name = pca_data,
    command = get_pca_data(data_parsed, elements)
  ),
  tar_target(
    name = pca_recipe,
    command = make_pca_recipe(pca_data)
  ),
  tar_target(
    name = pca_prep,
    command = prep(pca_recipe)
  ),
  tar_target(
    name = pca_juiced,
    command = juice(pca_prep)
  ),
  tar_target(
    name = centroids,
    command = get_centroids(pca_juiced)
  ),
  tar_target(
    name = pca_figure,
    command = make_pca_figure(pca_juiced, centroids)
  ),
  ##### PERMANOVA
  tar_target(
    name = princ_components,
    command = dplyr::select(pca_juiced, PC1:PC5)
  ),
  tar_target(
    name = pca_distance,
    command = as.dist(as.matrix(dist(princ_components)))
  ),
  tar_target(
    name = adonis_table,
    command = adonis2(pca_distance ~ region * sex, data = pca_juiced, method = "eu", permutations = 1e5)
  ),
  ##### Map
  tar_download(
    name = downloaded_zips,
    urls = c(
      "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_lake_centerlines.zip",
      "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_europe.zip",
      "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip",
      "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes_europe.zip",
      "https://www.bodc.ac.uk/data/open_download/gebco/gebco_2023/geotiff/"
    ),
    paths = c(
      "downloads/ne_10m_rivers_lake_centerlines.zip",
      "downloads/ne_10m_rivers_europe.zip",
      "downloads/ne_10m_lakes.zip",
      "downloads/ne_10m_lakes_europe.zip",
      "downloads/gebco_2023_geotiff.zip"
    ),
    method = "curl",
    extra = "-Lo",
    cue = tar_cue(file = FALSE)
  ),
  tar_target(
    geotiff,
    extract_single_file(downloaded_zips[stringr::str_detect(downloaded_zips, pattern = "geotiff")],
                        desired_files = "gebco_2023_n90.0_s0.0_w0.0_e90.0.tif",
                        destination = here("data/map_data")),
    format = "file"
  ),
  tar_target(
    name = rivers_shp,
    extract_single_file(downloaded_zips[stringr::str_detect(downloaded_zips, pattern = "rivers_lake")],
                        desired_files = NULL,
                        destination = here("data/map_data")),
    format = "file"
  ),
  tar_target(
    name = rivers_europe_shp,
    extract_single_file(downloaded_zips[stringr::str_detect(downloaded_zips, pattern = "rivers_europe")],
                        desired_files = NULL,
                        destination = here("data/map_data")),
    format = "file"
  ),
  tar_target(
    name = lakes_shp,
    extract_single_file(downloaded_zips[stringr::str_detect(downloaded_zips, pattern = "lakes.zip")],
                        desired_files = NULL,
                        destination = here("data/map_data")),
    format = "file"
  ),
  tar_target(
    name = lakes_europe_shp,
    extract_single_file(downloaded_zips[stringr::str_detect(downloaded_zips, pattern = "lakes_europe")],
                        desired_files = NULL,
                        destination = here("data/map_data")),
    format = "file"
  ),
  tar_target(
    ne_world,
    ne_countries(scale = 10, returnclass = "sf")
  ),
  tar_target(
    iran_clipped1,
    clip_ne_world(ne_world,
                 "Iran",
                  xmin = 48,
                  ymin = 37,
                  xmax = 50.5,
                  ymax = 38)
  ),
  tar_target(
    iran_clipped2,
    clip_ne_world(ne_world,
                 "Iran",
                  xmin = 53.5,
                  ymin = 36.8,
                  xmax = 54,
                  ymax = 37.25)
  ),
  tar_target(
    russia_clipped,
    clip_ne_world(ne_world,
                  "Russia",
                  xmin = 48,
                  ymin = 45.5,
                  xmax = 49.5,
                  ymax = 46.5)
  ),
  tar_target(
    name = rivers,
    command = sf::read_sf(grab_shp_text(rivers_shp))
  ),
  tar_target(
    name = rivers_europe,
    command = sf::read_sf(grab_shp_text(rivers_europe_shp))
  ),
  tar_target(
    name = lakes,
    command = sf::read_sf(grab_shp_text(lakes_shp))
  ),
  tar_target(
    name = lakes_europe,
    command = sf::read_sf(grab_shp_text(lakes_europe_shp))
  ),
  tar_target(
    name = depth_raster,
    command = crop(raster(geotiff), extent(46,56,36,48)),
    packages = "raster"
    ),
  tar_target(
    name = depth,
    command = raster_to_df(depth_raster)
  ),
  tar_target(
    name = map,
    make_map(ne_world, 
                      depth, 
                      iran_clipped1, 
                      iran_clipped2, 
                      russia_clipped,
                      rivers,
                      rivers_europe,
                      lakes, 
                      lakes_europe)
  ),
  format = "file"
)
