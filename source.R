library(dplyr)
library(ggplot2)
library(sf)
library(forcats)
library(dbscan)
library(purrr)
library(plotly)
library(lubridate)
library(tidyr)
library(munsell)

# Import data -------------------------------------------------------------
# Live, free-range birds
df_fa <- read.csv(
  file = "data/S-BSST1522_SV_FA/Surveillance_Arbovirus_LiveBirds_Netherlands_2016-2022.csv",
  header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = FALSE
) %>%
  dplyr::mutate(capture.date = as.Date(capture.date))
# 
# # Dead, free-range birds
# df_fd <- read.csv(
#   file = "data/S-BSST1523_SV_FD/Surveillance_Arbovirus_DeadFreeRangingBirds_Netherlands_2016-2022.csv",
#   header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = FALSE
# ) %>%
#   dplyr::mutate(DeathOrSampleDate = as.Date(DeathOrSampleDate, "%d-%m-%Y"))
# 
# # Dead, captive birds
# df_cd <- read.csv(
#   file = "data/S-BSST1505_SV_DC/Surveillance_Arbovirus_DeadCaptiveBirds_Netherlands_2016-2022.csv",
#   header = TRUE, sep = ",", fill = TRUE, stringsAsFactors = FALSE
# ) %>%
#   dplyr::mutate(DeathOrSampleDate = as.Date(DeathOrSampleDate, "%d-%m-%Y"))

# Combine dead free range and captive birds
# fd_subset <- df_fd %>%
#   select(DeathOrSampleDate, Long, Lat, BirdUSUV) %>%
#   mutate(Wild_Cap = "Wild")
# 
# cd_subset <- df_cd %>%
#   select(DeathOrSampleDate, Long, Lat, Wild_Cap, BirdUSUV)
# 
# all_dead_birds <- rbind(fd_subset, cd_subset)

# Map of the Netherlands --------------------------------------------------
nl_map <- sf::st_read(file.path("data/map/gadm41_NLD_1.shp")) %>%
  dplyr::filter(TYPE_1 != 'Water body')

# Base layer for maps
base_map <- ggplot() +
  geom_sf(data = nl_map, fill = "ivory", color = "gray30", linewidth = 0.3) +
  coord_sf(datum = st_crs(nl_map), expand = FALSE) +
  theme_void() + # A cleaner starting point than building from theme()
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# Clustering function -----------------------------------------------------
#' Function to perform dbscan clustering on spatial points
#'
#' @param data A dataframe with 'Long' and 'Lat' columns.
#' @param eps The distance threshold for dbscan (in decimal degrees).
#' @param minPts The minimum number of points to form a cluster.
#' @return A summarized dataframe with representative lat/long and cluster size.
cluster_and_summarize <- function(data, eps = 0.15, minPts = 1) {
  # Ensure there's data to process
  if (nrow(data) == 0) return(NULL)

  # Discard rows with missing coordinates
  data <- data %>%
    dplyr::filter(!is.na(Long) | !is.na(Lat))

  # Select coordinate columns
  coords <- data %>% select(Long, Lat)

  # Perform dbscan clustering
  clusters <- dbscan(coords, eps = eps, minPts = minPts)

  # Add cluster assignments to the data
  data$cluster_id <- clusters$cluster

  # Summarize each cluster
  summary_df <- data %>%
    group_by(cluster_id) %>%
    summarise(
      # Calculate the centroid (mean location) for the representative point
      Long_rep = mean(Long),
      Lat_rep = mean(Lat),
      # Count the number of birds in the cluster
      cluster_size = n(),
      .groups = 'drop'
    )

  return(summary_df)
}

cluster_by_group <- function(data, group_col, eps) {
  data %>%
    group_by({{ group_col }}) %>%
    nest() %>% # Creates a list-column with data for each group
    mutate(clustered_data = map(data, ~cluster_and_summarize(.x, eps))) %>%
    select(-data) %>%
    unnest(clustered_data) %>%
    ungroup()
}

# Free-ranging live bird data
live_free_range <- cluster_and_summarize(df_fa, eps = 0.04)

# All dead bird data
# dead_birds_clustered <- cluster_and_summarize(all_dead_birds, eps = 0.04)

# Free-ranging live bird positives
fa_pos_clustered_df <- df_fa %>%
  dplyr::filter(BirdUSUV == 1 | BirdWNV == 1) %>%
  mutate(status = case_when(
    BirdUSUV == 1  ~ "USUV",
    BirdWNV == 1  ~ "WNV"
  )) %>%
  cluster_by_group(eps = 0.04, group_col = status)

# Dead bird positives
# dead_pos_clustered <- all_dead_birds %>%
#   dplyr::filter(BirdUSUV == 1) %>%
#   mutate(status = case_when(
#     BirdUSUV == 1  ~ "USUV"
#   )) %>%
#   cluster_by_group(eps = 0.04, group_col = status)

# Live birds: Monthly total sampling
monthly_samples <- df_fa %>%
  mutate(month_start = floor_date(capture.date, "month")) %>%
  count(month_start, class, name = "sample_count") %>%
  mutate(Month = strftime(month_start, "%Y-%m"))

# # Live birds: Monthly positive sampling
monthly_positives <- df_fa %>%
  filter(BirdUSUV == 1 | BirdWNV == 1) %>%
  pivot_longer(
    cols = c(BirdUSUV, BirdWNV),
    names_to = "Virus",
    values_to = "Status"
  ) %>%
  filter(Status == 1) %>%
  mutate(Virus = sub("Bird", "", Virus)) %>%
  mutate(month_start = floor_date(capture.date, "month")) %>%
  count(month_start, Virus, name = "sample_count") %>%
  mutate(Month = strftime(month_start, "%Y-%m"))

# # Dead birds: Monthly total sampling
# monthly_dead <- all_dead_birds %>%
#   mutate(month_start = floor_date(DeathOrSampleDate, "month")) %>%
#   count(month_start, Wild_Cap, name = "sample_count") %>%
#   mutate(Month = strftime(month_start, "%Y-%m"))

# Dead birds: Monthly positive sampling
# monthly_dead_positives <- all_dead_birds %>%
#   filter(BirdUSUV == 1) %>%
#   pivot_longer(
#     cols = BirdUSUV, 
#     names_to = "Virus", 
#     values_to = "Status"
#   ) %>%
#   mutate(
#     Virus = sub("Bird", "", Virus),
#     month_start = floor_date(DeathOrSampleDate, "month")
#   ) %>%
#   count(month_start, Virus, name = "sample_count") %>%
#   mutate(Month = strftime(month_start, "%Y-%m"))