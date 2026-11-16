# ----------------------------------------------
# 07_where_page.R
# Plot-ready data for the "Where it's hot" page.
# Maps and charts are drawn in index.qmd from these files.
#
# Input:  data-raw/hackney_lst_summer2026.csv  (06_lst_landsat.js)
#         data-raw/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv
# Output: data/where_lsoa.rds
# ----------------------------------------------

# ----------- 1. Surface temperature and deprivation -----------
library(tidyverse)

lst <- read_csv("data-raw/hackney_lst_summer2026.csv", show_col_types = FALSE) |>
  select(lsoa_code = LSOA21CD, lsoa_name = LSOA21NM,
         lst = mean, n_pixels = count)

imd <- read_csv("data-raw/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv", 
                show_col_types = FALSE) |>   
  select(lsoa_code  = `LSOA code (2021)`,
         imd_score  = starts_with("Index of Multiple Deprivation (IMD) Score"),
         imd_decile = starts_with("Index of Multiple Deprivation (IMD) Decile"))

where <- lst |>
  left_join(imd, by = "lsoa_code")

# Checks: 149 LSOAs, every one matched to IoD 2025
nrow(where)
sum(is.na(where$imd_score))

# Check: the smallest LSOAs still cover enough pixels
summary(where$n_pixels)

# First look at the page 3 question
cor(where$lst, where$imd_score, method = "spearman")

# ----------- 2. Tree canopy on 2021 LSOAs -----------
library(sf)

hackney_lsoa <- st_read("data-raw/gee/hackney_lsoa_2021.shp", quiet = TRUE) |>
  mutate(lsoa_area = as.numeric(st_area(geometry)))

# GLA layer: 2011 LSOA polygons with % tree canopy, but no LSOA codes
canopy_2011 <- st_read("data-raw/GLA_ClimateRiskVariables.gdb",
                       layer = "treecanopy", quiet = TRUE) |>
  select(tree_canopy = VulnMetrics_Project_TreeCanopy) |>
  st_zm() |>
  st_make_valid() |>
  st_transform(st_crs(hackney_lsoa))

# Overlay, then average canopy within each 2021 LSOA, weighted by overlap area
pieces <- st_intersection(hackney_lsoa, canopy_2011) |>
  mutate(piece_area = as.numeric(st_area(geometry)))

canopy <- pieces |>
  st_drop_geometry() |>
  group_by(lsoa_code = LSOA21CD) |>
  summarise(tree_canopy = weighted.mean(tree_canopy, piece_area),
            main_share  = max(piece_area) / first(lsoa_area),,
            covered     = sum(piece_area) / first(lsoa_area))

where <- where |>
  left_join(canopy, by = "lsoa_code")

# Checks: every 2021 LSOA fully covered by 2011 polygons
summary(where$covered)

# Check: share of each 2021 LSOA from its largest 2011 source
# (near 1 = unchanged or split; well below 1 = merge)
summary(where$main_share)
sum(where$main_share < 0.9)

# Does tree cover explain the cool tail?
cor(where$lst, where$tree_canopy, method = "spearman")

# ----------- 3. Save plot-ready data -----------
where_lsoa <- hackney_lsoa |>
  select(lsoa_code = LSOA21CD) |>
  left_join(where, by = "lsoa_code") |>
  select(lsoa_code, lsoa_name, lst, tree_canopy, imd_score, imd_decile) |>
  st_transform(4326)

# Check: 149 LSOAs, no missing values
nrow(where_lsoa)
colSums(is.na(st_drop_geometry(where_lsoa)))

write_rds(where_lsoa, "data/where_lsoa.rds")