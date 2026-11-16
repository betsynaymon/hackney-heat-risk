# ----------------------------------------------
# 07_where_page.R
# Plot-ready data for the "Where it's hot" page.
# Maps and charts are drawn in index.qmd from these files.
#
# Input:  data-raw/hackney_lst_summer2026.csv  (06_lst_landsat.js)
#         data-raw/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv
#         data-raw/MSOA-Names-2.3.csv
#         data-raw/haduk/*.nc
# Output: data/where_lsoa.rds
#         data/hot_days.rds
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
            main_share  = max(piece_area) / first(lsoa_area),
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
# House of Commons Library MSOA names, version 2.3 (2021 MSOAs)
msoa_names <- read_csv("data-raw/MSOA-Names-2.3.csv", show_col_types = FALSE) |>
  select(msoa_name = msoa21nm, msoa_hcl_name = msoa21hclnm)

where_lsoa <- hackney_lsoa |>
  select(lsoa_code = LSOA21CD) |>
  left_join(where, by = "lsoa_code") |>
  mutate(msoa_name = str_sub(lsoa_name, 1, -2)) |>    # "Hackney 002A" -> "Hackney 002"
  left_join(msoa_names, by = "msoa_name") |>
  select(lsoa_code, lsoa_name, msoa_hcl_name, lst, tree_canopy,
         imd_score, imd_decile) |>
  st_transform(4326)

# Check: 149 LSOAs, no missing values
nrow(where_lsoa)
colSums(is.na(st_drop_geometry(where_lsoa)))

write_rds(where_lsoa, "data/where_lsoa.rds")

# ----------- 4. Hot days -----------
# HadUK-Grid v1.3.2, 1km annual summer days (max temperature above 25°C)
files <- list.files("data-raw/haduk",
                    pattern = "^summerdays_hadukgrid_uk_1km_ann_.*\\.nc$",
                    full.names = TRUE)

summer_days <- terra::rast(files)

# Already British National Grid, but the files don't name the datum,
# so set the CRS rather than reproject
terra::crs(summer_days) <- "EPSG:27700"

# Checks: 95 files, one layer each
length(files)
terra::nlyr(summer_days)

# Hackney as one outline, from the 2021 LSOAs read in section 2
st_crs(hackney_lsoa)$input   # expect British National Grid
hackney_v <- terra::vect(st_union(hackney_lsoa))

# Check: cells Hackney touches, and the share of each it covers
cells <- terra::extract(summer_days[[1]], hackney_v, exact = TRUE)
nrow(cells)
sum(cells$fraction)   # Hackney's area in km², about 19

# Area-weighted mean, so edge cells count by how much falls inside Hackney
hackney_mean <- terra::extract(summer_days, hackney_v, fun = mean,
                               exact = TRUE, ID = FALSE)

hot_days <- tibble(
  year = as.integer(format(terra::time(summer_days), "%Y")),
  days = unlist(hackney_mean, use.names = FALSE)
)

# Average of each year and the nine before it, so the line runs to 2025
hot_days <- hot_days |>
  mutate(avg_10y = as.numeric(stats::filter(days, rep(1/10, 10), sides = 1)))

# Checks: one row per year, no gaps, no missing values
nrow(hot_days)
all(diff(hot_days$year) == 1)
sum(is.na(hot_days$days))

# Check: the hottest years
hot_days |> slice_max(days, n = 5)

write_rds(hot_days, "data/hot_days.rds")