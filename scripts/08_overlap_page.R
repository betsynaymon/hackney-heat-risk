# ----------------------------------------------
# 08_overlap_page.R
# Plot-ready data for the "Where they overlap" page.
#
# Input:  data/where_lsoa.rds                (07_where_page.R)
#         data/census_age_hackney.csv        (02_process_census.R)
#         data/cqc_hackney_care_homes.csv    (03_fetch_cqc.R)
#         data-raw/CoolSpaceSites_2025.gpkg  (GLA Cool Space Data 2025)
# Output: data/overlap.rds   list: lsoa, care_homes, cool_spaces
#         data/census_age_hackney.csv   (02_process_census.R)
# ----------------------------------------------

# ----------- 1. Heat and share aged 75+ -----------
library(tidyverse)
library(sf)
library(biscale)

where_lsoa <- read_rds("data/where_lsoa.rds")

age <- read_csv("data/census_age_hackney.csv", show_col_types = FALSE) |>
  transmute(lsoa_code, aged_75plus,
            share_75plus = aged_75plus / total_pop * 100)

overlap <- where_lsoa |>
  left_join(age, by = "lsoa_code") |>
  bi_class(x = lst, y = share_75plus, style = "quantile", dim = 3)

# Checks: 149 LSOAs, every one matched to Census 2021
nrow(overlap)
sum(is.na(overlap$share_75plus))

# Check: LSOAs per cell ("3-3" is hot and older)
overlap |> st_drop_geometry() |> count(bi_class)

# ----------- 2. Care homes -----------
care_homes <- read_csv("data/cqc_hackney_care_homes.csv", show_col_types = FALSE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

care_in_lsoa <- care_homes |>
  st_join(select(overlap, lsoa_code)) |>
  st_drop_geometry()

# Check: 13 care homes, each inside a Hackney LSOA
nrow(care_in_lsoa)
sum(is.na(care_in_lsoa$lsoa_code))

overlap <- overlap |>
  left_join(count(care_in_lsoa, lsoa_code, name = "n_care_homes"),
            by = "lsoa_code") |>
  mutate(n_care_homes = coalesce(n_care_homes, 0L))

# Check: where the care home LSOAs fall
overlap |> st_drop_geometry() |> count(bi_class, care_home = n_care_homes > 0)

# Check: the hot-and-older LSOAs
overlap |>
  st_drop_geometry() |>
  summarise(hottest_third = sum(aged_75plus[str_starts(bi_class, "3")]),
            all_75plus    = sum(aged_75plus),
            share         = hottest_third / all_75plus * 100)

# ----------- 3. Cool spaces -----------
# GLA Cool Space Data 2025 (London Datastore)
cool <- st_read("data-raw/CoolSpaceSites_2025.gpkg", quiet = TRUE) |>
  st_transform(4326) |>
  st_filter(overlap) |>
  select(name = cs_name, tier, opening_hours = cs_opening_hours,
         heat_vulnerable = cs_heat_vulnerable,
         approve, borough = cs_borough)

# Checks: how many, all approved, all listed under Hackney, by tier
nrow(cool)
cool |> st_drop_geometry() |> count(approve, borough, tier)

# ----------- 4. Save plot-ready data -----------
band_names <- c("Lowest third", "Middle third", "Highest third")

overlap_lsoa <- overlap |>
  mutate(heat_band = band_names[as.integer(str_sub(bi_class, 1, 1))],
         age_band  = band_names[as.integer(str_sub(bi_class, 3, 3))]) |>
  select(lsoa_code, lsoa_name, msoa_hcl_name, lst, share_75plus,
         aged_75plus, n_care_homes, bi_class, heat_band, age_band)

care_pts <- care_homes |> select(name, service_type)
cool_pts <- cool |> select(name, tier, opening_hours)

# Checks: 149 LSOAs with no missing values; 13 care homes; 9 cool spaces
nrow(overlap_lsoa)
colSums(is.na(st_drop_geometry(overlap_lsoa)))
c(nrow(care_pts), nrow(cool_pts))

# Check: all three layers in longitude and latitude (EPSG:4326) for leaflet
st_crs(care_pts) == st_crs(overlap_lsoa)
st_crs(cool_pts) == st_crs(overlap_lsoa)

write_rds(list(lsoa = overlap_lsoa, care_homes = care_pts, cool_spaces = cool_pts),
          "data/overlap.rds")