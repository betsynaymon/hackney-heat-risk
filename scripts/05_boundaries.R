library(sf)
library(tidyverse)

hackney_codes <- read_csv("data/census_age_hackney.csv",
                          show_col_types = FALSE)$lsoa_code

hackney_lsoa <- st_read("data-raw/Lower_layer_Super_Output_Areas_(December_2021)_Boundaries_EW_BFE_(V10)_and_RUC") |>
  filter(LSOA21CD %in% hackney_codes) |>
  select(LSOA21CD, LSOA21NM)

# Check: one boundary per census LSOA
nrow(hackney_lsoa) == length(hackney_codes)

dir.create("data-raw/gee", showWarnings = FALSE)
st_write(hackney_lsoa, "data-raw/gee/hackney_lsoa_2021.shp", delete_dsn = TRUE)