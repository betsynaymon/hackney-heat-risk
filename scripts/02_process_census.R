library(readxl)
library(tidyverse)


# Census 2021 tables downloaded manually from Nomis
# because nomisr cannot handle these datasets.
#
# TS007A: https://www.nomisweb.co.uk/datasets/c2021ts007a
# TS003:  https://www.nomisweb.co.uk/datasets/c2021ts003
# LSOAs within Hackney. Downloaded 4 November 2026.

# TS007A: https://www.nomisweb.co.uk/datasets/c2021ts007a
# Hackney, London, and England. Downloaded 12 November 2026. 

# --- TS007A (age) ---
age_raw <- read_xlsx("data-raw/census_ts007a_hackney.xlsx", 
                     sheet = "Data", skip = 6, col_names = FALSE)

age <- age_raw |>
  filter(grepl("^E01", ...2)) |>
  select(lsoa_name = 1, lsoa_code = 2, total_pop = 3,
         aged_75_79 = 19, aged_80_84 = 20, aged_85plus = 21) |>
  mutate(across(total_pop:aged_85plus, as.numeric),
         aged_75plus = aged_75_79 + aged_80_84 + aged_85plus)

cat("Age data:", nrow(age), "LSOAs\n")
cat("Hackney 85+:", sum(age$aged_85plus), "\n")

# --- TS007A (age) for Hackney, London and England ---
# This download has ages as rows and areas as columns, unlike the LSOA file
area_codes <- c("lacu2023:Hackney" = "E09000012",
                "gor:London"       = "E12000007",
                "country:England"  = "E92000001")

aged_75plus_bands <- c("Aged 75 to 79 years", "Aged 80 to 84 years",
                       "Aged 85 years and over")

age_areas <- read_xlsx("data-raw/census_ts007a_areas.xlsx",
                       sheet = "Data", skip = 5) |>
  pivot_longer(-Age, names_to = "area_label", values_to = "n") |>
  group_by(area_label) |>
  summarise(total_pop   = n[Age == "Total"],
            aged_75plus = sum(n[Age %in% aged_75plus_bands])) |>
  mutate(area_code = unname(area_codes[area_label]))

age_areas


# --- TS003 (households) ---
hh_raw <- read_xlsx("data-raw/census_ts003_hackney.xlsx",
                    sheet = "Data", skip = 8, col_names = FALSE)

hh <- hh_raw |>
  filter(grepl("^E01", ...1)) |>
  select(lsoa_code = 1, total_hh = 2, one_person = 3,
         one_person_66plus = 4) |>
  mutate(across(total_hh:one_person_66plus, as.numeric))

cat("Household data:", nrow(hh), "LSOAs\n")
cat("Hackney one-person 66+:", sum(hh$one_person_66plus), "\n")

# Save processed files
write_csv(age, "data/census_age_hackney.csv")
write_csv(hh, "data/census_households_hackney.csv")
write_csv(age_areas, "data/census_age_areas.csv")