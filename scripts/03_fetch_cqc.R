library(readr)
library(dplyr)
library(httr2)
library(purrr)

# CQC care directory downloaded from:
# https://www.cqc.org.uk/about-us/transparency/using-cqc-data
# Version dated 28 October 2026.

cqc <- read_csv("data-raw/28_October_2026_CQC_directory.csv",
                skip = 4, show_col_types = FALSE)

hackney_care <- cqc |>
  filter(`Local authority` == "Hackney") |>
  filter(grepl("Nursing homes|Residential homes", `Service types`)) |>
  select(name = Name, postcode = Postcode, service_type = `Service types`)

# Coordinates from postcodes.io (bulk lookup, up to 100 postcodes per call)
lookup <- request("https://api.postcodes.io/postcodes") |>
  req_body_json(list(postcodes = hackney_care$postcode)) |>
  req_perform() |>
  resp_body_json()

coords <- lookup$result |>
  map(\(r) tibble(
    postcode  = r$query,
    longitude = r$result$longitude %||% NA_real_,
    latitude  = r$result$latitude  %||% NA_real_
  )) |>
  list_rbind()

hackney_care <- hackney_care |>
  left_join(coords, by = "postcode")

# Check: every care home has coordinates
sum(is.na(hackney_care$longitude))

write_csv(hackney_care, "data/cqc_hackney_care_homes.csv")
cat("Hackney care homes:", nrow(hackney_care), "\n")