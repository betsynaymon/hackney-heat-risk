library(readr)
library(dplyr)

# CQC care directory downloaded from:
# https://www.cqc.org.uk/about-us/transparency/using-cqc-data
# Version dated 28 October 2026.

cqc <- read_csv("data-raw/28_October_2026_CQC_directory.csv",
                skip = 4, show_col_types = FALSE)

hackney_care <- cqc |>
  filter(`Local authority` == "Hackney") |>
  filter(grepl("Nursing homes|Residential homes", `Service types`)) |>
  select(name = Name, postcode = Postcode, service_type = `Service types`)

write_csv(hackney_care, "data/cqc_hackney_care_homes.csv")
cat("Hackney care homes:", nrow(hackney_care), "\n")