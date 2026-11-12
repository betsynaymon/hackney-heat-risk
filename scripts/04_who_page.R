# ----------------------------------------------
# 04_who_page.R
# Plot-ready data for the "Who is at risk" page and the Overview table.
# Charts are drawn in index.qmd from these files.
#
# Input:  data-raw/fingertips_borough.csv   (01_fetch_fingertips.R)
#         data/census_age_areas.csv         (02_process_census.R)
# Output: data/who_latest.rds         "Latest" panels, at-a-glance table
#         data/who_trend.rds          "Over time" panels
#         data/dementia_dx_london.rds Dementia diagnosis rate, London boroughs
# ----------------------------------------------

# ----------- 1. Load and clean -----------
library(tidyverse)

hackney_code <- "E09000012"
london_code  <- "E12000007"
england_code <- "E92000001"

indicators <- tribble(
  ~IndicatorID, ~short_label,              ~group,                       ~age_keep,
  NA,           "Residents aged 75+",      "Age",                        NA,
  273,          "Coronary heart disease",  "Heart and lungs",            "All ages",
  262,          "Heart failure",           "Heart and lungs",            "All ages",
  219,          "Hypertension",            "Heart and lungs",            "All ages",
  253,          "COPD",                    "Heart and lungs",            "All ages",
  247,          "Dementia",                "Dementia and mental health", "All ages",
  92949,        "Dementia diagnosis rate", "Dementia and mental health", "65+ yrs",
  848,          "Depression",              "Dementia and mental health", "18+ yrs",
  90581,        "Serious mental illness",  "Dementia and mental health", "All ages",
  241,          "Diabetes",                "Diabetes",                   "17+ yrs",
  90280,        "Care users 65+ with enough social contact",    "Social contact", "65+ yrs"

) |>
  mutate(across(c(short_label, group), fct_inorder))

ft <- read_csv("data-raw/fingertips_borough.csv", show_col_types = FALSE) |>
  rename(lower_ci = LowerCI95.0limit, upper_ci = UpperCI95.0limit) |>
  inner_join(indicators, by = "IndicatorID") |>
  filter(is.na(CategoryType), Sex == "Persons", Age == age_keep)

# Checks: 10 indicators, and no duplicate rows per indicator, area and period
n_distinct(ft$IndicatorID)
ft |> count(IndicatorID, AreaCode, TimeperiodSortable) |> filter(n > 1)

# Hackney, London and England, labelled in display order
label_area <- function(code) {
  case_when(
    code == hackney_code ~ "Hackney",
    code == london_code  ~ "London",
    code == england_code ~ "England"
  ) |>
    factor(levels = c("Hackney", "London", "England"))
}

ft_areas <- ft |>
  filter(AreaCode %in% c(hackney_code, london_code, england_code)) |>
  mutate(area = label_area(AreaCode))

# ----------- 2. Latest values -----------
# Latest period with a Hackney value, so all three areas share a period
latest_period <- ft |>
  filter(AreaCode == hackney_code, !is.na(Value)) |>
  group_by(IndicatorID) |>
  summarise(TimeperiodSortable = max(TimeperiodSortable))

latest <- ft_areas |>
  semi_join(latest_period, by = c("IndicatorID", "TimeperiodSortable")) |> 
  select(IndicatorID, short_label, group, area, period = Timeperiod,
         value = Value, lower_ci, upper_ci,
         compared_england = ComparedtoEnglandvalueorpercentiles,
         recent_trend = RecentTrend)

# Share aged 75+ (Census 2021): no interval, comparison or trend
age_rows <- read_csv("data/census_age_areas.csv", show_col_types = FALSE) |>
  transmute(
    short_label = factor("Residents aged 75+", levels = levels(indicators$short_label)),
    group       = factor("Age", levels = levels(indicators$group)),
    area        = label_area(area_code),
    period      = "2021",
    value       = aged_75plus / total_pop * 100
  )

latest <- bind_rows(age_rows, latest)

# Check: 11 rows per area
latest |> count(area)

# Check: indicators where Hackney's value lags the newest period in the data
ft |>
  group_by(IndicatorID, short_label) |>
  summarise(newest = max(TimeperiodSortable), .groups = "drop") |>
  left_join(latest_period, by = "IndicatorID") |>
  filter(newest > TimeperiodSortable)

write_rds(latest, "data/who_latest.rds")

# ---------- 3. Trend series -----------
# Screen for register definition changes
ft_areas |>
  filter(area == "England") |>
  arrange(IndicatorID, TimeperiodSortable) |>
  group_by(short_label) |>
  mutate(change_pct = (Value / lag(Value) - 1) * 100) |>
  filter(abs(change_pct) > 10) |>
  select(short_label, Timeperiod, Value, change_pct)

trend <- ft_areas |>
  mutate(year = TimeperiodSortable %/% 10000) |>  # start year of each period
  # Keep only the years Hackney has, so all three lines start together
  group_by(IndicatorID) |>
  filter(year >= min(year[area == "Hackney"])) |>
  # Start a new segment after a missing year
  group_by(IndicatorID, area) |>
  arrange(year, .by_group = TRUE) |>
  mutate(series = cumsum(coalesce(year - lag(year) > 1, FALSE)) + 1L) |>
  ungroup() |>
  select(IndicatorID, short_label, group, area, period = Timeperiod,
         year, value = Value, series)

# Check: Hackney's segments per indicator
trend |>
  filter(area == "Hackney") |>
  group_by(short_label, series) |>
  summarise(from = min(year), to = max(year), .groups = "drop")

write_rds(trend, "data/who_trend.rds")

# ---- 4. Funnel: dementia diagnosis rate, London boroughs ----
# Spiegelhalter (2005) overdispersion check and additive random-effects
# adjustment. p_obs and p0 are proportions (0-1), n is the denominator.
estimate_overdispersion <- function(p_obs, n, p0, winsor = 0.1) {
  s <- sqrt(p0 * (1 - p0) / n)     # SE under "no real difference"
  z <- (p_obs - p0) / s            # each borough's z-score

  # Winsorise so a few extreme boroughs don't drive the estimate
  lims <- quantile(z, c(winsor, 1 - winsor), na.rm = TRUE)
  z_w  <- pmin(pmax(z, lims[1]), lims[2])

  I   <- length(z_w)
  phi <- sum(z_w^2) / I            # overdispersion factor; ~1 if none

  # Test: I * phi ~ chi-squared on I - 1 df if there is no overdispersion
  p_value <- pchisq(I * phi, df = I - 1, lower.tail = FALSE)

  # Between-borough variance (DerSimonian-Laird form); 0 if no overdispersion
  w    <- 1 / s^2
  tau2 <- (I * phi - (I - 1)) / (sum(w) - sum(w^2) / sum(w))
  tau2 <- max(tau2, 0)

  list(phi = phi, p_value = p_value, tau2 = tau2)
}

boroughs <- ft |>
  filter(IndicatorID == 92949, str_starts(AreaCode, "E09"),
         !is.na(Count), Denominator > 0) |>
  semi_join(latest_period, by = c("IndicatorID", "TimeperiodSortable"))

p0 <- sum(boroughs$Count) / sum(boroughs$Denominator)  # London average
od <- estimate_overdispersion(boroughs$Value / 100, boroughs$Denominator, p0)

z <- c("95" = 1.96, "99.8" = 3.09)

# Check: overdispersion, between-borough SD in % points
od$phi
sqrt(od$tau2) * 100

# 95% limits run from 53.9-83.3 at 89 (City of London) to 57.4-79.7 
# at 4,557. Flat apart from the City, so no funnel: a typical range of 
# London average +/- z x between-borough SD.
band <- tibble(
  level = names(z),
  lower = (p0 - z * sqrt(od$tau2)) * 100,
  upper = (p0 + z * sqrt(od$tau2)) * 100
)

dx_points <- boroughs |>
  transmute(area_name  = AreaName,
            is_hackney = AreaCode == hackney_code,
            value      = Value)

write_rds(list(points = dx_points, band = band,
               london_avg = p0 * 100, phi = od$phi),
          "data/dementia_dx_london.rds")