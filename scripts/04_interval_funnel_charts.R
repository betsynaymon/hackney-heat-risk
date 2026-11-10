# 04_interval_funnel_charts.R
# Interval chart and funnel plots
# Hackney heat risk dashboard
#
# Input:  data-raw/fingertips_borough.csv (written by 01_fetch_fingertips.R)
# Output: plots saved to outputs/plots/
#

library(tidyverse)
library(PHEindicatormethods)
library(ggrepel)

# ---- 1. Load Fingertips data -----

ft_raw <- read_csv("data-raw/fingertips_borough.csv", show_col_types = FALSE)

ft_raw |> distinct(IndicatorID, IndicatorName) |> print(n = Inf)

# Rename columns
ft <- ft_raw |>
  rename_with(~ case_when(
    . == "LowerCI95.0limit"  ~ "lower_ci_95",
    . == "UpperCI95.0limit"  ~ "upper_ci_95",
    . == "LowerCI99.8limit"  ~ "lower_ci_998",
    . == "UpperCI99.8limit"  ~ "upper_ci_998",
    TRUE ~ .
  ))

# ---- 2. Identify areas -------
# fingertips_data() returns boroughs, parent regions, and England.
# Work out which rows are which from the area codes.

hackney_code <- "E09000012"

# London boroughs all start with E09
london_borough_codes <- ft |>
  filter(str_starts(AreaCode, "E09")) |>
  distinct(AreaCode) |>
  pull()

# England code
england_code <- "E92000001"

# London region 
region_codes <- ft |>
  filter(str_starts(AreaCode, "E12")) |>
  distinct(AreaCode, AreaName)
print(region_codes)
london_region_code <- region_codes |>
  filter(str_detect(AreaName, "London")) |>
  pull(AreaCode)  # E12000007


# ---- 3. Short labels for each indicator ----
indicator_labels <- tribble(
  ~IndicatorID, ~short_label,                                    ~group,          ~age_keep,
  273,          "Coronary Heart Disease (CHD)",                  "Conditions",    "All ages",
  219,          "Hypertension",                                  "Conditions",    "All ages",
  262,          "Heart failure",                                 "Conditions",    "All ages",
  241,          "Diabetes",                                      "Conditions",    "17+ yrs",
  90280,        "Social contact as much as wanted (65+)",        "Social",        "65+ yrs",
  253,          "Chronic obstructive pulmonary disease (COPD)",  "Conditions",    "All ages",
  247,          "Dementia (QOF)",                                "Conditions",    "All ages",
  92949,        "Dementia diagnosis rate",                       "Conditions",    "65+ yrs",
  848,          "Depression",                                    "Mental health", "18+ yrs",
  90581,        "Serious mental illness",                        "Mental health", "All ages"
)

# Verify: returns IDs in ft that are msising from indicator_labels
setdiff(unique(ft$IndicatorID), indicator_labels$IndicatorID)

ft <- ft |> left_join(indicator_labels, by = "IndicatorID")

# ---- 4. Filter to one row per indicator per area ------
ft_latest <- ft |>
  filter(
    Sex == "Persons",
    Age == age_keep
  ) |>
  group_by(IndicatorID) |>
  filter(Timeperiod == max(Timeperiod)) |>
  ungroup()

# ---- 5. Interval chart: Hackney vs London vs England -----

hackney_data <- ft_latest |>
  filter(AreaCode == hackney_code) |>
  select(IndicatorID, short_label, group, Value, lower_ci_95, upper_ci_95)

# London: use the official region row where it exists; otherwise pool
# borough counts (API is down)
london_pooled <- ft_latest |>
  filter(AreaCode %in% london_borough_codes,
         !is.na(Count), !is.na(Denominator), Denominator > 0) |>
  group_by(IndicatorID) |>
  summarise(london_pooled = sum(Count) / sum(Denominator) * 100,
            .groups = "drop")

london_data <- ft_latest |>
  filter(AreaCode == london_region_code) |>
  select(IndicatorID, london_official = Value) |>
  full_join(london_pooled, by = "IndicatorID") |>
  mutate(london_val = coalesce(london_official, london_pooled))

# Check: where both exist, do they agree?
london_data |>
  filter(!is.na(london_official)) |>
  mutate(diff = london_official - london_pooled) |>
  left_join(indicator_labels, by = "IndicatorID") |>
  select(short_label, london_official, london_pooled, diff)

london_data <- london_data |> select(IndicatorID, london_val)

england_data <- ft_latest |>
  filter(AreaCode == england_code) |>
  select(IndicatorID, england_val = Value)

plot_data <- hackney_data |>
  left_join(london_data, by = "IndicatorID") |>
  left_join(england_data, by = "IndicatorID")

# Order: group, then alphabetical within group
label_order <- indicator_labels |>
  arrange(group, short_label) |>
  pull(short_label)

plot_data <- plot_data |>
  mutate(short_label = factor(short_label, levels = rev(label_order)))

dir.create("outputs/plots", showWarnings = FALSE, recursive = TRUE)

interval_chart <- ggplot(plot_data, aes(y = short_label)) +
  # England
  geom_point(aes(x = england_val, colour = "England"),
             shape = 124, size = 4) +
  # London
  geom_point(aes(x = london_val, colour = "London"),
             shape = 124, size = 4) +
  # Hackney with CI
  geom_pointrange(
    aes(x = Value, xmin = lower_ci_95, xmax = upper_ci_95,
        colour = "Hackney (95% CI)"),
    size = 0.4, linewidth = 0.6
  ) +
  scale_colour_manual(
    name = NULL,
    values = c("Hackney (95% CI)" = "#CC3311",
               "London"           = "#4477AA",
               "England"          = "#888888"),
    breaks = c("Hackney (95% CI)", "London", "England"),
    guide = guide_legend(
      override.aes = list(
        shape    = c(16, 124, 124),
        linetype = c("solid", "blank", "blank"),
        size     = c(2, 4, 4)
      )
    )
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(
    title = "Heat-related risk factors: Hackney vs London and England",
    x = "Prevalence (%)",
    y = NULL,
    caption = paste0(
      "Source: OHID Fingertips (QOF, PHOF). Most recent available period per indicator.\n",
      "QOF prevalence is crude (not age-standardised): Hackney's young population\n",
      "lowers recorded prevalence relative to older boroughs."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    strip.placement = "outside",
    strip.background.y = element_rect(fill = "#f0f0f0", colour = NA),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 10,
                                      margin = margin(r = 6, l = 4)),
    panel.spacing.y = unit(1, "lines"),
    panel.grid.major.y = element_line(colour = "#eeeeee"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 8, colour = "#666666",
                                hjust = 0.5, margin = margin(t = 15)),
    plot.caption.position = "plot",
    plot.title = element_text(size = 13, face = "bold")
  )

ggsave("outputs/plots/interval_chart.png", interval_chart,
       width = 9, height = 6, dpi = 300)


# ---- 5b. Spine chart: Hackney against all upper-tier local authorities -----
# Each row is scaled separately with England at the centre. Bars show the
# range (light) and middle 50% (dark) across upper-tier LAs in England.
# Lowest values on the left, highest on the right (no "better/worse" polarity).

# All upper-tier local authorities (drops region and England rows)
la_summary <- ft_latest |>
  filter(!AreaCode %in% c(england_code, region_codes$AreaCode)) |>
  group_by(IndicatorID) |>
  summarise(
    la_min = min(Value, na.rm = TRUE),
    la_p25 = quantile(Value, 0.25, na.rm = TRUE),
    la_p75 = quantile(Value, 0.75, na.rm = TRUE),
    la_max = max(Value, na.rm = TRUE),
    n_las  = sum(!is.na(Value)),
    .groups = "drop"
  )

# Check: roughly 150 LAs per indicator
print(la_summary)

# Rescale each row: England at 1, the furthest LA from England at 0 or 2
to_spine <- function(v, centre, half_width) 1 + (v - centre) / half_width

# One latest period per indicator
periods <- ft_latest |>
  distinct(IndicatorID, Timeperiod)

# Check: should return 0 rows (one period per indicator)
periods |> count(IndicatorID) |> filter(n > 1)

spine_data <- plot_data |>
  left_join(la_summary, by = "IndicatorID") |>
  left_join(periods, by = "IndicatorID") |>
  mutate(
    half_width = pmax(england_val - la_min, la_max - england_val),
    min_s      = to_spine(la_min,     england_val, half_width),
    p25_s      = to_spine(la_p25,     england_val, half_width),
    p75_s      = to_spine(la_p75,     england_val, half_width),
    max_s      = to_spine(la_max,     england_val, half_width),
    hackney_s  = to_spine(Value,      england_val, half_width),
    london_s   = to_spine(london_val, england_val, half_width),
    # Hackney vs England: does Hackney's 95% CI exclude England's value?
    sig = case_when(
      is.na(Value)              ~ NA_character_,
      lower_ci_95 > england_val ~ "Higher",
      upper_ci_95 < england_val ~ "Lower",
      TRUE                      ~ "Similar"
    ),
    sig = factor(sig, levels = c("Higher", "Similar", "Lower"))
  )

# Table columns to the left of the spine
fmt <- function(x) case_when(
  is.na(x)    ~ "\u2013",
  abs(x) < 10 ~ sprintf("%.2f", x),
  TRUE        ~ sprintf("%.1f", x)
)

col_x      <- c(Period = -2.9, Hackney = -2.4, London = -1.95, England = -1.5,
                Lowest = -1.05, Highest = -0.6)
col_labels <- c("Period", "Hackney", "London", "England", "Lowest\nLA", "Highest\nLA")

table_long <- spine_data |>
  transmute(
    short_label, group,
    Period  = Timeperiod,
    Hackney = fmt(Value),
    London  = fmt(london_val),
    England = fmt(england_val),
    Lowest  = fmt(la_min),
    Highest = fmt(la_max)
  ) |>
  pivot_longer(-c(short_label, group), names_to = "col", values_to = "label") |>
  mutate(x = col_x[col])

spine_chart <- ggplot(spine_data, aes(y = short_label)) +
  # LA range and middle 50%
  geom_segment(aes(x = min_s, xend = max_s, yend = short_label,
                   colour = "Range across LAs"), linewidth = 5) +
  geom_segment(aes(x = p25_s, xend = p75_s, yend = short_label,
                   colour = "Middle 50% of LAs"), linewidth = 5) +
  # England centre line
  geom_vline(data = tibble(x = 1), aes(xintercept = x, linetype = "England"),
             inherit.aes = FALSE, colour = "#333333", linewidth = 0.6) +
  # London
  geom_point(aes(x = london_s, shape = "London"),
             colour = "#555555", size = 2.5, na.rm = TRUE) +
  # Hackney, filled by comparison with England
  geom_point(aes(x = hackney_s, fill = sig),
             shape = 21, size = 4, colour = "black", stroke = 0.4,
             na.rm = TRUE) +
  # Table values
  geom_text(data = table_long, aes(x = x, y = short_label, label = label),
            size = 3.2, colour = "#333333") +
  scale_colour_manual(
    name = NULL,
    values = c("Range across LAs" = "#E3E3E3", "Middle 50% of LAs" = "#ABABAB")
  ) +
  scale_linetype_manual(name = NULL, values = c(England = "solid")) +
  scale_shape_manual(name = NULL, values = c(London = 18)) +
  scale_fill_manual(
    name = "Hackney vs England:",
    values = c(Higher = "#1F4E79", Similar = "#FFFFFF", Lower = "#9DC3E6"),
    drop = FALSE, na.translate = FALSE
  ) +
  guides(
    colour   = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
    shape    = guide_legend(order = 3),
    fill     = guide_legend(order = 4,
                            override.aes = list(shape = 21, size = 4,
                                                colour = "black"))
  ) +
  scale_x_continuous(
    position = "top",
    breaks   = c(col_x, 1),
    labels   = c(col_labels, "Lowest LA  \u2190  England  \u2192  Highest LA"),
    limits   = c(-3.25, 2.1),
    expand   = c(0, 0)
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(
    title = "Heat-related risk factors: Hackney compared with England",
    x = NULL, y = NULL,
    caption = paste0(
      "Source: OHID Fingertips (QOF, PHOF). Most recent period per indicator. ",
      "Each row is scaled separately, with England at the centre.\n",
      "Hackney's fill compares its 95% CI with England's value. ",
      "QOF prevalence is crude (not age-standardised). \u2013 = not available.\n",
      "Higher is better for dementia diagnosis rate and social contact; ",
      "for QOF prevalence, higher recorded prevalence can reflect better case-finding."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.location = "plot",
    legend.title = element_text(size=10),
    axis.text.x.top = element_text(face = "bold", size = 9, colour = "#333333"),
    panel.grid = element_blank(),
    strip.placement = "outside",
    strip.background.y = element_rect(fill = "#f0f0f0", colour = NA),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 10,
                                     margin = margin(r = 6, l = 4)),
    panel.spacing.y = unit(1, "lines"),
    plot.caption = element_text(size = 8, colour = "#666666",
                                hjust = 0.5, margin = margin(t = 15)),
    plot.caption.position = "plot",
    plot.title = element_text(size = 13, face = "bold")
  )

ggsave("outputs/plots/spine_chart.png", spine_chart,
       width = 12, height = 6.5, dpi = 300)

# ---- 6. Funnel plots: all London boroughs --------
# One funnel per indicator. X = denominator, Y = value.
# 95% control limits (solid), 99.8% (dashed), London average (blue).
# Hackney in red.

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

build_funnel <- function(data, ind_id) {

  meta <- indicator_labels |> filter(IndicatorID == ind_id)

  df <- data |>
    filter(IndicatorID == ind_id, AreaCode %in% london_borough_codes) |>
    filter(!is.na(Count), !is.na(Denominator), Denominator > 0)

  if (nrow(df) < 5) {
    message("Too few boroughs with count/denominator for ", meta$short_label)
    return(NULL)
  }

  # London average from the pooled borough data
  avg_rate <- sum(df$Count) / sum(df$Denominator)

  # Funnel limits
  # PHEindicatormethods::calculate_funnel_limits expects vectors.
  # Generate a smooth denominator sequence for the curves.
  d_range <- range(df$Denominator)
  d_seq   <- seq(d_range[1], d_range[2], length.out = 200)

  # Overdispersion-adjusted funnel limits (Spiegelhalter 2005)
  od <- estimate_overdispersion(df$Value / 100, df$Denominator, avg_rate)

  funnel_limits <- function(p, d, z, tau2 = 0) {
    se <- sqrt(p * (1 - p) / d + tau2)
    tibble(
      denominator = d,
      lowercl     = pmax(0, p - z * se) * 100,
      uppercl     = pmin(1, p + z * se) * 100
    )
  }

  limits_95  <- funnel_limits(avg_rate, d_seq, z = 1.96,   tau2 = od$tau2)
  limits_998 <- funnel_limits(avg_rate, d_seq, z = 3.0902, tau2 = od$tau2)

  avg_pct <- avg_rate * 100

  # Label Hackney plus boroughs outside the adjusted 99.8% limits
  df <- df |>
    mutate(
      se_adj      = sqrt(avg_rate * (1 - avg_rate) / Denominator + od$tau2),
      outside_998 = abs(Value / 100 - avg_rate) > 3.0902 * se_adj,
      is_hackney  = AreaCode == hackney_code,
      plot_label  = case_when(
        is_hackney  ~ "Hackney",
        outside_998 ~ AreaName,
        TRUE        ~ NA_character_
      )
    )
  
  p <- ggplot() +
    # 99.8% limits (dashed)
    geom_line(data = limits_998,
              aes(x = denominator, y = uppercl),
              linetype = "dashed", colour = "#999999", linewidth = 0.4) +
    geom_line(data = limits_998,
              aes(x = denominator, y = lowercl),
              linetype = "dashed", colour = "#999999", linewidth = 0.4) +
    # 95% limits (solid)
    geom_line(data = limits_95,
              aes(x = denominator, y = uppercl),
              linetype = "solid", colour = "#999999", linewidth = 0.4) +
    geom_line(data = limits_95,
              aes(x = denominator, y = lowercl),
              linetype = "solid", colour = "#999999", linewidth = 0.4) +
    # London average
    geom_hline(yintercept = avg_pct, colour = "#4477AA", linewidth = 0.5) +
        # Borough points
    geom_point(data = filter(df, !is_hackney),
               aes(x = Denominator, y = Value),
               colour = "#888888", size = 2, alpha = 0.7) +
    # Hackney
    geom_point(data = filter(df, is_hackney),
               aes(x = Denominator, y = Value),
               colour = "#CC3311", size = 3.5) +
    # Labels: Hackney + outliers
    ggrepel::geom_text_repel(
      data = filter(df, !is.na(plot_label)),
      aes(x = Denominator, y = Value, label = plot_label),
      colour = if_else(
        filter(df, !is.na(plot_label))$is_hackney, "#CC3311", "#555555"
      ),
      size = 3, fontface = "bold",
      max.overlaps = 20,
      box.padding = 0.4,
      point.padding = 0.3,
      min.segment.length = 0.2
    ) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title = meta$short_label,
      subtitle = paste0(
        "London boroughs. Limits adjusted for overdispersion (Spiegelhalter 2005).\n",
        "Solid = 95%. Dashed = 99.8%. Blue = London average."
      ) ,
      x = "Denominator (registered or counted population)",
      y = "Prevalence (%)",
      caption = paste0(
        "Source: OHID Fingertips. Crude prevalence (not age-standardised). ",
        "Overdispersion factor = ", round(od$phi, 1), "."
      )) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 8, colour = "#666666", hjust = 0),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "#444444")
    )

  return(p)
}

# Generate and save a funnel for each indicator
walk(indicator_labels$IndicatorID, function(id) {
  p <- build_funnel(ft_latest, id)
  if (!is.null(p)) {
    label <- indicator_labels$short_label[indicator_labels$IndicatorID == id]
    fname <- str_to_lower(str_replace_all(label, "\\W+", "_"))
    ggsave(paste0("outputs/plots/funnel_", fname, ".png"), p,
           width = 8, height = 5.5, dpi = 300)
  }
})
  
od_summary <- ft_latest |>
  filter(AreaCode %in% london_borough_codes,
         !is.na(Count), !is.na(Denominator), Denominator > 0) |>
  group_by(IndicatorID) |>
  summarise(
    p0 = sum(Count) / sum(Denominator),
    od = list(estimate_overdispersion(Value / 100, Denominator, p0)),
    .groups = "drop"
  ) |>
  mutate(
    phi     = map_dbl(od, "phi"),
    p_value = map_dbl(od, "p_value"),
    tau_pp  = sqrt(map_dbl(od, "tau2")) * 100   # between-borough SD, % points
  ) |>
  select(-od) |>
  left_join(indicator_labels, by = "IndicatorID") |>
  select(short_label, phi, p_value, tau_pp)

print(od_summary)
write_csv(od_summary, "data/funnel_overdispersion.csv")


# ---- 7. Checks before running -----------------------------------------------
#
# a) Indicator 90280 (social isolation) had duplicate rows per area.
#    The slice(1) in section 4 keeps one, but uncomment the diagnostic
#    glimpse() to check what you're keeping vs dropping.
#
# b) Some Fingertips indicators may not have Count and Denominator
#    filled in (e.g. composite indicators or DSRs). Those can't get
#    a funnel — build_funnel() skips them with a message.
#
# c) QOF prevalence is crude. The caption says so, but also say it on
#    the dashboard page: Hackney's young population depresses condition
#    prevalence. That's real for a "how many people are diagnosed"
  #    question but doesn't mean age-specific risk is lower.