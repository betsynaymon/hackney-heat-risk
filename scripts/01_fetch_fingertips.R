library(fingertipsR)
library(dplyr)
library(readr)

# Indicators at the older geography (402)
ids_402 <- c(273, 262, 219, 241, 90280)

# Indicators at the newer geography (502)
ids_502 <- c(253, 247, 92949, 848, 90581)

data_402 <- fingertips_data(IndicatorID = ids_402, AreaTypeID = 402)
data_502 <- fingertips_data(IndicatorID = ids_502, AreaTypeID = 502,
                            ParentAreaTypeID = 6)

borough_data <- bind_rows(data_402, data_502)

write_csv(borough_data, "data-raw/fingertips_borough.csv")

cat("Rows:", nrow(borough_data), "\n")
cat("Indicators pulled:", length(unique(borough_data$IndicatorID)), "\n")

borough_data |> 
  distinct(IndicatorID, IndicatorName)