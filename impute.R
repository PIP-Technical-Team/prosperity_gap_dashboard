

dt_imputed <- copy(dt_country)
country_combo <- unique(
  dt_imputed[
    ,
    .(Country_name, Country_code, Region_name, Region_code)
  ], 
  by = c("Country_name", "Country_code", "Region_name", "Region_code")
)

# Fill Missing ----
dt_imputed <- joyn::merge(
  y              = CJ(
    Country_name = dt_imputed$Country_name |> unique(), 
    Year         = dt_imputed$Year         |> unique()
  ),
  x              = dt_imputed, 
  by             = c("Country_name", "Year") 
)
dt_imputed[, report := NULL]
dt_imputed <- joyn::merge(
  x             = dt_imputed, 
  y             = country_combo, 
  by            = c("Country_name"),
  update_values = T 
)
dt_imputed[, report := NULL]

# Create RowTrue giving row number, and Row giving the row numbers but with missing values ----
dt_imputed[
  , 
  RowTrue := rowidv(.SD, cols = "Country_name")
]
dt_imputed[
  , 
  Row := ifelse(
    is.na(PG), NA, RowTrue
  )
]
# Look backward and forward to impute the row numbers for Row ----
dt_imputed[
  , 
  Backward := na.locf(Row, na.rm = F),
  by = Country_name
]
dt_imputed[
  , 
  Backward := ifelse(
    is.na(Backward), 1000, Backward
  )
]
dt_imputed[
  , 
  Forward := na.locf(Row, fromLast =T, na.rm = F),
  by = Country_name
]
dt_imputed[
  , 
  Forward := ifelse(
    is.na(Forward), 1000, Forward
  )
]
# Find whether Backward or Forward imputes the nearest row number ----
dt_imputed[
  , 
  `:=`(
    ImputeDirection = ifelse(
      abs(RowTrue - Backward) < abs(RowTrue - Forward), "Backward", "Forward"
    ), 
    ImputeDistance = ifelse(
      abs(RowTrue - Backward) < abs(RowTrue - Forward), abs(RowTrue - Backward) , abs(RowTrue - Forward)
    )
  )
]

dt_imputed[ImputeDirection == "Backward"]
dt_imputed[ImputeDirection=="Forward"]
# Replace X NAs with closest non-missing ----
dt_imputed[
  , 
  Reporting_level := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Reporting_level, na.rm = F, fromLast = TRUE), 
      na.locf(Reporting_level)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Welfare_type := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Welfare_type, na.rm = F, fromLast = TRUE), 
      na.locf(Welfare_type)
    )
    X_replace
  }
]
dt_imputed[
  , 
  PG := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(PG, na.rm = F, fromLast = TRUE), 
      na.locf(PG, na.rm = F)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Inequality := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Inequality, na.rm = F, fromLast = TRUE), 
      na.locf(Inequality)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Mean := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Mean, na.rm = F, fromLast = TRUE), 
      na.locf(Mean, na.rm = F)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Mean_b40 := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Mean_b40, na.rm = F, fromLast = TRUE), 
      na.locf(Mean_b40, na.rm = F)
    )
    X_replace
  }
]
dt_imputed[
  , 
  Survey_comparability := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(Survey_comparability, na.rm = F, fromLast = TRUE), 
      na.locf(Survey_comparability, na.rm = F)
    )
    X_replace
  }
]







