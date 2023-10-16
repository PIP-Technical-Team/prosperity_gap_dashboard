# Sample vector X with some NA values ----
X <- c(1, 7, NA, 40, NA, NA, 4, NA, 2)

# Convert X to a data.table ----
dt <- data.table(X)

# Create RowTrue giving row number, and Row giving the row numbers but with missing values ----
dt[
  , 
  `:=`(
    Row     = ifelse(is.na(X), NA, .I), 
    RowTrue = .I 
  )

]
# Look backward and forward to impute the row numbers for Row ----
dt[
  , 
  `:=`(
    Backward = na.locf(Row), 
    Forward  = na.locf(Row, fromLast = TRUE)
  )
]
# Find whether Backward or Forward imputes the nearest row number ----
dt[
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
# Replace X NAs with closest non-missing
dt[
  , 
  X2 := {
    X_replace <- ifelse(
      ImputeDirection == "Forward", 
      na.locf(X, fromLast = TRUE), 
      na.locf(X)
    )
    X_replace
  }
]
dt
