# Momentum & Winners/Losers for Portfolio Construction
# Santiago Diaz Tolivia
# Francesco Leopardi
# Evi Prousanidou
# Oriol Ripalta I Maso

# Load packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("PerformanceAnalytics")

library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)

# Load FF
ff_path <- file.path(getwd(), "data/ff_daily.csv")
ff <- read_csv(ff_path)

# Convert Date to date
ff <- ff |> mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))
ff <- ff |> relocate(Date)  #move Date to first column if needed

# Divide all columns except Date by 100
ff <- ff |> mutate(across(-Date, ~ .x / 100))
# Check
print(head(ff))


# Load your returns.csv
# returns from yahoo finance --> adjusted close calculated returns
returns_path <- file.path(getwd(), "returns.csv")
returns <- read_csv(returns_path)
print(head(returns))

returns <- returns |> select(-GEV, -SOLV, -SW)
print(head(returns))

# pivot to have date|symbol|return
returns <- returns |> 
  pivot_longer(
    cols = -Date,
    names_to = "symbol",
    values_to = "ret"
  ) |> 
  drop_na()
head(returns)

# Merge returns with Fama-French factors
returns_with_rf <- returns |> 
  left_join(ff, by = "Date")

#Calculate excess return
returns_with_rf <- returns_with_rf |> 
  mutate(excess_ret = ret - RF)
head(returns_with_rf)

# Create a 'month' variable
returns_with_rf <- returns_with_rf |> 
  mutate(month = floor_date(Date, unit = "month"))

# For each stock and month, get the last trading day's excess return
monthly_excess <- returns_with_rf |>
  group_by(symbol, month) |>
  slice_tail(n = 1) |>  # get the last row in each month
  ungroup()

head(monthly_excess)

####################### MOMENTUM SIGNAL #######################

install.packages("slider")
library(slider)

# We already have 'monthly_excess' with symbol, month, excess_ret

# Arrange properly
monthly_excess <- monthly_excess |> arrange(symbol, month)

# Calculate rolling 11 months cumulative return (months 2-12, skip the last month)
momentum_signal <- monthly_excess |> 
  group_by(symbol) |> 
  mutate(
    # sliding window: past 11 months before the last 1 month
    cum_return = slide_index_dbl(
      .x = excess_ret,
      .i = month,
      .f = ~ prod(1 + .x) - 1,
      .before = 12,  # 13 months back (0-based indexing)
      .complete = TRUE  # Only keep if full history is available
    )
  ) |> 
  ungroup()

# Filter out the months where we were supposed to skip the most recent 1 month
momentum_signal <- momentum_signal |> 
  group_by(symbol) |> 
  mutate(formation_month = lead(month, 1)) |>  # Move the window forward by 1 month
  ungroup()

# Only keep relevant columns
momentum_signal <- momentum_signal |> 
  select(symbol, formation_month, cum_return) |> 
  filter(!is.na(formation_month))

# View
head(momentum_signal)


# Rank and assign weights
top3_weights <- momentum_signal |> 
  group_by(formation_month) |> 
  arrange(desc(cum_return)) |> 
  mutate(
    rank = row_number(),
    weight = if_else(rank <= 3, 1/3, 0)
  ) |> 
  ungroup()

# Check
head(top3_weights)



