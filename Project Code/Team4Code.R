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
ff <- mutate(ff, Date = as.Date(as.character(Date), format = "%Y%m%d"))
ff <- relocate(ff, Date)

# Divide all columns except Date by 100
ff <- mutate(ff, across(-Date, ~ .x / 100))
print(head(ff))


# Load your returns.csv
# returns from yahoo finance --> adjusted close calculated returns
returns_path <- file.path(getwd(), "returns.csv")
returns <- read_csv(returns_path)
print(head(returns))

returns <- select(returns,-GEV,-SOLV,-SW)
print(head(returns))

# pivot to have date|symbol|return
returns <- pivot_longer(returns, cols = -Date, names_to = "symbol", values_to = "ret")
returns <- drop_na(returns)
returns <- filter(returns, ret != 0)

head(returns)

# Merge returns with Fama-French factors
returns_with_rf <- left_join(returns, ff, by = "Date")

#Calculate excess return
returns_with_rf <- mutate(returns_with_rf, excess_ret = ret - RF)

# Create a 'month' variable
returns_with_rf <- mutate(returns_with_rf, month = floor_date(Date, unit = "month"))

# For each stock and month, get the cummulative monthly excess return

'''monthly_excess <- group_by(returns_with_rf, symbol, month)
monthly_excess <- slice_tail(monthly_excess, n = 1)  # get the last row in each month
monthly_excess <- ungroup(monthly_excess)'''

monthly_excess <- group_by(returns_with_rf, symbol, month)
monthly_excess <- summarise(monthly_excess,
                            excess_ret = prod(1 + excess_ret) - 1,
                            .groups = "drop")

head(monthly_excess)

####################### MOMENTUM SIGNAL #######################
install.packages("slider")
library(slider)

# order monthly excess by symbol and month
monthly_excess <- arrange(monthly_excess, symbol, month)

# Calculate rolling 12 months cumulative return (T-2 to T-13)
momentum_signal <- group_by(monthly_excess, symbol)
momentum_signal <- mutate(momentum_signal,
                          cum_return = slide_index_dbl(
                            .x = excess_ret,
                            .i = month,
                            .f = ~ prod(1 + .x) - 1,
                            .before = 11,
                            .complete = TRUE
                          ))
momentum_signal <- ungroup(momentum_signal)

momentum_signal <- group_by(momentum_signal, symbol)
momentum_signal <- mutate(momentum_signal, formation_month = lead(month, 1))
momentum_signal <- ungroup(momentum_signal)

momentum_signal <- select(momentum_signal, symbol, formation_month, cum_return)
momentum_signal <- filter(momentum_signal, !is.na(formation_month))

momentum_signal <- filter(momentum_signal, !is.na(cum_return))  

head(momentum_signal)

'''# Rank and assign weights
top3_weights <- momentum_signal |> 
  group_by(formation_month) |> 
  arrange(desc(cum_return)) |> 
  mutate(
    rank = row_number(),
    weight = if_else(rank <= 3, 1/3, 0)
  ) |> 
  ungroup()

# Check
head(top3_weights)'''


# Rank and assign weights for top 10%
top10pct_weights <- group_by(momentum_signal, formation_month)
top10pct_weights <- arrange(top10pct_weights, desc(cum_return))
top10pct_weights <- mutate(top10pct_weights,
                            rank = row_number(),
                            n_stocks = n(),  # total number of stocks in that month
                            threshold = floor(0.10 * n_stocks),  # top 10% (round down)
                            weight = if_else(rank <= threshold, 1 / threshold, 0))
top10pct_weights <- ungroup(top10pct_weights)
top10pct_weights <- select(top10pct_weights, symbol, formation_month, cum_return, rank, weight)

head(top10pct_weights)

# Stocks selected each month
top10pct_selected_count <- filter(top10pct_weights, weight > 0)
top10pct_selected_count <- group_by(top10pct_selected_count, formation_month)
top10pct_selected_count <- summarise(top10pct_selected_count, n_selected = n())
top10pct_selected_count <- arrange(top10pct_selected_count, formation_month)

print(top10pct_selected_count)
print(arrange(top10pct_selected_count, desc(formation_month)))










