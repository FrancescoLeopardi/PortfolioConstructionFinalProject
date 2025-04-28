# Momentum & Winners/Losers for Portfolio Construction
# Santiago Diaz Tolivia
# Francesco Leopardi
# Evi Prousanidou
# Oriol Ripalta I Maso

# Load packages
install.packages("tidyverse")
install.packages("lubridate")

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)

# Load FF
ff_path <- file.path(getwd(), "data/ff_daily.csv")
ff <- read.csv(ff_path)

# Convert Date to date
ff <- mutate(ff, Date = as.Date(as.character(Date), format = "%Y%m%d"))
ff <- relocate(ff, Date)

# Divide all columns except Date by 100
ff <- mutate(ff, across(-Date, ~ .x / 100))
print(head(ff))

# Load your returns.csv
# returns from yahoo finance --> adjusted close calculated returns
returns_path <- file.path(getwd(), "returns.csv")
returns <- read.csv(returns_path)
returns$Date <- as.Date(returns$Date)
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
monthly_excess <- ungroup(monthly_excess)
monthly_excess <- group_by(returns_with_rf, symbol, month)
monthly_excess <- summarise(monthly_excess,
                            excess_ret = prod(1 + excess_ret) - 1,
                            .groups = "drop")
                            '''
rebalance_months <- arrange(distinct(returns_with_rf, month), month)
print(rebalance_months)

####################### MOMENTUM SIGNAL #######################
install.packages("slider")
library(slider)

# order monthly excess by symbol and month
'''monthly_excess <- arrange(monthly_excess, symbol, month)'''
#daily excess
daily_excess <- select(returns_with_rf, symbol, Date, month, excess_ret)
daily_excess <- arrange(daily_excess, symbol, Date)

#monthly excess
monthly_excess <- group_by(returns_with_rf, symbol, month)
monthly_excess <- summarise(monthly_excess,
                            excess_ret = prod(1 + excess_ret) - 1,
                            .groups = "drop")
monthly_excess <- arrange(monthly_excess, symbol, month)
monthly_excess

# Calculate rolling 12 months cummulative returns
momentum_signal <- monthly_excess %>%
  group_by(symbol) %>%
  arrange(month) %>%
  mutate(
    rolling_cum_return = rollapply(
      data = 1 + excess_ret,
      width = 12,
      FUN = function(x) prod(x, na.rm = FALSE) - 1,
      align = "right",
      fill = NA,
      partial = FALSE
    )
  ) %>%
  ungroup()

momentum_signal <- group_by(momentum_signal, symbol)
momentum_signal <- mutate(momentum_signal, formation_month = lead(month, 1))
momentum_signal <- ungroup(momentum_signal)

momentum_signal <- select(momentum_signal, symbol, formation_month, rolling_cum_return)
momentum_signal <- filter(momentum_signal, !is.na(formation_month))
momentum_signal <- filter(momentum_signal, !is.na(rolling_cum_return))  

head(momentum_signal)

######################## Momentum Long Only ##############################


# Rank and assign weights for top 10%
# initial weights are based on momentul signal --> meaning cum return per month
top10pct_weights <- group_by(momentum_signal, formation_month)
top10pct_weights <- arrange(top10pct_weights, desc(rolling_cum_return))
top10pct_weights <- mutate(top10pct_weights,
                            rank = row_number(),
                            n_stocks = n(),  # total number of stocks in that month
                            threshold = floor(0.10 * n_stocks),  # top 10% (round down)
                            weight = if_else(rank <= threshold, 1 / threshold, 0))
top10pct_weights <- ungroup(top10pct_weights)
top10pct_weights <- select(top10pct_weights, symbol, formation_month, rolling_cum_return, rank, weight)

head(top10pct_weights)

# Stocks selected each month
top10pct_selected_count <- filter(top10pct_weights, weight > 0)
top10pct_selected_count <- group_by(top10pct_selected_count, formation_month)
top10pct_selected_count <- summarise(top10pct_selected_count, n_selected = n())
top10pct_selected_count <- arrange(top10pct_selected_count, formation_month)

print(top10pct_selected_count)
print(arrange(top10pct_selected_count, desc(formation_month)))

## Portfolio Return & Rebalancing
# Create an empty data.frame# Start with empty data frame
portfolio_returns_daily <- data.frame()

# Attach formation_month to daily_excess
daily_excess <- mutate(daily_excess, formation_month = floor_date(Date, "month"))

# Create list of rebalance months
rebalance_months <- sort(unique(top10pct_weights$formation_month))

# Start with empty data frame
portfolio_returns_daily <- data.frame()

# Attach formation_month to daily_excess
daily_excess <- mutate(daily_excess, formation_month = floor_date(Date, "month"))

# Create list of rebalance months
rebalance_months <- sort(unique(top10pct_weights$formation_month))

# Loop over each rebalance month
for (month in rebalance_months) {
  
  month <- as.Date(month)  # Force month to be Date
  
  beg <- month
  end <- ceiling_date(beg, "month") - days(1)
  
  # Filter daily returns for the month
  ret_month <- filter(daily_excess, Date >= beg & Date <= end)
  
  if (nrow(ret_month) == 0) next  # Skip if no data
  
  # Select needed columns only
  ret_month <- select(ret_month, Date, symbol, excess_ret)
  
  # Pivot wider: one stock per column
  ret_month_wide <- pivot_wider(ret_month, names_from = symbol, values_from = excess_ret)
  
  # Extract dates and fix
  dates <- ret_month_wide$Date
  dates <- as.Date(dates)
  
  # Build return matrix
  ret_month_matrix <- as.matrix(select(ret_month_wide, -Date))
  
  # Compute cumulative returns
  cum_month <- apply(1 + ret_month_matrix, 2, cumprod)
  
  # Shift cumulative returns by 1 day
  cum_month <- rbind(rep(1, ncol(cum_month)), cum_month[-nrow(cum_month), ])
  
  # Get initial weights for this month
  ini_weight_df <- filter(top10pct_weights, formation_month == month)
  
  weight_vector <- rep(0, ncol(cum_month))
  names(weight_vector) <- colnames(cum_month)
  weight_vector[ini_weight_df$symbol] <- ini_weight_df$weight
  
  # Apply weights
  month_weights <- sweep(cum_month, 2, weight_vector, "*")
  
  # Calculate weighted portfolio return
  ret_portfolio <- rowSums(month_weights * ret_month_matrix, na.rm = TRUE)
  
  # Save this month's returns
  new_returns <- data.frame(Date = dates, portfolio_return = ret_portfolio)
  
  # rbind (base R only)
  portfolio_returns_daily <- rbind(portfolio_returns_daily, new_returns)
}

# Arrange final result
portfolio_returns_daily <- arrange(portfolio_returns_daily, Date)

# View first few rows
head(portfolio_returns_daily)

# graph of returns

# Calculate cumulative portfolio value
portfolio_returns_daily <- mutate(portfolio_returns_daily, cumulative_return = cumprod(1 + portfolio_return))

# Plot
ggplot(portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative Portfolio Return Over Time",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal()

mu_portfolio <- mean(portfolio_returns_daily$portfolio_return) * 252
sd_portfolio <- sd(portfolio_returns_daily$portfolio_return) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

print(mu_portfolio)
print(sd_portfolio)
print(sharpe_portfolio)

# Merge portfolio with FF factors directly
merged_data <- merge(portfolio_returns_daily, ff, by = "Date")
# Now separate
X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_return

#regression
regression <- lm(y ~ X)
# Extract alpha and betas
alpha <- coef(regression)[1]
betas <- coef(regression)[-1]  # drop intercept

# Portfolio stats (already calculated before but repeat cleanly)
print(mu_portfolio)
print(sd_portfolio)
print(sharpe_portfolio)
print(alpha * 252)        # Annualized alpha
print(betas[1])           # Beta to Market
print(betas[2])           # Beta to SMB
print(betas[3])           # Beta to HML

######################## Momentum Long & Short ##############################

# First, still rank stocks each month
momentum_ranked <- momentum_signal %>%
  group_by(formation_month) %>%
  arrange(desc(rolling_cum_return)) %>%
  mutate(
    rank = row_number(),
    n_stocks = n(),
    threshold = floor(0.10 * n_stocks)  # 10% threshold
  ) %>%
  ungroup()

# Then assign weights:
long_short_weights <- momentum_ranked %>%
  mutate(
    weight = case_when(
      rank <= threshold ~ 1 / threshold,        # Top 10% --> positive weight
      rank > n_stocks - threshold ~ -1 / threshold,  # Bottom 10% --> negative weight
      TRUE ~ 0    # Neutral stocks
    )
  ) %>%
  select(symbol, formation_month, rolling_cum_return, rank, weight)

head(long_short_weights)

# Start with empty data frame
portfolio_returns_daily <- data.frame()

# Attach formation_month to daily_excess
daily_excess <- mutate(daily_excess, formation_month = floor_date(Date, "month"))

# Create list of rebalance months
rebalance_months <- sort(unique(long_short_weights$formation_month))  # ⚡️ USE long_short_weights

# Loop over each rebalance month
for (month in rebalance_months) {
  
  month <- as.Date(month)
  
  beg <- month
  end <- ceiling_date(beg, "month") - days(1)
  
  # Filter daily returns for the month
  ret_month <- filter(daily_excess, Date >= beg & Date <= end)
  
  if (nrow(ret_month) == 0) next  # Skip if no data
  
  ret_month <- select(ret_month, Date, symbol, excess_ret)
  
  ret_month_wide <- pivot_wider(ret_month, names_from = symbol, values_from = excess_ret)
  
  dates <- ret_month_wide$Date
  dates <- as.Date(dates)
  
  ret_month_matrix <- as.matrix(select(ret_month_wide, -Date))
  
  cum_month <- apply(1 + ret_month_matrix, 2, cumprod)
  
  cum_month <- rbind(rep(1, ncol(cum_month)), cum_month[-nrow(cum_month), ])
  
  # Get initial weights for this month (⚡️ now from long_short_weights)
  ini_weight_df <- filter(long_short_weights, formation_month == month)
  
  weight_vector <- rep(0, ncol(cum_month))
  names(weight_vector) <- colnames(cum_month)
  weight_vector[ini_weight_df$symbol] <- ini_weight_df$weight
  
  month_weights <- sweep(cum_month, 2, weight_vector, "*")
  
  ret_portfolio <- rowSums(month_weights * ret_month_matrix, na.rm = TRUE)
  
  new_returns <- data.frame(Date = dates, portfolio_return = ret_portfolio)
  
  portfolio_returns_daily <- rbind(portfolio_returns_daily, new_returns)
}

# Arrange
portfolio_returns_daily <- arrange(portfolio_returns_daily, Date)

# Calculate cumulative returns
portfolio_returns_daily <- mutate(portfolio_returns_daily, cumulative_return = cumprod(1 + portfolio_return))

# Graph
ggplot(portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Long-Short Momentum: Cumulative Return",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal()

# Portfolio stats
mu_portfolio <- mean(portfolio_returns_daily$portfolio_return) * 252
sd_portfolio <- sd(portfolio_returns_daily$portfolio_return) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

print(mu_portfolio)
print(sd_portfolio)
print(sharpe_portfolio)

# Merge with FF
merged_data <- merge(portfolio_returns_daily, ff, by = "Date")

X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_return

# Regression
regression <- lm(y ~ X)

alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

# Final prints
print(mu_portfolio)
print(sd_portfolio)
print(sharpe_portfolio)
print(alpha * 252)        # Annualized alpha
print(betas[1])           # Beta to Market
print(betas[2])           # Beta to SMB
print(betas[3])           # Beta to HML)







