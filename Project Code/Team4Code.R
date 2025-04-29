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
long_momentum_portfolio_returns_daily <- arrange(portfolio_returns_daily, Date)

# View first few rows
head(long_momentum_portfolio_returns_daily)

# graph of returns

# Calculate cumulative portfolio value
long_momentum_portfolio_returns_daily <- mutate(long_momentum_portfolio_returns_daily, cumulative_return = cumprod(1 + portfolio_return))

p <- ggplot(long_momentum_portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative Portfolio Return Over Time",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # background outside the plot
    panel.background = element_rect(fill = "white", color = NA)  # background inside the plot
  )

# Save the plot
ggsave("long_momentum_cumulative_return_plot.png", plot = p, width = 8, height = 5, dpi = 300)

mu_portfolio <- mean(long_momentum_portfolio_returns_daily$portfolio_return) * 252
sd_portfolio <- sd(long_momentum_portfolio_returns_daily$portfolio_return) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio
mean(ret_portfolio)

print(mu_portfolio)
print(sd_portfolio)
print(sharpe_portfolio)

# Merge portfolio with FF factors directly
merged_data <- merge(long_momentum_portfolio_returns_daily, ff, by = "Date")
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
#########################################
# Momentum Long-Short Portfolio
#########################################

# 1. Rank stocks each formation month
momentum_ranked <- momentum_signal %>%
  group_by(formation_month) %>%
  arrange(desc(rolling_cum_return)) %>%
  mutate(
    rank = row_number(),
    n_stocks = n(),
    threshold = floor(0.10 * n_stocks)  # Top and bottom 10%
  ) %>%
  ungroup()

# 2. Assign weights: +1/top 10%, -1/bottom 10%, 0 otherwise
long_short_weights <- momentum_ranked %>%
  mutate(
    weight = case_when(
      rank <= threshold ~ 1 / threshold,         # Long top 10%
      rank > n_stocks - threshold ~ -1 / threshold, # Short bottom 10%
      TRUE ~ 0
    )
  ) %>%
  select(symbol, formation_month, weight)

# 3. Initialize daily portfolio returns
portfolio_returns_daily <- data.frame()

# Make sure daily_excess has formation_month
daily_excess <- daily_excess %>%
  mutate(formation_month = floor_date(Date, "month"))

# List of rebalance months
rebalance_months <- sort(unique(long_short_weights$formation_month))

# 4. Loop over rebalance months
for (month in rebalance_months) {
  
  beg <- as.Date(month)
  end <- ceiling_date(beg, "month") - days(1)
  
  # Subset daily returns for this month
  ret_month <- daily_excess %>%
    filter(Date >= beg & Date <= end)
  
  if (nrow(ret_month) == 0) next
  
  # Pivot wider: one stock per column
  ret_month_wide <- ret_month %>%
    pivot_wider(names_from = symbol, values_from = excess_ret)
  
  dates <- ret_month_wide$Date
  
  # Build clean return matrix
  ret_matrix <- ret_month_wide %>%
    select(-Date) %>%
    mutate(across(everything(), as.numeric)) %>%
    replace_na(as.list(rep(0, ncol(.)))) %>%
    as.matrix()
  
  # Get weights for this month
  ini_weights <- filter(long_short_weights, formation_month == month)
  weight_vector <- rep(0, ncol(ret_matrix))
  names(weight_vector) <- colnames(ret_matrix)
  weight_vector[ini_weights$symbol] <- ini_weights$weight
  
  # Compute daily portfolio returns
  ret_portfolio <- as.vector(ret_matrix %*% weight_vector)
  
  # Store
  new_returns <- data.frame(Date = dates, portfolio_return = ret_portfolio)
  portfolio_returns_daily <- rbind(portfolio_returns_daily, new_returns)
}

# 5. Final portfolio arrangement
portfolio_returns_daily <- portfolio_returns_daily %>%
  arrange(Date) %>%
  filter(!is.na(portfolio_return), is.finite(portfolio_return)) %>%
  mutate(cumulative_return = cumprod(1 + portfolio_return))

# 6. Plot
# Create the plot with white background
p <- ggplot(portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Long-Short Momentum Portfolio", 
       x = "Date", 
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("momentum_long_short_returns.png", plot = p, width = 8, height = 5, dpi = 300)


# 7. Portfolio statistics
mu_portfolio <- mean(portfolio_returns_daily$portfolio_return) * 252
sd_portfolio <- sd(portfolio_returns_daily$portfolio_return) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

print(mu_portfolio)
print(sd_portfolio)
print(sharpe_portfolio)

# 8. Regression on Fama-French factors
merged_data <- merge(portfolio_returns_daily, ff, by = "Date")

X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_return

regression <- lm(y ~ X)

alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

print(alpha * 252)  # Annualized alpha
print(betas[1])     # Beta to Market
print(betas[2])     # Beta to SMB
print(betas[3])     # Beta to HML

####################### Winners&Losers SIGNAL #######################


print(head(ff))

print(head(returns_with_rf))

#daily excess
daily_excess 

# Monthly excess
monthly_excess

# Calculate the rolling 36 month performance for each stock

#monthly excess
monthly_excess <- group_by(returns_with_rf, symbol, month)
monthly_excess <- summarise(monthly_excess,
                            excess_ret = prod(1 + excess_ret) - 1,
                            .groups = "drop")
monthly_excess <- arrange(monthly_excess, symbol, month)
monthly_excess

rolling_returns <- monthly_excess %>%
  group_by(symbol) %>%
  arrange(month) %>%
  mutate(
    rolling_cum_return = rollapply(
      data = 1 + excess_ret,
      width = 36,
      FUN = function(x) prod(x, na.rm = FALSE) - 1,
      align = "right",
      fill = NA,
      partial = FALSE
    )
  ) %>%
  ungroup()

rolling_returns <- group_by(rolling_returns, symbol, month)
rolling_returns <- arrange(rolling_returns, symbol, month)
rolling_returns <- select(rolling_returns, symbol, month, rolling_cum_return)

# Rearrange for each month
rolling_returns <- arrange(rolling_returns, month, desc(rolling_cum_return))
rolling_returns <- filter(rolling_returns, !is.na(rolling_cum_return))

# If not yet done
rolling_returns <- rolling_returns %>%
  mutate(year = year(month))

  # Filter only the December data for each year to form portfolios
rebalance_dates <- rolling_returns %>%
  filter(month(month) == 12, year %% 3 == 0)

# For each year, assign positions
portfolio_positions <- rebalance_dates %>%
  group_by(year) %>%
  mutate(
    n_stocks = n(),
    rank = rank(desc(rolling_cum_return), ties.method = "first"),
    pct_rank = rank / n_stocks,
    position = case_when(
      pct_rank <= 0.10 ~ -1,    # Short top 10%
      pct_rank >= 0.90 ~ 1,   # Long bottom 10%
      TRUE ~ 0                # Neutral
    )
  ) %>%
  select(symbol, year, position) %>%
  ungroup()

returns_with_positions <- monthly_excess %>%
  mutate(year = year(month)) %>%
  left_join(portfolio_positions, by = c("symbol", "year")) %>%
  arrange(symbol, month)

# Fill forward the positions through the year
# (positions selected in December are applied in the NEXT year — adjust if needed)
returns_with_positions <- returns_with_positions %>%
  group_by(symbol) %>%
  mutate(position = lag(position)) %>%  # December selection applied from January
  fill(position, .direction = "down") %>%  # Fill 36 months
  ungroup()
  
# Calculate monthly portfolio return
portfolio_returns <- returns_with_positions %>%
  filter(!is.na(position)) %>%
  group_by(month) %>%
  summarize(portfolio_ret = mean(position * excess_ret, na.rm = TRUE)) %>%
  ungroup()

# Calculate cumulative returns
portfolio_returns <- portfolio_returns %>%
  mutate(cum_return = cumprod(1 + portfolio_ret) - 1)

# --- Step 8: Plot ---
p <- ggplot(portfolio_returns, aes(x = month, y = cum_return)) +
  geom_line() +
  labs(title = "Cumulative Returns of the Portfolio (3-Year Rebalancing)",
       x = "Month",
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("long_short_reversal_portfolio_cumulative_3yr.png", plot = p, width = 8, height = 5, dpi = 300)

# Calculate statistics
mu_portfolio <- mean(portfolio_returns$portfolio_ret, na.rm = TRUE) * 12
sd_portfolio <- sd(portfolio_returns$portfolio_ret, na.rm = TRUE) * sqrt(12)
sharpe_portfolio <- mu_portfolio / sd_portfolio


print(mu_portfolio)      # Annualized mean return
print(sd_portfolio)      # Annualized volatility
print(sharpe_portfolio)  # Sharpe ratio

# --- Step 11: Regression on Fama-French factors ---

# Merge portfolio returns with Fama-French factors
merged_data <- merge(portfolio_returns, ff, by.x = "month", by.y = "Date")

# Define X (factors) and y (portfolio excess returns)
X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_ret

# Run regression
regression <- lm(y ~ X)

# Extract alpha and betas
alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

# Print results
print(alpha * 12)   # Annualized alpha
print(betas[1])     # Beta to Market
print(betas[2])     # Beta to SMB
print(betas[3])     # Beta to HML

#########################################
######## Mean Reversal - Long only #################
#########################################

print(head(ff))

print(head(returns_with_rf))

#daily excess
daily_excess 

# Monthly excess
monthly_excess

# Calculate the rolling 36 month performance for each stock

#monthly excess
monthly_excess <- group_by(returns_with_rf, symbol, month)
monthly_excess <- summarise(monthly_excess,
                            excess_ret = prod(1 + excess_ret) - 1,
                            .groups = "drop")
monthly_excess <- arrange(monthly_excess, symbol, month)
monthly_excess

rolling_returns <- monthly_excess %>%
  group_by(symbol) %>%
  arrange(month) %>%
  mutate(
    rolling_cum_return = rollapply(
      data = 1 + excess_ret,
      width = 36,
      FUN = function(x) prod(x, na.rm = FALSE) - 1,
      align = "right",
      fill = NA,
      partial = FALSE
    )
  ) %>%
  ungroup()

rolling_returns <- group_by(rolling_returns, symbol, month)
rolling_returns <- arrange(rolling_returns, symbol, month)
rolling_returns <- select(rolling_returns, symbol, month, rolling_cum_return)

# Rearrange for each month
rolling_returns <- arrange(rolling_returns, month, desc(rolling_cum_return))
rolling_returns <- filter(rolling_returns, !is.na(rolling_cum_return))

# If not yet done
rolling_returns <- rolling_returns %>%
  mutate(year = year(month))

  # Filter only the December data for each year to form portfolios
rebalance_dates <- rolling_returns %>%
  filter(month(month) == 12, year %% 3 == 0)

# For each year, assign positions
portfolio_positions <- rebalance_dates %>%
  group_by(year) %>%
  mutate(
    n_stocks = n(),
    rank = rank(desc(rolling_cum_return), ties.method = "first"),
    pct_rank = rank / n_stocks,
    position = case_when(
      pct_rank >= 0.90 ~ 1,   # Long bottom 10%
      TRUE ~ 0                # Neutral
    )
  ) %>%
  select(symbol, year, position) %>%
  ungroup()

returns_with_positions <- monthly_excess %>%
  mutate(year = year(month)) %>%
  left_join(portfolio_positions, by = c("symbol", "year")) %>%
  arrange(symbol, month)

# Fill forward the positions through the year
# (positions selected in December are applied in the NEXT year — adjust if needed)
returns_with_positions <- returns_with_positions %>%
  group_by(symbol) %>%
  mutate(position = lag(position)) %>%  # December selection applied from January
  fill(position, .direction = "down") %>%  # Fill 36 months
  ungroup()
  
# Calculate monthly portfolio return
portfolio_returns <- returns_with_positions %>%
  filter(!is.na(position)) %>%
  group_by(month) %>%
  summarize(portfolio_ret = mean(position * excess_ret, na.rm = TRUE)) %>%
  ungroup()

# Calculate cumulative returns
portfolio_returns <- portfolio_returns %>%
  mutate(cum_return = cumprod(1 + portfolio_ret) - 1)

p <- ggplot(portfolio_returns, aes(x = month, y = cum_return)) +
  geom_line() +
  labs(title = "Cumulative Returns of the Portfolio (3-Year Rebalancing)",
       x = "Month",
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("mean_reversal_long_short_cumulative_returns.png", plot = p, width = 8, height = 5, dpi = 300)




############################################## Long only mean reversal #############

# --- Step 1: Load and prepare daily excess returns ---
# Assumes 'returns_with_rf' is your dataframe with columns: Date, symbol, excess_ret

# Make sure Date is a Date type
returns_with_rf <- returns_with_rf %>%
  mutate(Date = as.Date(Date))

# --- Step 2: Calculate 3-year (756-day) rolling cumulative returns ---
rolling_returns <- returns_with_rf %>%
  group_by(symbol) %>%
  arrange(Date) %>%
  mutate(
    rolling_cum_return = rollapply(
      data = 1 + excess_ret,
      width = 756,  # Approx. 3 years of trading days
      FUN = function(x) prod(x, na.rm = FALSE) - 1,
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()

# --- Step 3: Assign year and find rebalance dates ---
rolling_returns <- rolling_returns %>%
  mutate(year = year(Date))

# Select last available date of December in every 3rd year
rebalance_dates <- rolling_returns %>%
  filter(month(Date) == 12) %>%
  group_by(symbol, year) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  filter(year %% 3 == 0)

# --- Step 4: Assign positions (long bottom 10%) ---
portfolio_positions <- rebalance_dates %>%
  group_by(year) %>%
  mutate(
    n_stocks = n(),
    rank = rank(desc(rolling_cum_return), ties.method = "first"),
    pct_rank = rank / n_stocks,
    position = case_when(
      pct_rank >= 0.90 ~ 1,  # Long bottom 10%
      TRUE ~ 0
    )
  ) %>%
  select(symbol, year, position) %>%
  ungroup()

# --- Step 5: Merge with full returns and apply positions forward ---
returns_with_positions <- returns_with_rf %>%
  mutate(year = year(Date)) %>%
  left_join(portfolio_positions, by = c("symbol", "year")) %>%
  group_by(symbol) %>%
  arrange(Date) %>%
  mutate(position = lag(position)) %>%  # Apply from January next year
  fill(position, .direction = "down") %>%
  ungroup()

# --- Step 6: Compute daily portfolio returns ---
long_only_mean_reversal_portfolio_returns_daily <- returns_with_positions %>%
  filter(!is.na(position)) %>%
  group_by(Date) %>%
  summarize(portfolio_ret = mean(position * excess_ret, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cumulative_return = cumprod(1 + portfolio_ret) - 1)

# --- Step 7: Plot cumulative return ---
p <- ggplot(long_only_mean_reversal_portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative Portfolio Return Over Time (Daily)",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# --- Step 8: Save plot ---
ggsave("long_only_mean_reversal_daily_mean_reversion_cumulative_returns.png", plot = p, width = 8, height = 5, dpi = 300)
