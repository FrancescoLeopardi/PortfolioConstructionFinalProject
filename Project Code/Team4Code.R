# Momentum & Winners/Losers for Portfolio Construction
# Santiago Diaz Tolivia
# Francesco Leopardi
# Evi Prousanidou
# Oriol Ripalta I Maso

# packages
library(tidyverse) # package bundle
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)
library(slider)

# load FF data
ff_path <- file.path(getwd(), "data/ff_daily.csv")
ff <- read.csv(ff_path)

ff$Date <- as.Date(paste0(ff$Date, ""), format = "%Y%m%d")
ff <- mutate(ff, across(-Date, ~ .x / 100))

# load returns data
# returns from yahoo finance --> adjusted close calculated returns
returns_path <- file.path(getwd(), "returns.csv")
returns <- read.csv(returns_path)
returns$Date <- as.Date(returns$Date)


returns <- select(returns, -GEV, -SOLV, -SW) # remove fully empty cols
dim(returns)

# pivot to have date | symbol | return to filter out emtpy data
returns <- pivot_longer(returns, cols = -Date, names_to = "symbol", values_to = "ret")
returns <- drop_na(returns)
returns <- filter(returns, ret != 0)

# Merge returns with ff
returns_with_rf <- left_join(returns, ff, by = "Date")
returns_with_rf$excess_ret <- returns_with_rf$ret - returns_with_rf$RF
# Create a month column
returns_with_rf$month <- floor_date(returns_with_rf$Date, unit = "month")

# List of months
rebalance_months <- arrange(distinct(returns_with_rf, month), month)


####################### MOMENTUM ####################### 

# order monthly excess by symbol and month
# daily excess
xrets <- select(returns_with_rf, symbol, Date, month, excess_ret)
xrets <- arrange(xrets, symbol, Date)

# monthly excess
month_xrets <- group_by(returns_with_rf, symbol, month)
month_xrets <- summarise(month_xrets,
                            excess_ret = prod(1 + excess_ret) - 1,
                            .groups = "drop")

month_xrets <- arrange(month_xrets, symbol, month)

# calculate rolling 12 months cumulative returns
momentum <- month_xrets %>%
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

momentum <- group_by(momentum, symbol)
momentum <- mutate(momentum, formation_month = lead(month, 1))
momentum <- ungroup(momentum)

momentum <- select(momentum, symbol, formation_month, rolling_cum_return)
momentum <- filter(momentum, !is.na(formation_month))
momentum <- filter(momentum, !is.na(rolling_cum_return))  

######################## MOMENTUM LONG ONlY ##############################

# Rank and assign weights for top 10%
top10pct_weights <- group_by(momentum, formation_month)
top10pct_weights <- arrange(top10pct_weights, desc(rolling_cum_return))
top10pct_weights <- mutate(top10pct_weights,
                            rank = row_number(),
                            n_stocks = n(),  # total number of stocks in that month
                            threshold = floor(0.10 * n_stocks),  # top 10% (round down)
                            weight = if_else(rank <= threshold, 1 / threshold, 0))
top10pct_weights <- ungroup(top10pct_weights)
top10pct_weights <- select(top10pct_weights, symbol, formation_month, rolling_cum_return, rank, weight)


# stocks selected each month
top10pct_selected_count <- filter(top10pct_weights, weight > 0)
top10pct_selected_count <- group_by(top10pct_selected_count, formation_month)
top10pct_selected_count <- summarise(top10pct_selected_count, n_selected = n())
top10pct_selected_count <- arrange(top10pct_selected_count, formation_month)

# Portfolio Returns & Rebalancing
portfolio_returns_daily <- data.frame() # empty dataframe
# attatch formation_month to xrets
xrets <- mutate(xrets, formation_month = floor_date(Date, "month"))
rebalance_months <- sort(unique(top10pct_weights$formation_month))


portfolio_returns_daily <- data.frame() # empty dataframe
# attatch formation_month to xrets
xrets <- mutate(xrets, formation_month = floor_date(Date, "month"))
rebalance_months <- sort(unique(top10pct_weights$formation_month))

# rebalance loop
for (month in rebalance_months) {
  
  month <- as.Date(month)
  
  beg <- month
  end <- ceiling_date(beg, "month") - days(1)
  
  # filter returns
  ret_month <- filter(daily_excess, Date >= beg & Date <= end)
  
  if (nrow(ret_month) == 0) next  # Skip if no data
  
  ret_month <- select(ret_month, Date, symbol, excess_ret)
  ret_month_wide <- pivot_wider(ret_month, names_from = symbol, values_from = excess_ret)
  
  dates <- ret_month_wide$Date
  dates <- as.Date(dates)
  
  # build returns matrix
  ret_month_matrix <- as.matrix(select(ret_month_wide, -Date))
  # compute cumulative returns
  cum_month <- apply(1 + ret_month_matrix, 2, cumprod)
  # shift cumulative returns by 1 day
  cum_month <- rbind(rep(1, ncol(cum_month)), cum_month[-nrow(cum_month), ])
  # get initial weights for this month
  ini_weight_df <- filter(top10pct_weights, formation_month == month)
  weight_vector <- rep(0, ncol(cum_month))
  names(weight_vector) <- colnames(cum_month)
  weight_vector[ini_weight_df$symbol] <- ini_weight_df$weight
  # apply weights
  month_weights <- sweep(cum_month, 2, weight_vector, "*")
  # calculate weighted portfolio return
  ret_portfolio <- rowSums(month_weights * ret_month_matrix, na.rm = TRUE)
  
  # save this month's returns
  new_returns <- data.frame(Date = dates, portfolio_return = ret_portfolio)
  portfolio_returns_daily <- rbind(portfolio_returns_daily, new_returns)
}

# arrange final result
long_momentum_portfolio_returns_daily <- arrange(portfolio_returns_daily, Date)
# calculate cumulative portfolio value
long_momentum_portfolio_returns_daily <- mutate(long_momentum_portfolio_returns_daily, cumulative_return = cumprod(1 + portfolio_return))

ggplot(long_momentum_portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative Portfolio Return Over Time",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # background outside the plot
    panel.background = element_rect(fill = "white", color = NA)  # background inside the plot
  )

mu_portfolio <- mean(long_momentum_portfolio_returns_daily$portfolio_return) * 252
sd_portfolio <- sd(long_momentum_portfolio_returns_daily$portfolio_return) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

mu_portfolio
sd_portfolio
sharpe_portfolio

# merge portfolio with FF factors directly
merged_data <- merge(long_momentum_portfolio_returns_daily, ff, by = "Date")
# get regression variables
X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_return

regression <- lm(y ~ X)
alpha <- coef(regression)[1]
betas <- coef(regression)[-1] # drop intercept

residuals <- regression$residuals
sigma_epsilon <- sqrt(mean(residuals^2))

alpha * 252 # annualized alpha
betas[1] # beta to MKT
betas[2] # beta to SMB
betas[3] # beta to HML

IR <- alpha / sigma_epsilon
IR

######################## MOMENTUM LONG SHORT ##############################

# Create stock ranking
momentum_ranked <- momentum %>%
  group_by(formation_month) %>%
  arrange(desc(rolling_cum_return)) %>%
  mutate(
    rank = row_number(),
    n_stocks = n(),
    threshold = floor(0.10 * n_stocks)  # Top and bottom 10%
  ) %>%
  ungroup()

# assign weights: +1/top 10%, -1/bottom 10%, 0 otherwise
long_short_weights <- momentum_ranked %>%
  mutate(
    weight = case_when(
      rank <= threshold ~ 1 / threshold,         # Long top 10%
      rank > n_stocks - threshold ~ -1 / threshold, # Short bottom 10%
      TRUE ~ 0
    )
  ) %>%
  select(symbol, formation_month, weight)

portfolio_returns_daily <- data.frame()

# ensure xrets has formation month
xrets <- xrets %>%
  mutate(formation_month = floor_date(Date, "month"))

rebalance_months <- sort(unique(long_short_weights$formation_month))

for (month in rebalance_months) {
  
  beg <- as.Date(month)
  end <- ceiling_date(beg, "month") - days(1)
  
  # subset daily returns for this month
  ret_month <- xrets %>%
    filter(Date >= beg & Date <= end)
  
  if (nrow(ret_month) == 0) next
  ret_month_wide <- ret_month %>%
    pivot_wider(names_from = symbol, values_from = excess_ret)
  
  dates <- ret_month_wide$Date
  
  # build clean return matrix
  ret_matrix <- ret_month_wide %>%
    select(-Date) %>%
    mutate(across(everything(), as.numeric)) %>%
    replace_na(as.list(rep(0, ncol(.)))) %>%
    as.matrix()
  
  # get weights for this month
  ini_weights <- filter(long_short_weights, formation_month == month)
  weight_vector <- rep(0, ncol(ret_matrix))
  names(weight_vector) <- colnames(ret_matrix)
  weight_vector[ini_weights$symbol] <- ini_weights$weight
  
  # compute daily portfolio returns
  ret_portfolio <- as.vector(ret_matrix %*% weight_vector)
  new_returns <- data.frame(Date = dates, portfolio_return = ret_portfolio)
  portfolio_returns_daily <- rbind(portfolio_returns_daily, new_returns)
}

long_short_momentum_portfolio_returns_daily <- arrange(portfolio_returns_daily, Date)
# calculate cumulative portfolio value
long_short_momentum_portfolio_returns_daily <- mutate(long_short_momentum_portfolio_returns_daily, cumulative_return = cumprod(1 + portfolio_return))

# portfolio arrangement
long_momentum_portfolio_returns_daily <- portfolio_returns_daily %>%
  arrange(Date) %>%
  filter(!is.na(portfolio_return), is.finite(portfolio_return)) %>%
  mutate(cumulative_return = cumprod(1 + portfolio_return))


ggplot(long_short_momentum_portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Long-Short Momentum Portfolio", 
       x = "Date", 
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

mu_portfolio <- mean(portfolio_returns_daily$portfolio_return) * 252
sd_portfolio <- sd(portfolio_returns_daily$portfolio_return) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

mu_portfolio
sd_portfolio
sharpe_portfolio

merged_data <- merge(portfolio_returns_daily, ff, by = "Date")
X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_return

regression <- lm(y ~ X)
alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

residuals <- regression$residuals
sigma_epsilon <- sqrt(mean(residuals^2))

alpha * 252 # annualized alpha
betas[1] # beta to MKT
betas[2] # beta to SMB
betas[3] # beta to HML

IR <- alpha / sigma_epsilon
IR

######################## MEAN REVERSAL LONG ONLY ##############################

# Ensure Date column is datetype
returns_with_rf$Date <- as.Date(returns_with_rf$Date)

# Generate 3-year (~ 756-day) rolling cumulative returns
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

# Assign year and calculate rebalancing dates
rolling_returns$year <- year(rolling_returns$Date)

rebalance_dates <- rolling_returns %>%
  filter(month(Date) == 12) %>%
  group_by(symbol, year) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  filter(year %% 3 == 0)

# Assign positions
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

# Merge with full returns and apply positions forward
returns_with_positions <- returns_with_rf %>%
  mutate(year = year(Date)) %>%
  left_join(portfolio_positions, by = c("symbol", "year")) %>%
  group_by(symbol) %>%
  arrange(Date) %>%
  mutate(position = lag(position)) %>%  # Apply from January next year
  fill(position, .direction = "down") %>%
  ungroup()

# Compute daily portfolio returns
long_only_mean_reversal_portfolio_returns_daily <- returns_with_positions %>%
  filter(!is.na(position)) %>%
  group_by(Date) %>%
  summarize(portfolio_ret = mean(position * excess_ret, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cumulative_return = cumprod(1 + portfolio_ret) - 1)

ggplot(long_only_mean_reversal_portfolio_returns_daily, aes(x = Date, y = cumulative_return)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative Portfolio Return Over Time (Daily)",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

mu_portfolio <- mean(long_only_mean_reversal_portfolio_returns_daily$portfolio_ret, na.rm = TRUE) * 252
sd_portfolio <- sd(long_only_mean_reversal_portfolio_returns_daily$portfolio_ret, na.rm = TRUE) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

mu_portfolio
sd_portfolio
sharpe_portfolio

merged_data <- merge(long_only_mean_reversal_portfolio_returns_daily, ff, by.x = "Date", by.y = "Date")

X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_ret

regression <- lm(y ~ X)

alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

residuals <- regression$residuals
sigma_epsilon <- sqrt(mean(residuals^2))

alpha * 252 # annualized alpha
betas[1] # beta to MKT
betas[2] # beta to SMB
betas[3] # beta to HML

IR <- alpha / sigma_epsilon
IR

####################### MEAN REVERSAL LONG SHORT ###############################

returns_with_rf$Date <- as.Date(returns_with_rf$Date)

# Generate 3-year (~ 756-day) rolling cumulative returns
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

# Assign year and calculate rebalancing dates
rolling_returns$year <- year(rolling_returns$Date)

rebalance_dates <- rolling_returns %>%
  filter(month(Date) == 12) %>%
  group_by(symbol, year) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  filter(year %% 3 == 0)

# Assign positions
portfolio_positions <- rebalance_dates %>%
  group_by(year) %>%
  mutate(
    n_stocks = n(),
    rank = rank(desc(rolling_cum_return), ties.method = "first"),
    pct_rank = rank / n_stocks,
    position = case_when(
      pct_rank <= 0.10 ~ -1,
      pct_rank >= 0.90 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(symbol, year, position) %>%
  ungroup()

# Merge with full returns and apply positions forward
returns_with_positions <- returns_with_rf %>%
  mutate(year = year(Date)) %>%
  left_join(portfolio_positions, by = c("symbol", "year")) %>%
  group_by(symbol) %>%
  arrange(Date) %>%
  mutate(position = lag(position)) %>%  # Apply from January next year
  fill(position, .direction = "down") %>%
  ungroup()


# Compute daily portfolio returns
long_short_reversal_portfolio_returns_daily <- returns_with_positions %>%
  filter(!is.na(position)) %>%
  group_by(Date) %>%
  summarize(portfolio_ret = mean(position * excess_ret, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cum_return = cumprod(1 + portfolio_ret) - 1)


ggplot(long_short_reversal_portfolio_returns_daily, aes(x = Date, y = cum_return)) +
  geom_line(color = "blue") +
  labs(title = "Cumulative Daily Returns: Long-Short Reversal Strategy",
       x = "Date",
       y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

mu_portfolio <- mean(long_short_reversal_portfolio_returns_daily$portfolio_ret, na.rm = TRUE) * 252
sd_portfolio <- sd(long_short_reversal_portfolio_returns_daily$portfolio_ret, na.rm = TRUE) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

mu_portfolio
sd_portfolio
sharpe_portfolio

merged_data <- merge(long_short_reversal_portfolio_returns_daily, ff, by.x = "Date", by.y = "Date")

X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_ret

regression <- lm(y ~ X)

alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

residuals <- regression$residuals
sigma_epsilon <- sqrt(mean(residuals^2))

alpha * 252 # annualized alpha
betas[1] # beta to MKT
betas[2] # beta to SMB
betas[3] # beta to HML

IR <- alpha / sigma_epsilon
IR

####################################### AGGREGATION ##########################################

momentum <- long_momentum_portfolio_returns_daily %>%
  rename(momentum_ret = portfolio_return)

reversal <- long_only_mean_reversal_portfolio_returns_daily %>%
  rename(reversal_ret = portfolio_ret)

momentum$Date <- as.Date(momentum$Date)
reversal$Date <- as.Date(reversal$Date)

long_only_combined_returns <- inner_join(momentum, reversal, by = "Date")

# Assign 50% to each strategy
long_only_combined_returns <- long_only_combined_returns %>%
  mutate(
    portfolio_ret = 0.5 * momentum_ret + 0.5 * reversal_ret
  ) %>%
  select(Date, portfolio_ret)

# compute cumulative return (optional)
long_only_combined_returns <- long_only_combined_returns %>%
  mutate(cum_return = cumprod(1 + portfolio_ret) - 1)

ggplot(long_only_combined_returns, aes(x = Date, y = cum_return)) +
  geom_line(color = "blue") +
  labs(title = "Combined Portfolio (Momentum + Reversal)",
       x = "Date", y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


mu_portfolio <- mean(long_only_combined_returns$portfolio_ret, na.rm = TRUE) * 252
sd_portfolio <- sd(long_only_combined_returns$portfolio_ret, na.rm = TRUE) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

mu_portfolio
sd_portfolio
sharpe_portfolio

merged_data <- merge(long_only_combined_returns, ff, by.x = "Date", by.y = "Date")

X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_ret

regression <- lm(y ~ X)

alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

residuals <- regression$residuals
sigma_epsilon <- sqrt(mean(residuals^2))

alpha * 252 # annualized alpha
betas[1] # beta to MKT
betas[2] # beta to SMB
betas[3] # beta to HML

IR <- alpha / sigma_epsilon
IR

#### COMBINE LONG SHORT PORTFOLIOS

momentum <- momentum_long_short_portfolio_returns_daily %>%
  rename(momentum_ret = portfolio_return)

reversal <- long_short_reversal_portfolio_returns_daily %>%
  rename(reversal_ret = portfolio_ret)

momentum$Date <- as.Date(momentum$Date)
reversal$Date <- as.Date(reversal$Date)

long_short_combined_returns <- inner_join(momentum, reversal, by = "Date")

# Assign 50% to each strategy
long_short_combined_returns <- long_short_combined_returns %>%
  mutate(
    portfolio_ret = 0.5 * momentum_ret + 0.5 * reversal_ret
  ) %>%
  select(Date, portfolio_ret)

# compute cumulative return (optional)
long_short_combined_returns <- long_short_combined_returns %>%
  mutate(cum_return = cumprod(1 + portfolio_ret) - 1)

ggplot(long_short_combined_returns, aes(x = Date, y = cum_return)) +
  geom_line(color = "blue") +
  labs(title = "Combined Portfolio (Momentum + Reversal)",
       x = "Date", y = "Cumulative Return") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

mu_portfolio <- mean(long_short_combined_returns$portfolio_ret, na.rm = TRUE) * 252
sd_portfolio <- sd(long_short_combined_returns$portfolio_ret, na.rm = TRUE) * sqrt(252)
sharpe_portfolio <- mu_portfolio / sd_portfolio

mu_portfolio
sd_portfolio
sharpe_portfolio

merged_data <- merge(long_short_combined_returns, ff, by.x = "Date", by.y = "Date")

X <- as.matrix(merged_data[, c("Mkt.RF", "SMB", "HML")])
y <- merged_data$portfolio_ret

regression <- lm(y ~ X)

alpha <- coef(regression)[1]
betas <- coef(regression)[-1]

residuals <- regression$residuals
sigma_epsilon <- sqrt(mean(residuals^2))

alpha * 252 # annualized alpha
betas[1] # beta to MKT
betas[2] # beta to SMB
betas[3] # beta to HML

IR <- alpha / sigma_epsilon
IR


