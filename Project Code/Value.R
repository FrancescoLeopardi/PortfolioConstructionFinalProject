install.packages("slider")
install.packages("tidyverse")
install.packages("PerformanceAnalytics")

library(tidyverse); library(slider); library(lubridate); library(PerformanceAnalytics)

csv_path    <- "/Users/santi/PycharmProjects/PortfolioConstructionFinalProject/Project\ Code/returns.csv"   # ⇦  point to your file
                      # ⇦  change if you have "^GSPC" etc.

daily <- read_csv(csv_path,
                  col_select = c(symbol, date, ret),
                  col_types  = cols(
                    symbol = col_character(),
                    date   = col_date(format = ""),
                    ret    = col_double()
                  )) |>
         drop_na()

# 2a)  Convert daily → monthly -------------------------------------------------
#       • first trading day of each calendar month := “month index”
#       • monthly return = Π (1+daily) − 1
monthly <- daily |>
  mutate(month = floor_date(date, unit = "month")) |>
  group_by(symbol, month) |>
  summarise(ret = prod(1 + ret) - 1, .groups = "drop") |>
  rename(date = month) |>
  arrange(symbol, date)

# 2b)  Split out market series -------------------------------------------------
# ret_mkt <- monthly |>
#   filter(symbol == mkt_ticker) |>
#   select(date, mkt_ret = ret)

# 2c)  Individual stocks (everything except the market proxy) ------------------
ret_stock <- monthly |>
  filter(symbol != mkt_ticker)

# 2d)  Merge and compute excess returns ---------------------------------------
ret_excess <- ret_stock |>
  left_join(ret_mkt, by = "date") |>
  mutate(excess = ret - mkt_ret)

signal <- ret_excess |>
  group_by(symbol) |>
  arrange(date) |>
  mutate(cum_exc = slide_dbl(excess, ~ prod(1 + .x) - 1,
                             .before = lookback_months - 1,
                             .complete = TRUE)) |>
  ungroup()

## 5)  Monthly re-ranking into loser / winner buckets --------------------------
ranks <- signal |>
  group_by(date) |>
  filter(!is.na(cum_exc)) |>
  mutate(rank = percent_rank(cum_exc)) |>
  mutate(bucket = case_when(
           rank <= tail_pct           ~ "loser",
           rank >= 1 - tail_pct       ~ "winner",
           TRUE                       ~ "neutral")) |>
  ungroup()

## 6)  Generate *formation* tables --------------------------------------------
# Only keep the rebalance dates we want (1st trading day already implicit)
formation <- ranks |>
  select(symbol, date, bucket) |>
  filter(bucket != "neutral")

## 7)  Build rolling K-month portfolios ----------------------------------------
# Helper: expand each formation into K rows (one per future holding month)
hold_table <- formation |>
  mutate(hold_month = map(date, ~ seq(.x %m+% months(1),
                                      .x %m+% months(holding_months),
                                      by = "1 month"))) |>
  unnest(hold_month) |>
  rename(port_date = hold_month, form_date = date)

# Merge with realised *raw* returns (not excess) for evaluation
port_rets <- hold_table |>
  left_join(ret_stock |> select(symbol, date, ret),
            by = c("symbol" = "symbol", "port_date" = "date"))

## 8)  Calculate portfolio returns each month ----------------------------------
portfolio_ls <- port_rets |>
  group_by(port_date, bucket) |>
  mutate(w = 1 / n()) |>             # equal-weight
  summarise(port_ret = sum(w * ret, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = bucket, values_from = port_ret)

# Fill NAs with 0 (means bucket was empty that month)
portfolio_ls[is.na(portfolio_ls)] <- 0

# ❶ Long-loser only
long_loser   <- portfolio_ls |> transmute(date = port_date, ret = loser)

# ❷ Long-loser / short-winner (dollar neutral)
ls_spread    <- portfolio_ls |> transmute(date = port_date,
                                          ret = loser - winner)

## 9)  Performance summary -----------------------------------------------------
charts.PerformanceSummary(
  long_loser |> column_to_rownames("date") |> xts::as.xts(),
  main = "Long-only Loser Portfolio")

charts.PerformanceSummary(
  ls_spread |> column_to_rownames("date") |> xts::as.xts(),
  main = "Long-Loser / Short-Winner Spread")

table.AnnualizedReturns(
  list("Loser-only" = long_loser$ret,
       "LS spread"  = ls_spread$ret),
  Rf = 0)

