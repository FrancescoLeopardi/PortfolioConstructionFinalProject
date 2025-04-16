# PortfolioConstructionFinalProject
**QUESTION:** do you merge all your alpha signals into one composite signal and then build
a portfolio (signal combination)?
Or do you construct separate portfolios from each alpha signal and then combine those
portfolios (portfolio aggregation)?
Are they different? Impact of T-costs? Can we prove one is better than the other?

**Objective:** This project explores two approaches to constructing active portfolios from multiple
alpha signals:
● Combining all signals into a single expected return vector and optimizing for information
ratio (and therefore sharpe ratio) only once (we call this the Single Aggregation)
● Optimizing a separate portfolio for each signal and then aggregating the resulting
portfolios (we call this the Portfolio Aggregation).
We aim to isolate the effect of portfolio construction (only) by examining two methods of
aggregation, and their effect on information ratio and sharpe ratio.
Alpha Signals: A panel of alpha signals will be constructed, including 6 month Momentum
(long <25th percentile, short >75 percentile), Value (long <25th percentile, short >75 percentile),
Size (long <25th percentile, short >75 percentile), and Profitability. These signals are commonly
used to explain returns associated with risk.
Data: We will use cross-sectional daily stock data (e.g., CRSP (pending approval) or
Bloomberg) for U.S. equities from 2015–2024. We will use this data for alpha signal creation
and for backtesting.

**Explanation:** all portfolio weights are optimized via the information ratio in order to increase
sharpe ratio

**Steps:**
Signal aggregation we have x alphas, lets say 2 (where x is the number of alpha signals,
Momentum and Value) and we create one joint signal called momentum and value and then we
find the alpha of this signal, could be a linear combination, or other form, and then we optimize
based on IR from this particular signal
Portfolio aggregation, we create two optimized portfolios using individual signal IR and then we
either trade both portfolios, or we combine the portfolios with arbitrary weights
