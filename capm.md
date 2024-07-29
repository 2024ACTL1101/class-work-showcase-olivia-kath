
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# Initialise columns to display the daily returns for AMD and the S&P 500. 
df$dailyReturn_AMD <- NA
df$dailyReturn_GSPC <- NA

# Calculating the daily return for AMD.
for (i in 2:nrow(df)) {
  todayPrice_AMD = df$AMD[i]
  prevPrice_AMD = df$AMD[i-1]
  df$dailyReturn_AMD[i] <- (todayPrice_AMD - prevPrice_AMD)/prevPrice_AMD
}

# Calculating the daily return for S&P 500.
for (i in 2:nrow(df)) {
  todayPrice_GSPC = df$GSPC[i]
  prevPrice_GSPC = df$GSPC[i-1]
  df$dailyReturn_GSPC[i] <- (todayPrice_GSPC - prevPrice_GSPC)/prevPrice_GSPC
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
# Initialise column to display the daily risk-free rate.
df$daily_risk_free_rate <- 0

# Calculating the daily risk free rate.
for (i in 1:nrow(df)) {
  df$daily_risk_free_rate[i] <- (1+(df$RF[i]/100))^(1/360)-1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
# Initialise columns to display the daily returns for AMD and the S&P 500. 
df$excessReturn_AMD <- 0
df$excessReturn_GSPC <- 0

# Calculating excess return for AMD.
for(i in 1:nrow(df)) {
  df$excessReturn_AMD[i] <- df$dailyReturn_AMD[i] - df$daily_risk_free_rate[i]
}

# Calculating excess return for GSPC
for(i in 1:nrow(df)) {
  df$excessReturn_GSPC[i] <- df$dailyReturn_GSPC[i] - df$daily_risk_free_rate[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
# Executing the linear regression and displaying a summary of the key figures.
model <- lm(excessReturn_AMD ~ excessReturn_GSPC, data = df)

summary(model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
From the results of the linear regression, the value of \(\beta\) is approximately 1.57. This value measures the sensitivity of AMD's returns relative to the returns of the S&P 500, as the market index. 

A \(\beta\) of 1 indicates that the stock's return moves with the market. Given that our \(\bets\) value of around 1.57 is greater than 1, this indicates that the AMD stock is more volatile than the market. In other words, for every 1% change in the excess return of the S&P 500, the excess return of AMD changes by approximately 1.57% on average. This implies that AMD's returns are more sensitive to market movements compared to the overall market. 

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# Creating a scatter plot of AMD vs S&P 500 excess returns WITH the CAPM regression line using ggplot.
ggplot(df, aes(x = excessReturn_GSPC, y = excessReturn_AMD)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatterplot Graph of AMD vs. S&P 500 Excess Returns (with CAPM Regression Line)",
       x = "S&P 500 Excess Returns", 
       y = "AMD Excess Returns")
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**
To summarise and explain the results below, the code calculates a 90% prediction interval for AMD's expected annual return. This prediction interval provides a range where AMD's annual return is expected to fall in with 90% confidence.
Where the annual expected return was found to be approximately 0.18, the range was determined to be around [-0.49, 0.85].

```r
# Extracting the standard error from the linear regression model summary.
model_summary <- summary(model)
s_f <- model_summary$sigma

# Calculating the annual standard error.
annual_s_f <- s_f * sqrt(252)

# Stating given values, including a 5% risk-free rate and 13.3% expected market return.
current_rfr <- 0.05 
expected_market_return <- 0.133
alpha <- 0.1  # Significance level for 90% prediction interval. 

# Calculating the expected return for AMD using the CAPM formula.
expected_return_amd <- current_rfr + model$coefficients[2] * (expected_market_return - current_rfr)

# Critical value for t-distribution for 90% prediction interval, with degrees of freedom: n - 2
t_critical <- qt(1 - alpha / 2, df = nrow(df) - 2)  

# Setting prediction interval bounds.
lower_bound <- expected_return_amd - t_critical * annual_s_f
upper_bound <- expected_return_amd + t_critical * annual_s_f

# Output results.
cat("Expected Annual Return for AMD: ", expected_return_amd, "\n")
cat("90% Prediction Interval: [", lower_bound, ", ", upper_bound, "]\n")
```
