
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
# Ensure strings are not converted to factors. 
stringsAsFactors = FALSE

# If the previous price = 0, OR the price of the current day is less than that of the previous day, then set the trade to buy (this process loops for each row in the dataframe). 
# Note that today's current price is then set to 'previous_price' for the next iteration of this loop occurring the following day i.e. tomorrow. 
for (i in 1:nrow(amd_df)) {
 if (previous_price==0 || amd_df$close[i] < amd_df$close[i-1]) {
   amd_df$trade_type[i] <- "buy"
   
   {
     previous_price <- amd_df$close[i]
  }
 }
}

# Showing the value of each purchase (i.e. "buy) using the current share price multiplied by the share_size value of 100 (negative value reflects money leaving the account). Specifically, if the trade type is set to "buy", then that day's price is multiplied by -100 and added to the costs_proceeds column. 
for (i in 1:nrow(amd_df)) {
  amd_df$costs_proceeds[i] <- ifelse(amd_df$trade_type[i] == "buy", -share_size*amd_df$close[i], NA)
}

# The sum of all shares bought is visible in the accumulated_shares column, where 100 is added each time there is a new purchase (i.e. the trade type is set to "buy"). Note that the if statement also accounts for the existence of "NA"s in the data frame (i.e. where there are no shares being bought) and these are excluded from the sum being calculated. 
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy") {
    accumulated_shares <- accumulated_shares + 100
  }
    amd_df$accumulated_shares[i] <- accumulated_shares
}

# Finding the last day of trading (by locating the final row first), and setting trade_type to sell. 
amd_df[nrow(amd_df), "trade_type"] <- "sell"

# On the last day of trading, setting costs_proceeds to the total number of accumulated shares multiplied by the last day's share price. Note, the if statement similarly accounts for the existence of "NA"s. Altogether, the calculated value is positive and represents money returning back into the account (specifically the value of all the accumulated shares on the final day of trading where they are being sold).
for (i in 1:nrow(amd_df)) {
if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "sell") {
  amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i]*amd_df$close[i]
  }
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Trading period is set from 2021-11-29 to 2022-10-14 (i.e. from the 29th November 2021 to 14th October 2022). 
start_date <- "2021-11-29"
end_date <- "2022-10-14"

# Create a new dataframe containing all stock & trading information only within the customised trading period. This new dataframe is a subset of the original one (i.e. all rows prior to the starting date and following the end date are removed). 
trading_period_df <- subset(amd_df, date >= start_date & date <= end_date)

# The following code reiterates the original trading algorithm for this new dataframe. 

# Initialize columns for trade type, cost/proceeds, and accumulated shares in trading_period_df. 
trading_period_df$trade_type <- NA
trading_period_df$costs_proceeds <- NA  # Corrected column name
trading_period_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic. 
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Ensure strings are not converted to factors. 
stringsAsFactors = FALSE

# If the previous price = 0, OR the price of the current day is less than that of the previous day, then set the trade to buy (this process loops for each row in the dataframe). 
# Note that today's current price is then set to 'previous_price' for the next iteration of this loop occurring the following day i.e. tomorrow. 
for (i in 1:nrow(trading_period_df)) {
 if (previous_price==0 || trading_period_df$close[i] < trading_period_df$close[i-1]) {
   trading_period_df$trade_type[i] <- "buy"
   
   {
     previous_price <- trading_period_df$close[i]
  }
 }
}

# Showing the value of each purchase (i.e. "buy) using the current share price multiplied by the share_size value of 100 (negative value reflects money leaving the account). Specifically, if the trade type is set to "buy", then that day's price is multiplied by -100 and added to the costs_proceeds column. 
for (i in 1:nrow(trading_period_df)) {
  trading_period_df$costs_proceeds[i] <- ifelse(trading_period_df$trade_type[i] == "buy", -share_size*trading_period_df$close[i], NA)
}

# The sum of all shares bought is visible in the accumulated_shares column, where 100 is added each time there is a new purchase (i.e. the trade type is set to "buy"). Note that the if statement also accounts for the existence of "NA"s in the data frame (i.e. where there are no shares being bought) and these are excluded from the sum being calculated. 
for (i in 1:nrow(trading_period_df)) {
  if (!is.na(trading_period_df$trade_type[i]) && trading_period_df$trade_type[i] == "buy") {
    accumulated_shares <- accumulated_shares + 100
  }
    trading_period_df$accumulated_shares[i] <- accumulated_shares
}

# Finding the last day of trading (by locating the final row first), and setting trade_type to sell. 
trading_period_df[nrow(trading_period_df), "trade_type"] <- "sell"

# On the last day of trading, setting costs_proceeds to the total number of accumulated shares multiplied by the last day's share price. Note, the if statement similarly accounts for the existence of "NA"s. Altogether, the calculated value is positive and represents money returning back into the account (specifically the value of all the accumulated shares on the final day of trading where they are being sold).
for (i in 1:nrow(trading_period_df)) {
if (!is.na(trading_period_df$trade_type[i]) && trading_period_df$trade_type[i] == "sell") {
  trading_period_df$costs_proceeds[i] <- trading_period_df$accumulated_shares[i]*trading_period_df$close[i]
  }
}

```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Note the following PnL & ROI calculations are for the specified trading period in Step 3 (i.e. 2021-11-29 to 2022-10-14)

# Finding the value of the Total Profit & Loss by taking the sum of all values (excluding "NA"s) in the costs_proceeds column. Then printing the result so the final value is visible; note the negative symbol denotes that a total LOSS has been incurred. 
total_PnL <- sum(trading_period_df$costs_proceeds, na.rm = TRUE)

print(paste0("Total Profit & Loss: $", total_PnL))

# Finding the Total Capital Invested by subtracting the value of the final sale (located in the last row of the dataframe) from the Total Profit & Loss. The Total Capital Invested may be calculated this way since the trading algorithm only calls for buying stocks (never selling), aside from selling them all on the last day. Then printing the result so the final value is visible; note the total capital invested will ALWAYS be positive since this number denotes the amount of money spent on purchasing stocks. 
for (i in 1:nrow(trading_period_df)) {
if (!is.na(trading_period_df$trade_type[i]) && trading_period_df$trade_type[i] == "sell") {
  total_capital_invested <- total_PnL - trading_period_df$costs_proceeds[i] 
  }
}

if (total_capital_invested < 0) {
  total_capital_invested <- total_capital_invested*(-1)
}

print(paste0("Total Capital Invested: $", total_capital_invested))

# Substituting the calculated Total Profit & Loss and Total Capital Invested into the ROI formula. Then printing the result so the final Return on Interest is visible;
ROI <- (total_PnL/total_capital_invested)*100

print(paste0("Return on Interest: ", ROI, "%"))
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Start by creating another new data frame upon which the stop-loss mechanism will be applied (Option 2). Similarly to trading_periof_df, this new data frame will also be defined as a subset of the original amd_df with the same customised time frame. The following code builds off the original trading algorithm to form the stop-loss mechanism. 

stoploss_mechanism_df <- subset(amd_df, date >= start_date & date <= end_date)

# Customised trading period remains the same for comparison purposes; i.e. from 2021-11-29 to 2022-10-14. 
start_date <- "2021-11-29"
end_date <- "2022-10-14"

# Initialize columns for trade type, cost/proceeds, and accumulated shares in stoploss_mechanism_df. Note that a "running average" and "threshold" column has also been created, for the purpose of demonstrating the stop-loss mechanism. 
stoploss_mechanism_df$trade_type <- NA
stoploss_mechanism_df$costs_proceeds <- NA  # Corrected column name
stoploss_mechanism_df$accumulated_shares <- 0  # Initialize if needed for tracking
stoploss_mechanism_df$running_av <- 0 
stoploss_mechanism_df$threshold <- 0

# Initialize variables for trading logic. Now, notice that the variable "result" has been globally defined as it is involved in the calculation and output of the  "average_fun" function appearing later in the code. A new variable "av" denoting the running average is also initialised here. 
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
result <<- stoploss_mechanism_df$close[1]
av <- 0

# Ensure strings are not converted to factors. 
stringsAsFactors = FALSE

# Defining function that will be called upon later to calculate the running average of buy-in prices. By inputting the price of a stock on any given day, the function will add this value to the running average of all buy-in prices so far, then divide this by two (per the formula for calculating the mean or average). 
average_fun <- function(close_value) {
  result <- (result+close_value)/2
  return(result)
}

# If the current price is 70% of the running average thus far (30% drop) or less, then activate stoploss mechanism and mark as "sell". Then similarly to the original strategy, define that if the previous price = 0, OR the price of the current day is less than that of the previous day, set the trade to buy. (This process loops for each row in the dataframe). 
# Note that today's current price is then set to 'previous_price' for the next iteration of this loop occurring the following day i.e. tomorrow. The running average for each row is also noted in the data frame, as well as the threshold which is defined as 70% multiplied by the average purchase price that day. 
for (i in 1:nrow(stoploss_mechanism_df)) {
  if (stoploss_mechanism_df$close[i] <= av*(7/10)) {
    stoploss_mechanism_df$trade_type[i] <- "sell"
  }
  else{
    if (previous_price==0 || stoploss_mechanism_df$close[i] <         
        stoploss_mechanism_df$close[i-1]) {
        stoploss_mechanism_df$trade_type[i] <- "buy"
        av <- average_fun(stoploss_mechanism_df$close[i])
    }
  }
  previous_price <- stoploss_mechanism_df$close[i]
  stoploss_mechanism_df$running_av[i] <- av
  stoploss_mechanism_df$threshold[i] <- av*(7/10)
 }

# The sum of all shares bought is visible in the accumulated_shares column, where 100 is added each time there is a new purchase (i.e. the trade type is set to "buy"). The total accumulated shares are halved where there is a trade marked "sell" (as per the stoploss mechanism). Note that the if statement also accounts for the existence of "NA"s in the data frame (i.e. where there are no shares being bought) and these are excluded from the sum being calculated. 
for (i in 1:nrow(stoploss_mechanism_df)) {
  if (!is.na(stoploss_mechanism_df$trade_type[i]) && 
      stoploss_mechanism_df$trade_type[i] == "sell") {
      accumulated_shares <- accumulated_shares/2
  }
  if (!is.na(stoploss_mechanism_df$trade_type[i]) && 
      stoploss_mechanism_df$trade_type[i] == "buy") {
      accumulated_shares <- accumulated_shares + 100
  }
  stoploss_mechanism_df$accumulated_shares[i] <- accumulated_shares
}

# Showing the value of each purchase (i.e. "buy) using the current share price multiplied by the share_size value of 100 (negative value reflects money leaving the account). Specifically, if the trade type is set to "buy", then that day's price is multiplied by -100 and added to the costs_proceeds column. To then account for "sell" transactions, the accumulates shares are multiplied by the share price on that day, with this positive value recorded in the costs_proceeds column (representing funds returning back into the account). 
for (i in 1:nrow(stoploss_mechanism_df)) {
      stoploss_mechanism_df$costs_proceeds[i] <- 
     ifelse(stoploss_mechanism_df$trade_type[i] == "buy", 
            -share_size*stoploss_mechanism_df$close[i], 
            ifelse(stoploss_mechanism_df$trade_type[i] == "sell", 
                   stoploss_mechanism_df$accumulated_shares[i]*stoploss_mechanism_df$
                     close[i], NA))
}

# Finding the last day of trading (by locating the final row first), and setting trade_type to "sold".  
stoploss_mechanism_df[nrow(stoploss_mechanism_df), "trade_type"] <- "sold"

# On the last day of trading, setting costs_proceeds to the total number of accumulated shares multiplied by the last day's share price. Note, the if statement similarly accounts for the existence of "NA"s. Altogether, the calculated value is positive and represents money returning back into the account (specifically the value of all the accumulated shares on the final day of trading where they are being sold).
for (i in 1:nrow(stoploss_mechanism_df)) {
  if (!is.na(stoploss_mechanism_df$trade_type[i]) &&  
      stoploss_mechanism_df$trade_type[i] == "sold") {
      stoploss_mechanism_df$costs_proceeds[i] <-   
      stoploss_mechanism_df$accumulated_shares[i]*stoploss_mechanism_df$close[i]
  }
}

# Following PnL & ROI calculations are for the specified trading period in Step 3 (i.e. 2021-11-29 to 2022-10-14), when using the stop-loss mechanism. 

# Finding the value of the Total Profit & Loss by taking the sum of all values (excluding NAs) in costs_proceeds
total_PnL_stoploss <- sum(stoploss_mechanism_df$costs_proceeds, na.rm = TRUE)

print(paste0("Total Profit & Loss (Stop-Loss): ", total_PnL_stoploss))

# Finding the Total Capital Invested. First create a new variable "soldcapital", which is identified as all values in the costs_proceeds column that are greater than 0, hence indicating that shares were sold since money is returning into the account (thus a positive number). All these values are summed and then subtracted from the Total Profit & Loss to obtain the value of the Total Capital Invested. 
soldcapital <<- 0
for (i in 1:nrow(stoploss_mechanism_df)) {
  if (!is.na(stoploss_mechanism_df$trade_type[i]) &&   
      stoploss_mechanism_df$costs_proceeds[i] > 0) {
      soldcapital <<- soldcapital + stoploss_mechanism_df$costs_proceeds[i]
  }
}

print(paste0("Sold Capital (Stop-Loss): $", soldcapital))

total_capital_invested_stoploss <- total_PnL_stoploss - soldcapital

# Ensures that Total Capital Invested is always a positive value. 
if (total_capital_invested_stoploss < 0) {
   total_capital_invested_stoploss <- total_capital_invested_stoploss*(-1)
}

print(paste0("Total Capital Invested (Stop-Loss): $", total_capital_invested_stoploss))

# Substituting calculated Total Profit & Loss and Total Capital Invested into ROI formula. 
ROI_stoploss <- (total_PnL_stoploss/total_capital_invested_stoploss)*100

print(paste0("Return on Interest (Stop-Loss): ", total_PnL, "%"))
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Note ALL following financial metrics are for the chosen trading period as defined in Step 3 (i.e. 29/11/2021 to 14/10/2022). 

# Displaying values for financial metrics using the original trading strategy
print("Financial Metrics with Original Trading Strategy")
print(paste0("Total Profit & Loss: $", total_PnL))
print(paste0("Return on Interest (ROI): ", ROI, "%"))

# Displaying values for financial metrics using the stop-loss mechanism
print("Financial Metrics with Stop-Loss Mechanism")
print(paste0("Total Profit & Loss: $", total_PnL_stoploss))
print(paste0("Return on Interest (ROI): ", ROI_stoploss, "%"))
```
For the purposes of this discussion, all calculated metrics are printed here. Also note that all numerical values will be rounded and considered to two decimal places. 

**Question 1: Did your P/L and ROI improve over your chosen period?**

  The original trading algorithm saw a total loss of $552439.01 during the specified time period between the 29th November 2021 and the 14th October 2022. The loss was reduced by $214469.06 when the stop-loss mechanism was implemented during the same time period, ending with a total loss of $337969.95. Overall, it is evident that the financial loss incurred using the stop-loss mechanism was less than the original trading algorithm. In this sense, the stop-loss mechanism produced a better outcome. However, since both algorithms produced an overall loss, it may be concluded that neither strategy is particularly desirable. 
  
  The original trading algorithm saw a return on interest (ROI) of approximately -44.53% also during this time period. The loss was reduced by 15.01% when the stop-loss mechanism was implemented during the same time period, ending with a total ROI of -29.52%. Overall, it is similarly evident that the ROI using the stop-loss mechanism was greater than the original trading algorithm. In this sense, the stop-loss mechanism produced a better outcome. Again, however, since both algorithms produced an overall negative ROI, it may be concluded that neither strategy is particularly desirable. 
  
  The ultimate goal would be to develop a trading algorithm that returned figures that are positive and as high as possible, indicating overall profits and high returns on interest. This would be a desirable outcome. However, it should be taken into account that the defined trading period saw an overall mass decline in the value of the AMD stock from approximately the beginning to the end of 2022. And so, regardless of the trading strategy used including both the original algorithm and the stop-loss mechanism, it would be extremely difficult to generate high profits, if any at all, when attempting to buy and sell during this period. 
  
  
**Question 2: Relate your results to a relevant market event and explain why these outcomes may have occured.**

December 2021 lies at the beginning of the defined trading period. It is evident that the AMD stock hit a record high during this month, which may be attributed to its strong PC sales during the international COVID-19 pandemic. With many individuals working, studying, and experiencing entertainment at home during lockdown periods, the AMD stock value was driven up significantly as the large volume of computer sales appealed to investors. Furthermore, there had been a reported collaboration between AMD and Microsoft with the two companies working on new developments in the cloud computing market. There were also new AMD product announcements at this time, all being designed to compete with NVIDIA and Intel. The culmination of these market events toward the end of 2021 cultivated excitement and anticipation among investors, thus boosting their confidence and resulting in a local maximum stock value. 

However, after this initial spike, the AMD stock value declined significantly for the remainder of the defined trading period, lasting approximately 12 months. For the first half of 2022, their stock decline is largely attributed to the Federal Reserve persistently raising interest rates. This saw high inflation that negatively impacted AMD growth stocks, as it eroded purchasing power and saw increased costs for companies, negatively affecting AMD's profit margins. The macroeconomic impacts became particularly apparent when some of AMD's earnings reports failed to meet market expectations. Adding to the company's additional costs in 2022 was their acquisition of Xilinx. Although a strategic move, the short-term financial impacts of this large deal caused further concerns to investors. Consequently, additional pressure on their stock continued to drive its value down during the year. Overall, the various challenges AMD faced during 2022 resulted in the stock's overall declining value from an initial peak within the defined trading period. 
