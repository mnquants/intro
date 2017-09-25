# introQR.R
# Introduction to Quantitative Research
# Author: Kyle Loomis
# Date Updated: 9/24/17
# Summary: This file is meant to highlight the quantitative
#          research workflow at a very high level.


# installing pacakges
install.packages("PerformanceAnalytics")

# loading packages
library(PerformanceAnalytics)
library(quantmod)
library(highcharter)
# OR
require(quantmod)

# notes:
# 1. don't order a program like I am doing - this is
#    only for explanatory reasons
# 2. consistency is necessary to faciliate effective
#    communication
# 3. naming variables - camelCase as variable name


# R uses vectors to store data
stocks <- c("NVDA","AMD","XOM","GS","CNP")
indexes <- c("^GSPC","^DJI","^IXIC")

# fetches timeseries stock data from symbolList
# vector from Yahoo! Finance (or Google, Wiki, etc)
dataFetch <- function(symbolList) {
  # initializes empty list
  dataList <- list()

  # applies getSymbols quantmod function
  # onto symbolList to fetch historical data
  dataList <-lapply(symbolList, function(x) { getSymbols(x,auto.assign=FALSE) })

  # applies symbolList names to name the dataList dataframe
  names(dataList) <- symbolList

  return(dataList)
}

# applies dataFetch function to the stocks and indexes vectors
stockData <- dataFetch(stocks)
indexData <- dataFetch(indexes)

# display stockData
stockData
# index each stock with stockData$STOCKNAME or
# stockData[n]
head(stockData$NVDA)

# issues with displaying the head of this
head(stockData[1])

# convert to a different data type to solve issues
head(data.frame(stockData[1]))

# index data by date
stockData$NVDA["2017-09-18::"]

# data visualization is very important!
# quantmod is a simple tool to visualize timeseries data
candleChart(stockData$NVDA)

# modify the chart to include name, date, and technical indicators
# notes:
#   1. technical analysis = pattern (technical) analysis
#   2. stochastic oscillators (technicals) give you insight into
#      the trends of an equity's price
candleChart(stockData$NVDA, type = 'candles', name = "NVDA", TA = 'addVo();addEMA();addWMA();addBBands();addRSI();addMACD();',
            theme = chartTheme('white'), up.col = 'green', dn.col = 'red', major.ticks = "1 month", show.grid = TRUE)

# chart visualization with highcharter
# notes:
#   1. generalizable functions should be developed then packaged
#   2. hierarchy of software
#   3. an xts object (with POSIX timestamp) is necessary for xts_chart
#      to work properly
xts_chart <- function(xts_df) {
  # require inside a function returns TRUE or FALSE
  # so suppressMessages is used to clean up output
  suppressMessages(require(quantmod))
  suppressMessages(require(highcharter))

  # notes:
  # 1. variable names can include '.'
  # 2. n referes to the number of periods the SMA is applied over

  # what happens when we adjust the n to something very large?
  xts_df.SMA.10 <- TTR::SMA(Cl(xts_df), n = 5)
  xts_df.SMA.200 <- TTR::SMA(Cl(xts_df), n = 100)
  xts_df.RSI.14 <- TTR::RSI(Cl(xts_df))
  xts_df.RSI.SellLevel <- xts(rep(70, NROW(xts_df)), index(xts_df))
  xts_df.RSI.BuyLevel <- xts(rep(30, NROW(xts_df)), index(xts_df))

  # issues using stockData$NVDA in a generalizable program?
  xts_df.Volume <- stockData$NVDA[,5]
  highcharter::highchart(type = "stock") %>%
    # notes:
    # 1. %>% is heavily used in dplyr - a package used for data cleaning
    # 2. works like a pipe '|' - passes data on LHS to function on RHS
    highcharter::hc_yAxis_multiples(
      create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
    ) %>%
    # add series data (incling technicals)
    highcharter::hc_add_series(xts_df, yAxis = 0, name = "NVDA") %>%
    highcharter::hc_add_series(xts_df.SMA.10, yAxis = 0, name = "Fast MA") %>%
    highcharter::hc_add_series(xts_df.SMA.200, yAxis = 0, name = "Slow MA") %>%
    highcharter::hc_add_series(xts_df.Volume, color = "gray", yAxis = 1, name = "volume", type = "column") %>%
    highcharter::hc_add_series(xts_df.RSI.14, yAxis = 2, name = "Osciallator", color = hex_to_rgba("green", 0.7)) %>%
    highcharter::hc_add_series(xts_df.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                               yAxis = 2, name = "Sell level") %>%
    highcharter::hc_add_series(xts_df.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                               yAxis = 2, name = "Buy level")
}

# plot the chart
# notes:
#   1. export to .html file to be embedded in website - real-time updates
#   2. highcharter is a very good tool for backtesting
#   3. API allows for the modification of charts (ex. display
#      buying and selling opportunities with green/red arrow)
xts_chart(stockData$NVDA)

# why log returns vs price or raw returns?
# notes:
# 1. READ THIS: https://quantivity.wordpress.com/2011/02/21/why-log-returns/
# 1. normalization - var-covar matrix
# 2. log normality (assumption of normally distributed returns)
# 3. algorithmic complexity - from n(0) to n(1) multiplications
logReturns <- function(timeseriesVec) {
  return(TTR::ROC(timeseriesVec))
}

# notes:
# 1. consistency - we usually use the closing (or WAP)
#    price as the timeseries vector for further analysis
# 2. this process can be generalized into a function
NVDA.Close <- stockData$NVDA[,4]
AMD.Close <- stockData$AMD[,4]

# notes:
# 1. NA values contained in a data set (vector, df, etc)
#    cause error with analysis / result in inconsistent data
# 2. omit the NA values with na.omit(data)
NVDA.Log <- na.omit(logReturns(NVDA.Close))
AMD.Log <- na.omit(logReturns(AMD.Close))

# calculate the correlation between log returns
cor(NVDA.Log,AMD.Log)

# what about the correlation YTD?
# notes:
# 1. timeframe of analysis matters!
cor(NVDA.Log["2017-01-01::"],AMD.Log["2017-01-01::"])


# why did the correlation of returns increase given the timeframe?
# why are periods of high correlation difficult to trade in?


# consolidate all log return vectors into a single dataframe
logReturnDF <- function(data) {
  df <- c()
  for (i in data) {
    # binds columns to existing vector which results in
    # a multi-dimensional matrix
    df <- cbind(df, na.omit(logReturns(i[,4])))
  }
  return(df)
}

# assign variable for repeatability
portfolioLogReturns <- logReturnDF(stockData)
indexLogReturns <- logReturnDF(indexData)

# view the data
head(portfolioLogReturns)

# correlation matrix
cor(portfolioLogReturns)

# covariance matrix
cov(portfolioLogReturns)


# takes in vector of log returns for a stock and an index
# and returns the beta - volatility of stock (or portfolio)
# relative to the index
calcBeta <- function(stock, index, riskFR) {
  # uses CAPM.beta method from Performance Analytics package
  # notes:
  # 1. PackageName::FunctionName(...) can also be achieved with:
  #     library(PackageName)
  #     FunctionName(...)
  return(PerformanceAnalytics::CAPM.beta(Ra = stock, Rb = index, Rf = riskFR))
}

# testing the beta of specific stocks
# notes:
# 1. selecting the right benchmark is very important

# beta of NVDA vs S&P 500
calcBeta(portfolioLogReturns[,1], indexLogReturns[,1], 0)
# beta of NVDA vs NASDAQ
calcBeta(portfolioLogReturns[,1], indexLogReturns[,3], 0)

# beta of each stock vs S&P 500
calcBeta(portfolioLogReturns, indexLogReturns[,1], 0)


# why does CNP have the lowest beta?
# notice anything odd about the Risk Free Rate?
# why would you want to use NASDAQ vs S&P 500 as the benchmark?


# sharpe ratio calculates risk-adjusted-return of portfolio
# (R - Rf) / volatility
# notes:
# 1. doesn't include Risk-Free Rate in this example
# 2. doesn't include weights for each asset in the portfolio
sharpeRatio <- function(data) {
  # combines returns of the data into consolidated returns
  # both functions below are from PerformanceAnalytics package
  pReturn <- Return.excess(Return.portfolio(R = data))
  return(SharpeRatio(R = pReturn, FUN = "StdDev"))
}

sharpeRatio(portfolioLogReturns)


# should an investor want their portfolio
# to have a higher or lower sharpe ratio?
