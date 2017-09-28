# blackScholes.R
# Black-Scholes options pricing 
# Author: Kyle Loomis
# Date Updated: 9/27/17
# Summary: Stores all functions necessary to
#          calculate options price

# install.packages("fOptions")
# install.packages("quantmod")
library(fOptions)
library(quantmod)

# log returns - returns without NA values
logReturns <- function(priceTS) {
  return(na.omit(TTR::ROC(priceTS)))
}

# annualized volatility
calcVol <- function(priceTS) {
  return((sd(priceTS) * sqrt(250) * 100)/100)
}

# general black scholes model to calclate 
# call or put options 
blackScholes <- function(priceTS, type, strike, days, rf, carryCost) {
  GBSOption(TypeFlag = type, S = as.numeric(tail(priceTS, n=1)), X = strike, Time = days/365, 
            r = rf, b = carryCost, sigma = calcVol(logReturns(priceTS)),
            title = NULL, description = NULL)
}

# NVDA price
getSymbols("NVDA", src="google")
# 1 year worth of closed prices
price <- NVDA$NVDA.Close["2016-09-26::"]

# assumes risk-free rate of 0.0123
blackScholes(priceTS = price, type = "p", strike = 180.00, days = 1, 
             rf = 0.0123, carryCost = 0.0)

