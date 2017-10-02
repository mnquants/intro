# blackScholes.R
# Black-Scholes Option Pricing
# Author: Kyle Loomis
# Date Updated: 9/27/17
# Summary: Stores all functions necessary to
#          calculate options price

# install.packages("fOptions")
# install.packages("quantmod")
library(fOptions)
library(quantmod)

#' Calculate Log Returns
#' @param priceTS Price Timeseries
#' @return Log Returns  
#' @export
logReturns <- function(priceTS) {
  return(na.omit(TTR::ROC(priceTS)))
}

#' Calculate Annualized Volatility
#' @param priceTS Price Timeseries
#' @return Annualized Volatility 
#' @export
calcVol <- function(priceTS) {
  return((sd(logReturns(priceTS)) * sqrt(250) * 100)/100)
}

#' General Black-Scholes formula 
#' @param priceTS Price Timeseries
#' @param type Call, "c", or Put, "p"
#' @param strike Strike Price
#' @param days Number of Days
#' @param rf Risk-Free Rate
#' @param carryCost Cost-of-Carry 
#' @return Options Data
#' @export
blackScholes <- function(priceTS, type, strike, days, rf, carryCost) {
  GBSOption(TypeFlag = type, S = as.numeric(tail(priceTS, n=1)), X = strike, Time = days/365, 
            r = rf, b = carryCost, sigma = calcVol(priceTS),
            title = NULL, description = NULL)
}

# NVDA price
getSymbols("NVDA", src="google")
# 1 year worth of closed prices
price <- NVDA$NVDA.Close["2016-09-26::"]

# assumes risk-free rate of 0.0123
blackScholes(priceTS = price, type = "c", strike = 180.00, days = 1, 
             rf = 0.0123, carryCost = 0.0)