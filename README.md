# Introductory Tools for Quantitative Research

## Resources
1. [R Style Guide](https://google.github.io/styleguide/Rguide.xml)
2. [R Resources (Free Courses, Books, Tutorials, & Cheatsheets)](https://paulvanderlaken.com/2017/08/10/r-resources-cheatsheets-tutorials-books/)

## Workflow
[Introduction to Quant Researh](introQR.R) - note the following main ideas:
1. Packages - installation/loading packages
1. Data sources - selecting data vendors
2. Cleaning - vectors, data frames, indexing
3. Visualization - packages to visualize data
4. Log returns - [Why Log Returns](https://quantivity.wordpress.com/2011/02/21/why-log-returns/)
5. Correlation - asset beta, correlation matrices, etc
6. Portfolio Analysis - sharpe ratio as a measure of 'alpha' 

## General Black-Scholes
blackScholes.R - prices put or call option with General Black-Scholes formula.
```
# test

install.packages("quantmod")
library(quantmod)

# NVDA price
getSymbols("NVDA", src="google")
# 1 year worth of closed prices
price <- NVDA$NVDA.Close["2016-09-26::"]

# assumes risk-free rate of 0.0123
blackScholes(priceTS = price, type = "c", strike = 180.00, days = 1, 
             rf = 0.0123, carryCost = 0.0)
```
