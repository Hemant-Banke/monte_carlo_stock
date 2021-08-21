# Simulate and Compare Stock Portfolio against Nifty

### Aim :
1. Compare 1 year performance of a Portfolio against Nifty.
2. Using Monte Carlo Simulation to predict individual Stock Prices 20 days ahead.
3. Compare predicted performance of Portfolio against Nifty.

### Instructions :
- Subscribe for Yahoo Finance API in RapdiAPI.
- Create a file `.Rprofile` in the directory containing
```RAPIDAPI_KEY = "<YOUR API KEY>" ```
- Run `main.R`

### Comparing Portfolio performance against Nifty
We first get the last one year closing prices for all stocks in Portfolio. 
![Image for All Stocks](https://github.com/Hemant-Banke/monte_carlo_stock/blob/main/img/plot_stocks_data?raw=true)


This is used to calculate Portfolio value in last one year. Comparing this value to Nifty's last year performance we get
![Image for Portfolio v/s Nifty](https://github.com/Hemant-Banke/monte_carlo_stock/blob/main/img/plot_portfolio_nifty?raw=true)

If Amount **100** were invested in the Portfolio and Nifty it could have been **180.3286** in Portfolio and 
**146.2694** in Nifty as of now.

### Simulating Stock Prices
We use Monte Carlo Simulation to get Expected paths of Indivisual stocks 20days ahead.
For a random path the stock price is modelled using Geometric Brownian Motion.

For this purpose we first find out percentage changes in stock closing price on daily basis. 
![Percentage Change in ADANIENT](https://github.com/Hemant-Banke/monte_carlo_stock/blob/main/img/plot_percent_change?raw=true)

The standard deviation of percentage changes gives diffusion of Geometric Brownian Motion. 
The Drift can be found by ```mean - (variance/2)```

We generate 10 such random paths and take their Geometric Mean to find Expected Path.
![Image for Prediction of Stocks](https://github.com/Hemant-Banke/monte_carlo_stock/blob/main/img/plot_pred_stocks?raw=true)

The portfolio value is then calculated giving us the expected path for portfolio.

### Comparing Predicted performance againt Nifty
We generate prediction for Nifty similarly and compare the two expected paths.
![Predicted performance of Portfolio v/s Nifty](https://github.com/Hemant-Banke/monte_carlo_stock/blob/main/img/plot_pred_portfolio_nifty?raw=true)

If Amount **100** is invested in the Portfolio and Nifty it may be **105.0995** in Portfolio and **102.1087** in Nifty 
within next 20days.


### Resources
- [Monte Carlo Simulation in R with focus on Option Pricing](http://github.cohttps://towardsdatascience.com/monte-carlo-simulation-in-r-with-focus-on-financial-data-ad43e2a4aedf)
- [Monte Carlo Simulation in Finance and Risk Management](https://www.mygreatlearning.com/blog/monte-carlo-simulation-in-finance-risk-management/)
