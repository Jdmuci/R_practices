#VaR 
#This is just a numeric example (for academic porpuses)

#Statistically speaking, VaR describes the specified quantile or percentile of the projected distribution of profits or losses over the target horizon.
#The larger the VaR, the higher the risk of the portfolio.
#VaR allows the user to specify the confidence level to reflect individual risk-averseness


#Let mu = 0.003, sigma = 0.23, alpha = 5% and n =10,000.
#Then, the 95% VaR corresponds to the 500th smallest return generated from the simulation

# Data and function
n <- 1000
alp <- 0.05
k <- round(n*alp)
mu <- 0.003
sigma <- 0.23


R <- rnorm(n,mu,sigma)
SR <- sort(R)
VaR <- -SR[k]

#This simulation shows that VaR = 0.37018