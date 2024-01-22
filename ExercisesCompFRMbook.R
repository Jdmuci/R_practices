
#Exercises compilation, Simulation Techniques Financial Risk Management
#Academic porpuses



########## Brownian Motion 
#pg  15
par(mfrow=c(2,2)) 
BMsim <- function(npaths, nSamples){
  p <- npaths
  N <- nSamples
  y <- matrix(rep(0, (N+1)*p),nrow = N+1)
  t <- (c(0:N))/N
  for (j in 1: p){
    z <- rnorm(N,0,1)
    y[1,j] <- 0
    for (i in 1:N){
      y[i + 1,j] <- (1/sqrt(N))*sum(z[1:i])
    }
  }
  matplot(t, y, type = "l", xlab = "t", ylab = "Brownian Motion", lty = 1, col = 1)
}
BMsim(1,1000)
BMsim(5,1000)
BMsim(20,1000)
BMsim(100,1000)


########## VaR 
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


# definición de parámetros
###Define parameter####### 
check1 <- proc.time()  #set de initial time count
n <- 1000
SO <- 10
maturity <- 1
K <- 12 
r <- 0.05
sigma <- 0.4
nu <- r-sigma^2/2
S <- rep(0,n)
C <- rep(0,n)
CO <- rep(0,n)
Del <- rep(0,n)



########## MontecarloSimulation
z <- rnorm(n,0,1)
S <- SO*exp(nu*maturity+sigma*sqrt(maturity)*z)
C <- pmax(S-K,0)*exp(-r*maturity)
Del <- C*z/SO/sigma/sqrt(maturity)

# computar valores de CALL y DELTA 
Call <- mean(C)
Delta <- mean(Del)
check2 <- proc.time()   #checkpoint for method without Ito
Call
Delta

#the CPU time
check2-check1








########## EXAMPLE 7.4 pg 134 path-dependent options

N <- 10000
n <- 100
dt <- 1/n
r <- 0.03
sigma <- 0.4
SO <- 10 
K <- 12 
nu <- r-sigma^2/2
stock <- matrix(0,N,n+1)
y <- c(rep(0,N))
put <- c(rep(0,N))
boundary <- c(rep(0,n))
stock[,1] <- SO
check1 <- proc.time()   ## the fisrt check point

# generar rutas de precios de los activos --se corrió hasta aqui
## generate asset price path
for (i in 1:n) {
  stock[,i+1] <- stock[,i]*exp(nu*dt+sigma*sqrt(dt)*rnorm(N,0,1))
}
for(j in n:2){
  a <- which(stock[,j]<K)   #identificar rutas in-the-money
  if(length(a)>=3) { #ensure there is a solution for the regression
    #compute the conditional expectation function 
    S <- stock[a,j]
    A <- coef(lm(y[a]~ S + S^2, singular.ok = "T"))
    if(is.na(A[3])) { A[3]<- 0}
    X <- matrix( c(rep(1,N), stock[,j], stock[,j]^2), ncol = 3)
    put <- X%*%A
    put <- exp(-r*dt)*pmax(put ,0)
    
    # determinate Y and find boundary of K-S(t) < f(S(t))
    
    b <- which ( (K-stock[,j]) > put ) 
    y <- exp(-r*dt)*y
    y[b] <- K-stock[b,j] #assign "y" as K-S when K-S > P
    if (length(b)==0) {boundary[j] <- NA} #boundary cannot be estimated 
    else{boundary[j] <- max(stock[b,j])}
  }
  else{
    y <- exp(-r*dt)*y
    boundary[j] <- NA
  }
}

check2 <- proc.time()  #the second check point 

boundary[n+1] <- K 
boundary[1:20] <- NA  #give up the estimate for t<0.2
time <- c(0:n)/n
plot(time, boundary, type="h",ylim=c(0,K),xlab="time",ylab = "asset price")

price <- exp(-r*dt)*mean(y)
check2-check1 # check the CPU time for the valuation
price #price for American Put Option


#pg 147
S0 <- (10,10)
r <- 0.5
sigma1 <- 0.3
sigma2 <- 0.4
rho <- 0.2
T <- 0.5
N <- 10000
Z1 <- rnorm(N)
Z2 <- rnorm(N)
Z1 <- Z1
X2 <- Z1*rho*Z2*sqrt(1-rho^2)
S1 <- S0[1]*exp((r-sigma1^2/2)*T+sigma1*sqrt(T)*X1)
S2 <- S0[2]*exp((r-sigma2^2/2)*T+sigma2*sqrt(T)*X2)
C <- mean(pmax(S1-S2,0))*exp(-r*T)
C


#Excercise pg149
#Basket options are often valued by assuming that the value of the portfolio of assets comprising
#follows the Black-Scholes dynamics jointly rather than each assetfollows the B&S individually.

#### simulate the historical stock price ####
n <-250;  r<- 0.05; dt <- 1/n;
sigma <- c(0.4,0.3,0.35);
SIGMA <- matrix(c(1,0.3,0.3,0.3,1,0.3,0.3,0.3,1),3,3);
SIGMA <- SIGMA * ( sigma %*% t (sigma) )

S <- matrix(0,3,n+1);
S[,1] <- c(100,90,80);
for (i in 1:n) {
  S[, i+1] <- S[, i] *exp(r*dt+rnorm(3)%*%chol(SIGMA) *sqrt(dt)); 
}
P <- S[1,]+S[2,]+S[3,] ; 
print(P)
returnS <- diff(t(log(S))); 
returnP <- diff(log(P));

cor(returnS) 
A <- var(returnS)/dt
B <- var(returnP)/dt
C <- S[,n+1]

time <-(0:n)/n;
range <-c(min(S)*0.5,max(S)*1.2);

plot(time,S[1,], type = "l",xlim=c(0,1.6), ylim=range, ylab = "stock price") 
lines(time,S[2,], type = "l")
lines(time,S[3,], type = "l")
lines(c(1,1), range,type = "l")

#### cont’d #####
range <- c(min(S)*0.5,max(S)*1.6);
plot(time,S[1,], type = "l",xlim=c(0,1.6), ylim = range, ylab = "stock price") 
lines(time,S[2,], type = "l")
lines(time,S[3,], type = "l")
lines(c(1,1),range,type = "l") 
print(C)


#### simulate scenarios ### 
K <- 250; t <- 0.5;
d1 <- (log(sum(C)/K)+(r+B^2/2)*t)/sqrt(B*t);
d2 <- d1-sqrt(B*t);
sum(C)*pnorm(d1)-K*exp(-r*t)*pnorm(d2);

N <- 20000;
S <- matrix(0,N,3); 
S[,1] <- C[1]; S[,2] <- C[2]; S[,3] <- C[3];

for (i in 1:125){
S <- S*exp(r*dt+matrix(rnorm(3*N) ,N,3)%*%chol(A)*sqrt(dt)) ;
}

P <- S[,1]+S[,2]+S[,3] ;
price <- exp(-r*T)*pmax(P-K,0);
mean(price)

for (i in 1:20) {
lines(c(1,1.5),c(C[1],S[i,1]),type = "l") 
lines(c(1,1.5),c(C[2],S[i,2]),type = "l") 
lines(c(1,1.5),c(C[3],S[i,3]),type = "l") 
}
