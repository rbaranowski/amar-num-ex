require(mzar)
require(wbs)
require(MASS)
### simulate a large mzar process ####
p <- 480
n <- 5500
alpha <-c(.04664633, 1.13961563,  -0.62686037) 
tau <- c(1,  148, 364)

beta <- rep(0,p)
for(j in 1:length(tau)) beta[1:tau[j]] <- beta[1:tau[j]] + alpha[j]/tau[j]

ts.plot(beta)

############

returns <- arima.sim(n = n, list(ar = beta))

ts.plot(returns)
acf(returns)
acf(abs(returns))


### try to estimate this model

X <- ARDesign(returns, p)
y <- ARResponse(returns, p)



### ols estimates
beta.ols <- coef(lm(y~0+X))
ts.plot(beta.ols)
w <- wbs(beta.ols)
w$res[order(w$res[,5], decreasing=TRUE)[1:length(alpha)],3]



### ridge estimates
beta.ridge <- coef(lm.ridge(y~0+X, lambda = n))
ts.plot(beta.ridge)
lines(beta)
w <- wbs(beta.ridge)
w$res[order(w$res[,5], decreasing=TRUE)[1:(length(alpha))],3]
tau


### splines estimates
D <- diag(p)
for(j in 2:p) D[j,j-1] <- -1
lambda <- nrow(X) 



beta.splines <- solve(t(X)%*%X + lambda * t(D)%*%D) %*% t(X) %*% y
ts.plot(beta.splines)
lines(beta)
w <- wbs(beta.splines)
w$res[order(w$res[,5], decreasing=TRUE)[1:(length(alpha))],3] 
tau



