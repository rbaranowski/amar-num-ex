#****  simulation function
simulate.asymptotic.model <- function(model, n) {
  
  returns <- amar.sim(n, model$coeff(n), model$scales(n), sigma=model$sigma)
  amar.des <- amar.design.matrix(returns, model$scales(n))
  amar.coef <- model$coefficients(n)
  conditional.mean <- amar.des %*% amar.coef
  
  list(returns = returns, conditional.mean = conditional.mean)
}

simulate.trading.model.with.real.vol <- function(model, volatility, n.days, day.length){
  
  n <- n.days * day.length
  returns <- amar.sim(n, model$coeff, model$scales, sigma = model$sigma) 
  
  amar.des <- amar.design.matrix(returns, model$scales)
  amar.coef <- model$coefficients
  conditional.mean <- amar.des %*% amar.coef
  
  k <- as.integer((length(volatility) - length(returns)) / day.length)
  
  returns <- returns * volatility[sample(k,1) * day.length+1:length(returns)]
  
  list(returns = returns, conditional.mean = conditional.mean)
  
}

simulate.trading.model <- function(model, n.days, day.length){
  
  n <- n.days * day.length
  returns <- amar.sim(n, model$coeff, model$scales, sigma = model$sigma) 
  
  amar.des <- amar.design.matrix(returns, model$scales)
  amar.coef <- model$coefficients
  conditional.mean <- amar.des %*% amar.coef
  
  list(returns = returns, conditional.mean = conditional.mean)
  
}


#****  estimation quality measures ##
log.ratio <- function(residuals.a, residuals.b) return(log(mean(residuals.a^2) / mean(residuals.b^2)))

hp.errors <- function(estimated.scales, true.scales, tolerance=5){
  
  n <- length(estimated.scales)
  k <- length(true.scales)
  
  d.mat <- abs(matrix(rep(estimated.scales, k), nrow=n) - matrix(rep(true.scales, n), nrow=n, byrow = TRUE))
  colnames(d.mat) <- true.scales
  rownames(d.mat) <- estimated.scales
  
  tp <- 0
  
  for(j in 1:k) {
    if(min(d.mat[,j]) <= tolerance) {
      tp <- tp + 1
      d.mat <- d.mat[-which.min(d.mat[,j]),,drop=FALSE]
      if(nrow(d.mat) == 0) break
    }
  }
  
  fp <- nrow(d.mat)
  
  return(list(tp=tp, fp=fp, fn = k - tp ))
  
}