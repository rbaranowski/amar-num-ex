require(mzar)
require(parallel)
#################################################################################
setwd("/home/rafal/Projects/MZAR/simulated examples")
#################################################################################
data.input <- "/home/rafal/Data/MZAR/TAQ raw data/"
data.output <- "/home/rafal/Data/MZAR/TAQ cleaned/"
#################################################################################
load("data/volatility.RData")
###################
### parameters ####
###################

## models ##

models <- list(
  list(
    name = "3cpts",
    coefficients = c(-0.15, 0.25, -3.15), 
    scales = c(1, 12,  5*80),
    max.order = 6*80,
    sigma = 2
  ),
  list(
    name =  "4cpts",
    coeff  = c(-0.15, 0.25, -3.15, -5.5),
    scales = as.integer(c(1, 12,   80, 5*80)),
    max.order = 6*80,
    sigma = 2
  )
)


## simulation models functions##

simulate.model.with.volatility <- function(model, volatility, n.days, day.length){
  
  n <- n.days * day.length
  returns <- mzar.sim(n, model$coeff, model$scales, sigma = model$sigma) 
  k <- as.integer((length(volatility) - length(returns)) / day.length)
  
  returns * volatility[sample(k,1) * day.length+1:length(returns)]
  
  
}

simulate.model <- function(model, n.days, day.length){
  
  n <- n.days * day.length
  returns <- mzar.sim(n, model$coeff, model$scales) 
  returns
  
}



## estimation quality measures ##
#deviation
dev <- function(residuals.a, residuals.b) return(100* (-2*log(mean(residuals.a^2) / mean(residuals.b^2))))
#true positives and false positives
tp.fp.fn <- function(estimated.scales, true.scales, tolerance=5){
  
  n <- length(estimated.scales)
  k <- length(true.scales)

  d.mat <- abs(matrix(rep(estimated.scales, k), nrow=n) - matrix(rep(true.scales, n), nrow=n, byrow = TRUE))
  colnames(d.mat) <- true.scales
  rownames(d.mat) <- estimated.scales
  
  tp <- 0
    
  for(j in 1:k) {
    if(min(d.mat[,j]) < tolerance) {
      tp <- tp + 1
      d.mat <- d.mat[-which.min(d.mat[,j]),,drop=FALSE]
      if(nrow(d.mat) == 0) break
    }
  }
  
  fp <- nrow(d.mat)

  return(list(tp=tp, fp=fp, fn = k - tp ))
      
}

############################################################
##### single simulation function ###########################
############################################################
sim <- function(model, n.days, day.length, real.vol = TRUE, tolerance=10){

  if(real.vol ==  TRUE){
    model$name = paste(model$name, "real.vol", sep=".")
    returns <- simulate.model.with.volatility(model, volatility, n.days, day.length)
  }else{
    model$name = paste(model$name, "constant.vol", sep=".")
    returns <- simulate.model(model, n.days, day.length)
  }
  
  # fit mzar model using our training procedure to find correct scales and remove volatility
  
  cat("Fitting mzar model...\n")
  timing.mzar <- proc.time()
  m <- mzar.train(returns, model$max.order, target = "r.squared", remove.volatility = real.vol, verbose = FALSE)
  timing.mzar <- (proc.time() - timing.mzar)[3]
  
  
  # fit mzar model with known scales and cleaned data
  m.known.scales <- mzar.fit(x = m$x[c(m$train.id[1] - (max(model$scales):1), m$train.id)],
                             scales = model$scales)
  cat("Fitting ar1 model...\n")
  # fit ar 1 model
  m.ar1 <- ar.train(x = m$x[c(m$train.id[1]-1, m$train.id, m$validate.id, m$test.id)],
                    max.order = 1, target = "r.squared", remove.volatility = FALSE, verbose = FALSE)
  
  cat("Fitting arp model...\n")
  # fit ar p model
  arp.max.order <- min(c(floor(sqrt(length(returns))), model$max.order))
  timing.ar <- proc.time()
  m.arp <- ar.train(x = m$x[c(m$train.id[1]-1:arp.max.order, m$train.id, m$validate.id, m$test.id)],
                    max.order = arp.max.order, target = "r.squared", remove.volatility = FALSE, verbose = FALSE)
  timing.ar <- (proc.time()-timing.ar)[3]
  
  #find residuals on the thest set for all models
  m.residuals <- m$response.test - m$predicted.test
  m.known.scales.residuals <- m$response.test - predict(m.known.scales,
                                                        newx = mzar.design.matrix(m$x[c(m$test.id[1] - (max(model$scales):1), m$test.id)],  model$scales))
  
  m.ar1.residuals <- m$response.test - m.ar1$predicted.test
  m.arp.residuals <- m$response.test - m.arp$predicted.test
  #find deviation measure for mzar and ar 1
  
  mzar.dev <- dev(m.residuals, m.known.scales.residuals)
  ar1.dev <- dev(m.ar1.residuals, m.known.scales.residuals)
  arp.dev <- dev(m.arp.residuals, m.known.scales.residuals)
  
  tmp <- tp.fp.fn(m$scales, model$scales, tolerance)
  
  fp <- tmp$fp
  tp <- tmp$tp
  fn <- tmp$fn
  
  return(data.frame(name = model$name,
                    n.days = as.integer(n.days),
                    mzar.dev = mzar.dev,
                    ar1.dev = ar1.dev,
                    arp.dev = arp.dev,
                    arp.order = as.integer(m.arp$opt.order),
                    fp = as.integer(fp),
                    tp = as.integer(tp),
                    fn = as.integer(fn),
                    timing.mzar = as.numeric(timing.mzar),
                    timing.ar = as.numeric(timing.ar),
                    stringsAsFactors = FALSE))
  
}

##########################
##### sim parameters #####
##########################
seed <- 12345

day.length <- 80 # equivalent to one trading day in 5 minutes intervals
n.days.list <- c(40, 90, 120, 220)
n.sim <- 1024 * 16
total.runs <- n.sim * length(n.days.list) * length(models) * 2


set.seed(seed)

results <- data.frame(name = character(total.runs),
                  n.days = integer(total.runs),
                  mzar.dev = numeric(total.runs),
                  ar1.dev = numeric(total.runs),
                  arp.dev = numeric(total.runs),
                  arp.order = integer(total.runs),
                  fp = integer(total.runs),
                  tp = integer(total.runs),
                  fn = integer(total.runs),
                  timing.mzar = numeric(total.runs),
                  timing.arp = numeric(total.runs),
                  stringsAsFactors = FALSE)

no.res <- 0

for(model in models){
  for(n.days in n.days.list){
    
    for(j in 1:n.sim){
      
      
      cat("Model: ", model$name, " n.days: ", n.days, "repetition ", j, "\n")
      
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n.days = n.days, real.vol = TRUE, day.length = day.length)
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n.days = n.days, real.vol = FALSE, day.length = day.length)
      
    }
    
  }
}
    


save(results, file = "results/real.volatility.sim.results.RData")

