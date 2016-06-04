require(mzar)
require(parallel)
#################################################################################
setwd("/home/rafal/Projects/MZAR/simulated examples")
#################################################################################

##################
### parameters ####
###################

## models ##

models <- list(
  list(
    name = "hd.alpha.constant",
    coefficients = function(n)  c(-0.115, -2.15, -15, 10) , 
    scales = function(n) as.integer(c(1, 20*log(n), 10 * log(n)^2, 20 * log(n)^2)),
    max.order = function(n) as.integer(20 * log(n)^2+100),
    sigma = 1
  ),
  list(
    name =  "hd.alpha.decreasing",
    coeff  = function(n)  c(-0.115, -2.15, -15, 10) / log(log(n)), 
    scales = function(n) as.integer(c(1, 20*log(n), 10 * log(n)^2, 20 * log(n)^2)),
    max.order = function(n) as.integer(20 * log(n)^2+100),
    sigma = 1
  )
)


## simulation models functions##

simulate.model <- function(model, n) return(mzar.sim(n, model$coeff(n), model$scales(n), sigma=model$sigma))

## estimation quality measures ##
#deviation
dev <- function(residuals.a, residuals.b) return(100* (2*log(mean(residuals.a^2) / mean(residuals.b^2))))

#true positives and false positives
tp.fp.fn <- function(estimated.scales, true.scales, tolerance=5){
  
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

############################################################
##### single simulation function ###########################
############################################################

sim <- function(model, n, tolerance.fun = function(k) log(k), mzar.method="threshold"){

  
  model$name <- paste(model$name, mzar.method, sep=".")
  
  returns <- simulate.model(model, n)

  
  # fit mzar model using our training procedure to find correct scales and remove volatility
  timing <- proc.time()
  m <- mzar(returns, model$max.order(n), method = "threshold", verbose = FALSE)
  timing <- (proc.time() - timing)[3]

  
  #find deviation measure for mzar and ar 1
  tmp <- tp.fp.fn(m$scales, model$scales(n), tolerance.fun(n))
  fp <- tmp$fp
  tp <- tmp$tp
  fn <- tmp$fn
  
  tmp <- tp.fp.fn(m$scales, model$scales(n), 0)
  
  fp.exact <- tmp$fp
  tp.exact <- tmp$tp
  fn.exact <- tmp$fn
  
  return(data.frame(name = model$name,
                    n = n,
                    fp.exact = fp.exact,
                    tp.exact = tp.exact,
                    fn.exact = fn.exact,
                    fp = fp,
                    tp = tp,
                    fn = fn,
                    timing = as.numeric(timing), stringsAsFactors = FALSE))
  
}

##########################
##### sim parameters #####
##########################
seed <- 12345

n.list <- c(1500, 5000, 10000, 20000, 40000)
n.sim <- 128
total.runs <- n.sim * length(n.list) * length(models) * 2


set.seed(seed)

results <- data.frame(name = character(total.runs),
                  n = numeric(total.runs),
                  fp.exact = numeric(total.runs),
                  tp.exact = numeric(total.runs),
                  fn.exact = numeric(total.runs),
                  fp = numeric(total.runs),
                  tp = numeric(total.runs),
                  fn = numeric(total.runs),
                  timing = numeric(total.runs),
                  stringsAsFactors = FALSE)

no.res <- 0


for(model in models){
  for(n in n.list){
    
    for(j in 1:n.sim){
      
      
      cat("Model: ", model$name, " n: ", n, "repetition ", j, "\n")

      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n , mzar.method = "threshold")
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n , mzar.method = "bic")
      
    }
    
  }
}
    


save(results, file = "asymptotic.sim.results.RData")

