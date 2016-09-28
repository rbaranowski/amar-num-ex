#################################################################################
source("R/header.R")
source("R/misc.R")
source("R/models.R")
load("data/volatility.RData")

models <- list(model.three.cpts, model.four.cpts)
n.days.list <- c(90, 120, 180, 250)
day.length <- 80

############################################################
##### single simulation function ###########################
############################################################

sim <- function(model, n.days, target="r.squared", real.vol = TRUE){

  if(real.vol ==  TRUE){
    model$name = paste(model$name, "real_vol", sep="_")
    tmp <- simulate.trading.model.with.real.vol(model, volatility, n.days, day.length)
  }else{
    model$name = paste(model$name, "const_vol", sep="_")
    tmp <- simulate.trading.model(model, n.days, day.length)
  }
  
  returns <- tmp$returns
  conditional.mean <- c(rep(0, length(returns) - length(tmp$conditional.mean)), tmp$conditional.mean)
  
  ### Fit a amar model
  cat("Fitting amar model...\n")
  timing.amar <- proc.time()
  m <- amar.train(returns, model$max.order, target = target, remove.volatility = real.vol, verbose = FALSE)
  timing.amar <- (proc.time() - timing.amar)[3]

  #compare the obtained amar fit to the true conditional mean
  rel.pred.error.amar <- mean((m$predicted.test- conditional.mean[m$test.id])^2) / mean(conditional.mean[m$test.id]^2)
  n.scales <- length(m$scales)
    
    
  cat("Fitting ar model...\n")
  timing.ar <- proc.time()
  m <- ar.train(returns, model$max.order, target = target, remove.volatility = real.vol, verbose = FALSE)
  timing.ar <- (proc.time() - timing.ar)[3]
  
  rel.pred.error.ar <- mean((m$predicted.test- conditional.mean[m$test.id])^2) / mean(conditional.mean[m$test.id]^2)
  ar.order <- m$opt.order

  return(data.frame(target = target,
                    n.days = n.days,
                    p = model$max.order,
                    n.scales = n.scales,
                    ar.order = ar.order,
                    rel.pred.error.amar = rel.pred.error.amar,
                    rel.pred.error.ar = rel.pred.error.ar,
                    timing.amar = as.numeric(timing.amar), 
                    timing.ar = as.numeric(timing.ar), 
                    stringsAsFactors = FALSE))
  
}

##########################
##### sim parameters #####
##########################

total.runs <- mc * length(n.days.list)

set.seed(seed)

# simulations with real volatility
for(model in models){
  
  results <- data.frame(target = character(total.runs),
                        n.days = numeric(total.runs),
                        p = numeric(total.runs),
                        n.scales = numeric(total.runs),
                        ar.order = numeric(total.runs),
                        rel.pred.error.amar = numeric(total.runs),
                        rel.pred.error.ar = numeric(total.runs),
                        timing.amar = numeric(total.runs),
                        timing.ar = numeric(total.runs),
                        rel.pred.error = numeric(total.runs),
                        timing = numeric(total.runs),
                        stringsAsFactors = FALSE)
  
  no.res <- 0
  
  for(n.days in n.days.list){
    
    for(j in 1:mc){
      
      cat("Model: ", model$name, " n.days: ", n.days, "repetition ", j, ", real volatility, \n")
      
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n.days , target = "r.squared", real.vol = TRUE)
      print(results[no.res, ])
      
    }
    
  }
  
  save(results, file = sprintf("results/%s_real_vol.RData", model$name))
}


# simulations with constant volatility
for(model in models){
  
  results <- data.frame(target = character(total.runs),
                        n.days = numeric(total.runs),
                        p = numeric(total.runs),
                        n.scales = numeric(total.runs),
                        ar.order = numeric(total.runs),
                        rel.pred.error.amar = numeric(total.runs),
                        rel.pred.error.ar = numeric(total.runs),
                        timing.amar = numeric(total.runs),
                        timing.ar = numeric(total.runs),
                        rel.pred.error = numeric(total.runs),
                        timing = numeric(total.runs),
                        stringsAsFactors = FALSE)
  
  no.res <- 0

  for(n.days in n.days.list){
    
    for(j in 1:mc){
      
      cat("Model: ", model$name, " n.days: ", n.days, "repetition ", j, ", constant volatility, \n")
      
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n.days , target = "r.squared", real.vol = FALSE)
      print(results[no.res, ])
      
    }
    
  }
  
  save(results, file = sprintf("results/%s_const_vol.RData", model$name))
}
    



