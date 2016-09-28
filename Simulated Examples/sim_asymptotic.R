source("R/header.R")
source("R/misc.R")
source("R/models.R")

models <- list(model.as1, model.as2)
n.list <- c(2500, 5000, 10000, 25000, 50000)
# n.list <- c(10000)

############################################################
##### single simulation function ###########################
############################################################
sim <- function(model, n, amar.method="bic", threshold.const=NA){

  tmp <- simulate.asymptotic.model(model, n)

  returns <- tmp$returns
  conditional.mean <- tmp$conditional.mean
  # fit amar model using our training procedure to find correct scales and remove volatility
  timing <- proc.time()
  m <- amar(returns, model$max.order(n), method = amar.method, threshold.const=threshold.const, verbose = FALSE)
  timing <- (proc.time() - timing)[3]

  k <- min(c(length(m$fitted), length(conditional.mean)))

  #compare the obtained fit to the true conditional mean
  rel.pred.error <- mean((tail(m$fitted, k)- tail(conditional.mean, k))^2) / mean(conditional.mean^2)

  #find deviation measure for amar and ar 1
  tmp <- hp.errors(m$scales, model$scales(n), log(n))
  fp <- tmp$fp
  tp <- tmp$tp
  fn <- tmp$fn

  tmp <- hp.errors(m$scales, model$scales(n), 0)
  fp.exact <- tmp$fp
  tp.exact <- tmp$tp
  fn.exact <- tmp$fn

  return(data.frame(method = amar.method,
                    threshold.const = threshold.const,
                    n = n,
                    p = model$max.order(n),
                    fp.exact = fp.exact,
                    tp.exact = tp.exact,
                    fn.exact = fn.exact,
                    fp = fp,
                    tp = tp,
                    fn = fn,
                    rel.pred.error = rel.pred.error,
                    timing = as.numeric(timing), stringsAsFactors = FALSE))

}

##########################
##### sim parameters #####
##########################

total.runs <- mc * length(n.list) * 2

set.seed(seed)

for(model in models){
  
  results <- data.frame(name = character(total.runs),
                        threshold.const =  numeric(total.runs),
                        n = numeric(total.runs),
                        p = numeric(total.runs),
                        fp.exact = numeric(total.runs),
                        tp.exact = numeric(total.runs),
                        fn.exact = numeric(total.runs),
                        fp = numeric(total.runs),
                        tp = numeric(total.runs),
                        fn = numeric(total.runs),
                        rel.pred.error = numeric(total.runs),
                        timing = numeric(total.runs),
                        stringsAsFactors = FALSE)
  
  no.res <- 0
  
  for(n in n.list){
    
    for(j in 1:mc){
      
      
      cat("Model: ", model$name, " n: ", n, "repetition ", j, "\n")
      
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n , amar.method = "threshold", threshold.const = 0.25)
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n , amar.method = "threshold", threshold.const = 0.5)
      no.res <- no.res + 1
      results[no.res, ] <- sim(model, n , amar.method = "bic")
      
    }
    
  }
  
  save(results, file = sprintf("results/%s.RData", model$name))
}
    




