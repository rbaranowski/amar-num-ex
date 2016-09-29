require(Rcpp)
require(TAQMNGR)
require(amar)
require(RcppEigen)
require(PerformanceAnalytics)

#################################################################################
data.input <- "/home/rafal/Projects/AMAR/Data/TAQ raw data/"
data.output <- "/home/rafal/Projects/AMAR/Data/TAQ cleaned/"

source("R/misc.R")
source("R/header.R")
###################################################################################

price.type <- "LAST"
startDate <- 20040101
endDate <- 20131230

bins <- c(300, 600)

split.pct <- c( 1/2, 1/4, 1/4)

# 
tickers <- c( "AAPL", "BAC",  "CVX",  "CSCO", "F", "GE", "GOOG",  "MSFT", "T")

price.columns <- c("FIRST",  "MIN",    "MAX",   "LAST",  "VWAP")

target <- "r.squared"

initial.cash <- 500000
cost.per.trade <- 10
cost.per.share <- 0.0005
max.timescale.days <- c(5, 12)
windows.days <- as.integer(252)

set.seed(seed)

for(max.timescale in max.timescale.days)
  for(bin in bins)
    for(ticker in tickers){
    
      cat(sprintf("Analysing %s at %s bin frequency with p days %s", ticker, bin, max.timescale))
      TAQ.Aggregate(data.output ,  ticker, bin, useAggregated = TRUE)
      
      # Read data 
      data <-  TAQ.Read(data.output ,ticker,  startDate=startDate, endDate=endDate, bin=bin)
    

      # Fill in NA's
      for(id in which(!complete.cases(data))) if(id>1) data[id, price.columns]  <- data[id-1, price.columns]
      day.time <- nrow(data[data[,'DATE']==data[1,'DATE'],])
      max.order <- max.timescale * day.time
      ar.max.order <- day.time 
      
      window.size <- c(max.order + windows.days  * day.time)
      
      step.size <- (window.size - max.order) - (floor((window.size - max.order) * split.pct[1]) + floor((window.size - max.order) * split.pct[2]))
      returns <- c(0, diff(log(data[,price.type])))
      returns[is.na(returns)] <- 0
      

      
      no.windows <- floor((length(returns) - window.size)/step.size) + 1    
      
      results <- data.frame(
        date = integer(no.windows),
        time = integer(no.windows),
        r.sq.amar = numeric(no.windows),
        r.sq.ar = numeric(no.windows),
        r.sq.amar.val = numeric(no.windows),
        r.sq.ar.val = numeric(no.windows),
        hr.amar = numeric(no.windows),
        hr.ar = numeric(no.windows),
        zero.pct = numeric(no.windows),
        cash.amar = rep(0, no.windows),
        cash.ar = rep(0, no.windows),
        returns.amar = rep(0, no.windows),
        returns.ar = rep(0, no.windows)
      )
      
      scales <- matrix(0, no.windows,  max.order)
      ar.order <- rep(0, no.windows)
      
      
      for(j in 1:no.windows){
        
        current.window <- 1:window.size + (j-1)*step.size
        current.returns <- returns[current.window]
        results$zero.pct[j] <- mean(current.returns==0)
        
        results$date[j] <- data[current.window[1], "DATE"]
        results$time[j] <- data[current.window[1], "TIME"]
        
        cat(ticker, " Window starting on", results$date[j], ", ", results$time[j], "\n")
        cat("Fitting AMAR model...\n")
  
        
        m.fit <- amar.train(current.returns, 
                            max.order, 
                            remove.volatility = TRUE,
                            target = target, 
                            split.pct = split.pct,
                            max.scales = 10,
                            verbose=FALSE,
                            rand.intervals = FALSE, 
                            intervals = t(combn(max.order,2)),
                            parallel = FALSE)

        print(m.fit$scales)

        
        scales[j, m.fit$scales] <- 1
        
        cat("Fitting AR model...\n")
        a.fit <- ar.train(m.fit$x[c( m.fit$train.id[1] -  ((ar.max.order):1), m.fit$train.id, m.fit$validate.id, m.fit$test.id)],
                          ar.max.order, 
                          remove.volatility = FALSE,
                          target = target, 
                          split.pct = split.pct,
                          verbose=FALSE)
        
        ar.order[j] <- a.fit$opt.order
        print(a.fit$opt.order)
        
        results$hr.amar[j] <- hit.rate(m.fit$response.test, m.fit$predicted.test)
        results$hr.ar[j] <- hit.rate(a.fit$response.test, a.fit$predicted.test)
        
        results$r.sq.amar[j] <- r.squared(m.fit$response.test, m.fit$predicted.test)
        results$r.sq.ar[j] <- r.squared(a.fit$response.test, a.fit$predicted.test)
        
        results$r.sq.amar.val[j] <- m.fit$criterion.validate
        results$r.sq.ar.val[j] <-  a.fit$criterion.validate
        
        last.price <- data[current.window[m.fit$test.id], "FIRST"]
        
        n.days <- length(m.fit$predicted.test) / day.time
        
        for(k in 1:n.days)
          results$cash.amar[j] <-
          eval.strategy(m.fit$predicted.test[1:day.time + (k-1)*day.time],
                      last.price[1:day.time + (k-1)*day.time],
                      ifelse(k==1, initial.cash, results$cash.amar[j]),
                      cost.per.trade = cost.per.trade,
                      cost.per.share = cost.per.share,
                      cost = "per.trade",
                      verbose=FALSE)

        for(k in 1:n.days)
          results$cash.ar[j] <-
          eval.strategy(a.fit$predicted.test[1:day.time + (k-1)*day.time],
                        last.price[1:day.time + (k-1)*day.time],
                        ifelse(k==1, initial.cash, results$cash.ar[j]),
                        cost.per.trade = cost.per.trade,
                        cost.per.share = cost.per.share,
                        cost = "per.trade",
                        verbose=FALSE)
        
        # results$cash.ar[j] <-
        #   eval.strategy(a.fit$predicted.test,
        #                 last.price,
        #                 initial.cash,
        #                 cost.per.trade = cost.per.trade,
        #                 cost.per.share = cost.per.share,
        #                 cost = "per.trade",
        #                 verbose=FALSE)

        print(results[j,])

      }
      
      results$returns.amar <- (results$cash.amar-initial.cash) / initial.cash
      results$returns.ar <- (results$cash.ar-initial.cash) / initial.cash
        
      save(results, scales, ar.order, file=sprintf("results/%s_%s_%s.RData", ticker, bin, max.timescale))
      
    }


f <- function(results){
  round(c(median(results$r.sq.amar),
    median(results$r.sq.ar),
    median(results$returns.amar),
    median(results$returns.ar),
    median(results$hr.amar),
    median(results$hr.ar)), 5)
}

f(results)

