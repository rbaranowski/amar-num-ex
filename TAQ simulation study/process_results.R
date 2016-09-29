require(Rcpp)
require(TAQMNGR)
require(amar)
require(RcppEigen)
require(PerformanceAnalytics)
require(xtable)

#################################################################################

source("R/misc.R")
source("R/header.R")

results.row <- function(ticker, results){
  
res <-
  data.frame(
    ticker = c(sprintf("\\multirow{2}{*}{%s}",ticker), ""),
    method = c("AMAR", "AR"),
    r.sq = round(c(mean(results$r.sq.amar), mean(results$r.sq.ar)), 5),
    hr =  100 *round(c(mean(results$hr.amar), mean(results$hr.ar))-0.50, 3),
    sharpe = round(c(mean(results$returns.amar)/sd(results$returns.amar),
                     mean(results$returns.ar)/sd(results$returns.ar)), 3),
    zeros = c(sprintf("\\multirow{2}{*}{%.2f}",  mean(100 * results$zero.pct)), ""))
    
    if(res$r.sq[1] > res$r.sq[2]) {
      res$r.sq[1] <- sprintf("\\textbf{%.5f}", as.numeric(res$r.sq[1]))
      res$r.sq[2] <- sprintf("%.5f", as.numeric(res$r.sq[2]))
    }else{
      res$r.sq[2] <- sprintf("\\textbf{%.5f}", as.numeric(res$r.sq[2]))
      res$r.sq[1] <- sprintf("%.5f", as.numeric(res$r.sq[1]))
    }

    if(res$hr[1] >= res$hr[2]) {
      res$hr[1] <- sprintf("\\textbf{%s}", res$hr[1])
      res$hr[2] <- sprintf("%s", res$hr[2])
    }else{
      res$hr[2] <- sprintf("\\textbf{%s}", res$hr[2])
      res$hr[1] <- sprintf("%s", res$hr[1])
    }



    if(res$sharpe[1] >= res$sharpe[2]) {
      res$sharpe[1] <- sprintf("\\textbf{%s}", res$sharpe[1])
      res$sharpe[2] <- sprintf("%s", res$sharpe[2])
    }else{
      res$sharpe[2] <- sprintf("\\textbf{%s}", res$sharpe[2])
      res$sharpe[1] <- sprintf("%s", res$sharpe[1])
    }

  res
}






###################################################################################


bins <- c(300, 600)
tickers <- c( "AAPL", "BAC",  "CVX",  "CSCO",  "F", "GE", "GOOG",  "MSFT", "T")
max.timescale.days <- c(6, 12)

## tables with the results

for(max.timescale in max.timescale.days)
  for(bin in bins){
    aggregated.results <- data.frame()
    
    cat(bin, max.timescale, "\n")
  
    for(ticker in tickers){

      load(sprintf("results/%s_%s_%s.RData", ticker, bin, max.timescale))
      aggregated.results <- rbind(aggregated.results, results.row(ticker, results))
      
    }
    
    print(aggregated.results)
    
    aggregated.results.latex <- xtable(aggregated.results, 
                                  align = rep("l", ncol(aggregated.results)+1))
    
    print(aggregated.results.latex,
          include.rownames = FALSE,
          include.colnames = FALSE,
          sanitize.colnames.function = identity,
          sanitize.text.function = identity, 
          only.contents = TRUE, 
          file = sprintf("tables/taq_results_%s_%s.tex", 
                       bin, max.timescale),
          hline.after = 2 * c(1:length(tickers)))
    
    
  }

