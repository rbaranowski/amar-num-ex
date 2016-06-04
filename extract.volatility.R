require(TAQMNGR)
require(Rcpp)
require(mzar)
#################################################################################
setwd("/home/rafal/Projects/MZAR/simulated examples")
#################################################################################
data.input <- "/home/rafal/Data/MZAR/TAQ raw data/"
data.output <- "/home/rafal/Data/MZAR/TAQ cleaned/"
#################################################################################
###################
### parameters ####
###################
bin <- 300
price.type <- "LAST"
tickers <- c("BAC")
startDate <- 20060101
endDate <- 20131230

data <-  TAQ.Read(data.output, "BAC",  startDate=startDate, endDate=endDate, bin=bin)
x <- data[,price.type]
returns <- diff(log(x))
returns[is.na(returns)] <- 0

volatility <- remove.volatility(returns, train.id = 1:(length(returns/2)))$volatility
save(volatility, file="data/volatility.RData")

