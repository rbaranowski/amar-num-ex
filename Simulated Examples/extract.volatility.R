require(TAQMNGR)
require(Rcpp)
require(amar)

# ** Path to TAQ data downloaded from WRDS database and processed with TAQMNGR package
data.path <- "/home/rafal/Projects/AMAR/Data/TAQ cleaned/" 

# ** set parameters
bin <- 300
price.type <- "LAST"
tickers <- c("BAC")
startDate <- 20060101
endDate <- 20131230

# ** read in data and get returns
data <-  TAQ.Read(data.path, "BAC",  startDate=startDate, endDate=endDate, bin=bin)
x <- data[,price.type]
returns <- diff(log(x))
returns[is.na(returns)] <- 0

# ** extract volatility from the data using exponential smoothing
volatility <- remove.volatility(returns, train.id = 1:(length(returns/2)))$volatility
save(volatility, file="data/volatility.RData")
