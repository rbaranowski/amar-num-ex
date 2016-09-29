################################################################################
data.input <- "/home/rafal/Data/MZAR/TAQ raw data/"
data.output <- "/home/rafal/Data/MZAR/TAQ cleaned/"
source("R/misc.R")
source("R/header.R")

###################################################################################

price.type <- "LAST"
startDate <- 20070101
endDate <- 20131230

bin <- c(600)
split.pct <- c(1/2, 1/4, 1/4)
max.timescale <- 6
windows.days <- 252 

target <- "r.squared"

ticker <- c("AAPL")
price.columns <- c("FIRST",  "MIN",    "MAX",   "LAST",  "VWAP")


# Read data 
data <-  as.data.frame(TAQ.Read(data.output ,ticker,  startDate=startDate, endDate=endDate, bin=bin))
time.as.char <- rep(0, length(data$TIME))

for(j in 1:length(data$TIME)){
  hs <- data$TIME[j] %/% 3600
  mm <- (data$TIME[j] - hs * 3600)  %/%  60
  ss <- data$TIME[j] - hs * 3600 - mm * 60
  time.as.char[j] <- sprintf("%02d:%02d:%02d", hs,mm,ss)
}

data$TIMESTAMP <- as.POSIXct(paste(data$DATE, time.as.char), format="%Y%m%d %H:%M:%S",tz = "EST")

# Fill in NA's
for(id in which(!complete.cases(data))) if(id>1) data[id, price.columns]  <- data[id-1, price.columns]
day.time <- nrow(data[data[,'DATE']==data[1,'DATE'],])
max.order <- max.timescale * day.time
window.size <- max.order + day.time * windows.days

step.size <- (window.size - max.order) - (floor((window.size - max.order) * split.pct[1]) + floor((window.size - max.order) * split.pct[2]))
price <- data[,price.type]

returns <- c(0, diff(log(data[,price.type])))

returns <- c(0, diff(data[,price.type]) / data[-nrow(data), price.type])
returns[is.na(returns)] <- 0
no.windows <- floor((length(returns) - window.size)/step.size) + 1    
      
j <- 21
current.window <- 1:window.size + (j-1)*step.size
current.returns <- returns[current.window]
current.price <- price[current.window]

set.seed(seed)

m.fit <- amar.train(current.returns,
                    max.order,
                    remove.volatility = TRUE,
                    target = target,
                    split.pct = split.pct,
                    max.scales = 10,
                    verbose=TRUE,
                    M=25000,
                    rand.intervals=TRUE,
                    use.RcppEigen = TRUE,
                    RcppEigen.method = 1)

ts.plot(m.fit$ar.coefficients.train)
cor(m.fit$predicted.test, m.fit$response.test)
m.fit$criterion.test
m.fit$criterion.validate
m.fit$scales
#**** plot of the price series, return series, normalised returns
plot.data <- data.frame(time = data$TIMESTAMP[current.window],
                        price = current.price,
                        returns = current.returns,
                        normalised.returns = m.fit$x)


#*** price plot
file.name <- sprintf("%s_%s_price", ticker, bin)

pl <- ggplot(plot.data, aes(x=1:length(time), y=price))+ geom_line(aes(color="data"))+
  scale_color_manual(values=c("black")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(labels = function(x) as.yearmon(plot.data$time[as.integer(x)+1]))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#*** returns plot
file.name <- sprintf("%s_%s_returns", ticker, bin)

pl <- ggplot(plot.data, aes(x=1:length(time), y=returns))+ geom_line(aes(color="data"))+
  scale_color_manual(values=c("black")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(labels = function(x) as.yearmon(plot.data$time[as.integer(x)+1]))
pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()


tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()


#*** standardised returns plot
file.name <- sprintf("%s_%s_normalised_returns", ticker, bin)

pl <- ggplot(plot.data, aes(x=1:length(time), y=normalised.returns))+ geom_line(aes(color="data"))+
  scale_color_manual(values=c("black")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("")+
  ylab("")+
  scale_x_continuous(labels = function(x) as.yearmon(plot.data$time[as.integer(x)+1]))
file.name <- sprintf("%s_%s_normalised_returns", ticker, bin)
pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#*** Plot of the AR coefficeints and amar coeffcients

alpha <- amar.fit(m.fit$x[1:max(m.fit$validate.id)], m.fit$scales)$coefficients
cpt.opt <- m.fit$scales

beta.amar <- rep(0,max.order)
for(j in 1:length(cpt.opt)) beta.amar[1:cpt.opt[j]] <- beta.amar[1:cpt.opt[j]] + alpha[j]/cpt.opt[j]

plot.data <- data.frame(lag=0:max.order,
                        beta.amar = c(beta.amar[1], beta.amar),
                        beta.ar = c(NA, m.fit$ar.coefficients.train),
                        block = c(0, apply(matrix(rep(1:max.order, length(cpt.opt)), nrow=max.order) > matrix(rep(cpt.opt, max.order), nrow=max.order, byrow=TRUE), 1, sum)))

file.name <- sprintf("%s_%s_coeff", ticker, bin)

pl <-  ggplot(plot.data)+
  geom_hline(yintercept=0, color="gray", linetype=2)+
  geom_line(aes(x=lag, y=beta.ar, color="ar"))+
  geom_line(aes(x=lag, y=beta.amar, group=block,  color="amar"),  size=2)+
  xlab("lag")+
  ylab("")+
  theme_bw()+
  theme(axis.title.x=element_text(vjust=0.5)) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("black","red")) 

pl
pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()


#*** Plot of the acf functions - dirty returns
tmp <- acf(current.returns[m.fit$train.id], lag.max = max.order,  plot=FALSE)

plot.acf.data <- data.frame(lag = tmp$lag, 
                            acf=tmp$acf)

file.name <- sprintf("%s_%s_acf_dirty", ticker, bin)


pl <- ggplot(data=plot.acf.data[-1,], aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept= -qnorm((1 - 0.95)/2)/sqrt(length(m.fit$train.id)), linetype="dashed") + 
  geom_hline(yintercept= qnorm((1 - 0.95)/2)/sqrt(length(m.fit$train.id)), linetype="dashed") + 
  xlab("lag")+
  ylab("")+
  theme_bw()+
  theme(axis.title.x=element_text(vjust=0.5)) +
  theme(axis.title.y=element_text(angle=90, vjust=1))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

pl

#*** Plot of the acf functions - cleaned returns
tmp <- acf(m.fit$response.train, lag.max = max.order,  plot=FALSE)

plot.acf.data <- data.frame(lag = tmp$lag, 
                            acf=tmp$acf)

file.name <- sprintf("%s_%s_acf_cleaned", ticker, bin)


pl <- ggplot(data=plot.acf.data[-1,], aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept= -qnorm((1 - 0.95)/2)/sqrt(length(m.fit$train.id)), linetype="dashed") + 
  geom_hline(yintercept= qnorm((1 - 0.95)/2)/sqrt(length(m.fit$train.id)), linetype="dashed") + 
  xlab("lag")+
  ylab("")+
  theme_bw()+
  theme(axis.title.x=element_text(vjust=0.5)) +
  theme(axis.title.y=element_text(angle=90, vjust=1))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

pl


