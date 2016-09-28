source("R/header.R")
source("R/misc.R")
source("R/models.R")

n <- 2500
lag.max <- 400

set.seed(seed)

#**** model as1
x <- simulate.asymptotic.model(model.as1, n)$returns
model.as1$scales(n)
acf(x, lag.max = 400)

#* plot data
plot.data <- cbind(data.frame(time=1:n,
                              x=tail(x, n)))

pl <- ggplot(plot.data) +
  geom_line(aes(x=time, y=x, color="data")) +
  scale_color_manual(values=c("black")) +
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")

file.name <- paste(model.as1$name, "example", sep="_")

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#* plot acf
tmp <- acf(x, lag.max = lag.max,  plot=FALSE)

plot.acf.data <- data.frame(lag = tmp$lag, 
                            acf=tmp$acf)

file.name <- paste(model.as1$name, "example_acf", sep="_")


pl <- ggplot(data=plot.acf.data[-1,], aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept= -qnorm((1 - 0.95)/2)/sqrt(length(plot.data$x)), linetype="dashed") + 
  geom_hline(yintercept= qnorm((1 - 0.95)/2)/sqrt(length(plot.data$x)), linetype="dashed") + 
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

#* plot maximum of the char root
plot.data <- read.csv("data/max_roots_as1.csv", header = FALSE)
colnames(plot.data) <- c("n", "mod")

pl <- ggplot(plot.data) +
  geom_line(aes(x=n, y=mod, color="data")) +
  scale_color_manual(values=c("black")) +
  theme_bw()+
  theme(legend.position="none")+
  xlab("$T$")+
  ylab("")+
  geom_hline(yintercept= 1, linetype="dashed")

file.name <- paste(model.as1$name, "example_char_root", sep="_")

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()


#**** model as2
x <- simulate.asymptotic.model(model.as2, n)$returns
model.as2$scales(n)
acf(x, lag.max = 400)

#* plot data
plot.data <- cbind(data.frame(time=1:n,
                              x=tail(x, n)))

pl <- ggplot(plot.data) +
  geom_line(aes(x=time, y=x, color="data")) +
  scale_color_manual(values=c("black")) +
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")

file.name <- paste(model.as2$name, "example", sep="_")

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#* plot acf
tmp <- acf(x, lag.max = lag.max,  plot=FALSE)

plot.acf.data <- data.frame(lag = tmp$lag, 
                            acf=tmp$acf)

file.name <- paste(model.as2$name, "example_acf", sep="_")


pl <- ggplot(data=plot.acf.data[-1,], aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept= -qnorm((1 - 0.95)/2)/sqrt(length(plot.data$x)), linetype="dashed") + 
  geom_hline(yintercept= qnorm((1 - 0.95)/2)/sqrt(length(plot.data$x)), linetype="dashed") + 
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

#* plot maximum of the char root
plot.data <- read.csv("data/max_roots_as2.csv", header = FALSE)
colnames(plot.data) <- c("n", "mod")

pl <- ggplot(plot.data) +
  geom_line(aes(x=n, y=mod, color="data")) +
  scale_color_manual(values=c("black")) +
  theme_bw()+
  theme(legend.position="none")+
  xlab("$T$")+
  ylab("")+
  geom_hline(yintercept= 1, linetype="dashed")

file.name <- paste(model.as2$name, "example_char_root", sep="_")

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()