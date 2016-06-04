require(mzar)
require(parallel)
#################################################################################
setwd("/home/rafal/Projects/MZAR/simulated examples")
#################################################################################
load("results/real.volatility.sim.results.RData")
###################
require(dplyr)
require(xtable)
head(results)


tmp <- results %>%
  group_by(name,n.days) %>%
  summarise(mzar.dev = mean(mzar.dev),
            ar1.dev = mean(ar1.dev),
            fp = mean(fp),
            fn = mean(fn),
            tp = mean(tp))
tmp <- as.data.frame(tmp)
xtable(tmp)
