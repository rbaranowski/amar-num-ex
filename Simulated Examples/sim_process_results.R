#################################################################################
setwd("/home/rafal/Projects/amar/numerical examples")
#################################################################################
source("R/header.R")
source("R/misc.R")
source("R/models.R")
require("plyr")
require("xtable")

#################################################################################
###### The asymptotic example                                            ########
#################################################################################
files <- c("as1.RData", "as2.RData")
labels <- c("as_3_cpts", "as_4_cpts")

for(j in 1:length(files)){
  
  file <- files[[j]]
  label <- labels[[j]]
  
  
  load(sprintf("results/%s", file))

  results.table <- 
    ddply(results,~ name + threshold.const+ n ,function(df){
    
    res <- data.frame(
      n = as.integer(df$n[1]),
      p = as.integer(df$p[1]),
      fp.exact = round(mean(df$fp.exact), 1),
      tp.exact = round(mean(df$tp.exact), 1),
      fn.exact = round(mean(df$fn.exact),1),
      fp = round(mean(df$fp),1),
      tp = round(mean(df$tp),1),
      fn = round(mean(df$fn),1),
      rel.pred.error = round(mean(df$rel.pred.error),3),
      timing = round(mean(df$timing),2),
      stringsAsFactors = FALSE
    )
    
    res
    
    
  })

  results.table <- cbind(method=results.table[,1],
                         model="",
                         results.table[,-c(1,2)],
                         stringsAsFactors = FALSE)

  nrows <- nrow(results.table)
  
  results.table$method <- ""
  results.table$method[1] <- sprintf("\\multirow{%d}{*}{SIC}", nrows/3)
  results.table$model[1] <- sprintf("\\multirow{%d}{*}{\\ref{Model:%s}}", nrows/3, labels[j])
  
  results.table$method[nrows/3+1] <- sprintf("\\multirow{%d}{*}{THR $C=0.25$}", nrows/3)
  results.table$model[nrows/3+1] <- sprintf("\\multirow{%d}{*}{\\ref{Model:%s}}", nrows/3, labels[j])
  
  results.table$method[2*nrows/3+1] <- sprintf("\\multirow{%d}{*}{THR $C=0.5$}", nrows/3)
  results.table$model[2*nrows/3+1] <- sprintf("\\multirow{%d}{*}{\\ref{Model:%s}}", nrows/3, labels[j])
  
  results.table[, 5:10] <- sapply(results.table[, 5:10], 
                                 function(x) formatC(x, digits=1, format="f"))
  
  results.table[, 11] <- sapply(results.table[, 11], 
                                  function(x) formatC(x, digits=3, format="f"))
  results.latex.table <- xtable(results.table, 
                                align = rep("l", ncol(results.table)+1))
  
  print(results.latex.table,
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.colnames.function=identity,
        sanitize.text.function=identity, 
        only.contents=TRUE, 
        file=sprintf("tables/%s_sim_results.tex", 
                     label),
        hline.after=c(nrows/3, 2*nrows/3))


}

#################################################################################
###### the trading example                                     ########
#################################################################################

files <- c("three_cpts_real_vol.RData", "four_cpts_real_vol.RData",
           "three_cpts_const_vol.RData", "four_cpts_const_vol.RData")
labels <- c("three_cpts_real_vol", "four_cpts_real_vol",
            "three_cpts_const_vol", "four_cpts_const_vol")

for(j in 1:length(files)){

  file <- files[[j]]
  label <- labels[[j]]


  load(sprintf("results/%s", file))

  results.table <- 
    ddply(results,~ n.days, function(df){
      
      res <- data.frame(
        n.days = as.integer(df$n.days[1]),
        p = as.integer(df$p[1]),
        n.scales = round(mean(df$n.scales),2),
        ar.order = round(mean(df$ar.order),2),
        rel.pred.error.amar = round(mean(df$rel.pred.error.amar),3),
        rel.pred.error.ar = round(mean(df$rel.pred.error.ar),3), 
        timing.amar = round(mean(df$timing.amar),2),
        timing.ar = round(mean(df$timing.ar),2)
      )
      
      res
      
      
    })
  

  
  results.table <- cbind(model=c(sprintf("\\multirow{%d}{*}{\\ref{Model:%s}}", nrow(results.table), label), rep("", nrow(results.table)-1)),
                         results.table)
  
  results.table[, 4:5] <- sapply(results.table[, 4:5], 
                                 function(x) formatC(x, digits=1, format="f"))
  
  results.table[, 6:7] <- sapply(results.table[, 6:7], 
                                  function(x) formatC(x, digits=3, format="f"))
  
  results.table[, 8:9] <- sapply(results.table[, 8:9], 
                                function(x) formatC(x, digits=2, format="f"))
  
  results.latex.table <- xtable(results.table, 
                                align = rep("l", ncol(results.table)+1))
  
  print(results.latex.table,
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.colnames.function=identity,
        sanitize.text.function=identity, 
        only.contents=TRUE, 
        file=sprintf("tables/%s_sim_results.tex", 
                     label),
        hline.after=c())

}