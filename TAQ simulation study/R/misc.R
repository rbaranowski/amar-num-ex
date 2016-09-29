eval.strategy <- function(predicted, 
                          last.price, 
                          cash=1000, 
                          cost=c("per.trade", "per.share"), 
                          cost.per.share=0.005, 
                          cost.per.trade = 5,
                          verbose=FALSE){
  
  position <- 0 # 1 long, -1 short
  quantity <- 0 # number of shares hold
  entry.price <- 0
  
  cost <- match.arg(cost, c("per.trade", "per.share"))
  
  if(cost == "per.share"){
    
    for(j in 1:length(predicted)) 
      
      if(position == 0){
        
        # cat("j=",j, " no active positions ..\n" )
        
        if(sign(predicted[j]) == 1) {
          entry.price <- last.price[j]
          quantity <- cash/(entry.price+cost.per.share)
          position <- 1
          # cat("j=",j, " entering long position ..\n" )
        }
        
        if(sign(predicted[j]) == -1){
          entry.price <- last.price[j]
          quantity <- cash/(entry.price+cost.per.share)
          position <- -1
          # cat("j=",j, " entering short position ..\n" )
        }
        
      } else if(position == 1){
        # cat("j=",j, " long position active ..\n" )
        #change position if we predict change of the sign
        if(sign(predicted[j]) == -1){
          
          # cat("j=",j, "predicted negative, changing position from long to short ..\n" )
          #close long posistion
          cash <- quantity * (last.price[j] - cost.per.share)
          
          #open short
          entry.price <- last.price[j]
          quantity <- cash/(last.price[j]+cost.per.share)
          position <- -1
        }
        
      }else  if(position == -1){
        # cat("j=",j, " short position active ..\n" )
        #change position if we predict change of the sign
        if(sign(predicted[j]) == 1){
          # cat("j=",j, "predicted positive, changing position from short to long ..\n" )
          
          #close short position
          cash <- -quantity * (last.price[j]+cost.per.share)+ 2 * quantity * entry.price
          
          #open long position
          entry.price <- last.price[j]
          quantity <- cash / (last.price[j]+cost.per.share)
          position <- 1
          
        }
      }
    
    #close final position
    
    if(position == 1)  cash <- quantity * (last.price[j] - cost.per.share)
    else if(position == -1) cash <- -quantity * (last.price[j]+cost.per.share)+ 2 * quantity * entry.price
    
  }else if(cost == "per.trade"){ 
    
    for(j in 1:length(predicted)){
      
      if(position == 0){
        
        if(sign(predicted[j]) == 1) {
          entry.price <- last.price[j]
          quantity <- (cash-cost.per.trade)/entry.price
          position <- 1
        }
        
        if(sign(predicted[j]) == -1){
          entry.price <- last.price[j]
          quantity <- (cash-cost.per.trade)/entry.price
          position <- -1
        }
        
      } else if(position == 1){
        
        #change position if we predict change of the sign
        if(sign(predicted[j]) == -1){
          #close long posistion
          cash <- quantity * last.price[j] - cost.per.trade
          
          #open short
          entry.price <- last.price[j]
          quantity <- (cash-cost.per.trade)/last.price[j]
          position <- -1
        }
        
      }else  if(position == -1){
        #change position if we predict change of the sign
        if(sign(predicted[j]) == 1){
          
          #close short position
          cash <- -quantity * last.price[j]+ 2 * quantity * entry.price - cost.per.trade
          
          #open long position
          entry.price <- last.price[j]
          quantity <- (cash-cost.per.trade) /last.price[j]
          position <- 1
          
        }
      }
      
      if(cash <= 0) return(0)
    } 
    #close final position
    
    if(position == 1)  cash <- quantity * last.price[j] - cost.per.trade
    else if(position == -1) cash <- -quantity * last.price[j]+ 2 * quantity * entry.price - cost.per.trade
    
  }
  
  
  
  cash
  
}