model.as2 <- 
  list(
    name = "as2",
    coefficients = function(n)  c(-0.115, -2.15, -15, 10) , 
    scales = function(n) as.integer(c(1, 20*log(n), 10 * log(n)^2, 20 * log(n)^2)),
    max.order = function(n) as.integer(20 * log(n)^2+100),
    sigma = 1
  )

model.as1 <-
  list(
    name =  "as1",
    coefficients  = function(n)  c(-0.115, -3.15, -15), 
    scales = function(n)  as.integer(c(1,   20*log(n), log(n)*40)),
    max.order = function(n) as.integer(log(n)*40+100),
    sigma = 1
  )


model.three.cpts <-
  list(
      name = "three_cpts",
      coefficients = c(-0.15, 0.25, -3.15), 
      scales = c(1, 12,  5*80),
      max.order = 6*80,
      sigma = 2
)

model.four.cpts <- 
  list(
    name =  "four_cpts",
    coefficients  = c(-0.15, 0.25, -3.15, -5.5),
    scales = as.integer(c(1, 12,   80, 5*80)),
    max.order = 6*80,
    sigma = 2
)