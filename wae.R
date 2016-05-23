test <- function (i, popSize, maxIter){
  f <- function(x, y){
    if (length(x) != length(y))
      stop()
    m <- matrix(c(x,y),nrow=length(x))
    
    cec2013(i,m)
  
  }
  min <- -100; max <- 100
  x <- seq(min, max, by = 1)
  y <- seq(min, max, by = 1)
  persp3D(x, y, outer(x, y, f), theta = 50, phi = 20, color.palette = bl2gr.colors)
  
  fitness = function(x) -f(x[1],x[2])
  
  minPoint = c(min,min)
  maxPoint = c(max,max)
  
  b = FALSE
  if(b){
  GA <- ga(type = "real-valued",
            fitness = fitness,
            min = minPoint,
            max = maxPoint,
            popSize = popSize,
            maxiter = maxIter,
            elitism = 0,
            pmutation = 0.1,
            pcrossover = 0,
            selection = gareal_rwSelection)
  }else{
  GA <- gaisl(type = "real-valued", 
            fitness =  fitness,
            min = minPoint,
            max = maxPoint, 
            popSize = popSize,
            maxiter = maxIter,
            elitism = 0,
            pcrossover = 0,
            pmutation = 0.1,
            
            selection = gareal_rwSelection,
            numIslands = 4, 
            migrationRate = 0.2,
            migrationInterval = 50,
            parallel = FALSE)
  }
  summary(GA)
  plot(GA)
}
test(9, 100, 100)