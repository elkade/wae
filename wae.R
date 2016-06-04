test <- function (popSize, numIslands, pcrossover, pmutation, migrationRate, migrationInterval, funNum){
  f <- function(x, y){
    if (length(x) != length(y))
      stop()
    m <- matrix(c(x,y),nrow=length(x))
    
    cec2013(funNum,m)
  
  }
  maxFes = 20000
  maxIter = floor(maxFes / popSize)

  min <- -100; max <- 100
  x <- seq(min, max, by = 1)
  y <- seq(min, max, by = 1)
  persp3D(x, y, outer(x, y, f), theta = 50, phi = 20, color.palette = bl2gr.colors)
  
  fitness = function(x) -f(x[1],x[2])
  
  minPoint = c(min,min)
  maxPoint = c(max,max)
  
  GaClassicList <- c()
  GaIslandsList <- c()
  
  for (i in 1:5) {
    GaClassic <- ga(type = "real-valued",
                    fitness = fitness,
                    min = minPoint,
                    max = maxPoint,
                    popSize = popSize,
                    maxiter = maxIter,
                    run = maxIter,
                    elitism = 0,
                    pmutation = pmutation,
                    pcrossover = pcrossover,
                    selection = gareal_rwSelection)
    GaClassicList[[i]] <- GaClassic@fitnessValue
    
    # GaIslands <- gaisl(type = "real-valued", 
    #                    fitness =  fitness,
    #                    min = minPoint,
    #                    max = maxPoint, 
    #                    popSize = popSize,
    #                    maxiter = maxIter,
    #                    run = maxIter,
    #                    elitism = 0,
    #                    pcrossover = pcrossover,
    #                    pmutation = pmutation,
    #                    
    #                    selection = gareal_rwSelection,
    #                    numIslands = numIslands, 
    #                    migrationRate = migrationRate,
    #                    migrationInterval = migrationInterval,
    #                    parallel = FALSE)
    # 
    # GaIslandsList[[i]] <- GaIslands@fitnessValue
    print(summary(GaClassic))
  }
  print(GaClassicList)
}
test(70,
     2,
     0,
     0.005,
     0.05,
     35,
     9)
testAllCec <- function (popSize, numIslands, pcrossover, pmutation, migrationRate, migrationInterval){
  for (funNum in 1:28) {
    test(popsize,
         numIslands,
         pcrossover,
         pmutation,
         migrationRate,
         migrationInterval,
         funNum)
  }
}


popSizeSeq =            seq(70,120,by=10)
numIslandsSeq =         seq(2,8,by=1)
pcrossoverSeq =         seq(0,0.6,by=0.3)
pmutationSeq =          seq(0.005,0.02,by=0.005)
migrationRateSeq =      seq(0.05,0.3,by=0.05)
migrationIntervalSeq =  seq(35,65,by=5)


for (popsize in popSizeSeq){
  for (numIslands in numIslandsSeq){
    for (pcrossover in pcrossoverSeq){
      for (pmutation in pmutationSeq){
        for (migrationRate in migrationRateSeq){
          for (migrationInterval in migrationIntervalSeq){
            testAllCec(popsize,
                 numIslands,
                 pcrossover,
                 pmutation,
                 migrationRate,
                 migrationInterval)
          }
        }
      }
    }
  }
}