test <- function (popSize, numIslands, pcrossover, pmutation, migrationRate, migrationInterval, funNum, isClassic){
  f <- function(x, y){
    if (length(x) != length(y))
      stop()
    m <- matrix(c(x,y),nrow=length(x))
    return(cec2013(funNum,m))
  }
  
  maxIter = 100

  min <- -100; max <- 100
  x <- seq(min, max, by = 1)
  y <- seq(min, max, by = 1)
  #persp3D(x, y, outer(x, y, f), theta = 50, phi = 20, color.palette = bl2gr.colors)
  
  fitness = function(x) -f(x[1],x[2])
  
  minPoint = c(min,min)
  maxPoint = c(max,max)
  
  GaClassicList <- c()
  GaIslandsList <- c()
  if(isClassic){
  for (i in 1:10) {
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
                    selection = gareal_rwSelection,
                    parallel = FALSE,
                    monitor = TRUE)
    GaClassicList[[i]] <- GaClassic@fitnessValue
  }
    print("Klasyczny: ")
    print(paste('best: ',max(GaClassicList)))
    print(paste('worst: ',min(GaClassicList)))
    print(paste('mediana: ',median(GaClassicList)))
    print(paste('œrednia: ',mean(GaClassicList)))
    print(paste('odchynenie: ',sd(GaClassicList)))
  }
  else{
  for (i in 1:10) {
    GaIslands <- gaisl(type = "real-valued",
                       fitness =  fitness,
                       min = minPoint,
                       max = maxPoint,
                       popSize = popSize,
                       maxiter = maxIter,
                       run = maxIter,
                       elitism = 0,
                       pcrossover = pcrossover,
                       pmutation = pmutation,

                       selection = gareal_rwSelection,
                       numIslands = numIslands,
                       migrationRate = migrationRate,
                       migrationInterval = migrationInterval,
                       parallel = FALSE,
                       monitor = TRUE)

    GaIslandsList[[i]] <- GaIslands@fitnessValue
  }
    print('Wyspowy: ')
    print(paste('best: ',max(GaIslandsList)))
    print(paste('worst: ',min(GaIslandsList)))
    print(paste('mediana: ',median(GaIslandsList)))
    print(paste('œrednia: ',mean(GaIslandsList)))
    print(paste('odchynenie: ',sd(GaIslandsList)))
  }

  

}
# test(70,
#      2,
#      0,
#      0.005,
#      0.05,
#      35,
#      9)


# 
# popSizeSeq =            seq(70,120,by=10)
# numIslandsSeq =         seq(2,8,by=1)
# pcrossoverSeq =         seq(0,0.6,by=0.3)
# pmutationSeq =          seq(0.005,0.02,by=0.005)
# migrationRateSeq =      seq(0.05,0.3,by=0.05)
# migrationIntervalSeq =  seq(35,65,by=5)
perform <- function (popSizeSeq,
                     numIslandsSeq,
                     pcrossoverSeq,
                     pmutationSeq,
                     migrationRateSeq,
                     migrationIntervalSeq){
for (funNum in 1:28) {
  print(paste('Funkcja: ',funNum))
  for (popsize in popSizeSeq){
      for (pcrossover in pcrossoverSeq){
        test(popsize,
             numIslands,
             pcrossover,
             pmutation,
             migrationRate,
             migrationInterval,
             funNum,
             TRUE)
        for (pmutation in pmutationSeq){
          for (numIslands in numIslandsSeq){
          for (migrationRate in migrationRateSeq){
            for (migrationInterval in migrationIntervalSeq){
              
              print(paste('Parametry: ',popsize,
                          numIslands,
                          pcrossover,
                          pmutation,
                          migrationRate,
                          migrationInterval,
                          funNum, sep=" "))
              
              test(popsize,
                   numIslands,
                   pcrossover,
                   pmutation,
                   migrationRate,
                   migrationInterval,
                   funNum,
                   FALSE)
            }
          }
        }
      }
    }
  }
}
}