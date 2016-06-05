printLog <- function(msg){
  write(msg, stderr())
}
printParamsIsl <- function(pcrossover, numIslands, migrationRate, migrationInterval){
  print(paste('prawdopodobieñstwo krzy¿owania: ', pcrossover))
  print(paste('liczba wysp: ', numIslands))
  print(paste('rozmiar migracji: ', migrationRate))
  print(paste('interwa³ miêdzy migracjami: ', migrationInterval))
}
printParamsCl<-function(pcrossover){
  print(paste('prawdopodobieñstwo krzy¿owania: ', pcrossover))
}
test <- function (popSize, numIslands, pcrossover, pmutation, migrationRate, migrationInterval, funNum, isClassic){
  
  maxIter = 100

  min <- -100; max <- 100

  fitness = function(x) -f(x[1],x[2])
  
  minPoint = c(min,min)
  maxPoint = c(max,max)
  
  GaClassicList <- c()
  GaIslandsList <- c()
  best<-NULL
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
                    parallel = FALSE,
                    monitor = FALSE)
    GaClassicList[[i]] <- GaClassic@fitnessValue
    if(GaClassic@fitnessValue >= max(GaClassicList))
      best = GaClassic
  }
    plot(best)
    print("Klasyczny: ")
    cat('\n')
    printParamsCl(pcrossover)
    cat('\n')
    print(paste('najlepszy: ',max(GaClassicList)))
    print(paste('najgorszy: ',min(GaClassicList)))
    print(paste('mediana: ',median(GaClassicList)))
    print(paste('œrednia: ',mean(GaClassicList)))
    print(paste('odchylenie standardowe: ',sd(GaClassicList)))
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
                       numIslands = numIslands,
                       migrationRate = migrationRate,
                       migrationInterval = migrationInterval,
                       parallel = FALSE,
                       monitor = FALSE)

    GaIslandsList[[i]] <- GaIslands@fitnessValue
    if(GaIslands@fitnessValue >= max(GaIslandsList))
      best = GaIslands
  }
    plot(best)
    print('Wyspowy: ')
    cat('\n')
    printParamsIsl(pcrossover, numIslands, migrationRate, migrationInterval)
    cat('\n')
    print(paste('najlepszy: ',max(GaIslandsList)))
    print(paste('najgorszy: ',min(GaIslandsList)))
    print(paste('mediana: ',median(GaIslandsList)))
    print(paste('œrednia: ',mean(GaIslandsList)))
    print(paste('odchylenie standardowe: ',sd(GaIslandsList)))
  }

  

}

perform <- function (popSizeSeq,
                     numIslandsSeq,
                     pcrossoverSeq,
                     pmutationSeq,
                     migrationRateSeq,
                     migrationIntervalSeq,
                     funNum){
  
  
  allTestsNum = length(popSizeSeq)  *length(pcrossoverSeq)  *length(pmutationSeq)  *length(numIslandsSeq)  *length(migrationRateSeq)  *length(migrationIntervalSeq)
  

  

  f <<- function(x, y){
    if (length(x) != length(y))
      stop()
    m <- matrix(c(x,y),nrow=length(x))
    return(cec2013(funNum,m))
  }
  min <- -100; max <- 100
  x <- seq(min, max, by = 1)
  y <- seq(min, max, by = 1)
  
  print(paste('Numer funkcji cec2013: ',funNum))
  
  persp3D(x, y, outer(x, y, f), theta = 50, phi = 20, color.palette = bl2gr.colors)
  
  # knit(text = '\\newpage')
  
  for (popsize in popSizeSeq){
      for (pcrossover in pcrossoverSeq){
        for (pmutation in pmutationSeq){
          
          printLog(paste('Algorytm klasyczny: ',
                         popsize,
                         pcrossover,
                         pmutation,
                         funNum, sep=" "))
          
          
          test(popsize,
               numIslands,
               pcrossover,
               pmutation,
               migrationRate,
               migrationInterval,
               funNum,
               TRUE)
          for (numIslands in numIslandsSeq){
          for (migrationRate in migrationRateSeq){
            for (migrationInterval in migrationIntervalSeq){
              
              printLog(paste('Algorytm wyspowy: ',
                          popsize,
                          pcrossover,
                          pmutation,
                          numIslands,
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
