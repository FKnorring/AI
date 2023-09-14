library("DeliveryMan")

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # Vi har ett paket
  if(carInfo$load == 1){
    if(correctPlace){
      carInfo$nextMove <- 5
    }
    else {
      # Vi har paket, hitta bäst vägen till dropoff
    } 
  }
  # Vi har inte ett paket
  else {
    carInfo$mem$target <- findBestPackage(carInfo, packageMatrix)
    temp <- findBestRoute(trafficMatrix,carInfo,packageMatrix )
    # hitta bäst väg dit
  }
  carInfo$nextMove <- 2
  return (carInfo)
}

manhattanDistance <- function(pos1, pos2){
  distance <- abs(pos1[1]-pos2[1]) + abs(pos1[2]-pos2[2])
  return (distance)
}

# Finds the best path using the A* algorithm and returns the next move as an integer
findBestRoute <- function(trafficMatrix, carInfo, packageMatrix){
  frontier <- list()
  visitedNodes <- list()
  carPos <- c(carInfo$x,carInfo$y)
  
  # targetPos should be either pickup or delivery location, currently only pickup
  targetPos <- c(carInfo$mem$target[1],carInfo$mem$target[2])
  # A node is [nodeX,nodeY, cost(g+h), manhattanToTarget, path]
  startNode <- c(posX = carInfo$x, posY=carInfo$y,cost = 0, manhattan = manhattanDistance(carPos, targetPos), path = list())
  frontier[[length(frontier)+1]] <- startNode
  
  while(1){
    # Find the best cost score node in the frontier set
    costs=sapply(frontier,function(item)item$cost)
    lowestCostIndex = which.min(costs)
    currentNode = frontier[[lowestCostIndex]]
    # Move the node from frontier to visitedNodes
    visitedNodes[[length(visitedNodes)+1]] <- currentNode
    frontier[-lowestCostIndex]
    
    
    if (currentNode$manhattan == 0){
      # Return first step in path
    }
  
    
    
    # Expanding and creating the nodes for neighbours
    posRight <- c(carInfo$x+1,carInfo$y)
    posUp <- c(carInfo$x,carInfo$y+1)
    posLeft <- c(carInfo$x-1,carInfo$y)
    posDown <- c(carInfo$x,carInfo$y-1)
    currentPath = currentNode$path
    
    nodeRight <- c(posX = posRight[1],posY=posRight[2], cost = 0, manhattan = manhattanDistance(posRight, targetPos), path = append(currentPath, posRight, after=(length(currentPath))))
    nodeUp <- c(posX = posUp[1],posY=posUp[2], cost = 0, manhattan = manhattanDistance(posUp, targetPos), path = append(currentPath, posUp, after=(length(currentPath))))
    nodeLeft <- c(posX = posLeft[1],posY=posLeft[2], cost = 0, manhattan = manhattanDistance(posLeft, targetPos), path = append(currentPath, posLeft, after=(length(currentPath))))
    nodeDown <- c(posX = posDown[1],posY=posDown[2], cost = 0, manhattan = manhattanDistance(posDown, targetPos), path = append(currentPath, posDown, after=(length(currentPath))))
    
    nodeList <- list(nodeRight,nodeUp,nodeLeft,nodeDown)
    
    
    for (i in 1:length(nodeList)) {
      nodeToCheck <- nodeList[[i]]
      posX <- nodeToCheck["posX"]
      posY <- nodeToCheck["posY"]
      
      if(validPosition(posX, posY)){
        print(posX)
      }
    }
    
        
      
    
    
    # Lägg till noden i openSet (skapa den osv)
    # Kolla om den inte ligger där redan.
    # Kolla isf om nya vägen är billigare, skriv över med den.
    
      
    return(1)
  }
}

validPosition <- function(posX,posY){
  if (posX > 10 || posX < 1 || posY < 1 || posY > 10 ){
    return(FALSE)
  }
  return(TRUE)
}

# Finds the best package location and returns its information
# Could try to calculate a weighted with its distance to the dropoff aswell
findBestPackage <- function(carInfo, packageMatrix){
  index <- 1
  shortestDistance <- 200
  bestPackage <- NULL
  while(index <= 5){
    currentPackage <- packageMatrix[index,]
    if(currentPackage[5] == 0){
      currentDistance <- abs(currentPackage[1]- carInfo$x) + abs(currentPackage[2] - carInfo$y)
      if(currentDistance < shortestDistance){
        shortestDistance <- currentDistance
        bestPackage <- currentPackage
      }
    } 
    index <- index + 1
  }
  return(bestPackage) 
}



runDeliveryMan(carReady = myFunction)

