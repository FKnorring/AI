library("DeliveryMan")

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # Vi har ett paket
  if(carInfo$load == 0){
    carInfo$mem$target <- findBestPackage(carInfo, packageMatrix)
    if(carInfo$mem$target[1] == 1 && carInfo$mem$target[2] == 1){
      print("wtf")
      carInfo$mem$goal <- c(carInfo$mem$target[3], carInfo$mem$target[4])
    }
    else{
      carInfo$mem$goal <- c(carInfo$mem$target[1], carInfo$mem$target[2])
    }
    carInfo$nextMove <- findBestRoute(trafficMatrix,carInfo,packageMatrix )
    return(carInfo)
  }
  # Vi har inte ett paket
  else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3, 4)]
    carInfo$nextMove <- findBestRoute(trafficMatrix,carInfo,packageMatrix)
    return (carInfo)
    # hitta bäst väg dit
  }
  return (carInfo)
}

manhattanDistance <- function(pos1, pos2){
  distance <- abs(pos1[1]-pos2[1]) + abs(pos1[2]-pos2[2])
  return (distance)
}

# Finds the best path using the A* algorithm and returns the next move as an integer
findBestRoute <- function(trafficMatrix, carInfo, packageMatrix) {
  frontier <- list()
  visitedNodes <- list()
  carPos <- c(carInfo$x, carInfo$y)
  
  # targetPos should be either pickup or delivery location, currently only pickup
  targetPos <- carInfo$mem$goal
  
  # A node is [posX, posY, cost(g+h), manhattanToTarget, path]
  startNode <- list(
    posX = carInfo$x, 
    posY = carInfo$y, 
    cost = 0, 
    manhattan = manhattanDistance(carPos, targetPos), 
    path = list(c(carInfo$x, carInfo$y))
  )
  
  frontier <- list(startNode)
  
  while (length(frontier) > 0) {
    # Find the best cost score node in the frontier set
    costs <- sapply(frontier, function(item) item[["cost"]])
    lowestCostIndex <- which.min(costs)
    currentNode <- frontier[[lowestCostIndex]]
    
    # Move the node from frontier to visitedNodes
    visitedNodes <- append(visitedNodes, list(currentNode))
    frontier <- frontier[-lowestCostIndex]
    
    if (is.null(currentNode[["manhattan"]]) || length(currentNode[["manhattan"]]) <= 0){
      print(currentNode)
      print(targetPos)
      print(packageMatrix)
    }
    
    if (currentNode[["manhattan"]] == 0 ) {
      # Calculate the direction code based on the difference in coordinates
      
      if (length(currentNode[["path"]]) >= 2) {
        diffX <- currentNode[["path"]][[2]][1] - carInfo$x
        diffY <- currentNode[["path"]][[2]][2] - carInfo$y
      } else {
        diffX <- 0
        diffY <- 0
      }
      
      if (diffX == 0 && diffY == 0) {
        return(5)  # Still
      } else if (diffX == 0 && diffY > 0) {
        return(8)  # Up
      } else if (diffX == 0 && diffY < 0) {
        return(2)  # Down
      } else if (diffX > 0 && diffY == 0) {
        return(6)  # Right
      } else if (diffX < 0 && diffY == 0) {
        return(4)  # Left
      }
    }
      
    
    # Expanding and creating the nodes for neighbors
    posRight <- c(currentNode[["posX"]] + 1, currentNode[["posY"]])
    posUp <- c(currentNode[["posX"]], currentNode[["posY"]] + 1)
    posLeft <- c(currentNode[["posX"]] - 1, currentNode[["posY"]])
    posDown <- c(currentNode[["posX"]], currentNode[["posY"]] - 1)
    
    positions <- list(posRight, posUp, posLeft, posDown)
    
    for (i in 1:4) {
      posX <- positions[[i]][1]
      posY <- positions[[i]][2]
      
      if (!nodeExistsIn(posX, posY, visitedNodes) && validPosition(posX, posY)) {
        costToNode <- NULL
        if (i == 1) {
          costToNode <- trafficMatrix$hroads[posX-1, posY]
        } else if (i == 2) {
          costToNode <- trafficMatrix$vroads[posX, posY - 1]
        } else if (i == 3) {
          costToNode <- trafficMatrix$hroads[posX, posY]
        } else if (i == 4) {
          costToNode <- trafficMatrix$vroads[posX, posY]
        }
        
        newCost <- currentNode[["cost"]] + costToNode
        newManhattan <- manhattanDistance(c(posX, posY), targetPos)
        
        newNode <- list(
          posX = posX,
          posY = posY,
          cost = newCost,
          manhattan = newManhattan,
          path = append(currentNode[["path"]], list(c(posX, posY)))
        )
        
        if (!nodeExistsIn(posX, posY, frontier)) {
          frontier <- append(frontier, list(newNode))
        }
        #else if a node with the same posX, posY exists in frontier
        # but it has a higher cost, replace that node with
      }
    }
  }
  
  # Return an empty path if no path is found
  return(list())
}

findBestNode <- function(frontier){
  ## HÄR MÅSTE VI KOLLA TRAFFIC MATRIX??
}

nodeExistsIn <- function(posXToCheck,posYToCheck, frontier) {
  #posXToCheck <- nodeToCheck[["posX"]]
  #posYToCheck <- nodeToCheck[["posY"]]
  
  if (length(frontier) == 0) {
    return(FALSE)
  }
  
  for (i in 1:length(frontier)) {
    nodeInFrontier <- frontier[[i]]
    if (nodeInFrontier[["posX"]] == posXToCheck && nodeInFrontier[["posY"]] == posYToCheck) {
      return(TRUE)
    }
  }
  
  return(FALSE)
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

