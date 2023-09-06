pq <- function() {
  # Internal lists to hold items and their priorities
  items <- list()
  priorities <- numeric()
  
  # Enqueue an item with a given priority
  enqueue <- function(item, priority) {
    # Find the correct position to insert the item
    idx <- sum(priorities <= priority) + 1
    
    # Insert the item and its priority at the correct position
    items <<- append(items, list(item), after = idx - 1)
    priorities <<- append(priorities, priority, after = idx - 1)
  }
  
  # Dequeue the highest priority item
  dequeue <- function() {
    if (length(items) == 0) {
      stop("Queue is empty!")
    }
    item <- items[[1]]
    items <<- items[-1]
    priorities <<- priorities[-1]
    return(item)
  }
  
  # Peek at the highest priority item without removing it
  peek <- function() {
    if (length(items) == 0) {
      stop("Queue is empty!")
    }
    return(items[[1]])
  }
  
  # Check if the queue is empty
  is_empty <- function() {
    return(length(items) == 0)
  }
  
  # Return the methods as a list
  return(list(
    enqueue = enqueue,
    dequeue = dequeue,
    peek = peek,
    is_empty = is_empty
  ))
}

# Find the nearest pickup location for an undelivered package
# Rewrite this to use A* for best package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector <- abs(packageMatrix[, 1] - carInfo$x) + abs(packageMatrix[, 2] - carInfo$y)
  distanceVector[packageMatrix[, 5] != 0] <- Inf
  return(packageMatrix[which.min(distanceVector), c(1, 2)])
}

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # Determine the goal based on car's load
  if (carInfo$load == 0) {
    carInfo$mem$goal <- nextPickup(trafficMatrix, carInfo, packageMatrix)
  } else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3, 4)]
  }
  
  # Get the path using A* algorithm
  path <- aStar(trafficMatrix, carInfo, packageMatrix)
  
  # If path is not empty, determine the next move
  if (length(path) > 0) {
    nextCoord <- path[[1]]
    currentCoord <- carCords(carInfo)
    # Determine the direction based on the difference between current and next coordinates
    deltaX <- nextCoord[1] - currentCoord[1]
    deltaY <- nextCoord[2] - currentCoord[2]
    
    if (deltaX == 1) {
      carInfo$nextMove <- 6 # Right
    } else if (deltaX == -1) {
      carInfo$nextMove <- 4 # Left
    } else if (deltaY == 1) {
      carInfo$nextMove <- 8 # Up
    } else if (deltaY == -1) {
      carInfo$nextMove <- 2 # Down
    }
  } else {
    # If no path is found or the goal is already reached, set nextMove to a default value
    carInfo$nextMove <- 5
  }
  return(carInfo)
}


# estimated (manhattan distance)
h <- function(carInfo, frontierNode) {
  goalX <- carInfo$mem$goal[1]
  goalY <- carInfo$mem$goal[2]
  
  frontierX <- frontierNode[1]
  frontierY <- frontierNode[2]
  
  return(abs(goalX - frontierX) + abs(goalY - frontierY))
}

# actual (traffic cost)
g <- function(carInfo, frontierNode, trafficMatrix) {
  carX <- carInfo$x
  carY <- carInfo$y
  
  frontierX <- frontierNode[1]
  frontierY <- frontierNode[2]
  
  # Determine the direction of movement
  if (carX == frontierX) { # Vertical movement
    return(trafficMatrix$vroads[carX, min(carY, frontierY)])
  } else { # Horizontal movement
    return(trafficMatrix$hroads[min(carX, frontierX), carY])
  }
}

f <- function(carInfo, frontierNode, trafficMatrix) {
  return(g(carInfo, frontierNode, trafficMatrix) + h(carInfo, frontierNode))
}

carCords <- function(carInfo) {
  return(c(carInfo$x, carInfo$y))
}

getNeighbors <- function(carNode, dim) {
  x <- carNode[[1]]
  y <- carNode[[2]]
  
  neighbors <- vector("list", 4)
  idx <- 1
  
  if (x > 1) {
    neighbors[[idx]] <- c(x - 1, y)
    idx <- idx + 1
  }
  
  if (x <= dim) {
    neighbors[[idx]] <- c(x + 1, y)
    idx <- idx + 1
  }
  
  if (y > 1) {
    neighbors[[idx]] <- c(x, y - 1)
    idx <- idx + 1
  }
  
  if (y <= dim) {
    neighbors[[idx]] <- c(x, y + 1)
  }
  
  return(neighbors)
}

aStar <- function(trafficMatrix, carInfo, packageMatrix) {
  q <- pq()
  
  # add start node to queue
  # node looks like [carCoords, cost, path]
  startNode <- list(cords = carCords(carInfo), cost = 0, path = list())
  gridDim <- nrow(trafficMatrix$hroads)
  visitedMatrix <- matrix(FALSE, nrow = gridDim+1, ncol = gridDim+1)
  visitedMatrix[carInfo$x, carInfo$y] <- TRUE
  
  q$enqueue(startNode, 0)
  
  while (!q$is_empty()) {
    frontierNode <- q$dequeue()
    cords <- frontierNode$cords
    
    if (all(cords == carInfo$mem$goal)) {
      return(frontierNode$path)
    }
    
    neighbors <- getNeighbors(cords, gridDim)
    
    for (neighbor in neighbors) {
      if (is.null(neighbor)) next
      if (visitedMatrix[neighbor[[1]], neighbor[[2]]]) next
      
      visitedMatrix[neighbor[[1]], neighbor[[2]]] <- TRUE
      newCost <- frontierNode$cost + f(carInfo, neighbor, trafficMatrix)
      q$enqueue(list(cords = neighbor, cost = newCost, path = c(frontierNode$path, list(neighbor))), newCost)
    }
  }
  print("Search failed")
  return(NULL)
}