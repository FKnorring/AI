aStarPathfinding <- function(start, goal, hroads, vroads, dim) {
  # Initialize open and closed lists
  openList <- list()
  closedList <- list()
  
  # Add start node to open list
  startNode <- list(x = start[1], y = start[2], g = 0, h = abs(goal[1] - start[1]) + abs(goal[2] - start[2]))
  startNode$h <- abs(goal[1] - start[1]) + abs(goal[2] - start[2])
  startNode$f <- startNode$g + startNode$h
  openList[[paste0(start[1], "-", start[2])]] <- startNode
  
  while (length(openList) > 0) {
    # Find the node with the least f in the open list
    currentKey <- names(which.min(sapply(openList, function(x) x$f)))
    currentNode <- openList[[currentKey]]
    
    # Pop the current node off the open list
    openList[currentKey] <- NULL
    
    # Generate the current node's successors and set their parents to the current node
    successors <- list()
    
    for (dx in c(-1, 1, 0, 0)) {
      for (dy in c(0, 0, -1, 1)) {
        x <- currentNode$x + dx
        y <- currentNode$y + dy
        
        if (x >= 1 && x <= dim && y >= 1 && y <= dim) {
          g <- currentNode$g
          if (dx == 0) {
            g <- g + vroads[x, y]
          } else {
            g <- g + hroads[x, y]
          }
          h <- abs(goal[1] - x) + abs(goal[2] - y)
          f <- g + h
          
          successor <- list(x = x, y = y, parent = currentNode, g = g, h = h, f = f)
          successors[[paste0(x, "-", y)]] <- successor
        }
      }
    }
    
    # For each successor
    for (key in names(successors)) {
      successor <- successors[[key]]
      
      # If successor is the goal, stop search
      if (successor$x == goal[1] && successor$y == goal[2]) {
        path <- list()
        while (!is.null(successor$parent)) {
          dx <- successor$x - successor$parent$x
          dy <- successor$y - successor$parent$y
          if (dx == 1 && dy == 0) {
            path <- c(6, path)
          } else if (dx == -1 && dy == 0) {
            path <- c(4, path)
          } else if (dx == 0 && dy == 1) {
            path <- c(8, path)
          } else if (dx == 0 && dy == -1) {
            path <- c(2, path)
          }
          successor <- successor$parent
        }
        return(path)
      }
      
      # Otherwise, add the successor to the open list
      if (is.null(closedList[[key]]) || closedList[[key]]$f > successor$f) {
        openList[[key]] <- successor
      }
    }
    
    # Push current node to the closed list
    closedList[[currentKey]] <- currentNode
  }
  
  return(NULL)  # Return NULL if there's no path
}

aStarDM <- function(roads, car, packages) {
  # Initialize car$nextMove to 5 (no move) by default
  car$nextMove <- 5
  
  # Determine the next goal (either a package to pick up or a delivery point)
  nextGoal <- NULL
  offset <- 0
  if (car$load == 0) {
    nextGoal <- which(packages[, 5] == 0)[1]
  } else {
    nextGoal <- car$load
    offset <- 2
  }
  
  # If there's a goal, find the path to it
  if (!is.null(nextGoal)) {
    startX <- car$x
    startY <- car$y
    goalX <- packages[nextGoal, 1 + offset]
    goalY <- packages[nextGoal, 2 + offset]
    
    # Use A* to find the path
    path <- aStarPathfinding(c(startX, startY), c(goalX, goalY), roads$hroads, roads$vroads, dim = dim(roads$hroads)[1])
    
    # Set the next move based on the first step of the path
    if (length(path) > 0) {
      car$nextMove <- path[[1]]
    }
  }
  
  return(car)
}

