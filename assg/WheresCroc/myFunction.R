myFunction = function(moveInfo,readings,positions,edges,probs){
  if(moveInfo$mem$status == 0 || moveInfo$mem$status == 1){
    moveInfo$mem$status = 2
    moveInfo$mem$sprob = init_probabilities(positions)
  }
  sprob = moveInfo$mem$sprob
  new_sprob <- rep(0,40)
  state_space = get_emissions(readings,probs)
  for (i in 1:40){
    new_sprob[i] <- get_new_probabilities(readings, probs,node,edges,i,sprob,state_space)
  }
  # Check for travelers
  if(!is.na(positions[1])){
    if(positions[1] < 0){
      new_sprob <- rep(0,40)
      new_sprob[-1*positions[1]] = 1
    }else{
      new_sprob[positions[1]] = 0
    }
  } else if (!is.na(positions[2])){
    if(positions[2] < 0){
      new_sprob <- rep(0,40)
      new_sprob[-1*positions[2]] = 1
    }else{
      new_sprob[positions[2]] = 0
    }
  }
  moveInfo$mem$sprob = new_sprob
  goal = which.max(new_sprob)
  move = get_move(goal,edges,positions[3])
  moveInfo$moves = move
  return(moveInfo)
}

get_move = function(goal,edges,position){
  # Check if goal is in neighbours (or same node)
  neighbours = getNeighbours(position,edges)
  if(goal %in% neighbours){
    return (c(goal,0))
  }
  # Else calculate path to it
  path = bfs_shortest_path(position,goal, edges)
  return (c(path[1], path[2]))
}

bfs_shortest_path <- function(start, goal, edges) {
  queue = c(start)
  parents = replicate(40, 0)
  visited = c(start)
  parents[start] = 100
  while(length(queue) != 0){
    current = head(queue,n=1)
    queue = setdiff(queue, c(current))
    neighbours = getNeighbours(current,edges)
    neighbours = setdiff(neighbours, c(current))
    neighbours = setdiff(neighbours, visited)
    for(neighbour in neighbours){
      queue = c(queue,neighbour)
      parents[neighbour] = current
      visited = c(visited, c(neighbour))
    }
  }
  path = backtrack_path(goal,parents)
  return (path)
}

backtrack_path = function(goal, parents){
  backtrack = goal
  path = numeric()
  while (backtrack != 100) {
    if (parents[backtrack] != 100) {
      path = c(c(backtrack), path)
    }
    backtrack = parents[backtrack]
  }
  return (path)
}

get_node_probabilities = function( edges,position){
  neighbours = getNeighbours(position,edges)
  value = (1/length(neighbours))
  return (value)
}

get_new_probabilities = function(readings,probs,node,edges,position, sprob,state_space){
  neighbours = getNeighbours(position,edges)
  sum = 0
  for(neighbour in neighbours){
    sum = sum + get_node_probabilities(edges,neighbour) * sprob[neighbour]
  }
  return (sum * state_space[position])
}

init_probabilities = function(positions){
  initial_probs <- rep(1/38, 40)
  positions_to_set_to_0 <- c(positions[1], positions[2])
  initial_probs[positions_to_set_to_0] <- 0
  return (initial_probs)
}

getNeighbours=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

get_emissions = function(readings, probs) {
  prob_salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  prob_phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  prob_nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  product = prob_salinity * prob_phosphate * prob_nitrogen
  
  sum_product = sum(product)
  
  normalized_probs = product / sum_product
  return(normalized_probs)
}