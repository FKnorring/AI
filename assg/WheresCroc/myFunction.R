myFunction = function(moveInfo,readings,positions,edges,probs){
  if(moveInfo$mem$status == 0 || moveInfo$mem$status == 1){
    moveInfo$mem$status = 2
    moveInfo$mem$sprob = init_probabilities(positions)
  }
  sprob = moveInfo$mem$sprob
  new_sprob <- rep(0,40)
  state_space = get_state_space(readings,probs)
  for (i in 1:40){
    new_sprob[i] <- get_new_probabilities(readings, probs,node,edges,i,sprob,state_space)
  }
  # Check the travelers, change their probabilities accordingly
  moveInfo$mem$sprob = new_sprob
  print(new_sprob)
  print(which.max(new_sprob))
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)
  return(moveInfo)
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
  initial_probs <- rep(1/40, 40)
  positions_to_set_to_0 <- c(positions[1], positions[2])
  initial_probs[positions_to_set_to_0] <- 0
  return (initial_probs)
}

getNeighbours=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}

get_state_space = function(readings, probs) {
  prob_salinity = dnorm(readings[1], probs[["salinity"]][, 1], probs[["salinity"]][, 2], FALSE)
  prob_phosphate = dnorm(readings[2], probs[["phosphate"]][, 1], probs[["phosphate"]][, 2], FALSE)
  prob_nitrogen = dnorm(readings[3], probs[["nitrogen"]][, 1], probs[["nitrogen"]][, 2], FALSE)
  product = prob_salinity * prob_phosphate * prob_nitrogen
  
  sum_product = sum(product)

  normalized_probs = product / sum_product
  return(normalized_probs)
}