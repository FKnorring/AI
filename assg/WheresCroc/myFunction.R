myFunction = function(moveInfo,readings,positions,edges,probs){
  if(moveInfo$mem$status == 0 || moveInfo$mem$status == 1){
    moveInfo$mem$status == 2
    
  }
    
  
  print(moveInfo)
  print(readings)
  print(positions)
  print(edges)
  print(probs)
  
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)
  get_state_space(readings,probs)
  return(moveInfo)
}

get_state_space = function(readings, probs){
  prob_salinity = dnorm(readings[1],probs[["salinity"]][,1],probs[["salinity"]][,2],FALSE)
  prob_phosphate = dnorm(readings[2],probs[["phosphate"]][,1],probs[["phosphate"]][,2],FALSE)
  prob_nitrogen = dnorm(readings[3],probs[["nitrogen"]][,1],probs[["nitrogen"]][,2],FALSE)
  product = prob_salinity * prob_phosphate * prob_nitrogen
  return (product)
}