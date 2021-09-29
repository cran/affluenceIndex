r.cha <-
function(x, weight, k, beta){
  n <- length(x)
  if (is.null(weight)) weight <- rep(1, n)
  
  rhow <- k*weighted.median(x, weight)
  ind <- ifelse(x > rhow, 1, 0)
  r.cha <- sum((1-(rhow/(x))^beta*ind)*weight)/sum(weight)
  
  return(r.cha)
}
