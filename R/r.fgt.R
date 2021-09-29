r.fgt <-
function(x, weight, k, alpha){
  n <- length(x)
  if (is.null(weight)) weight <- rep(1, n)
  
  rhow <- k*weighted.median(x, weight)
  ind <- ifelse(x > rhow, 1, 0)
  r.fgt <- sum(((((x - rhow)/x)*ind)*weight)^alpha)/sum(weight)
  return(r.fgt)
}
