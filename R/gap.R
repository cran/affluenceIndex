gap <-
function(x,k){
rho <-k*median(x)
gap <-sum(max(x-rho,0))/length(x)
return(gap)
}
