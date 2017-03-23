r.fgt <-
function(x, k, alpha){
rho <-k*median(x)

y.7 <-vector(length=length(x))
r.77 <-vector(length=length(x))

for (i in 1:length(x)){
y.7[i]=(x[i]-rho)/rho

if (y.7[i]>0) 
r.77[i] <-(y.7[i])^alpha
else r.77[i] <-0
}
r.7 <-sum(r.77)/length(x)

outlist=list(r=r.77, r.fgt=r.7)
return(outlist)}
