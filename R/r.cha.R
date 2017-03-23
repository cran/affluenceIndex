r.cha <-
function(x, k, beta){
rho <-k*median(x)

y.6 <-vector(length=length(x))
r.66 <-vector(length=length(x))

for (i in 1:length(x)){
y.6[i]=1-(rho/x[i])^beta

if (y.6[i]>0) 
r.66[i] <-y.6[i]
else r.66[i] <-0
}
r.6 <-sum(r.66)/length(x)

outlist=list(r=r.66, r.cha=r.6)
return(outlist)
}
