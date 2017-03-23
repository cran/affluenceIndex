polar.aff <-
function(x){
x.s <-sort(x)
up <-vector(length=length(x))

for (i in (1:length(x))){
up[i] <-(2*i-length(x)-1)*x.s[i]
}
gini <-sum(up)/((length(x)^2)*mean(x)) 
m <-round(length(x.s)/2)
T <-0.5-sum(x.s[1:m])/sum(x.s)

p <-(T-gini/2)*(mean(x)/median(x))
p.scalar <-2*(2*T-gini)*(mean(x)/median(x))

outlist=list(gini=gini, p=p, p.scalar=p.scalar, T=T)
return(outlist)
}
