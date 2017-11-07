# Parameters
u<-0.07
sigx<-0.02
sige<-0.14
gamma<-3
len<-120
# Part (a)
# Generate a path for rt1
xt<-rnorm(len,0,sigx)
et1<-rnorm(len,0,sige)
rt1<-u+xt+et1
# Compute the path of optimal market-timing weights
wt<-(u+xt)/(gamma*sige^2)
# Get the constant path of weights 
wcon<-rep(u/(gamma*(sigx^2+sige^2)), times=len)
# Plot the path of of optimal market-timing weights and the constant path of weights
plot(wt, type='l', col='red', xlab='t', ylab='weights', main='Optimal market-timing weights vs. constant path of weights')
lines(wcon, col='blue')
legend(100, 2.0, legend=c("Optimal", "Constant"), col=c("red", "blue"), lty=1:1, cex=0.8)
# Part (b)
# Compute the wealth path using market-timing strategy optimal wieghts
vt1<-numeric(len)
v0<-1
vt1[1]<-v0*(1+wt[1]*rt1[1])
for (i in 2:len)
{
  vt1[i]<-vt1[i-1]*(1+wt[i]*rt1[i])
}
# Compute the wealth path using constant weights
vt2<-numeric(len)
vt2[1]<-v0*(1+wcon[1]*rt1[1])
for (i in 2:len)
{
  vt2[i]<-vt2[i-1]*(1+wcon[i]*rt1[i])
}
# plot the wealth paths using two shcemes of weights
plot(vt1, type='l', col='red', xlab='t', ylab='wealth', main='Wealth paths using market-timing weights vs. constant weights')
lines(vt2, col='blue')
legend(20, 2500, legend=c("Optimal path", "Constant path"), col=c("red", "blue"), lty=1:1, cex=0.8)
