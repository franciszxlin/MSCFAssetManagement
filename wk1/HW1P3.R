# Obtain the data
mly<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk1\\monthly.csv', header=TRUE, sep=',')
qly<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk1\\quarterly.csv', header=TRUE, sep=',')
aly<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk1\\annually.csv', header=TRUE, sep=',')
gamma<-3
# Part (a)
# Compute excess returns
Rindex<-diff(log(as.numeric(mly$Index)))
cleanmly<-mly[-1,]
cleanmly<-cbind(cleanmly, Rindex)
cleanmly$Rex<-cleanmly$Rindex-cleanmly$Rfree
# Estimate mean and variance of excess monthly return until 198012
est_mean<-mean(cleanmly[cleanmly$ï..yyyymm<=198012,]$Rex)
est_var<-var(cleanmly[cleanmly$ï..yyyymm<=198012,]$Rex)
# Compute optimal portfolio weight for SP500
w_opt<-est_mean/(gamma*est_var)
# Compute portfolio excess monthly ret
cleanmly$Rport<-w_opt*cleanmly$Rex
# Compute and plot wealth plot from 198101 to 201312
b1<-which(cleanmly$ï..yyyymm==198101)
e1<-which(cleanmly$ï..yyyymm==201312)
siz1<-e1-b1+1
wvec1<-rep(1, times=siz1)
for (i in 2:siz1)
{
  wvec1[i]<-wvec1[i-1]*(1+cleanmly$Rfree[b1+i-1]+cleanmly$Rport[b1+i-1])
}
plot(wvec1, type='l', xlab='t', ylab='wealth', main='Wealth path from January 1981 to December 2013')
# Compute and plot wealth plot from 198101 to 199012
b2<-which(cleanmly$ï..yyyymm==198101)
e2<-which(cleanmly$ï..yyyymm==199012)
siz2<-e2-b2+1
wvec2<-rep(1, times=siz2)
for (i in 2:siz2)
{
  wvec2[i]<-wvec2[i-1]*(1+cleanmly$Rfree[b2+i-1]+cleanmly$Rport[b2+i-1])
}
plot(wvec2, type='l', xlab='t', ylab='wealth', main='Wealth path from January 1981 to December 1990')
# Compute and plot wealth plot from 199101 to 200012
b3<-which(cleanmly$ï..yyyymm==199101)
e3<-which(cleanmly$ï..yyyymm==200012)
siz3<-e3-b3+1
wvec3<-rep(1, times=siz3)
for (i in 2:siz3)
{
  wvec3[i]<-wvec3[i-1]*(1+cleanmly$Rfree[b3+i-1]+cleanmly$Rport[b3+i-1])
}
plot(wvec3, type='l', xlab='t', ylab='wealth', main='Wealth path from January 1991 to December 2000')
# Compute and plot wealth plot from 200101 to 201012
b4<-which(cleanmly$ï..yyyymm==200101)
e4<-which(cleanmly$ï..yyyymm==201012)
siz4<-e4-b4+1
wvec4<-rep(1, times=siz4)
for (i in 2:siz4)
{
  wvec4[i]<-wvec4[i-1]*(1+cleanmly$Rfree[b4+i-1]+cleanmly$Rport[b4+i-1])
}
plot(wvec4, type='l', xlab='t', ylab='wealth', main='Wealth path from January 2001 to December 2010')
# Part (b)
# clean monthly data
dpRatio<-as.numeric(mly$dividend)/as.numeric(mly$Index)
regmly<-cbind(mly, dpRatio)
regmly$Rindex<-0
Rindex<-diff(log(as.numeric(mly$Index)))
regmly$Rindex[2:nrow(regmly)]<-Rindex
regmly$Rex<-regmly$Rindex-regmly$Rfree
df1<-as.data.frame(cbind(regmly$Rex[2:nrow(regmly)], regmly$dpRatio[1:nrow(regmly)-1]))
# clean quarterly data
dpRatio<-as.numeric(qly$dividend)/as.numeric(qly$Index)
regqly<-cbind(qly, dpRatio)
regqly$Rindex<-0
Rindex<-diff(log(as.numeric(qly$Index)))
regqly$Rindex[2:nrow(regqly)]<-Rindex
regqly$Rex<-regqly$Rindex-regqly$Rfree
df2<-as.data.frame(cbind(regqly$Rex[2:nrow(regqly)], regqly$dpRatio[1:nrow(regqly)-1]))
# clean annual data
dpRatio<-as.numeric(aly$dividend)/as.numeric(aly$Index)
regaly<-cbind(aly, dpRatio)
regaly$Rindex<-0
Rindex<-diff(log(as.numeric(aly$Index)))
regaly$Rindex[2:nrow(regaly)]<-Rindex
regaly$Rex<-regaly$Rindex-regaly$Rfree
df3<-as.data.frame(cbind(regaly$Rex[2:nrow(regaly)], regaly$dpRatio[1:nrow(regaly)-1]))
# Implement regression
holdfit1<-lm(V1~V2, data=df1)
holdfit2<-lm(V1~V2, data=df2)
holdfit3<-lm(V1~V2, data=df3)
summary(holdfit1)
summary(holdfit2)
summary(holdfit3)
# Part (c)
begin<-which(regmly$ï..yyyymm==198101)
end<-which(regmly$ï..yyyymm==201312)
numerator<-0
denomenator<-0
for (i in begin:(end-1))
{
  reg<-lm(regmly$Rex[2:i]~regmly$dpRatio[1:i-1])
  histmean<-mean(regmly$Rex[2:i])
  predictmean<-reg$coefficients[1]+reg$coefficients[2]*regmly$dpRatio[i]
  numerator<-numerator+(regmly$Rex[i+1]-predictmean)^2
  denomenator<-denomenator+(regmly$Rex[i+1]-histmean)^2
}
OOS_Rsquared<-1-numerator/denomenator
OOS_Rsquared
# Part (d)
# i: set the regression coefficient to 0 whenever it is negative
begin<-which(regmly$ï..yyyymm==198101)
end<-which(regmly$ï..yyyymm==201312)
numerator<-0
denomenator<-0
for (i in begin:(end-1))
{
  reg<-lm(regmly$Rex[2:i]~regmly$dpRatio[1:i-1])
  histmean<-mean(regmly$Rex[2:i])
  coef0<-min(reg$coefficients[1], 0)
  coef<-max(reg$coefficients[2], 0)
  predictmean<-coef0+coef*regmly$dpRatio[i]
  numerator<-numerator+(regmly$Rex[i+1]-predictmean)^2
  denomenator<-denomenator+(regmly$Rex[i+1]-histmean)^2
}
OOS_Rsquared<-1-numerator/denomenator
OOS_Rsquared
# ii: set the forecast value to zero whenever it is negative
begin<-which(regmly$ï..yyyymm==198101)
end<-which(regmly$ï..yyyymm==201312)
numerator<-0
denomenator<-0
for (i in begin:(end-1))
{
  reg<-lm(regmly$Rex[2:i]~regmly$dpRatio[1:i-1])
  histmean<-mean(regmly$Rex[2:i])
  coef0<-min(reg$coefficients[1], 0)
  coef<-max(reg$coefficients[2], 0)
  predictmean<-coef0+coef*regmly$dpRatio[i]
  predictmean<-max(predictmean, 0)
  numerator<-numerator+(regmly$Rex[i+1]-predictmean)^2
  denomenator<-denomenator+(regmly$Rex[i+1]-histmean)^2
}
OOS_Rsquared<-1-numerator/denomenator
OOS_Rsquared
# Part (e)
# I will use regression results to tilt the weights 
mly<-read.csv('C:\\Users\\zil20\\Desktop\\Asset Management\\wk1\\monthly.csv', header=TRUE, sep=',')
Rindex<-diff(log(as.numeric(mly$Index)))
cleanmly<-mly[-1,]
cleanmly<-cbind(cleanmly, Rindex)
cleanmly$Rex<-cleanmly$Rindex-cleanmly$Rfree
cleanmly$dpRatio<-as.numeric(cleanmly$dividend)/as.numeric(cleanmly$Index)
# Estimate mean and variance of excess monthly return until 198012
est_mean<-mean(cleanmly[cleanmly$ï..yyyymm<=198012,]$Rex)
est_var<-var(cleanmly[cleanmly$ï..yyyymm<=198012,]$Rex)
# Compute optimal portfolio weight for SP500
w_opt<-est_mean/(gamma*est_var)
# Compute and plot wealth plot
b1<-which(cleanmly$ï..yyyymm==200101)
e1<-which(cleanmly$ï..yyyymm==201012)
siz1<-e1-b1+1
w1<-rep(1, times=siz1)
wvec1<-rep(1, times=siz1)
for (i in 2:siz1)
{
  reg<-lm(cleanmly$Rex[2:(b1+i-1)]~cleanmly$dpRatio[1:(b1+i-2)])
  nextpred<-reg$coefficients[1]+reg$coefficients[2]*cleanmly$dpRatio[b1+i-1]
  w1[i-1]<-w_opt*(nextpred/cleanmly$Rex[b1+i])
  wvec1[i]<-wvec1[i-1]*(1+cleanmly$Rfree[b1+i-1]+w1[i-1]*cleanmly$Rex[b1+i-1])
}
wvec11<-rep(1, times=siz1)
for (i in 2:siz1)
{
  wvec11[i]<-wvec11[i-1]*(1+cleanmly$Rfree[b1+i-1]+w_opt*cleanmly$Rex[b1+i-1])
}
# Plot the path of of optimal market-timing weights and the constant path of weights
plot(w1, type='l', col='red', xlab='t', ylab='weights', main='Optimal market-timing weights vs. constant path of weights')
lines(rep(w_opt, times=siz1), col='blue')
legend(60,0.5,legend=c("Optimal", "Constant"), col=c("red", "blue"), lty=1:1, cex=0.8)
# Plot the wealth path
plot(wvec11,type='l', xlab='t', ylab='wealth', main='Wealth path from January 2001 to December 2010', col='red')
lines(wvec1, type='l', col='blue')
legend(80,1.1,legend=c("Optimal", "Constant"), col=c("blue", "red"), lty=1:1, cex=0.8)


