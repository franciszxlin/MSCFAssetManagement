bond=read.csv(file="C:\\Users\\zil20\\Desktop\\Asset Management\\wk4\\bondpriceclean.csv", header=TRUE)
#Data selection
#bond1=bond[1:which(bond$Month==19830630),]
#Part (b)
#Fama=Bliss
bond$f2y1=bond$f2-bond$y1
bond$f3y1=bond$f3-bond$y1
bond$f4y1=bond$f4-bond$y1
bond$f5y1=bond$f5-bond$y1
fm2=lm(rx2~f2y1, data=bond)
summary(fm2)
fm3=lm(rx3~f3y1, data=bond)
summary(fm3)
fm4=lm(rx4~f4y1, data=bond)
summary(fm4)
fm5=lm(rx5~f5y1, data=bond)
summary(fm5)
#Cochrane-Piazzesi
bond$rxhat=(bond$rx2+bond$rx3+bond$rx4+bond$rx5)/4
cpgamma=lm(rxhat~y1+f2+f3+f4+f5, data=bond)
summary(cpgamma)
gamma=cpgamma$coefficients
gamma
bond$factor=gamma[1]+gamma[2]*bond$y1+gamma[3]*bond$f2+gamma[4]*bond$f3+gamma[5]*bond$f4+gamma[6]*bond$f5
cp2=lm(rx2~factor, data=bond)
summary(cp2)
cp3=lm(rx3~factor, data=bond)
summary(cp3)
cp4=lm(rx4~factor, data=bond)
summary(cp4)
cp5=lm(rx5~factor, data=bond)
summary(cp5)
#Part (c) Rerun above code with data selection
#part (d) 
bond=read.csv(file="C:\\Users\\zil20\\Desktop\\Asset Management\\wk4\\bondpriceclean.csv", header=TRUE)
end=nrow(bond)
mid=which(bond$Month==19830630)
# Do necessary computation
bond$f2y1=bond$f2-bond$y1
bond$f3y1=bond$f3-bond$y1
bond$f4y1=bond$f4-bond$y1
bond$f5y1=bond$f5-bond$y1
bond$rxhat=(bond$rx2+bond$rx3+bond$rx4+bond$rx5)/4
#Allocations
fm2numer=c()
denom2=c()
fm3numer=c()
denom3=c()
fm4numer=c()
denom4=c()
fm5numer=c()
denom5=c()
cp2numer=c()
cp3numer=c()
cp4numer=c()
cp5numer=c()
#Main loop
for (i in mid:end)
{
  fm2=lm(rx2~f2y1, data=bond[1:(i-1), ])
  pred2=predict(fm2, newdata=bond[i,])
  fm2numer=c(fm2numer, (pred2-bond$rx2[i])^2)
  denom2=c(denom2, (mean(bond$rx2[1:(i-1)])-bond$rx2[i])^2)
  
  fm3=lm(rx3~f3y1, data=bond[1:(i-1), ])
  pred3=predict(fm3, newdata=bond[i,])
  fm3numer=c(fm3numer, (pred3-bond$rx3[i])^2)
  denom3=c(denom3, (mean(bond$rx3[1:(i-1)])-bond$rx3[i])^2)  
  
  fm4=lm(rx4~f4y1, data=bond[1:(i-1), ])
  pred4=predict(fm4, newdata=bond[i,])
  fm4numer=c(fm4numer, (pred4-bond$rx4[i])^2)
  denom4=c(denom4, (mean(bond$rx4[1:(i-1)])-bond$rx4[i])^2)
  
  fm5=lm(rx5~f5y1, data=bond[1:(i-1), ])
  pred5=predict(fm5, newdata=bond[i,])
  fm5numer=c(fm5numer, (pred5-bond$rx5[i])^2)
  denom5=c(denom5, (mean(bond$rx5[1:(i-1)])-bond$rx5[i])^2)
  
  cpgamma=lm(rxhat~y1+f2+f3+f4+f5, data=bond[1:(i-1),])
  gamma=cpgamma$coefficients
  bond$factor=gamma[1]+gamma[2]*bond$y1+gamma[3]*bond$f2+gamma[4]*bond$f3+gamma[5]*bond$f4+gamma[6]*bond$f5
  
  cp2=lm(rx2~factor, data=bond[1:(i-1),])
  cpred2=predict(cp2, newdata=bond[i,])
  cp2numer=c(cp2numer, (cpred2-bond$rx2[i])^2)
  
  cp3=lm(rx3~factor, data=bond[1:(i-1),])
  cpred3=predict(cp3, newdata=bond[i,])
  cp3numer=c(cp3numer, (cpred3-bond$rx3[i])^2) 
  
  cp4=lm(rx4~factor, data=bond[1:(i-1),])
  cpred4=predict(cp4, newdata=bond[i,])
  cp4numer=c(cp4numer, (cpred4-bond$rx4[i])^2)

  cp5=lm(rx5~factor, data=bond[1:(i-1),])
  cpred5=predict(cp5, newdata=bond[i,])
  cp5numer=c(cp5numer, (cpred5-bond$rx5[i])^2)  
}
oosfm2=1-sum(fm2numer)/sum(denom2)
oosfm2
oosfm3=1-sum(fm3numer)/sum(denom3)
oosfm3
oosfm4=1-sum(fm4numer)/sum(denom4)
oosfm4
oosfm5=1-sum(fm5numer)/sum(denom5)
oosfm5
ooscp2=1-sum(cp2numer)/sum(denom2)
ooscp2
ooscp3=1-sum(cp3numer)/sum(denom3)
ooscp3
ooscp4=1-sum(cp4numer)/sum(denom4)
ooscp4
ooscp5=1-sum(cp5numer)/sum(denom5)
ooscp5

