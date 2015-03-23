#convert an array of implied volaility to option prices using BS
vol2price = function(moneyness,impvol,rate,S0,T) {
  
  strike = S0*moneyness
  
  echo = rep(1,length(impvol))
  for (i in seq (1,length(impvol))) {
    if (moneyness[i] < 1.0) {
      echo[i] = -1 #OTM put if moneyness < 100%
    }
  }
  
  price = rep(NA,length(impvol))
  if (length(impvol) !=0) {
    for (i in seq (1,length(impvol))) {
      indicator = echo[i]
      d1 = (log(S0/strike[i])+rate*T+0.5*impvol[i]*impvol[i]*T)/(impvol[i]*sqrt(T))
      d2 = d1 - impvol[i]*sqrt(T)
      price[i] = indicator*(S0*pnorm(indicator*d1)-strike[i]*exp(-rate*T)*pnorm(indicator*d2))
    }
  }
  
  return(price)
}

PMXE = function(lambda0,S,moneyness,price,rate,T,S0,q) {
  
  K = S0 * moneyness
  
  ObjectiveFunction = function(lambda) {
    
    D = exp(-rate*T)
    nS = length(S)
    nP = length(price)
    nLambda = length(lambda)
    dS = S[2] - S[1]
    
    Mat = matrix(0,nrow = (nP+1), ncol = nS)
    
    for (i in seq(1,nS)) {
      for (j in seq(1,nP)) {
        if (S0>K[j]) {
          w = -1 #put
        }
        else{
          w = 1 #call
        }
        
        if (w*S[i] > w*K[j]) {
          Mat[j,i] = D*(w*S[i]-w*K[j])
        }
        else {
          Mat[j,i] = 0
        }
      }
      # Asset price itself
      Mat[nP+1,i] = D*(S[i])
    }
    
    sum = 0
    mu = 0
    pdftmp = rep(NA,length(q))
    for (i in seq(1,nS)) {      
      tmp1 = 0 #sum
      tmp2 = 0 #mu
      for (j in seq(1,(nP+1))) {
        if (j==(nP+1)) {
          incr1 = lambda[j]*(Mat[j,i]-S0)
          tmp1 = tmp1 + incr1
          
          incr2 = lambda[j]*Mat[j,i]
          tmp2 = tmp2 + incr2
        }
        else {
          incr1 = lambda[j]*(Mat[j,i]-price[j])
          tmp1 = tmp1 + incr1
          
          incr2 = lambda[j]*Mat[j,i]
          tmp2 = tmp2 + incr2
        }
      }
      sum = sum + exp(tmp1)*q[i]*dS
      mu = mu + exp(tmp2)*q[i]*dS
      pdftmp[i] = exp(tmp2)*q[i]
    }
    
    return(list(func=sum,pdf=pdftmp/mu))    
  }
  
  # helper function
  f = function(lambda) ObjectiveFunction(lambda)$func
  
  # trigger optimization
  res = optim(par=lambda0,fn=f,method="BFGS")
  
  lambda_solution = res$par
  pdf = ObjectiveFunction(lambda_solution)$pdf
  
  return(list(sol=lambda_solution,pdf=pdf))
}


S0 = 560.0
SMin = 0*S0
SMax = 3*S0
dS = 1.0
S = seq(SMin,SMax, by=dS)
T = 2/12 #0.25
rate = 0.01
#moneyness = c(1.00)
#IV = c(0.0846306806)


moneyness1 = c(0.60,0.70,0.80,0.90,1.00,1.10,1.20,1.30,1.40)
IV1 = rep(0.25,length(moneyness1))
#moneyness1 = c(0.80,0.90,1.00,1.10,1.20)
#IV1 = c(0.1394885216,0.1091677596,0.0846306806,0.0801399671,0.0919957639)



#moneyness = seq(0.50,1.50,by=0.05)
#IV = c(23.7, 21.8, 20.1, 18.4, 16.9, 15.4, 14.0,
#       12.7, 11.3, 10.0, 9.0, 8.4, 8.5, 8.9, 9.3,
#       9.7, 10.1, 10.5, 10.8, 11.1, 11.3) / 100

#IV = c(26.5,24.1,22.1,21.2,19.8,18.0,16.6,15.4,14.1,
#       12.8,12.0,11.5,11.7,11.8,12.1,13.1,13.7,14.2,15.0,
#       15.7,16.2) / 100
price1 = vol2price(moneyness1,IV1,rate,S0,T)
lambda0 = rep(0,length(price1)+1)
q = rep(1,length(S))/length(S)
res1 = PMXE(lambda0,S,moneyness1,price1,rate,T,S0,q)
cat("LMs:",res1$sol)
p1 = res1$pdf

data.df = data.frame(S=S,prob=p1)

mu = sum(S*(p1*dS))
sigma = sqrt(sum((S-mu)^2 * p1*dS))
phi = sqrt(sigma^2 + mu^2)
mu_lnorm = log(mu^2/phi)
sigma_lnorm = sqrt(log(phi^2/mu^2))


g = ggplot(data.df)
g = g + geom_line(aes(x = S, y=prob, colour = "data"))
g = g + stat_function(fun = function(x) dlnorm(x,meanlog=mu_lnorm,sdlog=sigma_lnorm), size=1, alpha=0.5, aes(color='dlnorm'))
g = g + xlim(350,800)
g = g + ggtitle("Asset distribution with constant volatility")
g = g + xlab("Asset value")
g = g + ylab("Probability mass")
g = g + scale_color_manual("Legend",labels = c("data", "dlnorm"), values = c("blue", "black")) 
plot(g)

plot(S,p1,type="l")

barplot(p1)

cat("mean:",mu <- sum(S*(p1*dS)))
cat("variance:",sigma2 <- sum((S-mu)^2 * p1*dS))
cat("std deviation:",stddev <- sqrt(sigma2))

#install.packages("distrEx")
library(distrEx)
P1 <- DiscreteDistribution(supp = S, prob = p1*dS)
E(P1); var(P1); sd(P1); skewness(P1); kurtosis(P1)

plot(S,p1,type="l")
x = seq(0,150,by=0.1)
lines(x,dnorm(x,mean=mu,sd=stddev),type="l",col="red")

# relationship between mean / variance of lognormal and normal distribution
phi = sqrt(stddev^2 + mu^2)
mu_implied = log(mu^2/phi)
sigma_implied = sqrt(log(phi^2/mu^2))

x = seq(0,150,by=0.001)
lines(x,dnorm(x,mean=mu_implied,sd=sigma_implied),type="l",col="green")

plot(exp(x),dnorm(x,mean=mu_implied,sd=sigma_implied),type="l",col="green", xlim=c(1,100))


lines(exp(x),dnorm(x,mean=mu_implied,sd=sigma_implied),type="l",col="green", xlim=c(1,100))




#ret = rnorm()
#x = seq(5,30,by=0.1)
#plot(x,dnorm(x,mean=15,sd=2),type="l")
#lines(x,dlnorm(x,meanlog=15,sdlog=2),type="l",col="red")
#x1 = seq(5,30000,by=0.1)
#plot(x1,dlnorm(x1,meanlog=15,sdlog=2),type="l",col="red")
#lines(x,dnorm(x,mean=50,sd=sqrt(110.2)),col="red",type="l")
#plot(dlnorm(log(x),mean=0,sd=0.25),col="red",type="l")
#
#plot(function(x) dlnorm(x,meanlog=0,sdlog=0.20),xlim=c(0,3))
#lines(S,p1/mean(p1),type="l",col="red")


#x = rlnorm(500,1,.6)
#grid = seq(0,25,.1)
#plot(grid,dlnorm(grid,0,.6),type="l",xlab="x",ylab="f(x)")
#lines(density(x),col="red")
#legend("topright",c("True Density","Estimate"),lty=1,col=1:2)


#posterior / impact (decrease of ATM volatility)
moneyness2 = c(1.00)
IV2 = c(0.23)
price2 = vol2price(moneyness2,IV2,rate,S0,T)
lambda0 = rep(0,length(price2)+1)
res2 = PMXE(lambda0,S,moneyness2,price2,rate,T,S0,p1)
cat("LMs:",res2$sol)
p2 = res2$pdf

plot(S,p2,type="n")
lines(S,p1,type="l",col="blue")
lines(S,p2,type="l",col="red")

#posterior / impact (increase of ATM volatility)
moneyness3 = c(1.00)
IV3 = c(0.27)
price3 = vol2price(moneyness3,IV3,rate,S0,T)
lambda0 = rep(0,length(price3)+1)
res3 = PMXE(lambda0,S,moneyness3,price3,rate,T,S0,p1)
cat("LMs:",res3$sol)
p3 = res3$pdf

plot(S,p2,type="n",xlim=c(30,80))
lines(S,p1,type="l",col="blue")
lines(S,p2,type="l",col="red")
lines(S,p3,type="l",col="darkgreen")

#posterior / impact (decrease of 110% volatility)
moneyness4 = c(1.10)
IV4 = c(0.20)
price4 = vol2price(moneyness4,IV4,rate,S0,T)
lambda0 = rep(0,length(price4)+1)
res4 = PMXE(lambda0,S,moneyness4,price4,rate,T,S0,p1)
cat("LMs:",res2$sol)
p4 = res4$pdf

#posterior / impact (increase of 110% volatility)
moneyness5 = c(1.10)
IV5 = c(0.30)
price5 = vol2price(moneyness5,IV5,rate,S0,T)
lambda0 = rep(0,length(price5)+1)
res5 = PMXE(lambda0,S,moneyness5,price5,rate,T,S0,p1)
cat("LMs:",res5$sol)
p5 = res5$pdf

#posterior / impact (decrease of 110% volatility)
moneyness6 = c(0.90)
IV6 = c(0.20)
price6 = vol2price(moneyness6,IV6,rate,S0,T)
lambda0 = rep(0,length(price6)+1)
res6 = PMXE(lambda0,S,moneyness6,price6,rate,T,S0,p1)
cat("LMs:",res6$sol)
p6 = res6$pdf

#posterior / impact (increase of 110% volatility)
moneyness7 = c(0.90)
IV7 = c(0.30)
price7 = vol2price(moneyness7,IV7,rate,S0,T)
lambda0 = rep(0,length(price7)+1)
res7 = PMXE(lambda0,S,moneyness7,price7,rate,T,S0,p1)
cat("LMs:",res7$sol)
p7 = res7$pdf

# --------------------------------

library(reshape2)
#ATM
dataATM = data.frame(S=S,
                  prior=p1,
                  Decrease=p2,
                  Increase=p3)
dataATM.melt = melt(dataATM,id = c("S"))
g = ggplot(dataATM.melt)
g = g + geom_line(aes(x = S, y=value,  group = variable, colour = variable))
g = g + xlim(30,80)
plot(g)

#110
data110 = data.frame(S=S,
                     prior=p1,
                     Decrease=p4,
                     Increase=p5)
data110.melt = melt(data110,id = c("S"))
g = ggplot(data110.melt)
g = g + geom_line(aes(x = S, y=value,  group = variable, colour = variable))
g = g + xlim(30,80)
plot(g)

#90
data90 = data.frame(S=S,
                     prior=p1,
                     Decrease=p6,
                     Increase=p7)
data90.melt = melt(data90,id = c("S"))
g = ggplot(data90.melt)
g = g + geom_line(aes(x = S, y=value,  group = variable, colour = variable))
g = g + xlim(30,80)
plot(g)

# combine plot
dataATM.new = dataATM
dataATM.new$group = "ATM"
data110.new = data110
data110.new$group = "OTM-110%"
data90.new = data90
data90.new$group = "OTM-90%"

data.new = rbind(dataATM.new,data110.new,data90.new)
data.new.melt = melt(data.new,id=c("S","group"))

g = ggplot(data.new.melt)
#g = ggplot(subset(data.new.melt,variable=="Increase"))
g = g + geom_line(aes(x = S, y=value,  group = variable, colour = variable))
#g = g + geom_bar(aes(x = S, y=value,  group = group, colour = variable))
g = g + facet_grid(.~group) 
g = g + xlim(30,80)
plot(g)


g = ggplot(subset(dataATM.melt,variable=="prior"))
g = g + geom_line(aes(x = S, y=value,  group = variable, colour = variable))
g = g + stat_function(fun = function(x) dlnorm(x,meanlog=mu_implied,sdlog=sigma_implied), size=1, color='gray')
g = g + stat_function(fun = function(x) dnorm(x,mean=mu,sd=stddev), size=1, color='blue')
g = g + xlim(30,80)
plot(g)


# --------------------------------

#with real market data


