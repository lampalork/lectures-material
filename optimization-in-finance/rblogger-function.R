library(RCurl)
library(jsonlite)
library(plyr)
library(fOptions)

opts <- list(
  proxy         = "https://ocbcpnet.ocbc.local",
  proxyusername = "OCBCGROUP\\A5105198", 
  proxypassword = "Bln01)sox", 
  proxyport     = 8080
)
options(RCurlOptions = opts)

volatility2price = function(type,strike,impvol,rate,spot,T) {
  if (type == "Call") q = 1
  else q = -1
  
  d1 = (log(spot/strike)+rate*T+0.5*impvol*impvol*T)/(impvol*sqrt(T))
  d2 = d1 - impvol*sqrt(T)
  price = q*(spot*pnorm(q*d1)-strike*exp(-rate*T)*pnorm(q*d2))
  return(price)
}

price2volatility = function(type,strike,price,rate,spot,T) {
  f = function(impvol) {
    return (volatility2price(type,strike,impvol,rate,spot,T)-price)
  }
  volatility = NA
  #volatility = uniroot(f, lower=0, upper=1.0)$root
  try(volatility <- uniroot(f,lower=0, upper=10.0)$root, silent=T)
  return(volatility)
}

pdf2price = function(S,Karray,rate,T,S0,prob) {
  
  D = exp(-rate * T)
  nS = length(S)
  nK = length(Karray)
  dS = S[2] - S[1]
  C = matrix(0,nrow = nK, ncol = nS)
  
  
  for (i in seq(1,nS)) {
    for (j in seq(1,nK)) {
      if (S0>Karray[j]) {
        w = -1
      }
      else {
        w = 1
      }
      
      if (w*S[i] > w*Karray[j]) {
        C[j,i] = D*(w*S[i]-w*Karray[j])
      }
      else {
        C[j,i] = 0
      }
    }
  }
  
  return(t(prob)%*%t(C)*dS)  
}

price2vol = function(moneyness,price,rate,S0,T) {
  
  strike = S0*moneyness
  V = rep(NA,length(price))
  
  for (i in seq(1, length(price))) {
    ObjectiveFunc = function(impvol) {
      return (vol2price(moneyness[i],impvol,rate,S0,T)-price[i])
    }
    
    V[i] = uniroot(ObjectiveFunc, lower=0, upper=10.0)$root   
  }
  
  return(V)
}

vol2price = function(moneyness,impvol,rate,S0,T) {
  
  strike = S0*moneyness
  
  echo = rep(1,length(impvol))
  for (i in seq (1,length(impvol))) {
    if (moneyness[i] < 1.0) {
      echo[i] = -1 #OTM put if moneyness < 100%
    }
  }
  
  P = rep(NA,length(impvol))
  if (length(impvol) !=0) {
    for (i in seq (1,length(impvol))) {
      indicator = echo[i]
      d1 = (log(S0/strike[i])+rate*T+0.5*impvol[i]*impvol[i]*T)/(impvol[i]*sqrt(T))
      d2 = d1 - impvol[i]*sqrt(T)
      P[i] = indicator*(S0*pnorm(indicator*d1)-strike[i]*exp(-rate*T)*pnorm(indicator*d2))
    }
  }
  
  return(P)
}

fixJSON <- function(json){
  #gsub('([^,{:]+):', '"\1":', json)
  gsub("(\\w+)\\s*:",'"\\1":',json) 
}

#library(RJSONIO)
#library(RCurl)
#raw_data <- getURL("http://www.google.com/finance/option_chain?q=NASDAQ:GOOG&output=json")
#raw_data <- gsub("(\\w+)\\s*:",'"\\1":',raw_data)   # enclose keys in double quotes
#data <- fromJSON(raw_data)

"http://www.google.com/finance/option_chain?q=NASDAQ:GOOG&output=json"

URL1 = 'http://www.google.com/finance/option_chain?q=%s&output=json'
URL2 = 'http://www.google.com/finance/option_chain?q=%s&output=json&expy=%d&expm=%d&expd=%d'

getOptionQuotes <- function(symbol){
  url = sprintf(URL1, symbol)

  chain = fromJSON(fixJSON(getURL(url)))

  options = mlply(chain$expirations, function(y, m, d) {
    url = sprintf(URL2, symbol, y, m, d)
    expiry = fromJSON(fixJSON(getURL(url)))

    expiry$calls$type = "Call"
    expiry$puts$type  = "Put"

    prices = rbind(expiry$calls, expiry$puts)

    prices$expiry = sprintf("%4d-%02d-%02d", y, m, d)
    prices$underlying.price = expiry$underlying_price

    prices
  })

  options = cbind(data.frame(symbol), rbind.fill(options))
  
  options = options[,c("symbol","p","b","a","oi","strike","expiry","type","underlying.price")]
  colnames(options) = c("symbol","price","bid","ask","open.interest","strike","expiry","type","spot")
  
  for (col in c("strike", "price", "bid", "ask")) {
     options[, col] = suppressWarnings(as.numeric(options[, col]))
  }
    
  options[, "open.interest"] = suppressWarnings(as.integer(options[,"open.interest"]))

  return(options)
}

mapper = function(PutCall) {
  if (PutCall == "Call") return ("c")
  else if (PutCall == "Put") return ("p")
  else return (NA)
}

# GOOG = getOptionQuotes("NASDAQ:GOOG")
# head(GOOG)
# 
# r = 0.01 # int. rate 1%
# today = Sys.Date()
# GOOG$mid = (GOOG$bid+GOOG$ask)/2
# GOOG$expiry.time = as.numeric(difftime(GOOG$expiry,today,units="days"))/365
# GOOG = subset(GOOG,(GOOG$type == "Call" & GOOG$strike>GOOG$spot) | (GOOG$type == "Put" & GOOG$strike<GOOG$spot))
# 
# GOOG$impvol = rep(NA,nrow(GOOG))
# 
# for (i in seq(1,nrow(GOOG))) {
#   GOOG$impvol[i] = price2volatility(GOOG$type[i],GOOG$strike[i],GOOG$mid[i],r,GOOG$spot[i],GOOG$expiry.time[i])
# }
# 
# GOOG = GOOG[order(GOOG$strike),]
# 
# GOOG.melt = GOOG[,c("spot","strike","impvol","price","expiry")]
# GOOG.melt = GOOG.melt[!is.na(GOOG.melt$impvol),]

#install.packages("quantmod")
#library(quantmod)
#GOOG.yahoo = getOptionChain("AAPL")

# bid / ask
GOOG = getOptionQuotes("NASDAQ:GOOG")
r = 0.01 # int. rate 1%
today = Sys.Date()
GOOG$mid = rep(NA,nrow(GOOG))
for (i in seq(1,nrow(GOOG))) GOOG$mid[i] = mean(c(GOOG$bid[i],GOOG$ask[i]),na.rm = TRUE)
#GOOG$mid = GOOG$price
GOOG$expiry.time = as.numeric(difftime(GOOG$expiry,today,units="days"))/365

#keep liquid call and put
GOOG = subset(GOOG,(GOOG$type == "Call" & GOOG$strike>GOOG$spot) | (GOOG$type == "Put" & GOOG$strike<GOOG$spot))

GOOG$impvol = rep(NA,nrow(GOOG))

for (i in seq(1,nrow(GOOG))) {  
  
  try( GOOG$impvol[i] <- 
         GBSVolatility(price = GOOG$mid[i],
                       TypeFlag = mapper(GOOG$type[i]),
                       S = GOOG$spot[i],
                       X = GOOG$strike[i],
                       Time = GOOG$expiry.time[i],
                       r = r,
                       b = r)
       , silent=T) 
}


GOOG = GOOG[!is.na(GOOG$impvol),]

unique(GOOG$expiry)

g = ggplot(subset(GOOG,expiry=="2016-01-15"))
g = g + geom_line(aes(x = strike, y = impvol, group = type, colour = type))
g = g + geom_vline(xintercept = GOOG$spot[1] )
g = g + ggtitle("Volatility Smile for 3M")
g = g + xlab("Asset")
g = g + ylab("Implied Volatility")
g = g + annotate("text", x=375, y=0.45, label="In-the-Money Puts\nOut-of-the-Money Calls")
g = g + annotate("text", x=725, y=0.45, label="In-the-Money Calls\nOut-of-the-Money Puts")
plot(g)

#install.packages("dplyr")
library(dplyr)
View(subset(GOOG,expiry=="2015-09-18"))
View(filter(GOOG,expiry=="2015-09-18") %>% arrange(-open.interest,strike))
View(filter(GOOG,expiry=="2015-09-18") %>%  filter(GOOG,open.interest > 100) %>% arrange(-open.interest,strike))

with( filter(GOOG,expiry=="2015-09-18",open.interest > 10) %>% arrange(strike),plot(strike,impvol,type="l"))

g = ggplot(filter(GOOG,expiry=="2015-09-18",open.interest > 10) %>% arrange(strike))
g = g + geom_line(aes(x = strike, y = impvol, group = type, colour = type))
plot(g)

str(GOOG)

View(GOOG)


#all IV - vol
g = ggplot(GOOG.melt)
g = g + geom_line(aes(x = strike, y=impvol,  group = expiry, colour = expiry))
g = g + geom_vline(xintercept = GOOG$spot[1] )
g = g + ggtitle("Volatility Surface")
g = g + xlab("Asset")
g = g + ylab("Implied Volatility")
plot(g)

g = ggplot(pdf.data.melt)
g = g + geom_line(aes(x = S, y=value, group = variable, color = variable))
g = g + xlim(350,800)
plot(g)

#all price
g = ggplot(GOOG.melt)
g = g + geom_line(aes(x = strike, y=price,  group = expiry, colour = expiry))
plot(g)

# only one mat
g = ggplot(subset(GOOG.melt,expiry=="2016-01-15"))
g = g + geom_line(aes(x = strike, y=impvol,  group = expiry, colour = expiry))
plot(g)

# entropy algo
unique(GOOG$expiry)
GOOG.melt.reduced = subset(GOOG.melt,expiry=="2016-01-15" & strike %in% c(400,450,500,560,600,650,700))

# ------------- process all tenor in a loop -----------

S0 = GOOG$spot[1]
SMin = 0*S0
SMax = 2*S0
dS = 50.0
S = seq(SMin,SMax, by=dS)
T = as.numeric(difftime(GOOG.1M$expiry[1],Sys.Date(),units="days"))/365
rate = 0.01 #1%

asset.dist = matrix(NA,nrow=length(unique(GOOG$expiry)),ncol=length(S))
i = 0
for (expiry.date in unique(GOOG$expiry)) {  
  tmp = subset(GOOG,expiry == expiry.date)
  tmp = subset(tmp,strike <= S0 * 1.25)
  tmp = subset(tmp,strike >= S0 * 0.25)
  tmp = tmp[order(tmp$strike),]
  expiry.time = unique(tmp$expiry.time)
  
  cat("---Tenor:",expiry.time,"---\n")
  cat("Strikes:",tmp$strike,"\n")
  cat("ImpVol:",tmp$impvol,"\n")
  cat("Price:",tmp$mid,"\n")
  
  #price = vol2price(impvol.strikes,impvol.surface[i,],rate,S0,impvol.tenors[i])
  #cat("Price:",price,"\n")
  
  lambda0 = rep(0,length(tmp$mid)+1)
  q = rep(1,length(S))/length(S)
  res = PMXE(lambda0,S,tmp$strike/tmp$spot,tmp$mid,rate,expiry.time,S0,q)
  cat("LMs:",res$sol,"\n")
  asset.dist[i,] = res$pdf
  i = i+1
}

pdf.data = data.frame(S=S,asset.dist[1,],asset.dist[2,],asset.dist[3,],asset.dist[4,],asset.dist[5,],asset.dist[6,],asset.dist[7,],asset.dist[8,],asset.dist[9,])
pdf.data.melt = melt(pdf.data,id=c("S"))

g = ggplot(pdf.data.melt)
g = g + geom_line(aes(x = S, y=value, group = variable, color = variable))
#g = g + xlim(350,800)
#g = g + geom_vline(xintercept = GOOG$spot[1] )
#g = g + ggtitle("Asset distribution inferred from Option Prices")
#g = g + xlab("Asset")
#g = g + ylab("Probability")
plot(g)



#----------- 1M ----------

subset(GOOG.melt,expiry=="2015-04-24")
GOOG.1M = subset(GOOG.melt,expiry=="2015-04-24" & strike %in% c(440,500,560,600,640))


g = ggplot(GOOG.1M)
g = g + geom_line(aes(x = strike, y=impvol,  group = expiry, colour = expiry))
plot(g)

S0 = GOOG.1M$spot[1]
SMin = 0*S0
SMax = 2*S0
dS = 1.0
S = seq(SMin,SMax, by=dS)
T = as.numeric(difftime(GOOG.1M$expiry[1],Sys.Date(),units="days"))/365
rate = 0.01 #1%

lambda0 = rep(0,length(GOOG.1M$price)+1)
q = rep(1,length(S))/length(S)
res = PMXE(lambda0,S,GOOG.1M$strike/GOOG.1M$spot,GOOG.1M$price,rate,T,S0,q)
cat("LMs:",res$sol)
p.1M = res$pdf

# ---convert to price / IV
relstrikes = seq(0.50,1.50,by=0.05)
absstrikes = relstrikes*S0
price.1M = pdf2price(S,absstrikes,rate,T,S0,p.1M)
impvol.1M = price2vol(relstrikes,price.1M,rate,S0,T)

pdf2price = function(S,Karray,rate,T,S0,prob) {

# ---- 2M --------------
subset(GOOG.melt,expiry=="2015-05-15")
GOOG.2M = subset(GOOG.melt,expiry=="2015-05-15" & strike %in% c(440,500,560,600,635))

S0 = GOOG.2M$spot[1]
SMin = 0*S0
SMax = 2*S0
dS = 1.0
S = seq(SMin,SMax, by=dS)
T = as.numeric(difftime(GOOG.2M$expiry[1],Sys.Date(),units="days"))/365
rate = 0.01 #1%

lambda0 = rep(0,length(GOOG.2M$price)+1)
q = rep(1,length(S))/length(S)
res = PMXE(lambda0,S,GOOG.2M$strike/GOOG.2M$spot,GOOG.2M$price,rate,T,S0,q)
cat("LMs:",res$sol)
p.2M = res$pdf

# ---convert to price / IV
relstrikes = seq(0.50,1.50,by=0.05)
absstrikes = relstrikes*S0
price.2M = pdf2price(S,absstrikes,rate,T,S0,p.2M)
impvol.2M = price2vol(relstrikes,price.2M,rate,S0,T)

# ---- 3M --------------
subset(GOOG.melt,expiry=="2015-06-19")
GOOG.3M = subset(GOOG.melt,expiry=="2015-06-19" & strike %in% c(440,500,560,600,635))

S0 = GOOG.3M$spot[1]
SMin = 0*S0
SMax = 2*S0
dS = 1.0
S = seq(SMin,SMax, by=dS)
T = as.numeric(difftime(GOOG.3M$expiry[1],Sys.Date(),units="days"))/365
rate = 0.01 #1%

lambda0 = rep(0,length(GOOG.3M$price)+1)
q = rep(1,length(S))/length(S)
res = PMXE(lambda0,S,GOOG.3M$strike/GOOG.3M$spot,GOOG.3M$price,rate,T,S0,q)
cat("LMs:",res$sol)
p.3M = res$pdf

# ---convert to price / IV
relstrikes = seq(0.50,1.50,by=0.05)
absstrikes = relstrikes*S0
price.3M = pdf2price(S,absstrikes,rate,T,S0,p.3M)
impvol.3M = price2vol(relstrikes,price.3M,rate,S0,T)

# ---- 6M --------------
subset(GOOG.melt,expiry=="2015-09-18")
GOOG.6M = subset(GOOG.melt,expiry=="2015-09-18" & strike %in% c(400,500,560,600,700))

S0 = GOOG.6M$spot[1]
SMin = 0*S0
SMax = 2*S0
dS = 1.0
S = seq(SMin,SMax, by=dS)
T = as.numeric(difftime(GOOG.6M$expiry[1],Sys.Date(),units="days"))/365
rate = 0.01 #1%

lambda0 = rep(0,length(GOOG.6M$price)+1)
q = rep(1,length(S))/length(S)
res = PMXE(lambda0,S,GOOG.6M$strike/GOOG.6M$spot,GOOG.6M$price,rate,T,S0,q)
cat("LMs:",res$sol)
p.6M = res$pdf

plot(p.6M)

# ---convert to price / IV
relstrikes = seq(0.50,1.50,by=0.05)
absstrikes = relstrikes*S0
price.6M = pdf2price(S,absstrikes,rate,T,S0,p.6M)
impvol.6M = price2vol(relstrikes,price.6M,rate,S0,T)

#----------
pdf.data = data.frame(S=S,pdf1M=p.1M,pdf2M=p.2M,pdf3M=p.3M,pdf6M=p.6M)
pdf.data.melt = melt(pdf.data,id=c("S"))

g = ggplot(pdf.data.melt)
g = g + geom_line(aes(x = S, y=value, group = variable, color = variable))
g = g + xlim(350,800)
g = g + geom_vline(xintercept = GOOG.1M$spot[1] )
g = g + ggtitle("Asset distribution inferred from Option Prices")
g = g + xlab("Asset")
g = g + ylab("Probability")
plot(g)

# ------------------------
impvol.data = data.frame(strikes=absstrikes,impvol1M=impvol.1M,impvol2M=impvol.2M,impvol3M=impvol.3M,impvol6M=impvol.6M)
impvol.data.melt = melt(impvol.data,id=c("strikes"))

g = ggplot(impvol.data.melt)
g = g + geom_line(aes(x = strikes, y=value, group = variable, color = variable))
g = g + geom_vline(xintercept = GOOG.1M$spot[1] )
g = g + ggtitle("Implied Volatility Surface")
g = g + xlab("Strike")
g = g + ylab("Implied Volatility")
plot(g)


