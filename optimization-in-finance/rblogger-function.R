library(RCurl)
library(jsonlite)
library(plyr)

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



fixJSON <- function(json){
  #gsub('([^,{:]+):', '"\1":', json)
  gsub("(\\w+)\\s*:",'"\\1":',json) 
}

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

GOOG = getOptionQuotes("NASDAQ:GOOG")
head(GOOG)

r = 0.01 # int. rate 1%
today = Sys.Date()
GOOG$mid = (GOOG$bid+GOOG$ask)/2
GOOG$expiry.time = as.numeric(difftime(GOOG$expiry,today,units="days"))/365
GOOG= subset(GOOG,(GOOG$type == "Call" & GOOG$strike>GOOG$spot) | (GOOG$type == "Put" & GOOG$strike<GOOG$spot))

GOOG$impvol = rep(NA,nrow(GOOG))

for (i in seq(1,nrow(GOOG))) {
  GOOG$impvol[i] = price2volatility(GOOG$type[i],GOOG$strike[i],GOOG$mid[i],r,GOOG$spot[i],GOOG$expiry.time[i])
  
  #if ((GOOG$type == "Call") && (GOOG$strike[i] > GOOG$spot[i])) {
  #  GOOG$impvol[i] = price2volatility(GOOG$type[i],GOOG$strike[i],GOOG$mid[i],r,GOOG$spot[i],1)
  #}
  #else if ((GOOG$type == "Put") && (GOOG$strike[i] < GOOG$spot[i])) {
  #  GOOG$impvol[i] = price2volatility(GOOG$type[i],GOOG$strike[i],GOOG$mid[i],r,GOOG$spot[i],1)
  #}
}

GOOG = GOOG[order(GOOG$strike),]

GOOG.melt = GOOG[,c("spot","strike","impvol","price","expiry")]
GOOG.melt = GOOG.melt[!is.na(GOOG.melt$impvol),]

#all IV - vol
g = ggplot(GOOG.melt)
g = g + geom_line(aes(x = strike, y=impvol,  group = expiry, colour = expiry))
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



#only take Call when strike 
#volatility2price(600,0.20,0.01,557,1/12)
#[1] 1.605767
#> price2volatility(600,2.30,r,557,1/12)
#[1] 0.2199463


