library(ggplot2)

RelativeEntropy = function (P,Q) {
  xentropy = -1
  if (length(P) == length(Q)) {
    xentropy = 0
    for (i in seq(1,length(P)))
      xentropy = xentropy + P[i] * log(P[i]/Q[i])
  }
  else {
    warning("P and Q do not have same number of elements")
  }
  return(xentropy)
}


X = seq(-5,5,by=0.001)
P1 = sapply(X,function (x) dnorm(x, mean=0,sd=sqrt(0.9)))
P2 = sapply(X,function (x) dnorm(x, mean=0,sd=sqrt(1.5)))
P3 = sapply(X,function (x) dt(x,df=1))
P4 = sapply(X,function (x) dunif(x,min=-5,5))
Q = sapply(X,function (x) dnorm(x, mean=0,sd=1))


data.df = data.frame(
  x = rep(X,4),
  q = rep(Q,4),
  p = c(P1,P2,P3,P4),
  dist = factor(rep(c("P1","P2","P3","P4"),rep(length(Q),4)))
)

distribution_names <- list(
  'P1'="Norm(meam=0,var=0.9)",
  'P2'="Norm(mean=0,var=1.5)",
  'P3'="T(df=1)",
  'P4'="Unif(min=-5,max=-5)"
)

distribution_labeller <- function(variable,value){
  return(distribution_names[value])
}

annotation.df = data.frame(
  x = rep(0,4),
  q = rep(0,4),
  p = rep(0,4),
  dist = factor(c("P1","P2","P3","P4")),
  lab = round(unlist(lapply(list(P1,P2,P3,P4),RelativeEntropy, Q = Q)),2)
)


# plot distro
g = ggplot(data.df, aes(x = x, group = dist))
g = g + geom_line(aes(y = p, colour="Posterior"))
g = g + geom_line(aes(y = q, colour="Prior"))
g = g + facet_grid(.~dist,labeller=distribution_labeller) 
g = g + geom_text(data = annotation.df, aes(y = p, label =lab) ) 
g = g + scale_colour_manual("", values = c("Posterior"="red", "Prior"="blue"))
g = g + theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),               
  panel.background = element_blank(), 
  axis.line = element_line(colour = "black"))

plot(g)

# old working
#g = ggplot(data.df, aes(x = x, y = p, group = dist, colour ="Posterior")) + geom_line()
#g = g + geom_line(aes(x = x, y = q, group = dist, colour="Prior"), show_guide = FALSE)
#g = g + facet_grid(.~dist,labeller=distribution_labeller) 
#g = g + annotate("text", label = "Toto", size = 4, x = 0, y = 0)
#g = g + geom_text(df, aes(x = xlab, y = y,group = dist, label=lab)) 
#g = g + geom_text(data = annotation.df, aes(label =lab) ) 
#g = g + scale_colour_manual("", values = c("Posterior"="green", "Prior"="blue")) +
#  plot(g)




