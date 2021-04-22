library(ggplot2)
library(dplyr)

#cena europejskiej opcji kupna
s0 <- 100
K <- 100
r <- 0.02
T <- 0.25
sigma <- 0.2

cenaOpcji <- function(s0, K, r, T, sigma){
  d1 <- (log(s0/K) + (r+sigma^2/2)*T)/(sigma*T^(.5))
  d2 <- d1 - sigma*T^(.5)
  
  C <- s0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(C)
}
C <- cenaOpcji(s0, K, r,T, sigma)

# +/- 10%
fun1 <- function(x){
  seq(.9*x, 1.1*x, x*.01)
}

vecK <- fun1(K)
vecR <- fun1(r)
vecS <- fun1(sigma)

resK <- c()
for(i in 1:length(vecK)){
  resK[i] <- cenaOpcji(s0,vecK[i],r,T,sigma)
}
dataK <- data.frame(vecK, resK)


resR <- c()
for(i in 1:length(vecR)){
  resR[i] <- cenaOpcji(s0,K,vecR[i],T,sigma)
}
dataR <- data.frame(vecR, resR)

resS <- c()
for(i in 1:length(vecS)){
  resS[i] <- cenaOpcji(s0,K,r,T,vecS[i])
}
dataS <- data.frame(vecS, resS)

ggplot(dataK, aes(x=vecK,y=resK)) + geom_line()
ggplot(dataR, aes(x=vecR,y=resR)) + geom_line()
ggplot(dataS, aes(x=vecS,y=resS)) + geom_line()

dataK <- dataK %>% mutate(delta = (resK - C)/(vecK - K))
ggplot(dataK, aes(x=vecK, y=delta)) + geom_line()