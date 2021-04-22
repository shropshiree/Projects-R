library(GAS) #Backtest
library(goftest)
library(fitdistrplus)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)

data <- read.csv("C:/Users/Wiktoria/Documents/IiE/Rok 3/Semestr 6/ilosciowe aspekty/VaR/kursy.csv", sep=";")

data <- data[,c(1,3)]
#CAD, UAH, CZK
data <- data[, c(1,6,13,15)]
data$data <- ymd(data$data)


summary(data[,2:4])
data_gather <- gather(data[,1:4], key=waluta, value = kurs, -data)
str(data_gather)
ggplot(data=data_gather, aes(x=data, y=kurs, col=waluta)) + geom_line(lwd=1) + ggtitle("Kursy walut w latach 2012-2018")


return_rate <- function(vec){
  rate <- NA
  
  for (i in 2:length(vec)){
    rate[i] <-log(vec[i]/vec[i-1])
  }
  #prawy ogon - straty
  -rate*100
}

data$rateCAD <- return_rate(data$X1CAD)
data$rateUAH <- return_rate(data$X1UAH)
data$rateCZK <- return_rate(data$X1CZK)

rate_gather <- gather(data[,-(2:4)], key=waluta, value = stopa, -data)


ggplot(data=rate_gather, aes(x=data, y=stopa, col=waluta)) + geom_line(lwd=1) + ggtitle("Stopy zwrotu w latach 2012-2018")
p1 <- ggplot(data=data, aes(x=data, y=rateCAD)) + geom_line(col='deeppink') + ggtitle("Stopy zwrotu CAD w latach 2012-2018")
p2 <- ggplot(data=data, aes(x=data, y=rateUAH)) + geom_line(col='deeppink') + ggtitle("Stopy zwrotu UAH w latach 2012-2018")
p3 <- ggplot(data=data, aes(x=data, y=rateCZK)) + geom_line(col='deeppink') + ggtitle("Stopy zwrotu CZK w latach 2012-2018")
grid.arrange(p1, p2, p3, nrow=3)

hist_method <- function(vec_rate, days = 500, level=.99){
  
  VaR <- NA
  ES <- NA
  
  for (i in 2:(length(vec_rate)-500)){
    temp_vec <- vec_rate[i:(days+i-1)]
    VaR[i] <- quantile(temp_vec, level, na.rm = TRUE)
    ES[i] <- mean(temp_vec[temp_vec >= VaR[i]])
  }
  return(list(VaR=VaR, ES=ES))
}

hist_CAD <- hist_method(data$rateCAD)
hist_UAH <- hist_method(data$rateUAH)
hist_CZK <- hist_method(data$rateCZK)


CAD1 <- data.frame(data = data$data[502:1764], rate = data$rateCAD[502:1764], VaR = hist_CAD$VaR[-1], ES = hist_CAD$ES[-1])
CAD1 <- gather(CAD1, key = name, value = value, -data)
ggplot(data=CAD1, aes(x=data, y=value, col=name)) + geom_line(lwd=1) + ggtitle("Metoda historyczna CAD")

UAH1 <- data.frame(data = data$data[502:1764], rate = data$rateUAH[502:1764], VaR = hist_UAH$VaR[-1], ES = hist_UAH$ES[-1])
UAH1 <- gather(UAH1, key = name, value = value, -data)
ggplot(data=UAH1, aes(x=data, y=value, col=name)) + geom_line(lwd=1) + ggtitle("Metoda historyczna UAH")


CZK1 <- data.frame(data = data$data[502:1764], rate = data$rateCZK[502:1764], VaR = hist_CZK$VaR[-1], ES = hist_CZK$ES[-1])
CZK1 <- gather(CZK1, key = name, value = value, -data)
ggplot(data=CZK1, aes(x=data, y=value, col=name)) + geom_line(lwd=1) + ggtitle("Metoda historyczna CZK")

boot_method <- function(vec_rate, days = 50, level = .99, N = 100){
  
  VaR <- NA
  ES <- NA
  
  for (i in 2:(length(vec_rate)-500)){
    
    temp_vec <- vec_rate[i:(days+i-1)]
    temp_var <- NULL
    temp_es <- NULL
    
    for (j in 1:N){
      temp <- sample(temp_vec,700, replace = T)
      temp_var[j] <- quantile(temp, level, na.rm = TRUE)
      temp_es[j] <- mean(temp[temp >= temp_var[j]])
    }
    
    VaR[i] <- mean(temp_var)
    ES[i] <- mean(temp_es)
  }
  
  return(list(VaR=VaR, ES=ES))
}

boot_CAD <- boot_method(data$rateCAD)
boot_UAH <- boot_method(data$rateUAH)
boot_CZK <- boot_method(data$rateCZK)

CAD2 <- data.frame(data = data$data[502:1764], rate = data$rateCAD[502:1764], VaR = boot_CAD$VaR[-1], ES = boot_CAD$ES[-1])
CAD2 <- gather(CAD2, key = name, value = value, -data)
ggplot(data=CAD2, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda bootstrap CAD")

UAH2 <- data.frame(data = data$data[502:1764], rate = data$rateUAH[502:1764], VaR = boot_UAH$VaR[-1], ES = boot_UAH$ES[-1])
UAH2 <- gather(UAH2, key = name, value = value, -data)
ggplot(data=UAH2, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda bootstrap UAH")


CZK2 <- data.frame(data = data$data[502:1764], rate = data$rateCZK[502:1764], VaR = boot_CZK$VaR[-1], ES = boot_CZK$ES[-1])
CZK2 <- gather(CZK2, key = name, value = value, -data)
ggplot(data=CZK2, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda bootstrap CZK")

#### 1700 - badam rozklad, a potem w kazdym oknie dopasowywuje do niego parametry VS w kazdym oknie test dopasowania (zawsze wieksze ni¿ 5%), andersona-durbinga
###### wyznaczam rozklad teoretyczny - porownuje do tego z 500 obserwacji, wybieram rozk³ad z najwiekszym p-value, najbardziej podobny do tego z 500
##### losuje z niego 1000 obser, licze var i es. to wszystko w petli

monte_method <- function(vec_rate, days = 500, level = .99, N = 2){
  
  VaR <- NA
  ES <- NA
  
  for (i in 2:(length(vec_rate)-500)){
    
    temp_vec <- vec_rate[i:(days+i-1)]
    temp_var <- NULL
    temp_pval <- data.frame(0,0,0,0,0)
    
    temp_pval[1,1] <- ad.test(temp_vec, "pt",df=fitdistr(temp_vec, "t")$estimate[3])$p.value
    temp_pval[2,1] <- "t"
    
    temp_pval[1,2] <- ad.test(temp_vec, "pnorm",fitdistr(temp_vec, "normal")$estimate)$p.value
    temp_pval[2,2] <- "normal"
    
    temp_pval[1,3] <- ad.test(temp_vec, "pcauchy",fitdistr(temp_vec, "cauchy")$estimate)$p.value
    temp_pval[2,3] <- "cauchy"
    
    temp_pval[1,4] <- ad.test(temp_vec, "plogis",fitdistr(temp_vec, "logistic")$estimate)$p.value
    temp_pval[2,4] <- "logistic"
    
    #temp_pval[1,5] <- ad.test(temp_vec, "plnorm",fitdistr(temp_vec, "lognormal")$estimate)$p.value
    #temp_pval[2,5] <- "lognormal"
    
    colnames(temp_pval) <- c("t","norm", "cauchy", "logistic","lognormal")
    
    
    if(max(as.numeric(temp_pval[1,]))<0.01) {
      VaR[i] <- 0
      ES[i] <- 0
    }else{
      dis <- which.max(temp_pval[1,])
      eval(parse(text = paste0( "distribution <- r", names(dis))))
      
      for (j in 1:N){
        
        temp_var[j] <- quantile(distribution(1000, fitdistr(temp_vec, temp_pval[2,dis])$estimate), level, na.rm = TRUE)
        
      }
      
      VaR[i] <- mean(temp_var)
      ES[i] <- mean(temp_vec[temp_vec >= VaR[i]])
    }
  }
  return(list(VaR=VaR, ES=ES))
}




monte_method2 <- function(vec_rate, days = 500, level = .99, N = 2){
  
  VaR <- data.frame(NA, NA, NA)
  colnames(VaR) <- c("t", "logis", "norm")
  ES <- data.frame(NA, NA, NA)
  colnames(ES) <- c("t", "logis", "norm")
  
  for (i in 2:(length(vec_rate)-500)){
    
    temp_vec <- vec_rate[i:(days+i-1)]
    temp_var <- data.frame(0,0,0)
    temp_es <- data.frame(0,0,0)
    colnames(temp_var) <- c("t", "logis", "norm")
    colnames(temp_es) <- c("t", "logis", "norm")
    
    for(j in 1:N){
      
      temp_t <- rt(10000, fitdistr(temp_vec, "t")$estimate[3])
      temp_var[j,1] <- quantile(temp_t, level, na.rm = TRUE)
      temp_es[j,1] <- mean(temp_t[temp_t >= temp_var[j,1]])
      
      temp_l <- rlogis(10000, fitdistr(temp_vec, "logistic")$estimate)
      temp_var[j,2] <- quantile(temp_l, level, na.rm = TRUE)
      temp_es[j,2] <- mean(temp_l[temp_l >= temp_var[j,2]])
      
      
      temp_n <- rnorm(10000, fitdistr(temp_vec, "normal")$estimate)
      temp_var[j,3] <- quantile(temp_n, level, na.rm = TRUE)
      temp_es[j,3] <- mean(temp_n[temp_n >= temp_var[j,3]])
    }
    
    for(k in 1:3){
      VaR[i,k] <- mean(temp_var[,k])
      ES[i,k] <- mean(temp_es[,k])
    }
    
  }
  return(list(VaR=VaR, ES=ES))
}

monte_CAD <- monte_method2(data$rateCAD)
monte_UAH <- monte_method2(data$rateUAH)
monte_CZK <- monte_method2(data$rateCZK)



CAD3 <- data.frame(data = data$data[501:1764], rate = data$rateCAD[501:1764], monte_CAD)

CAD3_t <- gather(CAD3[,c(1,2,3,6)], key = name, value=value, -data )
p1 <- ggplot(data=CAD3_t, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad t-Studenta - CAD")


CAD3_l <- gather(CAD3[,c(1,2,4,7)], key = name, value=value, -data )
p2 <- ggplot(data=CAD3_l, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad logistyczny - CAD")

CAD3_n <- gather(CAD3[,c(1,2,5,8)], key = name, value=value, -data )
p3 <- ggplot(data=CAD3_n, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad normalny - CAD")


grid.arrange(p1,p2,p3,nrow=3)

CAD3_all <- gather(CAD3[,c(1,2,3,4,5)], key = name, value=value, -data )
ggplot(data=CAD3_all, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - CAD")

monte_USD <- monte_method2(data$rateUSD)
USD <- data.frame(data = data$data[501:1764], rate = data$rateUSD[501:1764], monte_USD)
USD_all <- gather(USD[,c(1,2,3,4,5)], key = name, value=value, -data )
ggplot(data=USD_all, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - CAD")


UAH3 <- data.frame(data = data$data[501:1764], rate = data$rateUAH[501:1764], monte_UAH)

UAH3_t <- gather(UAH3[,c(1,2,3,6)], key = name, value=value, -data )
p1 <- ggplot(data=UAH3_t, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad t-Studenta - UAH")

UAH3_l <- gather(UAH3[,c(1,2,4,7)], key = name, value=value, -data )
p2 <- ggplot(data=UAH3_l, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad logistyczny - UAH")

UAH3_n <- gather(UAH3[,c(1,2,5,8)], key = name, value=value, -data )
p3 <- ggplot(data=UAH3_n, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad normalny - UAH")

grid.arrange(p1,p2,p3,nrow=3)

UAH3_all <- gather(UAH3[,c(1,2,3,4,5)], key = name, value=value, -data )
ggplot(data=UAH3_all, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - UAH")





CZK3 <- data.frame(data = data$data[501:1764], rate = data$rateCZK[501:1764], monte_CZK)

CZK3_t <- gather(CZK3[,c(1,2,3,6)], key = name, value=value, -data )
p1 <- ggplot(data=CZK3_t, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad t-Studenta - CZK")

CZK3_l <- gather(CZK3[,c(1,2,4,7)], key = name, value=value, -data ) 
p2 <- ggplot(data=CZK3_l, aes(x=data, y=value, col=name)) + geom_line()+ ggtitle("Metoda Monte Carlo - rozk³ad logistyczny - CZK")

CZK3_n <- gather(CZK3[,c(1,2,5,8)], key = name, value=value, -data )
p3 <- ggplot(data=CZK3_n, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - rozk³ad normalny - CZK")

grid.arrange(p1,p2,p3,nrow=3)

CZK3_all <- gather(CZK3[,c(1,2,4,5)], key = name, value=value, -data )
ggplot(data=CZK3_all, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda Monte Carlo - CZK")






CAD_all <- data.frame(data = data$data[502:1764], rate = data$rateCAD[502:1764], VaR.boot = boot_CAD$VaR[-1], VaR.hist = hist_CAD$VaR[-1], VaR.logis = monte_CAD$VaR$logis[-1], VaR.norm = monte_CAD$VaR$norm[-1], VaR.t = monte_CAD$VaR$t[-1])
CAD_all <- gather(CAD_all, key=name, value = value, -data)
ggplot(data=CAD_all,aes(x=data, y=value, col=name)) + geom_line() 



UAH_all <- data.frame(data = data$data[502:1764], rate = data$rateUAH[502:1764], VaR.boot = boot_UAH$VaR[-1], VaR.hist = hist_UAH$VaR[-1], VaR.logis = monte_UAH$VaR$logis[-1], VaR.norm = monte_UAH$VaR$norm[-1], VaR.t = monte_UAH$VaR$t[-1])
UAH_all <- gather(UAH_all, key=name, value = value, -data)
ggplot(data=UAH_all,aes(x=data, y=value, col=name)) + geom_line() 



CZK_all <- data.frame(data = data$data[502:1764], rate = data$rateCZK[502:1764], VaR.boot = boot_CZK$VaR[-1], VaR.hist = hist_CZK$VaR[-1], VaR.logis = monte_CZK$VaR$logis[-1], VaR.norm = monte_CZK$VaR$norm[-1], VaR.t = monte_CZK$VaR$t[-1])
CZK_all <- gather(CZK_all, key=name, value = value, -data)
ggplot(data=CZK_all,aes(x=data, y=value, col=name)) + geom_line() 



#TESTY WSTECZNE


test_real_value <- function(rate, VaR){
  
  acc = 0
  
  for (i in 1:length(rate)){
    if(VaR[i] > rate[i]){
      acc = acc+1
    }
  }
  return (acc/length(rate))
}




real_value <- data.frame(0,0,0)


real_value_CAD <- c(test_real_value(data$rateCAD[502:1764],hist_CAD$VaR[-1]),
                    test_real_value(data$rateCAD[502:1764],boot_CAD$VaR[-1]),
                    test_real_value(data$rateCAD[502:1764],monte_CAD$VaR$logis[-1]),
                    test_real_value(data$rateCAD[502:1764],monte_CAD$VaR$norm[-1]),
                    test_real_value(data$rateCAD[502:1764],monte_CAD$VaR$t[-1]))

real_value_UAH <- c(test_real_value(data$rateUAH[502:1764],hist_UAH$VaR[-1]),
                 test_real_value(data$rateUAH[502:1764],boot_UAH$VaR[-1]),
                 test_real_value(data$rateUAH[502:1764],monte_UAH$VaR$logis[-1]),
                 test_real_value(data$rateUAH[502:1764],monte_UAH$VaR$norm[-1]),
                 test_real_value(data$rateUAH[502:1764],monte_UAH$VaR$t[-1])) 


real_value_CZK <- c(test_real_value(data$rateCZK[502:1764],hist_CZK$VaR[-1]),
                    test_real_value(data$rateCZK[502:1764],boot_CZK$VaR[-1]),
                    test_real_value(data$rateCZK[502:1764],monte_CZK$VaR$logis[-1]),
                    test_real_value(data$rateCZK[502:1764],monte_CZK$VaR$norm[-1]),
                    test_real_value(data$rateCZK[502:1764],monte_CZK$VaR$t[-1])) 

real_value <- data.frame(real_value_CAD,real_value_UAH,real_value_CZK)
colnames(real_value) <- c("CAD", "UAH", "CZK")
rownames(real_value) <- c("hist", "boot", "log", "normal","t")
real_value


test_christofferson1 <- function(rate, VaR, window=500){
  
  stat <- NULL
  value <- NULL
  
  for (i in 1:length(VaR)) {
    
    
    temp_vec <- rate[i:(window+i-1)]
    #wyjatek 1 
    excep <- ifelse(temp_vec>VaR[i],1,0)
    
    u00 <- 0
    u01 <- 0
    u10 <- 0
    u11 <- 0
    
    for(j in 1:(length(excep)-1)){
      if(excep[j] == 0 && excep[j+1]==0){
        u00 = u00+1
      } else if(excep[j] == 0 && excep[j+1]==1){
        u01 = u01+1
      } else if(excep[j] == 1 && excep[j+1]==0){
        u10 = u10+1
      } else {
        u11 = u11+1
      }
    }
    
    p <- (u00+u01)/(u00+u01+u10+u11)
    p01 <- u01/(u00+u01)
    p11 <- u11/(u10+u11)
    
    stat[i] <- 2*log(((1-p01)^u00*p01^u01*(1-p11)^u10*p11^u11)/((1-p)^(u00+u10)*p^(u01+u11)))
    
    #alfa 0.01 
    if(stat[i]>=3.841){
      value[i] = 0
    } else {
      value[i] = 1
    }
    
  }
  
  acc <- sum(value)/length(VaR)
  return(acc)
  
}






test_christofferson <- function(rate, VaR, days=500){
  
  stat <- NULL
  value <- NULL
  
  for (i in 1:length(VaR)) {
    
    
    temp_vec <- rate[i:(days+i-1)]
    #wyjatek 1 
    excep <- ifelse(temp_vec>VaR[i],1,0)
    
    u00 <- 0
    u01 <- 0
    u10 <- 0
    u11 <- 0
    
    for(j in 1:(length(excep)-1)){
      if(excep[j] == 0 && excep[j+1]==0){
        u00 = u00+1
      } else if(excep[j] == 0 && excep[j+1]==1){
        u01 = u01+1
      } else if(excep[j] == 1 && excep[j+1]==0){
        u10 = u10+1
      } else {
        u11 = u11+1
      }
    }
    
    p <- (u00+u10)/(u00+u01+u10+u11)
    p0 <- u00/(u00+u01)
    p1 <- u10/(u10+u11)
    
    stat[i] <- -2*log((p/p0)^u00*((1-p)/(1-p0))^u01*(p/p1)^u10*((1-p)/(1-p1))^u11)
    
    #alfa 0.01
    if(stat[i]>=6.635){
      value[i] = 0
    } else {
      value[i] = 1
    }
    
  }
  
  acc <- sum(value)/length(VaR)
  return(acc)
  
}

chris_CAD <- c(test_christofferson(data$rateCAD[-1],hist_CAD$VaR[-1]),
               test_christofferson(data$rateCAD[-1],boot_CAD$VaR[-1]))

chris_UAH <- c(test_christofferson(data$rateUAH[-1],hist_UAH$VaR[-1]),
               test_christofferson(data$rateUAH[-1],boot_UAH$VaR[-1]))

chris_CZK <- c(test_christofferson(data$rateCZK[-1],hist_CZK$VaR[-1]),
               test_christofferson(data$rateCZK[-1],boot_CZK$VaR[-1]))


chris_value <- data.frame(chris_CAD,chris_UAH,chris_CZK)
colnames(chris_value) <- c("CAD", "UAH", "CZK")
rownames(chris_value) <- c("hist", "boot")
chris_value



test_christofferson(data$rateCAD[-1],hist_CAD$VaR[-1])
test_christofferson(data$rateCAD[-1],boot_CAD$VaR[-1])
test_christofferson(data$rateCAD[-1],monte_CAD$VaR$logis[-1])
test_christofferson(data$rateCAD[-1],monte_CAD$VaR$t[-1])
test_christofferson(data$rateCAD[-1],monte_CAD$VaR$norm[-1])


test_christofferson(data$rateUAH[-1],hist_UAH$VaR[-1])
test_christofferson(data$rateUAH[-1],boot_UAH$VaR[-1])
test_christofferson(data$rateUAH[-1],monte_UAH$VaR$logis[-1])
test_christofferson(data$rateUAH[-1],monte_UAH$VaR$t[-1])
test_christofferson(data$rateUAH[-1],monte_UAH$VaR$norm[-1])


test_christofferson(data$rateCZK[-1],hist_CZK$VaR[-1])
test_christofferson(data$rateCZK[-1],boot_CZK$VaR[-1])
test_christofferson(data$rateCZK[-1],monte_CZK$VaR$logis[-1])
test_christofferson(data$rateCZK[-1],monte_CZK$VaR$t[-1])
test_christofferson(data$rateCZK[-1],monte_CZK$VaR$norm[-1])



test_kupca <- function(rate, VaR, days = 500){
  
  value <- NULL
  stat <- NULL
  
  for (i in (1:length(var)))
  {
    temp_vec <- rate[i:(days+i-1)]
    
    #wyjatek 1
    excep <- ifelse(temp_vec > VaR[i], 1 , 0)
    stat[i] <- sum(excep)
    
    #przedzial odpowiedni - 1-10
    if (stat[i] < 11 && stat[i] > 0){
      value[i] <- 1
    }else {
      value[i] <- 0}
  }
  
  acc <- sum(value)/length(VaR)
  return(round(acc),3)
}



kupc_CAD <- c(test_kupca(data$rateCAD[-1],monte_CAD$VaR$logis[-1]),
test_kupca(data$rateCAD[-1],monte_CAD$VaR$t[-1]),
test_kupca(data$rateCAD[-1],monte_CAD$VaR$norm[-1]))

kupc_UAH <- c(test_kupca(data$rateUAH[-1],monte_UAH$VaR$logis[-1]),
test_kupca(data$rateUAH[-1],monte_UAH$VaR$t[-1]),
test_kupca(data$rateUAH[-1],monte_UAH$VaR$norm[-1]))


kupc_CZK <- c(test_kupca(data$rateCZK[-1],monte_CZK$VaR$logis[-1]),
test_kupca(data$rateCZK[-1],monte_CZK$VaR$t[-1]),
test_kupca(data$rateCZK[-1],monte_CZK$VaR$norm[-1]))

kupc_value <- data.frame(kupc_CAD,kupc_UAH,kupc_CZK)
colnames(kupc_value) <- c("CAD", "UAH", "CZK")
rownames(kupc_value) <- c("log", "t","normal")
kupc_value

ggplot(data=CAD1, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda bootstrap CAD") + scale_color_manual(values=c("#FF0099", "#0033FF", "#00FF00"))
ggplot(data=CAD1, aes(x=data, y=value, col=name)) + geom_line() + ggtitle("Metoda historyczna CAD")
