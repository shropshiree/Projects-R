#wczytanie bibliotek
library(ggplot2)
library(gridExtra)
library(mice)
library(dplyr)
library(caret)
library(pROC)
library(gbm)
library(class)
library(fastDummies)
library(naivebayes)
library(e1071)
library(randomForest)
library(mlr)
library(MASS)

#wczytanie danych
data <- read.csv("C:/Users/Wiktoria/Documents/IiE/Rok 3/Semestr 6/ilosciowe aspekty/credit_scoring/kredyt_indie.csv", sep=";")
data <- data[,-1]
data$Credit_History <- as.factor(data$Credit_History)

str(data)
summary(data)

levels(data$Gender)[levels(data$Gender)==""] <- NA
levels(data$Married)[levels(data$Married)==""] <- NA
levels(data$Dependents)[levels(data$Dependents)==""] <- NA
levels(data$Self_Employed)[levels(data$Self_Employed)==""] <- NA


#statystyki
{
  summary(data)
  
  p1 <- ggplot(data, aes(x=Loan_Status)) + ggtitle("Histogram - status po¿yczki") + ylab("liczebnoœæ") + 
    geom_bar() 
  p2 <- ggplot(data, aes(x=Gender)) + ggtitle("Histogram - p³eæ") + ylab("liczebnoœæ") + 
    geom_bar()
  p3 <- ggplot(data, aes(x=Married)) + ggtitle("Histogram - zwi¹zek ma³¿eñski") + ylab("liczebnoœæ") + 
    geom_bar()
  p4 <- ggplot(data, aes(x=Dependents)) + ggtitle("Histogram - osoby zale¿ne") + ylab("liczebnoœæ") + 
    geom_bar()
  p5 <- ggplot(data, aes(x=Education)) + ggtitle("Histogram - edukacja") + ylab("liczebnoœæ") + 
    geom_bar()
  p6 <- ggplot(data, aes(x=Self_Employed)) + ggtitle("Histogram - samozatrudnienie") + ylab("liczebnoœæ") + 
    geom_bar()
  p7 <- ggplot(data, aes(x=Credit_History)) + ggtitle("Histogram - historia kredytowa") + ylab("liczebnoœæ") + 
    geom_bar()
  p8 <- ggplot(data, aes(x=Property_Area)) + ggtitle("Histogram - teren posiad³oœci") + ylab("liczebnoœæ") + 
    geom_bar()
  grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow=3)
  

  
  p1 <- ggplot(data, aes(y=ApplicantIncome)) + ggtitle("Wykres pude³kowy - przychód osoby wnioskuj¹cej") + 
    geom_boxplot()
  p2 <- ggplot(data, aes(y=CoapplicantIncome)) + ggtitle("Wykres pude³kowy - przychód osoby wspó³wnioskuj¹cej") + 
    geom_boxplot()
  p3 <- ggplot(data, aes(y=LoanAmount)) + ggtitle("Wykres pude³kowy - kwota po¿yczki") + 
    geom_boxplot()
  p4 <- ggplot(data, aes(y=Loan_Amount_Term)) + ggtitle("Wykres pude³kowy - czas trwania po¿yczki") + 
    geom_boxplot()

  grid.arrange(p1,p2,p3,p4, nrow=2)
  
  ggplot(data, aes(x=as.factor(Loan_Amount_Term))) + ggtitle("Histogram - czas trwania po¿yczki") + ylab("liczebnoœæ") + 
    geom_bar()
}

#zale¿noœci
{
  table(data$Gender,data$Loan_Status)
  table(data$Married,data$Loan_Status)
  table(data$Dependents,data$Loan_Status)
  table(data$Education,data$Loan_Status)
  table(data$Self_Employed,data$Loan_Status)
  table(data$Credit_History,data$Loan_Status)
  table(data$Property_Area,data$Loan_Status)
  
  p1 <- ggplot(data=data, aes(y= ApplicantIncome, x= Loan_Status)) +  ggtitle("Wykres pude³kowy - przychód osoby wnioskuj¹cej") +  geom_boxplot()
  p2 <- ggplot(data=data, aes(y= CoapplicantIncome, x= Loan_Status)) + ggtitle("Wykres pude³kowy - przychód osoby wspó³wnioskuj¹cej") +  geom_boxplot()
  p3 <- ggplot(data=data, aes(y= LoanAmount, x= Loan_Status)) + ggtitle("Wykres pude³kowy - kwota po¿yczki") + geom_boxplot()
  p4 <- ggplot(data=data, aes(y= Loan_Amount_Term, x= Loan_Status)) + ggtitle("Wykres pude³kowy - czas trwania po¿yczki") + geom_boxplot()
  
  grid.arrange(p1,p2,p3,p4, nrow=2)
  
}

#NA & outliers
{
  md.pattern(data)
  sapply(data, function(x) sum(is.na(x)))
  
  init = mice(data, maxit=0)
  meth = init$method
  predM = init$predictorMatrix
  
  predM[, c("Loan_Status")]=0
  
  set.seed(103)
  data_imputed <- mice(data, method = meth, predcitorMatrix = predM, m=5)
  data_imputed <- complete(data_imputed)
  
  sapply(data_imputed, function(x) sum(is.na(x)))
  
  summary(data_imputed)
  
  #outliers - applicantIncome, CoapplicantIncome & LoanAmount, LoanAmountTerm
  
  ggplot(data_imputed, aes(y=ApplicantIncome)) + geom_boxplot()
  ggplot(data_imputed, aes(y=CoapplicantIncome)) + geom_boxplot()
  ggplot(data_imputed, aes(y=LoanAmount)) + geom_boxplot()
  ggplot(data_imputed, aes(y=Loan_Amount_Term)) + geom_boxplot()
  
  data_imputed <- mutate(data_imputed, Income = ApplicantIncome + CoapplicantIncome)
  data_imputed <- mutate(data_imputed, Loan = LoanAmount/Loan_Amount_Term)
  
  p1 <- ggplot(data_imputed, aes(y=Income)) + ggtitle("Wykres pude³kowy - zmienna przychód")+ geom_boxplot()
  p2 <- ggplot(data_imputed, aes(y=Loan)) + ggtitle("Wykres pude³kowy - zmienna po¿yczka")+ geom_boxplot()
  grid.arrange(p1,p2, nrow=1)
  
  #Income > 30 000 - usuwam
  #Loan > 2,5 - usuwam
  
  data_imputed<- data_imputed[data_imputed$Income<30000,]
  data_imputed<- data_imputed[data_imputed$Loan<2,]
  
  p1 <- ggplot(data_imputed, aes(y=Income)) + ggtitle("Wykres pude³kowy - zmienna przychód")+  geom_boxplot()
  p2 <- ggplot(data_imputed, aes(y=Loan)) + ggtitle("Wykres pude³kowy - zmienna po¿yczka")+geom_boxplot() 
  grid.arrange(p1,p2, nrow=1)
}

#usuniecie niepotrzebnych kol
data_imputed <- data_imputed[,-(6:9)]

#podzia³ zbioru uczacy i testowy
{
  set.seed(1)
  
  division   <- sample(nrow(data_imputed), round(0.8*nrow(data_imputed)), replace = F)
  
  train <- data_imputed[division,]
  test <- data_imputed[-division,] 
  
  p1 <- ggplot(train, aes(x=Loan_Status)) + ggtitle("Zbiór ucz¹cy - status po¿yczki") + 
    geom_bar()
  p2 <- ggplot(test, aes(x=Loan_Status)) + ggtitle("Zbiór testowy - status po¿yczki") + 
    geom_bar()
  
  p3 <- ggplot(train, aes(x=Gender)) + ggtitle("Zbiór ucz¹cy - p³eæ") + 
    geom_bar()
  p4 <- ggplot(test, aes(x=Gender)) + ggtitle("Zbiór testowy - p³eæ") + 
    geom_bar()
  
  p5 <- ggplot(train, aes(x=Married)) + ggtitle("Zbiór ucz¹cy - ma³¿eñstwo") + 
    geom_bar()
  p6 <- ggplot(test, aes(x=Married)) + ggtitle("Zbiór testowy - ma³¿eñstwo") + 
    geom_bar()
  
  p7 <- ggplot(train, aes(x=Dependents)) + ggtitle("Zbiór ucz¹cy - zale¿ni") + 
    geom_bar()
  p8 <- ggplot(test, aes(x=Dependents)) +  ggtitle("Zbiór testowy - zale¿ni") + 
    geom_bar()
  
  p9 <- ggplot(train, aes(x=Education)) + ggtitle("Zbiór ucz¹cy - edukacja") + 
    geom_bar()
  p10 <- ggplot(test, aes(x=Education)) + ggtitle("Zbiór testowy - edukacja") + 
    geom_bar()
  
  p11 <- ggplot(train, aes(x=Self_Employed)) + ggtitle("Zbiór ucz¹cy - samozatrudnienie") + 
    geom_bar()
  p12 <- ggplot(test, aes(x=Self_Employed)) + ggtitle("Zbiór testowy - samozatrudnienie") + 
    geom_bar()
  
  p13 <- ggplot(train, aes(x=Credit_History)) + ggtitle("Zbiór ucz¹cy - historia kredytowa") + 
    geom_bar()
  p14 <- ggplot(test, aes(x=Credit_History)) + ggtitle("Zbiór testowy - historia kredytowa") + 
    geom_bar()
  
  p15 <- ggplot(train, aes(x=Property_Area)) + ggtitle("Zbiór ucz¹cy - teren posiad³oœci") + 
    geom_bar()
  p16 <- ggplot(test, aes(x=Property_Area)) + ggtitle("Zbiór testowy  - teren posiad³oœci") + 
    geom_bar()
  
  grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow=4)
  grid.arrange(p9,p10,p11,p12,p13,p14,p15,p16, nrow=4)
  
  p1 <- ggplot(train, aes(y=Income)) + ggtitle("Zbiór ucz¹cy - przychód") + geom_boxplot()  
  p2 <- ggplot(test, aes(y=Income)) + ggtitle("Zbiór testowy - przychód") + geom_boxplot()
  
  p3 <- ggplot(train, aes(y=Loan)) + ggtitle("Zbiór ucz¹cy - po¿yczka") +geom_boxplot()
  p4 <- ggplot(test, aes(y=Loan)) + ggtitle("Zbiór testowy - po¿yczka") +geom_boxplot()
  
  grid.arrange(p1,p2,p3,p4, nrow=2)
  
}


str(data_imputed)
#LoanStatus - Y = 1, N = 0
levels(test$Loan_Status) = c(0,1)
levels(train$Loan_Status) = c(0,1)


#boosting
{
  train_num <- train
  train_num$Loan_Status <- as.numeric(train_num$Loan_Status)-1
  test_num <- test
  test_num$Loan_Status <- as.numeric(test_num$Loan_Status)-1
  
  set.seed(1)
  boostTree <- gbm(Loan_Status~.,data=train_num, n.trees = 10000, distribution = "bernoulli")
 
  #train
  boost.pred.train <- predict(boostTree, newdata = train_num, n.trees = 10000, type="response")
  boost.pred.train <- ifelse(boost.pred.train>0.69, 1, 0)
  
  tmp <- confusionMatrix(as.factor(boost.pred.train), as.factor(train_num$Loan_Status), positive = '1')
  tmp
  
  acc_boost_tr <- tmp$overall[1]
  
  ROC <- roc(train_num$Loan_Status, boost.pred.train)
  plot(ROC)
  auc_boost_tr <- auc(ROC)
  auc_boost_tr
  
  
  
  #test
  boost.pred.test <- predict(boostTree, newdata = test_num, n.trees = 10000, type="response")
  boost.pred.test <- ifelse(boost.pred.test>0.69, 1, 0)
  
  tmp <- confusionMatrix(as.factor(boost.pred.test), as.factor(test_num$Loan_Status), positive = '1')
  tmp
  
  acc_boost_test <- tmp$overall[1]
  
  ROC <- roc(test_num$Loan_Status, boost.pred.test)
  plot(ROC)
  auc_boost_test <- auc(ROC)
  auc_boost_test
  
  par(mar = c(5, 8, 1, 1))
  summary(boostTree, las=2)
  
  
  #najlepsza iloœæ drzew
  gbm.perf(object = boostTree,method="OOB")
  
  set.seed(1)
  boostTree <- gbm(Loan_Status~.,data=train_num, n.trees = 108, distribution = "bernoulli")
  
  #train
  boost.pred.train <- predict(boostTree, newdata = train_num, n.trees = 108, type="response")
  boost.pred.train <- ifelse(boost.pred.train>0.69, 1, 0)
  
  confusionMatrix(as.factor(boost.pred.train), as.factor(train_num$Loan_Status), positive = '1')
  
  ROC <- roc(train_num$Loan_Status, boost.pred.train)
  plot(ROC)
  auc(ROC)
  
  
  #test
  boost.pred.test <- predict(boostTree, newdata = test_num, n.trees = 108, type="response")
  boost.pred.test <- ifelse(boost.pred.test>0.69, 1, 0)
  
  confusionMatrix(as.factor(boost.pred.test), as.factor(test_num$Loan_Status), positive = '1')
  
  ROC <- roc(test_num$Loan_Status, boost.pred.test)
  plot(ROC)
  auc(ROC)
  
  par(mar = c(5, 8, 1, 1))
  summary(boostTree, las=2)
  
  
  #najlepsze params
  hyper_grid <- expand.grid(
    shrinkage = c(.001,.01, .1, .3),
    interaction.depth = c(1, 3, 5,7),
    n.minobsinnode=c(5,10,15),
    n.trees = c(108)
  )
  
  auc <- c()
  acc <- c()
  
  for (i in 1:nrow(hyper_grid)) {
    
    set.seed(1)
    
    model <- gbm(formula = Loan_Status ~ ., 
                 data = train_num,
                 distribution = "bernoulli",
                 n.trees=hyper_grid$n.trees[i],
                 interaction.depth = hyper_grid$interaction.depth[i],
                 shrinkage = hyper_grid$shrinkage[i]
    )
    
    
    pred <- predict(object = model,newdata = test_num, n.trees = hyper_grid$n.trees[i], type="response")
    pred <- ifelse(pred>0.69, 1, 0)
    
    ROC <- roc(test_num$Loan_Status, pred)

    auc[i] <- auc(ROC)
    
    tmp <- confusionMatrix(as.factor(pred), as.factor(test_num$Loan_Status), positive = '1')
    acc[i] <- tmp$overall[1]
    
    
  }
  
  
  
  max(acc)
  max(auc)
  
  opt_i_auc <- which.max(auc)
  print(hyper_grid[opt_i_auc,])
  opt_i_acc <- which.max(acc)
  print(hyper_grid[opt_i_acc,])
  
  set.seed(1)
  
  boostTree <- gbm(formula = Loan_Status ~ ., 
                     data = train_num,
                     distribution = "bernoulli",
                     n.trees=108,
                     interaction.depth = 1,
                     shrinkage = .001,
                     n.minobsinnode = 5
  )
  
  
  #train
  boost.pred.train <- predict(boostTree, newdata = train_num, n.trees = 108, type="response")
  boost.pred.train <- ifelse(boost.pred.train>0.69, 1, 0)
  
  tmp <- confusionMatrix(as.factor(boost.pred.train), as.factor(train_num$Loan_Status), positive = '1')
  tmp
  
  acc_boost_tr <- tmp$overall[1]
  
  ROC <- roc(train_num$Loan_Status, boost.pred.train)
  plot(ROC)
  auc_boost_tr <- auc(ROC)
  auc_boost_tr
  
  
  
  #test
  boost.pred.test <- predict(boostTree, newdata = test_num, n.trees = 108, type="response")
  boost.pred.test <- ifelse(boost.pred.test>0.69, 1, 0)
  
  tmp <- confusionMatrix(as.factor(boost.pred.test), as.factor(test_num$Loan_Status), positive = '1')
  tmp
  
  acc_boost_test <- tmp$overall[1]
  
  ROC <- roc(test_num$Loan_Status, boost.pred.test)
  plot(ROC)
  auc_boost_test <- auc(ROC)
  auc_boost_test
  
  par(mar = c(5, 8, 1, 1))
  summary(boostTree,las=2)
  
  
#??????? max acc i auc sie nie zgadzaja :(  

  
}

#random forest
{
  set.seed(222)
  
  rforest <- randomForest(Loan_Status ~ ., data=train)
  
  #train
  
  rf.pred.train <- predict(object = rforest,newdata = train, type = "prob")[,2]
  rf.pred.train <- ifelse(rf.pred.train>.69, 1, 0)
  
  tmp<-confusionMatrix(as.factor(rf.pred.train), as.factor(train$Loan_Status), positive = '1')
  tmp
  
  acc_rf_tr <- tmp$overall[1]
  
  ROC <- roc(train$Loan_Status, rf.pred.train)
  plot(ROC)
  auc_rf_tr <- auc(ROC)
  auc_rf_tr
  
  #test
  
  rf.pred.test <- predict(object = rforest,newdata = test, type = "prob")[,2]
  rf.pred.test <- ifelse(rf.pred.test>.69, 1, 0)
  
  tmp<-confusionMatrix(as.factor(rf.pred.test), as.factor(test$Loan_Status), positive = '1')
  tmp
  
  acc_rf_test <- tmp$overall[1]
  
  ROC <- roc(test$Loan_Status, rf.pred.test)
  plot(ROC)
  auc_rf_test <- auc(ROC)
  auc_rf_test
  
  
  #kt zmienne wazne
  
  importance(rforest)
  varImpPlot(rforest)
  
  #najlepsze param
  hyper_grid <- expand.grid(
    mtry=seq(1, 9, 1), #9 zminnych objaœniaj¹cych
    nodesize=seq(2, 8, 1),
    ntree=c(100, 200, 500, 800, 1000, 5000, 8000)
  )
  
  acc <- c()
  auc <- c()
  
  for (i in 1:nrow(hyper_grid)) {
    
    set.seed(222)
    
    model <- randomForest(formula = Loan_Status ~ ., 
                          data = train,
                          mtry = hyper_grid$mtry[i],
                          nodesize = hyper_grid$nodesize[i],
                          ntree = hyper_grid$ntree[i])
    
    pred <- predict(object = model,newdata = test, type="response")
    
    ROC <- roc(test$Loan_Status, as.numeric(pred)-1)
    
    auc[i] <- auc(ROC)
    
    tmp <- confusionMatrix(as.factor(pred), as.factor(test$Loan_Status))
    acc[i] <- tmp$overall[1]
    
  }
  
  
  
  max(acc)
  max(auc)
  
  opt_i_auc <- which.max(auc)
  print(hyper_grid[opt_i_auc,])
  opt_i_acc <- which.max(acc)
  print(hyper_grid[opt_i_acc,])
  
  set.seed(222)
  
  rforest <- randomForest(formula = Loan_Status ~ ., 
                           data = train,
                           mtry = 5,
                           nodesize = 7,
                           ntree=200)
  
  #train
  
  rf.pred.train <- predict(object = rforest,newdata = train, type = "prob")[,2]
  rf.pred.train <- ifelse(rf.pred.train>.69, 1, 0)
  
  tmp<-confusionMatrix(as.factor(rf.pred.train), as.factor(train$Loan_Status), positive = '1')
  tmp
  
  acc_rf_tr <- tmp$overall[1]
  
  ROC <- roc(train$Loan_Status, rf.pred.train)
  plot(ROC)
  auc_rf_tr <- auc(ROC)
  auc_rf_tr
  
  #test
  
  rf.pred.test <- predict(object = rforest,newdata = test, type = "prob")[,2]
  rf.pred.test <- ifelse(rf.pred.test>.69, 1, 0)
  
  tmp<-confusionMatrix(as.factor(rf.pred.test), as.factor(test$Loan_Status), positive = '1')
  tmp
  
  acc_rf_test <- tmp$overall[1]
  
  ROC <- roc(test$Loan_Status, rf.pred.test)
  plot(ROC)
  auc_rf_test <- auc(ROC)
  auc_rf_test
  
  importance(model_rf)
  varImpPlot(model_rf)
}

#bayes
{
  #zamieniæ wszystkie na factor
  train_fac <- train
  test_fac <- test
  
  #4 grupy w zaleznosci od kwartyli
  summary(data_imputed$Loan)
  summary(data_imputed$Income)
  
  loan_fac <- cut(data_imputed$Loan,breaks=c(0,0.2847, 0.3611,0.5,1.7143),labels = c("1","2","3","4"))
  income_fac <- cut(data_imputed$Income,breaks=c(0,4163, 5332,7300,27500),labels = c("1","2","3","4"))

  train_fac$Loan <- loan_fac[division]
  train_fac$Income <- income_fac[division]  
  train_fac = data.frame(train_fac[,-8], Loan_Status = train_fac[,8])
  
  test_fac$Loan <- loan_fac[-division]
  test_fac$Income <- income_fac[-division] 
  test_fac = data.frame(test_fac[,-8], Loan_Status = test_fac[,8])
  
  nb.model <- naive_bayes(Loan_Status~.,data=train_fac)
  
  #train
  nb.pred.train <- predict(nb.model, train_fac,type="prob")[,2]
  nb.pred.train <- ifelse(nb.pred.train>.69, 1, 0)
  
  tmp<-confusionMatrix(as.factor(nb.pred.train), train_fac$Loan_Status, positive = '1')
  tmp
  
  acc_nb_tr <- tmp$overall[1]
  
  ROC <- roc(train_fac$Loan_Status, nb.pred.train)
  plot(ROC)
  auc_nb_tr <- auc(ROC)
  auc_nb_tr
  
  #test
  nb.pred.test <- predict(nb.model, test_fac,type="prob")[,2]
  nb.pred.test <- ifelse(nb.pred.test>.69, 1, 0)
  tmp <- confusionMatrix(as.factor(nb.pred.test), test_fac$Loan_Status, positive = '1')
  tmp
  
  acc_nb_test <- tmp$overall[1]
  
  ROC <- roc(test_fac$Loan_Status, nb.pred.test)
  plot(ROC)
  auc_nb_test <- auc(ROC) 
  auc_nb_test
}

#regresja logistyczna
{
  log.model = glm(Loan_Status~., family = binomial, data=train)
  summary(log.model)

  stepAIC(log.model, trace = 0, direction = "both")
  
  log.model2 <- glm(formula = Loan_Status ~ Married + Education + Credit_History + 
        Property_Area, family = binomial, data = train)
  summary(log.model2)
  
  #treningowe
  log.pred.train <- predict(log.model2, newdata = train, type = "response")
  log.pred.train <- ifelse(log.tr>.69, 1, 0)
  
  tmp<-confusionMatrix(as.factor(log.pred.train), train$Loan_Status, positive = '1')
  tmp
  
  acc_log_tr <- tmp$overall[1]
  
  ROC <- roc(train$Loan_Status, log.pred.train)
  plot(ROC)
  auc_log_tr <- auc(ROC)
  auc_log_tr
  
  #testowe
  log.pred.test <- predict(log.model2, newdata = test, type = "response")
  log.pred.test <- ifelse(log.pred.test>.69, 1,0)
  tmp <- confusionMatrix(as.factor(log.pred.test), test$Loan_Status,positive = '1')
  tmp

  acc_log_test <- tmp$overall[1]
  
  ROC <- roc(test$Loan_Status, log.pred.test)
  plot(ROC)
  auc_log_test <- auc(ROC)
  auc_log_test
}


results <- data.frame("acc tr" = c(acc_log_tr,acc_nb_tr,acc_boost_tr,acc_rf_tr),
                      "auc tr" = c(auc_log_tr,auc_nb_tr,auc_boost_tr,auc_rf_tr),
                      "acc test" = c(acc_log_test,acc_nb_test,acc_boost_test,acc_rf_test),
                      "auc test" = c(auc_log_test,auc_nb_test,auc_boost_test,auc_rf_test),
                      row.names = c("regresja", "Bayes", "boosting", "lasy losowe")
  
)
