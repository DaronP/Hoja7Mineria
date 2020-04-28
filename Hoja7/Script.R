install.packages("nnet")
install.packages("RWeka")
install.packages("neuralnet")
install.packages("neural")
install.packages("car")

library(caret)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(car)


dataset<-read.csv("train.csv")

#---------Variable categorica-------
dataset$SalePrice_C<-recode(dataset$SalePrice, "lo:12664=1;12665:235197=2;235198:hi=3")

corte<-sample(nrow(dataset),nrow(dataset)*0.7)

train<-dataset[corte,]
test<-dataset[-corte,]

dataset.SaleCondition



#---------------Con nnet-----------------------------------
modelo.nn2 <- nnet(train$SalePrice_C~.,data = train, subset = corte, size=2, rang=0.1, decay=5e-4, maxit=200) 
prediccion2 <- as.data.frame(predict(modelo.nn2, newdata = test[,c(44,47,82)]))
columnaMasAlta<-apply(prediccion2, 1, function(x) colnames(prediccion2)[which.max(x)])
test$prediccion2<-columnaMasAlta

cfm<-confusionMatrix(as.factor(test$prediccion2),test$SalePrice_C)
cfm


#------------Con NeuralNet--------------

modelo.nn <- neuralnet(train$SalePrice_C~train$GrLivArea+train$X1stFlrSF, train[,c(44,47,82)], hidden = 2, rep = 3)
plot(modelo.nn, newdata=test) 
test$predNeuralNet<-round(predict(modelo.nn,newdata = test[,c(44,47,82)]),0)
cfmNeuralNet<-confusionMatrix(as.factor(test$predNeuralNet),as.factor(test$SalePrice_C))
cfmNeuralNet