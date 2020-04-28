install.packages("nnet")
install.packages("RWeka")
install.packages("neuralnet")
install.packages("neural")
install.packages("car")
install.packages("e1071")

library(caret)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(car)
library(e1071)


dataset<-read.csv("train.csv")

set.seed(123)

#---------Variable categorica-------
dataset$SalePrice_C<-recode(dataset$SalePrice, "lo:126520=1;126521:234963=2;234964:hi=3")

corte<-sample(nrow(dataset),nrow(dataset)*0.7)

train<-dataset[corte,]
test<-dataset[-corte,]




#---------------Con caret-----------------------------------
modeloCaret <- train(SalePrice_C~GrLivArea+X1stFlrSF, data = train, method="nnet", trace=F)
test$prediccionCaret<-predict(modeloCaret, newdata = test[,c(44,47,82)])
cfmCaret<-confusionMatrix(as.factor(test$prediccionCaret),as.factor(test$SalePrice_C))
cfmCaret


#------------Con NeuralNet--------------

modelo.nn <- neuralnet(SalePrice_C~GrLivArea+X1stFlrSF, data = train, hidden = 4, rep = 2)
plot(modelo.nn, newdata=test) 
test$predNeuralNet<-round(predict(modelo.nn,newdata = test[,c(44,47,82)]),0)
cfmNeuralNet<-confusionMatrix(as.factor(test$predNeuralNet),as.factor(test$SalePrice_C))
cfmNeuralNet

#------------SVM---------------
mdodeloSVM1 <- svm(SalePrice~GrLivArea+X1stFlrSF, data = train, kernel = "polynomial", cost = 0.1)
mdodeloSVM1
plot(mdodeloSVM1, train[,corte])
predSVM1 <- predict(mdodeloSVM1, newdata = test, type = "class")
plot(predSVM1)


mdodeloSVM2 <- svm(SalePrice_C~GrLivArea+X1stFlrSF, data = train, kernel = "polynomial")
predSVM2 <- predict(mdodeloSVM1, newdata = test)



