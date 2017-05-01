install.packages("e1071")
library(e1071)
library(rpart)
library(mlbench)
data(Glass)

#Este ejercicio trabaja con un dataset de ejemplo llamado Glass
#Del cual se tomara una muestra de 2/3 del total para entrenamiento
#y se dejará un 1/3 para la prueba
View(Glass)
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

#definion del modelo svm
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)

#prediccion, la columna 10 del dataset es la que define las clases
svm.pred <- predict(svm.model, testset[,-10])

#tabla de las predicciones
t1 <- table(pred = svm.pred, true = testset[,10])
correctos <- sum(diag(t1)) / nrow(testset) *100
t1
correctos
#sumario del modelo svm
summary(svm.model)

#funcion tune
mytunedsvm <- tune.svm(Type ~ ., data = trainset, gamma = 2^(-1:1), cost = 2^(2:4)) 
summary(mytunedsvm)

svm.model2 <- mytunedsvm$best.model
svm.pred2 <- predict(svm.model2, testset[,-10])
t2 <- table(pred = svm.pred2, true = testset[,10])
correctos2 <- sum(diag(t2)) / nrow(testset) *100
t2
correctos2

svm.model3 <- best.tune(svm, Type~., data=trainset, kernel = "radial")

svm.pred3 <- predict(svm.model3, testset[,-10])
t3 <- table(pred = svm.pred3, true = testset[,10])
correctos3 <- sum(diag(t3)) / nrow(testset) *100
t3
correctos3

plot(svm.model, trainset, Al ~ Si)
#la funcion tune trabaja con el conjunto de entrenamiento y en base a un rango de los 
#parametros definidos, obtiene los optimos para este modelo