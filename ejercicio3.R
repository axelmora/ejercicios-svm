data("iris")

#Este ejercicio trabaja con un dataset de ejemplo llamado iris
View(iris)
set.seed(101)
testindex <- sample(1:nrow(iris),size=105)
testset <- iris[-testindex,] #prueba
trainset <- iris[testindex,] #entrenamiento

####EXPERIMENTO 1####
#definion del modelo svm
svm.model <- svm(Species ~ Sepal.Width + Sepal.Length, data = trainset, cost = 100, gamma = 1)
#sumario del modelo svm
summary(svm.model)
#prediccion, la columna 5 del dataset es la que define las clases "Species"
svm.pred <- predict(svm.model, testset[,-5])
#tabla de las predicciones
t1 <- table(pred = svm.pred, true = testset[,5])
correctos <- sum(diag(t1)) / nrow(testset) *100
t1
correctos
#grafica del modelo
plot(svm.model, trainset, Sepal.Width ~ Sepal.Length)


####EXPERIMENTO 2####
#funcion tune.svm
mytunedsvm <- tune.svm(Species ~ Sepal.Width + Sepal.Length, data = trainset, gamma = 2^(-1:1), cost = 2^(2:4)) 
summary(mytunedsvm)
svm.model2 <- mytunedsvm$best.model
summary(svm.model2)
svm.pred2 <- predict(svm.model2, testset[,-5])
t2 <- table(pred = svm.pred2, true = testset[,5])
correctos2 <- sum(diag(t2)) / nrow(testset) *100
t2
correctos2
plot(svm.model2, trainset, Sepal.Width ~ Sepal.Length)

####EXPERIMENTO 3####
#funcion best.tune
svm.model3 <- best.tune(svm,Species ~ Sepal.Width + Sepal.Length, data = trainset) 
summary(svm.model3)
svm.pred3 <- predict(svm.model3, testset[,-5])
t3 <- table(pred = svm.pred3, true = testset[,5])
correctos3 <- sum(diag(t3)) / nrow(testset) *100
t3
correctos3
plot(svm.model3, trainset, Sepal.Width ~ Sepal.Length)

####EXPERIMENTO 4####
c <- seq(1,10, by = 1)
mytunedsvm4 <- tune.svm(Species ~ Sepal.Width + Sepal.Length, data = trainset, cost = c, gamma = c)
svm.model4 <- mytunedsvm4$best.model
summary(svm.model4)
svm.pred4 <- predict(svm.model4, testset[,-5])
t4 <- table(pred = svm.pred4, true = testset[,5])
correctos4 <- sum(diag(t4)) / nrow(testset) *100
t4
correctos4
plot(svm.model4, trainset, Sepal.Width ~ Sepal.Length)

####EXPERIMENTO 5####
svm.model5 <- best.tune(svm,Species ~ Sepal.Width + Sepal.Length, data = trainset, kernel="linear") 
summary(svm.model5)
svm.pred5 <- predict(svm.model5, testset[,-5])
t5 <- table(pred = svm.pred5, true = testset[,5])
correctos5 <- sum(diag(t5)) / nrow(testset) *100
t5
correctos5

####EXPERIMENTO 6####
svm.model6 <- best.tune(svm,Species ~ Sepal.Width + Sepal.Length, data = trainset, kernel="polynomial") 
summary(svm.model6)
svm.pred6 <- predict(svm.model6, testset[,-5])
t6 <- table(pred = svm.pred6, true = testset[,5])
correctos6 <- sum(diag(t6)) / nrow(testset) *100
t6
correctos6

####EXPERIMENTO 7####
svm.model7 <- best.tune(svm,Species ~ Sepal.Width + Sepal.Length, data = trainset, kernel="sigmoid") 
summary(svm.model7)
svm.pred7 <- predict(svm.model7, testset[,-5])
t7 <- table(pred = svm.pred7, true = testset[,5])
correctos7 <- sum(diag(t7)) / nrow(testset) *100
t7
correctos7
plot(svm.model7, trainset, Sepal.Width ~ Sepal.Length)


correctos
correctos2
correctos3
correctos4
correctos5
correctos6
correctos7

####EXPERIMENTO 8####
mytunedsvm8 <- tune.svm(Species ~ Sepal.Width + Sepal.Length, data = trainset, 
                        gamma = c(0.1,0.5,1,2), cost = c(1,2,4,8,10,16,100)) 
summary(mytunedsvm8)
svm.model8 <- mytunedsvm8$best.model
summary(svm.model8)
svm.pred8 <- predict(svm.model8, testset[,-5])
t8 <- table(pred = svm.pred8, true = testset[,5])
correctos8 <- sum(diag(t8)) / nrow(testset) *100
t8
correctos8