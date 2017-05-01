library(e1071)
library(caret)

set.seed(1000)
x=c(rnorm(500,500,100),rnorm(500,1000,200),rnorm(500,1500,400), rnorm(500,800,200), rnorm(500,1800,400))
y=c(abs(rnorm(500,50,25)),rnorm(500,200,50),rnorm(500,100,30), rnorm(500,150,20), rnorm(500,200,50))
clases=as.factor(c(rep(1,500),rep(2,500),rep(3,500),rep(4,500), rep(5,500)))
datos=data.frame(x,y,clases)
plot(x,y, data=datos, col = clases)

set.seed(1000)
testindex <- sample(1:nrow(datos),size=1800)
testset <- datos[-testindex,] #prueba
trainset <- datos[testindex,] #entrenamiento

plot(trainset, col = clases)

####EXPERIMENTO 1####
#definion del modelo svm
svm.model <- svm(clases ~ x+y, data = trainset, cost = 100, gamma = 1)
#sumario del modelo svm
summary(svm.model)
#prediccion, la columna 5 del dataset es la que define las clases "Species"
svm.pred <- predict(svm.model, testset[,-3])
  #tabla de las predicciones
  #t1 <- table(pred = svm.pred, true = testset[,3])
  #correctos <- sum(diag(t1)) / nrow(testset) *100
  #t1
  #correctos
cm1 <- confusionMatrix(svm.pred, testset[,3]) #Matriz de confusion paquete caret
cm1
#grafica del modelo
plot(svm.model, trainset, x ~ y)

####EXPERIMENTO 2####
#funcion tune.svm
mytunedsvm <- tune.svm(clases ~ x+y, data = trainset, gamma = 2^(-1:1), cost = 2^(2:4)) 
summary(mytunedsvm)
svm.model2 <- mytunedsvm$best.model
summary(svm.model2)
svm.pred2 <- predict(svm.model2, testset[,-3])
t2 <- table(pred = svm.pred2, true = testset[,3])
correctos2 <- sum(diag(t2)) / nrow(testset) *100
t2
correctos2
plot(svm.model2, trainset, x~y)



####EXPERIMENTO 3####
#funcion best.tune
svm.model3 <- best.tune(svm,clases ~ x + y, data = trainset) 
summary(svm.model3)
svm.pred3 <- predict(svm.model3, testset[,-3])
t3 <- table(pred = svm.pred3, true = testset[,3])
correctos3 <- sum(diag(t3)) / nrow(testset) *100
t3
correctos3
plot(svm.model3, trainset, x ~ y)

svm.model
svm.model2
svm.model3
correctos
correctos2
correctos3

####EXPERIMENTO 8####
mytunedsvm8 <- tune.svm(clases ~ x + y, data = trainset, 
                        gamma = c(0.1,0.5,1,2), cost = c(1,2,4,8,10,16,100)) 
summary(mytunedsvm8)
svm.model8 <- mytunedsvm8$best.model
summary(svm.model8)
svm.pred8 <- predict(svm.model8, testset[,-3])

confusionMatrix(svm.pred8, testset[,3]) #Matriz de confusion paquete caret

t8 <- table(pred = svm.pred8, true = testset[,3])
correctos8 <- sum(diag(t8)) / nrow(testset) *100
t8
correctos8
plot(svm.model8, trainset, x ~ y)


porFila <- function(t){
  paste("1: ",t[1,1] / sum(t[,1]) * 100,"2: ",t[2,2] / sum(t[,2]) * 100,
        "3: ",t[3,3] / sum(t[,3]) * 100,"4: ",t[4,4] / sum(t[,4]) * 100)
}

summary(testset[,3])
summary(trainset[,3])
