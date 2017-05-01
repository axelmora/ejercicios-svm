install.packages("e1071")
library(e1071)

#Creacion de 10 datos, clasificados como a y b
data <- seq(1,11)
classes <- c('b','b','b','b','a','a','a','a','b','b','c')

#definicion del modelo svm con los datos creado, kernel del tipo lineal y clasificacion C
mysvm <- svm (data, classes, type='C', kernel='linear')

#prediccion de de la clasificacion de los 10 datos
pred1 <- predict (mysvm, data)
t1 <- table(pred1, classes)

#variacion del modelo con kernel polinomico y degree en 2
mysvm <- svm (data, classes, type='C', kernel='polynomial', degree=2)
pred2 <- predict (mysvm, data)
t2 <- table(pred2, classes)

#segunda variacion con kernel radial y gamma en 0.1
mysvm <- svm (data, classes, type='C', kernel='radial', gamma=0.1)
pred3 <- predict (mysvm, data)
t3 <- table(pred3, classes)

#tercer variacion con kernel radial, gamma en 0.1 y costo en 10
mysvm <- svm (data, classes, type='C', kernel='radial', gamma=0.1, cost=100)
pred4 <- predict (mysvm, data)
t4 <- table(pred4, classes)

#Se muestra el comportamiento 
#de los modelos SVM seg?n su configuracion en la precision en su prediccion
