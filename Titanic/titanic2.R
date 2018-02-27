## clean variables
rm(list=ls())
library(FSelector)
library(randomForest)


## read data files 
train <- read.csv("C:\\Users\\fcanigia\\Desktop\\R\\titanic\\train.csv", header=T)
test <- read.csv("C:\\Users\\fcanigia\\Desktop\\R\\titanic\\test.csv", header=T)
gender_submission <- read.csv("C:\\Users\\fcanigia\\Desktop\\R\\titanic\\gender_submission.csv", header=T)

## data cleaning
# copy of the original train
train2 <- train

# change $Survived from integer to factor (or else, RF will make a regression)
train2$Survived <- as.factor(train2$Survived)


######################################################## CABIN
# Esta vez no vamos a eliminar las cabinas 
levels(train2$Cabin)

# primero le pongo X a los vacios
class(train2$Cabin)
# [1] "factor"
# como es factor no deja asignar directamente, lo convierto a texto
train2$Cabin <- as.character(train2$Cabin)
train2[train2$Cabin == "", 11 ] <- "X"

# solo tomo el primer valor
train2$Cabin <- substr(train2$Cabin, 1, 1)

# lo vuelvo a setear como factor
train2$Cabin <- as.factor(train2$Cabin)


######################################################## AGE
# imputamos la edad
train2$Age <- impute(train2$Age, fun = median)

######################################################## EMBARKED
# tiene 2 vacios
# Southampton es el puerto que tiene mas embarques, asi que lo ponemos como default

train2[train2$Embarked == "", ]
#    PassengerId Survived Pclass                                      Name    Sex Age SibSp Parch Ticket Fare Cabin Embarked
# 62           62        1      1                       Icard, Miss. Amelie female  38     0     0 113572   80     B         
# 830         830        1      1 Stone, Mrs. George Nelson (Martha Evelyn) female  62     0     0 113572   80     B         
train2[62,]
#   PassengerId Survived Pclass                Name    Sex Age SibSp Parch Ticket Fare Cabin Embarked
# 62          62        1      1 Icard, Miss. Amelie female  38     0     0 113572   80     B         
train2[62,12]
# [1] 
# Levels:  C Q S
train2[62,12] <- "S"
train2[830,12] <- "S"

######################################################## FARE
# hay varios registros con Fare = 0, los convierto en NA para despues imputarlos
train2[train2$Fare == 0, 10]
# [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
train2[train2$Fare == 0, 10] <- NA
train2[train2$Fare == 0, 10]
# [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
train2[is.na(train2$Fare), 10]
# [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
train2[is.na(train2$Fare), 10] 

# hay un fare de 0, se le carga la mediana 
train2$Fare <- impute(train2$Fare, fun = median)


# tendria que plotear los resultados y ver la distribucion para analizar si hace falta normalizar
train2$Fare <- scale(train2$Fare) 

######################################################## TRAIN3
# elimino nombre, ticket y id pasajero
train3 <- subset(train2, select=-c(Name, Ticket, PassengerId))


######################################################## IMPORTANCE

selection_train3 <- random.forest.importance(Survived ~ ., train3)
print(selection_train3)
























# remove $PassengerID because it is irrelevant
train2 <- train2[,-1]

# name irrelevant too
train2 <- train2[,-3]

# ticket may be relevat, but lets remove it
train2 <- train2[, -7]

######## models

## randomForest
# doesnt like NA's, only take complete cases
train3 <- train2[complete.cases(train2),]

# Cabin has too many categories 
train3 <- train3[,-8]

# model 1: Survived vs all
rf.train3 <- randomForest(Survived ~., data=train3, ntree=100)
table(predict(rf.train3),train3$Survived)
#      0   1
#  0 376  87
#  1  48 203

# model 2: Survived vs Sex, Age and Pclass
rf.train3 <- randomForest(Survived ~Sex + Age + Pclass, data=train3, ntree=100)
table(predict(rf.train3),train3$Survived)
   
#      0   1
#  0 412 121
#  1  12 169

# model 3: Survived vs Sex, Age, Pclass and Embarked
rf.train3 <- randomForest(Survived ~Sex + Age + Pclass + Embarked, data=train3, ntree=100)
table(predict(rf.train3),train3$Survived)
   
#      0   1
#  0 400 113
#  1  24 177

# we can check the weight of the variables 
selection_train3 <- random.forest.importance(Survived ~ ., train3)
print(selection_train3)

#          attr_importance
# Pclass         49.888126
# Sex           110.487180
# Age            41.852072
# SibSp          24.077278
# Parch          16.126151
# Fare           39.298694
# Embarked        9.368362

# model 4: Survived vs > weight
rf.train3 <- randomForest(Survived ~Sex + Age + Pclass + Fare, data=train3, ntree=100)
table(predict(rf.train3),train3$Survived)
   
#      0   1
#  0 391  89
#  1  33 201

# More cleaning
# Outlier detection
boxplot.stats(train3$Age)$out
# [1] 66.0 71.0 70.5 71.0 80.0 70.0 70.0 74.0
# Life expectancy in 1912 was 56 (m) and 51.5 (f)

boxplot.stats(train3$Fare)$out

# Normalization
train3$Fare <- scale(train3$Fare)
train3$Age <- scale(train3$Age)

# Predict 
rf.predict <- predict(rf.train3, test)
rf.predict

# Creo que los resultados son una mierda porque no esta normalizado como train3
test3 <- subset(test, select=-c(Name, Ticket, Cabin))

rf.predict <- predict(rf.train3, test3)
rf.predict

# Normalization
test3$Fare <- scale(test3$Fare)
test3$Age <- scale(test3$Age)


######## Vamos de nuevo
rm(list=ls())
library(FSelector)
library(randomForest)


## read data files 
train <- read.csv("C:\\Users\\fcanigia\\Desktop\\R\\titanic\\train.csv", header=T)
test <- read.csv("C:\\Users\\fcanigia\\Desktop\\R\\titanic\\test.csv", header=T)
gender_submission <- read.csv("C:\\Users\\fcanigia\\Desktop\\R\\titanic\\gender_submission.csv", header=T)

# creo una copia de los dataset originales
train2 <- train
test2 <- test

# leo train para analizar los datos
head(train)
summary(train)

# remuevo las columnas que no me interesan
train2 <- subset(train2, select=-c(PassengerId, Name, Ticket, Cabin))

# hago lo mismo con test2, no se si conviene eliminar PassengerId, por las dudas lo saque
test2 <- subset(test2, select=-c(PassengerId,Name, Ticket, Cabin))

# Age tiene muchos NA's
summary(train2)
summary(test2)

# Listo los NA's de train2
train2[is.na(train2$Age),]

# Imputacion de la edad
# usando la libreria Hmisc
install.packages("Hmisc")
library(Hmisc)

impute(train2$Age, fun = median)
impute(train2$Age, fun = mean)

# algo bueno de impute() es que se puede preguntar si el valor esta imputado con is.imputed()
train3 <- train2
train3$Age <- impute(train2$Age, fun = median)

test3 <- test2
test3$Age <- impute(test2$Age, fun = median)


# Normalization
train3$Fare <- scale(train3$Fare)
train3$Age <- scale(train3$Age)

test3$Fare <- scale(test3$Fare)
test3$Age <- scale(test3$Age)

# Random forest 1
rf.train3 <- randomForest(Survived ~., data=train3, ntree=100)
table(predict(rf.train3),train3$Survived)
#      0   1
#  0 510 109
#  1  39 233

# Peso de las variables
selection_train3 <- random.forest.importance(Survived ~ ., train3)
print(selection_train3)

#          attr_importance
# Pclass          54.15204
# Sex            119.27020
# Age             45.52944
# SibSp           29.69854
# Parch           23.36196
# Fare            45.38554
# Embarked        24.38720

# Ajusto por peso
rf.train3 <- randomForest(Survived ~ Sex + Pclass + Age + Fare, data=train3, ntree=100)
rf.train3

# Call:
#  randomForest(formula = Survived ~ Sex + Pclass + Age + Fare,      data = train3, ntree = 100) 
#                Type of random forest: classification
#                      Number of trees: 100
# No. of variables tried at each split: 2
# 
#         OOB estimate of  error rate: 16.95%
# Confusion matrix:
#     0   1 class.error
# 0 499  50  0.09107468
# 1 101 241  0.29532164

# Predict 
rf.predict <- predict(rf.train3, test3)
rf.predict

# Agrego el resultado en una columna y otra con el resultado de la prediccion
gender_submission$prediccion <- rf.predict
gender_submission$Resultado <- gender_submission$Survived == gender_submission$prediccion


# genero nuevo data frame con el id del pasajero y la prediccion 
gender_submission_results <- subset(gender_submission, select=c(PassengerId, prediccion))
gsr <- gender_submission_results

# al generar el csv, como prediccion es factor lo genera con comillas, lo convierto a numero
# NO gsr$prediccion <- as.numeric(gsr$prediccion)
gsr$prediccion <- as.numeric(levels(gsr$prediccion))[gsr$prediccion]

# genero csv sin nombre de fila
write.csv(gsr, "C:\\Users\\fcanigia\\Desktop\\R\\titanic\\grs.csv", row.names=FALSE)

# la concha de dios, habia un NA! no se de donde pija salio
# la fila 153 tiene Fare = NA

# SCORE 0.76076
# 7411 / 9548

# para eliminar el NA de Fare
# Normalizacion de nuevo
test3 <- test2
test3$Age <- impute(test2$Age, fun = median)
test3$Fare <- impute(test2$Fare, fun = median)
test3$Fare <- scale(test3$Fare)
test3$Age <- scale(test3$Age)

gender_submission$Resultado <- gender_submission$Survived == gender_submission$prediccion
summary(gender_submission)
#  PassengerId        Survived      prediccion Resultado      
# Min.   : 892.0   Min.   :0.0000   0:277      Mode :logical  
# 1st Qu.: 996.2   1st Qu.:0.0000   1:141      FALSE:37       
# Median :1100.5   Median :0.0000              TRUE :381      
# Mean   :1100.5   Mean   :0.3636                             
# 3rd Qu.:1204.8   3rd Qu.:1.0000                             
# Max.   :1309.0   Max.   :1.0000                             


# same resultados... 

# algunas ideas
# - separar edad en categorias (bebe, chico, adolescente, adulto joven, adulto, mayor)




