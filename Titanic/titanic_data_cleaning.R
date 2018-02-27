train2 <- train

# PassengerId: id del pasajero
# De 1 a 891.
# No es relevante para el modelo.

# Survived: indica si sobrevivio o no
# 1 (sobrevivio) o 0 (se ahogo). Cantidad de 0: 549, de 1: 342.
# Es la variable target
 
# Pclass: clase del ticket
# 1, 2 o 3. 
# Esta asociado a la cabina y al fare
# Indica nivel socioeconomico 
# 1: 216, 2: 184, 3: 491 

# Sex: sexo del pasajero
# male 577 y female 314

# Age: edad del pasajero
# Min.   : 0.42
# Max.   : 80.00
# NA's   : 177

# Hay outliers
boxplot.stats(train$Age)$out
# Life expectancy in 1912 was 56 (m) and 51.5 (f)
# Me llama la atencion el Master con edad 0.42 (supongo que el 0. esta de mas)
# Imputamos la edad para completar los NA's 
# Se podria hacer una imputacion variable a partir los Titulos del Name, por ahora no.
train2$Age <- impute(train2$Age, fun = median)
# Hay muchos pasajeros con conyuges, hermanos e hijos con edad < 10
# Un poco de google:
# What does master mean? Master is a title for an underage male. If a person is under 18, master would be used. Once a person turns 18 and enters adulthood, mister would be used.
# Master para menores de 18 años, mister para mayores. No estaria mal.                                                     
subset(train2, train2$Age <= 10)
# Ya no hay NA's
# Se podria categorizar (5):
# Chico 0 - 16
# Adulto 17-32                                                                                                                                                                                                            z
# Adulto_Avanzado 33-48  
# Viejo 48-64
# Super_Viejo 64-80
train2$CatAge <- cut(train2$Age, breaks=c(0 ,16, 32, 48, 64, 80), labels=c("kid", "young_adult", "adult", "old_adult", "old"))
# tambien se puede usar findInterval(train2$Age, c(16, 32, 48, 64, 80) )
summary(train2$CatAge)
#        kid young_adult       adult   old_adult         old 
#        100         523         188          69          11 



# Name: nombre del pasajero
# Indica el nombre del pasajero y su titulo (Mr., Dr., etc)



# SibSp: número de hermanos y cónyuges asociados con el pasajero a bordo
# Min.   :0.000
# Max.   :8.000
 
# Parch: número de padres e hijos asociados con el pasajero a bordo
# Min.   :0.000
# Max.   :6.0000

# Ticket: ticket del pasajero
# Son todos diferentes

# Fare: precio del ticket
# Min.   :  0.00
# Max.   :512.33

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


# Cabin: cabina del pasajero
# ""     :687
# el resto son todas diferentes, formato A999

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




# Embarked: puerto de embarque
# "" :  2 
# C:168
# Q: 77 
# S:644 

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

