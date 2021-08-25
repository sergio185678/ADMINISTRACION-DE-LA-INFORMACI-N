View(titanic_data)

#####################################################################

head(titanic_data,10)

#####################################################################

tail(titanic_data,10)

#####################################################################

names(titanic_data)

#####################################################################

str(titanic_data)

#####################################################################

summary(titanic_data$Age)

#####################################################################

titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)
titanic_data$SibSp <- as.integer(titanic_data$SibSp)
titanic_data$Parch <- as.integer(titanic_data$Parch)
titanic_data$Ticket <- as.factor(titanic_data$Ticket)
titanic_data$Cabin <- as.factor(titanic_data$Cabin)

#####################################################################

#funcion sin_valor(dataframe) que desliega cuantos valores NA posee cada variable
sin_valor <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(titanic_data)

#####################################################################

#funcion en_blanco(dataframe) que desliega cuantos valores en blanco posee cada variable
en_blanco <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores en blanco:",colSums(x[i]==""),"\n")
  }
}
en_blanco(titanic_data)

#####################################################################

titanic_data$PassengerId[titanic_data$Embarked == ""]
titanic_data$Pclass[titanic_data$PassengerId==62]
titanic_data$Fare[titanic_data$PassengerId==62]
titanic_data$Fare[titanic_data$PassengerId==830]

#####################################################################

library(dplyr)
embark_fare <- titanic_data %>% filter(PassengerId != 62 & PassengerId != 830)

#####################################################################

library(ggplot2)
library(scales)
ggplot(data = embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 80),
             colour = "red", linetype = "dashed", lwd = 2) +
  scale_y_continuous(labels = dollar_format()) +
  theme_bw()

#####################################################################

titanic_data$Embarked[c(62, 830)] <- "C"

#####################################################################

table(titanic_data$Survived)

#####################################################################

barplot(table(titanic_data$Survived), main="Pasajeros en Titanic", names= 
          c("Murieron", "Sobrevivieron"))

#####################################################################

prop.table(table(titanic_data$Survived))

#####################################################################

barplot(table(titanic_data$Pclass), main="Pasajeros de Titanic por Cl
ase", names= c("Primera", "Segunda", "Tercera"))

#####################################################################

table(titanic_data$Sex)

#####################################################################

barplot(table(titanic_data$Sex), main="Pasajeros del Titanic por Gen
ero", names= c("Mujer", "Hombre"))

#####################################################################

counts = table(titanic_data$Survived, titanic_data$Sex)
barplot(counts, col=c("green","yellow"), legend = c("Murieron", "Sobr
evivieron"), main = "Sobreviviencia de Pasajeros por Genero")

#####################################################################

counts1 = table(titanic_data$Survived, titanic_data$Pclass) 
barplot(counts1, col=c("green","yellow"), legend = c("Murieron","Sobr
evivieron"), main = "Sobreviviencia de Pasajeros por Clase", names= c("
Primera", "Segunda", "Tercera"))