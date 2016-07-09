####################################
# Aplicacion con datos educacionales
####################################
rm(list=ls())

# Instalacion de paquetes
install.packages(c("foreign","ltm"))
install.packages("psych")
library(foreign)
library(ltm)
library(psych)

# Datos
#setwd()
matematica <- read.csv(file.choose(),T)
head(matematica)
#str(matematica)
matematica <-matematica[,2:43]

# descriptivos
#fdescribe es una funcion del paquete pysch
describe(matematica)
# Â¿Cuales es el item con mayor tasa de acierto?
#item 22
# Â¿Cual es el item con menor tasa de acierto?
#el item 9
# En promedio cual es la tasa de acierto de la prueba?


###########################################################
# Modelo Rasch                                            #
###########################################################
#la discriminacion es igual a 1
mate.rasch <- rasch(matematica, constraint = cbind(length(matematica) + 1, 1))
summary(mate.rasch)

# ordena los parametros desde el mas facil al mas dificil
coef(mate.rasch, prob = TRUE, order = TRUE)


# Extraer los parametros de dificultad y compararlos con las proporciones de aciertos 
#extraer el vector de coeficientes
mate.rasch$beta = -mate.rasch$coefficients[,1]

comp <- cbind(mate.rasch$beta,prop=colMeans(matematica))

#Conforme la dificultad del item va incrementado, la tasa de acierto disminuye
plot(comp, col = "red", lwd = 2)

# Curva Caracterstica del item (CCI)
plot(mate.rasch, legend = FALSE, lwd = 2,
     main="Curvas Caracterticas de los items (CCI)",
     xlab = "Habilidad en Matematicas",
     ylab = "Probabilidad",
     cex.main = 1.0, cex.lab = 0.75, cex = 0.75)

## CCI para el Ãtem mas facil y el mas difÃcil

plot(mate.rasch, legend = FALSE, lwd = 2,
     items = c(22,9), #plotea el item mas facil (22) y para el mas dificil (9)
     main="Curvas Caracteristicas de los items (CCI)",
     xlab = "Habilidad en Matematicas",
     ylab = "Probabilidad",
     cex.main = 1.0, cex.lab = 0.75, cex = 0.75)

par(mfrow=c(1,2))

# Funcion de Informacion de los Items
plot(mate.rasch, type = "IIC", annot= TRUE, lwd = 3,
     #items = c(1, 3),
     main="Funcion de Informacion de los Items (FII)",
     xlab = "Habilidad en Matematicas",
     ylab = "Informacion",      
     cex.main = 2.0, cex.lab = 1.00, cex = 1.00)

#Aqui nos plotea curvas
# Funcion de Informacion del Test
plot(mate.rasch, type = "IIC", items = 0, lwd = 3,
     main="Funcion de Informacion del Test (FIT)",
     xlab = "Habilidad en Matematicas",
     ylab = "Informacion",      
     cex.main = 2.0, cex.lab = 1.00, cex = 1.00)
par(mfrow=c(1,1))

# Calculo de la informacion (area bajo la curva) de la prueba
## Zona baja
info1 <- information(mate.rasch, c(-10, 0)) #calcula el area bajo la curva por intrgracion
info1

## Zona alta
info2 <- information(mate.rasch, c(0, 10))
info2


# Habilidades
#z1: la estimacion del item
#ez1: error de la estimacion del item 
mate.rasch.habilidad <- ltm::factor.scores(mate.rasch)

head(mate.rasch.habilidad$score.dat[,45:46])
mate.rasch.habilidad$score <- mate.rasch.habilidad$score.dat[,45]

par(mfrow=c(2,1))
hab <-hist(mate.rasch.habilidad$score, col = "blue",
           xlim = c(-4,3),
           main = "Histograma de la habilidad en matematicas",
           xlab = "Habilidad en Matematicas"); hab

# Dificultades
dif <- hist(coef(mate.rasch), col = "red",
            xlim = c(-4,3),
            main = "Histograma de la dificultad de los items",
            xlab = "Dificultad del item"); dif



