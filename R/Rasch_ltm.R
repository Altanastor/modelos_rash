####################################
# Aplicación con datos educacionales
####################################
rm(list=ls())

# Instalación de paquetes
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
describe(matematica)
# ¿Cuáles es el ítem con mayor tasa de acierto?
# ¿Cuál es el ítem con menor tasa de acierto?
# En promedio cuál es la tasa de acierto de la prueba?


###########################################################
# Modelo Rasch                                            #
###########################################################
mate.rasch <- rasch(matematica, constraint = cbind(length(matematica) + 1, 1))
summary(mate.rasch)

# ordena los parámetros desde el más fácil al más difícil
coef(mate.rasch, prob = TRUE, order = TRUE)


# Extraer los parametros de dificultad y compararlos con las proporciones de aciertos 
mate.rasch$beta = -mate.rasch$coefficients[,1]

comp <- cbind(mate.rasch$beta,prop=colMeans(matematica))
plot(comp, col = "red", lwd = 2)

# Curva Característica del Ítem (CCI)
plot(mate.rasch, legend = FALSE, lwd = 2,
     main="Curvas Características de los Ítems (CCI)",
     xlab = "Habilidad en Matemáticas",
     ylab = "Probabilidad",
     cex.main = 1.0, cex.lab = 0.75, cex = 0.75)

## CCI para el ítem más fácil y el más difícil

plot(mate.rasch, legend = FALSE, lwd = 2,
     items = c(22,9),
     main="Curvas Características de los Ítems (CCI)",
     xlab = "Habilidad en Matemáticas",
     ylab = "Probabilidad",
     cex.main = 1.0, cex.lab = 0.75, cex = 0.75)

par(mfrow=c(1,2))

# Función de Información de los Items
plot(mate.rasch, type = "IIC", annot= TRUE, lwd = 3,
     #items = c(1, 3),
     main="Función de Información de los Items (FII)",
     xlab = "Habilidad en Matemáticas",
     ylab = "Información",      
     cex.main = 2.0, cex.lab = 1.00, cex = 1.00)


# Función de Información del Test
plot(mate.rasch, type = "IIC", items = 0, lwd = 3,
     main="Función de Información del Test (FIT)",
     xlab = "Habilidad en Matemáticas",
     ylab = "Información",      
     cex.main = 2.0, cex.lab = 1.00, cex = 1.00)
par(mfrow=c(1,1))

# Cálculo de la información (área bajo la curva) de la prueba
## Zona baja
info1 <- information(mate.rasch, c(-10, 0))
info1

## Zona alta
info2 <- information(mate.rasch, c(0, 10))
info2


# Habilidades
mate.rasch.habilidad <- factor.scores(mate.rasch)
head(mate.rasch.habilidad$score.dat[,45:46])
mate.rasch.habilidad$score <- mate.rasch.habilidad$score.dat[,45]

par(mfrow=c(2,1))
hab <-hist(mate.rasch.habilidad$score, col = "blue",
           xlim = c(-4,3),
           main = "Histograma de la habilidad en matemáticas",
           xlab = "Habilidad en Matemáticas"); hab

# Dificultades
dif <- hist(coef(mate.rasch), col = "red",
            xlim = c(-4,3),
            main = "Histograma de la dificultad de los ítems",
            xlab = "Dificultad del ítem"); dif



