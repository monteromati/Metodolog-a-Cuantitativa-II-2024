
### TALLER  ###
### Introduccion a R ###

# Conceptos basicos de R - Generacion de variables
x <- 7
x

NumVar <- c(5,3,4)
NumVar

CharVar <- c("verde", "amarillo", "verde")
CharVar

# Ejercicio 1: Generacion de variables
Edad <- c(40, 5, 16, 28, 32)
Edad

Nombres <- c("Daniel","Mariela","Miguel","Pablo","Javiera")
Nombres

MisDatos <- data.frame(Nombres,Edad)
MisDatos

# Conceptos basicos de R - Objetos
ls()

# Ejercicio 2: Funciones Basicas
CarTab <- mtcars
class(CarTab)
CarTab
str(CarTab)
head(CarTab)
summary(CarTab)
mean(CarTab$hp)

# Ejercicio 3: Graficos Basicos
plot(CarTab$hp, CarTab$mpg)
plot(CarTab$hp, CarTab$mpg, col="blue")
hist(CarTab$qsec)

# Ejercicio 4: Transformacion de Variables
CarTab$rhpwt <- CarTab$hp/CarTab$wt
CarTab$rhpwt
CarTab$Rapido <- ifelse(CarTab$qsec<18,1,0)
CarTab

# Ejercicio 5: Indexacion o extraccion de datos
Sub_CarTab <- CarTab[ , c("qsec", "Rapido")]
Sub_CarTab

# Ejercicio 6: Regresion
mod1 <- lm(qsec ~ hp + wt, data = CarTab)
summary(mod1)





