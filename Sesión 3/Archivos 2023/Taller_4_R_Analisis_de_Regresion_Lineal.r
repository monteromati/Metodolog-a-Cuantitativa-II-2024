
###############################################################################################################
######################## ########## Taller de R #4: ###########################################################
############### Análisis de Regresión Lineal en R con Datos PISA 2009 Chile ###################################
###############################################################################################################

rm(list=ls()); dev.off()

# Instalación de Paquetes ----------------------------------------------------

install.packages("foreign")  # Paquete para importar datos en formato SPSS
install.packages("ggplot2")  # Paquete para generar gráficos
install.packages("gmodels")  # Paquete que contiene la función CrossTable() y que permite calcular la prueba de Chi-Cuadrado 
install.packages("psych")    # Paquete para obtener estadísticos descriptivos más detallados
install.packages("corrplot") # Paquete para obtener correlogramas
install.packages("lmtest")
install.packages("broom")
install.packages("relaimpo")
install.packages("visreg")
install.packages("performance")
install.packages("see")
install.packages("patchwork")
install.packages("knitr")
install.packages("car")

# Instalación de Bibliotecas de Paquetes ----------------------------------------------------
library(foreign)
library(ggplot2)
library(gmodels)
library(psych)
library(corrplot)
library(lmtest)
library(broom)
library(relaimpo)
library(visreg)
library(performance)
library(see)
library(patchwork)
library(knitr)
library(car)

# Cambiar el directorio de trabajo -----------------------------------------------------------

getwd() # directorio de trabajo actual *get working directory*

# setwd() cambia de directorio, establece un nuevo directorio
# Hay que cambiar la dirección a aquella donde se guardaron los datos

setwd("D:/Drive/Patricio/Docencia/[Cursos] Metodología Cuantitativa II/1.4. Análisis de regresión")

getwd()

# Importar datos ----------------------------------------------------------

pisa2009_chl=read.spss(file="pisa2009_chl.sav",to.data.frame = TRUE) 

View(pisa2009_chl)  # Abre la base de datos en una nueva ventana


## ¿Qué tenemos en la base de datos? # Exploremos los datos

class(pisa2009_chl)    # Muestra la clase de objeto es pisa2009_chl
head(pisa2009_chl)     # Muestra los primeros 6 casos en la base de datos
names(pisa2009_chl)    # Lista los nombres de las variables en la base de datos
dim(pisa2009_chl)      # Muestra el número de variables y casos
summary(pisa2009_chl)	 # Entrega estadísticos descriptivos para cada variable
str(pisa2009_chl)      # Muestra la estructura interna de la base de datos 



### Analisis descriptivo ###

# Estadísticos básicos para el Indice de estatus economico, social y cultural (ses)

pisa2009_chl$ses = as.numeric(pisa2009_chl$ses)
summary(pisa2009_chl$ses)
sd(pisa2009_chl$ses,na.rm = TRUE); var(pisa2009_chl$ses,na.rm = TRUE)

# Estadísticos descriptivos más detallados con función describe() del paquete psych
describe(pisa2009_chl$ses)

# Visualización:
## Histograma y densidad: para conocer la distribución de los datos

#### Histograma
ggplot(pisa2009_chl, aes(x=ses))+
  geom_histogram()

#### Densidad
# Documentación de la función de densida en: https://ggplot2.tidyverse.org/reference/geom_density.html
# Documentación del kernel de la densidad en:https://rdrr.io/r/stats/density.html


ggplot(pisa2009_chl, aes(x=ses))+
  geom_density()

#### Ambos
ggplot(pisa2009_chl, aes(x=ses))+
  geom_histogram(aes(y = ..density..))+
  geom_density()

# Estadísticos básicos para el Puntaje en Matematicas (math)
summary(pisa2009_chl$math)
sd(pisa2009_chl$math,na.rm = TRUE); var(pisa2009_chl$math,na.rm = TRUE)

# Estadísticos descriptivos más detallados con función describe() del paquete psych
describe(pisa2009_chl$math)

# Visualización:
## Histograma y densidad: para conocer la distribución de los datos

#### Histograma
ggplot(pisa2009_chl, aes(x=math))+
  geom_histogram()

#### Densidad
ggplot(pisa2009_chl, aes(x=math))+
  geom_density()

#### Ambos
ggplot(pisa2009_chl, aes(x=math))+
  geom_histogram(aes(y = ..density..))+
  geom_density()


##########################################
########### Regresion Lineal   ###########
##########################################

# Regresión lineal simple:
m1 = lm(math ~ ses, pisa2009_chl)
summary(m1)

#####################################
# Supuestos de la Regresion Lineal
#####################################
# Otra funente para ver lo que estamos viendo ahora: https://www.datacamp.com/tutorial/linear-regression-R 


# Supuestos Conceptuales:
#
## 1. La variable dependiente es continua.
# El/la investigadora establece la variable dependiente.

## 2. Independencia entre las observaciones: 
#
# Suele ser el resultado del proceso de recolección de los datos, por lo que 
# usualmente no es fácil detectarlo mediante la evaluación estadística de residuos del modelo. 
# La mejor forma de evaluar este supuesto usualmente es comprender el proceso mediante 
# el cual se recopilaron los datos.

# Estructura de los datos: El muestreo por conglomerado utilizado en PISA y 
# la frecuente significativa varianza en los resultados de los estudiantes entre escuelas 
# sugieren que este supuesto no se cumpliría.
# Más sobre esto en la Sesión 6 (07/09/2022): Análisis multinivel 

# Supuestos Estadisticos:

## 3. Linealidad de los datos: 
# Se supone que la relacion entre cada predictor (x) y el resultado (y) es lineal.
# Diagrama de dispersión

ggplot(pisa2009_chl) +
  aes(x = ses, y = math) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

## 4. Normalidad de los residuos: 
# Se supone que los errores residuales se distribuyen normalmente.
# m1 es un objeto de clase m1 que almacena los residuos en residuals 

hist(m1$residuals, freq = F)
plot(density(m1$residuals))
qqnorm(m1$residuals, main='Gráfico Cuantil-Cuantil')
qqline(m1$residuals)
shapiro.test(m1$residuals[0:5000])

## 5. Homogeneidad de la varianza de los residuos: 
# Se supone que los residuos tienen una varianza constante (homocedasticidad).
# Breusch-Pagan Test: 
bptest(math ~ ses, data = pisa2009_chl)

## 6. Ausencia de multicolinearidad
# Regresion lineal multiple: Predecir una variable continua, con otras continuas
table(pisa2009_chl$sex)

# Genera y recodifica variable female, para ingresar su efecto al modelo
pisa2009_chl$female <-as.numeric(pisa2009_chl$sex)
pisa2009_chl$female[pisa2009_chl$female == "1"] <- 0
pisa2009_chl$female[pisa2009_chl$female == "2"] <- 1

m2 <- lm(math ~ ses + female + read + scie, pisa2009_chl)
summary(m2)

# Usemos un correlograma para observar la relacion entre las variables continuas
data <- pisa2009_chl[,c("math", "ses","read","scie")]

correlaciones = cor(data,use = "complete.obs")
correlaciones

corrplot(correlaciones, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

kable(vif(m2), digits = 2)

# Esta función permite obtener varios gráficos de diagnóstico para chequear supuestos:
check_model(m1)

######################################################################################################################## 

# Resultados Regresion Lineal
summary(m1)

# Interpretacion:
#
# El output de la regresion lineal nos muestra distintos indicadores:

# Call:
#   lm(formula = math ~ ses, data = pisa2009_chl)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -252.44  -47.62    0.07   48.53  338.75 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.586e+02  2.008e+00  178.57   <2e-16 ***
#   ses         5.313e-02  1.399e-03   37.99   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 71.8 on 5578 degrees of freedom
# (89 observations deleted due to missingness)
# Multiple R-squared:  0.2055,	Adjusted R-squared:  0.2054 
# F-statistic:  1443 on 1 and 5578 DF,  p-value: < 2.2e-16


# Residuals: nos muestra la distribucion de los errores, pero eso ya lo evaluamos 
# 
# Coefficients: 
#   # Estimate
#   - nos entrega el promedio de nuestra variable dependiente (math) cuando la variable predictora (ses) es 0 (Intercept)
#   - nos entrega la variacion en la variable dependiente (math) por unidad de la variable predictora (ses), 
#     en este caso cada punto de ses (Indice de estatus economico, social y cultural) sube .05 puntos en la variable dependiente (math)
#   # Std.Error: Error estandar de la media  
#   # t-value: Estadistico t; > |2| es significativo 
#   # Pr(>|t|): valor p; < .05 es signifitivo. Ademas marca con asteriscos: *<.05 **<.01 ***<.001  
#   
# Multiple R-squared:  0.2055
#   - nos muestra cuanto porcentage de la varianza presente en nuestra variable dependiente explica nuestro modelo.
#   El Indice de estatus economico, social y cultural (ses) explica 21% de la varianza en los puntajes de matemáticas!

# Resultados Regresion Lineal
summary(m2)

######################################################################################################################## 

# Visualizacion
m1. = augment(m1)
m2. = augment(m2)

# lm primer modelo
ggplot(m1., aes(ses, math)) +
  geom_point(lwd=3, alpha=.5)+
  stat_smooth(method = lm, se = T, color = "royalblue") +
  theme_bw(base_size = 22)

# lm
ggplot(m2., aes(ses, math)) +
  geom_point(lwd=3, alpha=.5)+
  stat_smooth(method = lm, se = T, color = "royalblue") +
  theme_bw(base_size = 22)

# lm
ggplot(m2., aes(read, math)) +
  geom_point(lwd=3, alpha=.5)+
  stat_smooth(method = lm, se = T, color = "royalblue") +
  theme_bw(base_size = 22)

# lm
ggplot(m2., aes(scie, math)) +
  geom_point(lwd=3, alpha=.5)+
  stat_smooth(method = lm, se = T, color = "royalblue") +
  theme_bw(base_size = 22)


## Comparacion del ajuste de los modelos
anova(m1,m2)

# El modelo m2 presenta un ajuste significativamente mejor a los datos que el modelo m1


## Calcula la importancia de cada predictor
calc.relimp(m2,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Ver lmg: La importancia predictiva de cada variable:
# scie > read > ses > female

