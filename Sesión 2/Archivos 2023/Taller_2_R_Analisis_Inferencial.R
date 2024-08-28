
###############################################################################################################
######################## ##########Taller de R #2: ############################################################
############### Análisis Inferencial Bivariado con Datos PISA 2009 Chile ######################################
###############################################################################################################

# Instalación de Paquetes ----------------------------------------------------

install.packages("foreign")  # Paquete para importar datos en formato SPSS
install.packages("ggplot2")  # Paquete para generar gráficos
install.packages("gmodels")  # Paquete que contiene la función CrossTable() y que permite calcular la prueba de Chi-Cuadrado 
install.packages("psych")    # Paquete para obtener estadísticos descriptivos más detallados
install.packages("pastecs")

# Instalación de Bibliotecas de Paquetes ----------------------------------------------------
library(foreign)
library(ggplot2)
library(gmodels)
library(psych)
library(pastecs)

# Cambiar el directorio de trabajo -----------------------------------------------------------

getwd() # directorio de trabajo actual *get working directory*

# setwd() cambia de directorio, establece un nuevo directorio
# Hay que cambiar la dirección a aquella donde se guardaron los datos

setwd("C:/Users/lcort/Downloads")

getwd()

# Importar datos ----------------------------------------------------------

pisa2009_chl<- read.spss(file="pisa2009_chl.sav",to.data.frame = TRUE) 

View(pisa2009_chl)  # Abre la base de datos en una nueva ventana


## ¿Qué tenemos en la base de datos? # Exploremos los datos

class(pisa2009_chl)    # Muestra la clase de objeto es pisa2009_chl
head(pisa2009_chl)     # Muestra los primeros 6 casos en la base de datos
names(pisa2009_chl)    # Lista los nombres de las variables en la base de datos
dim(pisa2009_chl)      # Muestra el número de variables y casos
summary(pisa2009_chl)	# Entrega estadísticos descriptivos para cada variable
str(pisa2009_chl)      # Muestra la estructura interna de la base de datos 


#################################################################
################## PRUEBA DE CHI-CUADRADO #######################
#################################################################

### Análisis descriptivo para variables categóricas

# Tabla de contingencia variable categórica
table(pisa2009_chl$sex)
table(pisa2009_chl$type)

# Tabla de contingencia variable categórica por grupo de interés
table(pisa2009_chl$type,pisa2009_chl$sex)

# Tabla de porcentajes variable categórica por grupo de interés: "margin = 2" entrega los valores (proporciones) de la distribución condicional de la variable 1 según la variable 2
prop.table(table(pisa2009_chl$type,pisa2009_chl$sex), margin = 2)

# Visualización
## Gráfico de Barras: 
plot = as.data.frame(table(pisa2009_chl$sex))
ggplot(plot, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity")

plot = as.data.frame(table(pisa2009_chl$type))
ggplot(plot, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity")

## Gráfico Circular:
plot = as.data.frame(table(pisa2009_chl$sex))
ggplot(plot, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", color="white") +
  coord_polar("y", start=0) + 
  theme_void()

plot = as.data.frame(table(pisa2009_chl$type))
ggplot(plot, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", color="white") +
  coord_polar("y", start=0) + 
  theme_void()

#### Prueba de Chi Cuadrado con la función CrossTable ####

## Pregunta de Investigación: 
# ¿Existe una asociación entre el género de los estudiantes y el tipo de establecimiento en el que estudian?

#Información sobre la función CrossTable
?CrossTable

# Primero, podemos usar la función CrossTable para generar una tabla de contingencia
# Las filas de la tabla representan: 
## N (resultado se almacena como t): Valores absolutos de las frecuencias cruzadas de 1 vs 2 
## N / Row Total (resultado se almacena como prop.row): Valores (proporciones) de la distribución condicional de la variable categórica 1 según la categórica 2
## N / Col Total (resultado se almacena como prop.col): Valores (proporciones) de la distribución condicional de la variable categórica 2 según la categórica 1
## N / Table Total (resultado se almacena como prop.tbl): Valores (proporciones) relativas de cada categoría

tabla <- CrossTable(pisa2009_chl$type,pisa2009_chl$sex, prop.chisq = FALSE)

propcol  <- tabla$prop.col

## Gráfico de Barras Condicional: 
barplot(propcol, beside = TRUE, col = rainbow(3), ylim = c (0,1))
legend ('topright', c("Public", "Private government-dependent", " Private independent"), pch = 15, col = rainbow (3), cex = 0.7)

# Segundo, podemos usar la función CrossTable para calcular la Prueba de Chi-Cuadrado
# A través de este código, pedimos que nos entregue:
### La prueba de chi-cuadrado (chisq = TRUE)
### Las frecuencias esperadas (expected = TRUE) (sirve para chequear supuesto > 5).
### Los residuos estandarizados (sresid =TRUE), que sirven para interpretar en mayor detalle un eventual resultado significativo.
### Tabla en formato similar a salida de SPSS (format = "SPSS").

# Hipótesis Nula e Hipótesis Alternativa
# H0 = No hay una asociación significativa entre las variables género del estudiante y dependencia del establecimiento
# H1 = Existe una asociación significativa entre las variables género del estudiante y dependencia del establecimiento

CrossTable(pisa2009_chl$type,pisa2009_chl$sex, prop.chisq = FALSE, chisq = TRUE, expected = TRUE, sresid =TRUE, format = "SPSS")

## Interpretación de Estadístico de chi-cuadrado y su valor de significancia: 
### El valor de p es lo suficientemente pequeño (por convención, p < .05; p < .01; p < .001) para rechazar la Hipótesis nula de que las variables son independientes entre sí. 
### Esto indica que el género de los estudiantes se asocia al tipo de establecimientos en el que estudian.

## Reporte de resultados:
### Existe una asociación significativa entre el género de los estudiantes y la dependencia administrativa del establecimiento
### en el que estudian (χ2(2) = 11.40, p < .01).


#################################################################
################## PRUEBA U DE MANN-WHITNEY #####################
#################################################################

pisa2009_chl$ses <-as.numeric(pisa2009_chl$ses)
pisa2009_chl$sex <-as.numeric(pisa2009_chl$sex)

# Estadísticos básicos
summary(pisa2009_chl$ses)
sd(pisa2009_chl$ses,na.rm = TRUE); var(pisa2009_chl$ses,na.rm = TRUE)

# Estadísticos descriptivos más detallados con función describe() del paquete psych
describe(pisa2009_chl$ses)

# Estadísticos básicos por grupo de interés: pisa2009_chl$sex
by(pisa2009_chl$ses, pisa2009_chl$sex, summary)

# Visualización:
## Histograma y densidad: para conocer la distribución de los datos

#### Histograma
ggplot(pisa2009_chl, aes(x=ses))+
  geom_histogram()

#### Densidad
ggplot(pisa2009_chl, aes(x=ses))+
  geom_density()

#### Ambos
ggplot(pisa2009_chl, aes(x=ses))+
  geom_histogram(aes(y = ..density..))+
  geom_density()

#### Histograma
ggplot(pisa2009_chl, aes(x=math))+
  geom_histogram()

## Gráfico cuartil-cuartil (Q-Q plot). 
## El gráfico cuartil-cuartil (Q-Q plot) permite observar cuan cerca está la distribución 
## de una variable respecto a una distribución ideal (en este caso, normal).
## Si los puntos en el gráfico caen aproximadamente a lo largo de una línea diagonal 
## recta, se asume que los datos se distribuyen normalmente.

qqnorm(pisa2009_chl$ses, main='Gráfico Cuartil-Cuartil')
qqline(pisa2009_chl$ses)

## Prueba de normalidad Shapiro-Wilk.
## Esta prueba plantea la hipótesis nula de que una muestra proviene de una 
## distribución normal. Elegimos un nivel de significancia (por ejemplo .05) y 
## tenemos una hipótesis alternativa que sostiene que la distribución no es normal.

## Si el valor p de la prueba es mayor que α = .05, se asume que los datos se 
## distribuyen normalmente.

## En R, la prueba se limita a un máximo de 5000 casos, así que elegimos los 
## primeros 5000 casos.
## Es importante señalar que las pruebas formales de normalidad son muy sensibles
## al tamaño de la muestra. En este caso, el nivel de significancia es muy bajo, 
## en parte, debido al  número de casos analizados. Continuaremos asumiendo que la 
## variable se distribuye de manera aproximadamente normal, en base a la inspección visual.

shapiro.test(pisa2009_chl$ses[0:5000])

#### Corre Prueba U de Mann-Whitney ####

## Pregunta de Investigación: 
# ¿Difieren significativamente los niños de las niñas en su estatus socio-económico?

# Hipótesis Nula e Hipótesis Alternativa
# H0 = Las niñas y los niños no difieren significativamente en su estatus socio-económico
# H1 = Las niñas y los niños sí difieren significativamente en su estatus socio-económico

prueba_u <- wilcox.test(pisa2009_chl$ses, pisa2009_chl$sex, paired = FALSE)
prueba_u

## Interpretación de Estadístico de U de Mann-Whitney (en R, su equivalente W) y su valor de significancia: 
### El valor de p es lo suficientemente pequeño (por convención, p < .05; p < .01; p < .001) para rechazar la Hipótesis nula de que 
### no existe diferencia entre los grupos. 
### Esto indica que el estatus socio-económico de los estudiantes difiere significativamente entre niños y niñas.

## Reporte de resultados:
### Los niños (Mdn = 1230) presentan significativamente menor SES que las
### niñas (Mdn = 1245), W = 31,627,387, p < .001).


#################################################################
################## PRUEBA H DE KRUSKAL-WALLIS ###################
#################################################################

# Estadísticos básicos por grupo de interés: pisa2009_chl$type
by(pisa2009_chl$ses, pisa2009_chl$type, summary)

## Pregunta de Investigación: 
# ¿Difiere significativamente el estatus socio-económico de los estudiantes según la 
# dependencia del establecimiento al que asisten?

# Hipótesis Nula e Hipótesis Alternativa
# H0 = El estatus socio-económico de los estudiantes no difiere significativamente según la dependencia del establecimiento al que asisten
# H1 = El estatus socio-económico de los estudiantes sí difiere significativamente según la dependencia del establecimiento al que asisten

kruskal.test(pisa2009_chl$ses ~ pisa2009_chl$type)

## Interpretación de Estadístico H de Kruskal-Wallis y su valor de significancia: 
### El valor de p es lo suficientemente pequeño (por convención, p < .05; p < .01; p < .001) para rechazar la Hipótesis nula de que 
### no existe diferencia entre los grupos. 

## Reporte de resultados:
### El estatus socio-económico de los estudiantes difiere según la 
### dependencia del establecimiento al que asisten, H(2) = 422.54, p < .001).


#################################################################
################## CORRELACIÓN DE SPEARMAN ######################
#################################################################

## Pregunta de Investigación: 
# ¿Existe una asociación significativa entre el estatus socio-económico de los estudiantes y su
# puntaje en matemáticas?

# Visualiza la relación entre las variables
pairs(pisa2009_chl$ses ~ pisa2009_chl$math)  # permite elaborar un plot de correlacion

ggplot(pisa2009_chl) +
  aes(x = ses, y = math) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# Hipótesis Nula e Hipótesis Alternativa
# H0 = El estatus socio-económico de los estudiantes no se asocia a los puntajes de matemáticas.
# H1 = El estatus socio-económico de los estudiantes sí se asocia a los puntajes de matemáticas.

cor.test(pisa2009_chl$ses, pisa2009_chl$math, method = "spearman",exact=FALSE)

## Interpretación de rho de Spearman y su valor de significancia: 
### El valor de p es lo suficientemente pequeño (por convención, p < .05; p < .01; p < .001) para rechazar la Hipótesis nula de que 
### no existe asociación entre las variables. 

## Reporte de resultados:
### Existe una asociación significativa y positiva entre el estatus socio-económico de los 
### estudiantes y su puntaje en matemáticas (rs = .45, p < .001).


#################################################################
############## PRUEBA T DE DIFERENCIA DE MEDIAS #################
###############  PARA MUESTRAS INDEPENDIENTES  ##################
#################################################################

# Estadísticos básicos
summary(pisa2009_chl$math)
sd(pisa2009_chl$math); var(pisa2009_chl$math)

# Estadísticos descriptivos más detallados con función describe() del paquete psych
describe(pisa2009_chl$math)

# Las pruebas estadísticas paramétricas asumen que una o más variables se distribuyen normalmente.
# Las formas más comunes de verificar este supuesto son:

## 1. (Método visual) Histograma.
## Es una representación gráfica de una variable en forma de barras, donde la 
## superficie de cada barra es proporcional a la frecuencia de los valores representados.
## Si el histograma tiene aproximadamente "forma de campana", se asume que los 
## datos se distribuyen normalmente.

#### Histograma
ggplot(pisa2009_chl, aes(x=math))+
  geom_histogram()

## 2. (Método visual) Gráfico cuartil-cuartil (Q-Q plot). 
## El gráfico cuartil-cuartil (Q-Q plot) permite observar cuan cerca está la distribución 
## de una variable respecto a una distribución ideal (en este caso, normal).
## Si los puntos en el gráfico caen aproximadamente a lo largo de una línea diagonal 
## recta, se asume que los datos se distribuyen normalmente.

qqnorm(pisa2009_chl$math, main='Gráfico Cuartil-Cuartil')
qqline(pisa2009_chl$math)

## 3. (Prueba estadística formal) Prueba de Shapiro-Wilk.
## Esta prueba plantea la hipótesis nula de que una muestra proviene de una 
## distribución normal. Elegimos un nivel de significancia (por ejemplo .05) y 
## tenemos una hipótesis alternativa que sostiene que la distribución no es normal.

## Si el valor p de la prueba es mayor que α = .05, se asume que los datos se 
## distribuyen normalmente.

## En R, la prueba se limita a un máximo de 5000 casos, así que elegimos los 
## primeros 5000 casos.
## Es importante señalar que las pruebas formales de normalidad son muy sensibles
## al tamaño de la muestra. En este caso, el nivel de significancia es muy bajo, 
## en parte, debido al  número de casos analizados. Continuaremos asumiendo que la 
## variable se distribuye de manera aproximadamente normal, en base a la inspección visual.

shapiro.test(pisa2009_chl$math[0:5000])

# Análisis descriptivo bivariado

by(pisa2009_chl$math,pisa2009_chl$sex, stat.desc, basic = FALSE, norm = TRUE)

#### Prueba t de diferencia de medias para muestras independientes con la función t.test ####

## Pregunta de Investigación: 
# ¿Difiere significativamente el promedio de puntajes en matemáticas según el género de los estudiantes?

# Hipótesis Nula e Hipótesis Alternativa
# H0 = Las niñas y los niños no difieren significativamente en sus puntajes promedio de matemáticas
# H1 = Las niñas y los niños sí difieren significativamente en sus puntajes promedio de matemáticas

# Información sobre la función t.test
?t.test

# Prueba t de diferencia de medias para muestras independientes
ind.t.test <- t.test(pisa2009_chl$math ~ pisa2009_chl$sex, paired = FALSE)
ind.t.test

## Interpretación del Estadístico t y su valor de significancia: 
### El valor de p es lo suficientemente pequeño (por convención, p < .05; p < .01; p < .001) 
### para rechazar la hipótesis nula de que el promedio no difiere significativamente entre los dos grupos. 
### Esto indica que el promedio de puntajes en matemáticas difiere significativamente entre niños y niñas.

# Cálculo del tamaño del efecto de esta diferencia de medias (transformamos el 
# estadístico t en el indicador de tamaño del efecto r)
# El indicador de tamaño del efecto r  varía entre 0 y 1.
# Por convención:
#  r = 0.1 Efecto pequeño
#  r = 0.3 Efecto mediano
#  r = 0.5 Efecto grande.

t <- ind.t.test$statistic[[1]]
df <- ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r,3)

## Reporte de resultados:
### En promedio, las niñas (M = 413.5) obtuvieron un puntaje de matemáticas
### significativamente menor que los niños (M = 435.9), t(5660.3) = 10.613, p < .001. 
### Esta diferencia representa un tamaño del efecto bajo (r = .14)


#################################################################
########################      ANOVA      ########################
#################################################################

# Visualizar variación entre y dento de los grupos
boxplot(pisa2009_chl$math~pisa2009_chl$type,
        xlab = "Dependencia", ylab = "Puntaje Pisa Mat")

## Pregunta de Investigación:
# ¿Difiere significativamente el puntaje promedio en matemáticas de los estudiantes según la 
# dependencia del establecimiento al que asisten?

# Hipótesis Nula e Hipótesis Alternativa
# H0 = No hay diferencias significativas  en los puntajes promedios de matemáticas 
# entre estudiantes en establecimientos de distintas dependencias.
# H1 = No todos los promedios son iguales.

#  Calcula el estadístico usando la función aov 
res_an <- aov(pisa2009_chl$math~pisa2009_chl$type)
summary(res_an)

## Interpretación de Estadístico F y su valor de significancia: 
### El valor de p es lo suficientemente pequeño (por convención, p < .05; p < .01; p < .001) para rechazar la Hipótesis nula de que 
### no existe diferencia entre los grupos. 

## Reporte de resultados:
### El puntaje promedio de los estudiantes en mateáticas difiere según la 
### dependencia del establecimiento al que asisten, F(2,4478) = 155.6, p < .001).


#################################################################
################## CORRELACIÓN DE PEARSON #######################
#################################################################

## Pregunta de Investigación: 
# ¿Existe una asociación significativa entre el puntaje en matemáticas y el puntaje 
# en lectura de los estudiantes?

# Hipótesis Nula e Hipótesis Alternativa
# H0 = Los puntajes en lectura de los estudiantes no se asocian significativamente a los puntajes de matemáticas.
# H1 = Los puntajes en lectura de los estudiantes sí se asocian significativamente a los puntajes de matemáticas.

# Visualiza la relación entre las variables
pairs(pisa2009_chl$math ~ pisa2009_chl$read)  

ggplot(pisa2009_chl) +
  aes(x = math, y = read) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

# Corre la prueba de correlación
r <- cor.test(pisa2009_chl$math, pisa2009_chl$read,
                method = "pearson")
r

# Calculo del coeficiente de determinacion
r2 <- r$estimate*r$estimate

r2

## Interpretación del coeficiente r y su valor de significancia: 
### El valor de p es lo suficientemente pequeño (por convención, p < .05; p < .01; p < .001) para rechazar la Hipótesis nula de que 
### no existe asociación entre las variables. 

## Reporte de resultados:
### Existe una asociación significativa, positiva y alta entre los puntajes de lectura
### y los puntajes en matemáticas (r = .83, p < .001). 
### Los puntajes en matemáticas explican un 69% de los puntajes en lenguaje.

# Genera tabla de correlación para más de dos variables
round(cor(pisa2009_chl[,7:9]),
      digits = 2) # redondea a dos decimales

