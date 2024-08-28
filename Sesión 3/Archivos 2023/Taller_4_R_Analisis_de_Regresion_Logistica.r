
rm(list=ls()); dev.off()


###############################################
# Taller 4: Analisis de Regresion Logistica   #
###############################################

install.packages("ggplot2")
install.packages("corrplot")
install.packages("GGally")
install.packages("tidyverse")
install.packages("lmtest")
install.packages("DescTools")
install.packages("stringr")
install.packages("car")
install.packages("knitr")
install.packages("reshape2")
install.packages("broom")

library(ggplot2)
library(corrplot)
library(GGally)
library(tidyverse)
library(lmtest)
library(DescTools)
library(stringr)
library(car)
library(knitr)
library(reshape2)
library(broom)

# Cambiar el directorio de trabajo -----------------------------------------------------------

getwd() # directorio de trabajo actual *get working directory*

# setwd() cambia de directorio, establece un nuevo directorio
# Hay que cambiar la dirección a aquella donde se guardaron los datos

setwd("D:/Drive/Patricio/Docencia/[Cursos] Metodología Cuantitativa II/1.4. Análisis de regresión")

getwd()

# Importar datos SIES Matricula 2021 y 2022----------------------------------------------------------

mat2021 = read.csv(file="20220719_Matrícula_Ed_Superior_2021_PUBL_MRUN.csv",header = TRUE, sep = ";") 
mat2022 = read.csv(file="20220719_Matrícula_Ed_Superior_2022_PUBL_MRUN.csv",header = TRUE, sep = ";") 

View(mat2021)  # Abre la base de datos en una nueva ventana

# Seleccionamos los casos de la cohorte que ingreso a educación superior en 2021 (Para estudiar deserción en primer año)

mat2021_1anio = mat2021[which(mat2021$anio_ing_carr_ori=="2021"),]
mat2022_2anio = mat2022[which(mat2022$anio_ing_carr_ori=="2021"),]

## Generación de variable dependiente: Indicador de Deserción

# Generamos una variable que indique si el estudiante de primer año 2021 
# aparece como estudiante de 2do anio en 2022 (0) o no (1)

mat2021_1anio$deserta = ifelse(mat2021_1anio$mrun %in% mat2022_2anio$mrun, "0", "1")

# Seleccionamos solo a estudiantes de pregrado

mat2021_1anio = mat2021_1anio[which(mat2021_1anio$nivel_global=="Pregrado"),]

# explorar datos
head(mat2021_1anio)
dim(mat2021_1anio)
str(mat2021_1anio)

##########################################
######### Regresion Logistica ############
##########################################

## Preparación de variables independientes

# Recodificamos variable género para que indique female (1) o male (0)
mat2021_1anio$female = as.numeric(mat2021_1anio$gen_alu)
mat2021_1anio$female[mat2021_1anio$female == "1"] = 0
mat2021_1anio$female[mat2021_1anio$female == "2"] = 1

# Calculamos edad al ingreso de la educación superior, en base a la fecha de nacimiento

mat2021_1anio$anio_nac = as.numeric(str_sub(mat2021_1anio$fec_nac_alu,1,4)) 
mat2021_1anio$anio_ing_carr_ori = as.numeric(mat2021_1anio$anio_ing_carr_ori)
mat2021_1anio$edad_alu = mat2021_1anio$anio_ing_carr_ori - mat2021_1anio$anio_nac
summary(mat2021_1anio$edad_alu)
mat2021_1anio = mat2021_1anio[!(mat2021_1anio$edad_alu==121),]
summary(mat2021_1anio$edad_alu)

## Analisis descriptivo
#Gráfico de barras frecuencias deserta 

ggplot(data = mat2021_1anio, aes(deserta, fill = deserta)) +
  geom_bar(position="dodge") +
  ggtitle("Deserta") +
  ylab("Cantidad") + 
  scale_fill_manual(values=c("#87ceeb", "#ffd700")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position='top')


# Regresion logistica: predecir una variable categorica (deserta) con otras variables (female y edad_alu)

mat2021_1anio$deserta = as.numeric(mat2021_1anio$deserta)

m2 = glm(deserta ~ female + edad_alu, family = binomial(logit), mat2021_1anio)
summary(m2)

###########################################################################################################################

####################################### 
# Supuestos de la Regresion Logistica
#######################################
#
# Supuestos Conceptuales:
#
## 1. La variable dependiente es categorica.
# El/la investigadora establece la variable dependiente.
#
## 2. Independencia entre las observaciones: 
# Suele ser el resultado del proceso de recolección de los datos, por lo que usualmente 
# no es fácil detectarlo mediante la evaluación estadística de residuos del modelo. 
# La mejor forma de evaluar este supuesto usualmente es comprender el proceso mediante 
# el cual se recopilaron los datos.
# Estructura de los datos: Estudiantes anidados en programas, anidados a su vez en instituciones. 
# ¿Tendrán algún efecto en la deserción los programas y las instituciones?
# Probablemente sí! Más sobre esto en la Sesión 6 (07/09/2022): Análisis multinivel 
#
# Supuestos Estadisticos:
#
## 3. Linealidad: 
# La relación entre el logit de la variable dependiente y cada variable independiente es lineal; 
# este supuesto se verifica únicamente para las variables numéricas continuas que se tengan en el modelo.
# Para verificar este supuesto, usamos el Test Box-Tidwell (función boxTidwell del paquete car). 
# Hipótesis nula: el cumplimiento del supuesto de linealidad.

logodds = m2$linear.predictors
boxTidwell(logodds ~ mat2021_1anio$edad_alu)

# Resultado Supuesto Linealidad: 
# No se rechaza el supuesto de linealidad entre el logit y la variable edad_alu.
#
## 4. Ausencia de multicolinealidad (en el caso de regresion multiple)
# Multicolinealidad: cuando dos o más variables independientes mantienen una relación lineal. 
# Es posible trabajar con un grado de correlación moderado.
# Cuando la correlación entre variables es muy alta, se genera un incremento en los errores estándar, lo cual hace que los coeficientes estimados no sean confiables, y en consecuencia, las estimaciones sean poco creíbles.
# Para evaluar:
#  - Calcular correlación entre las variables explicativas (continuas). 
#  - Verificar colinealidad por medio de los valores de tolerancia o los factores infladores de varianza (VIF). 
#
# Ejemplo de Modelo con tres variables independientes continuas:
mat2021_1anio$valor_arancel =  as.numeric(mat2021_1anio$valor_arancel)
m3 = glm(deserta ~ female + edad_alu + dur_total_carr + valor_arancel, family = binomial(logit), mat2021_1anio)
summary(m3)
#
## Correlacion entre variables independientes continuas
cormat = round(cor(mat2021_1anio[,c("edad_alu","dur_total_carr","valor_arancel")]),2)
cormat

melted_cormat = melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# El análisis de correlacion anterior permite concluir que no existe una correlación 
# extremadamente alta entre las variables independientes. La correlacion de mayor magnitud se da entre 
# las variables valor_arancel y dur_total_carr (r = 0.68)

## Variance Inflaction Factor (VIF) vif(modelo)
# Los factores infladores de varianza o VIF determinan el grado de relación entre variables independientes. 
# Sería problemático obtener valores VIF mayores a 5 o 10, ya que esto generaría un sesgo en el modelo, 
# por lo que sería viable descartar alguna de las variables predictoras. 

kable(vif(m3), digits = 2)

# Resultado Supuesto Ausencia de multicolinealidad: 
# Para el conjunto de datos considerado, los valores en la tabla parecen indicar 
# que no hay un problema evidente de multicolinealidad.


######################################################################################################################## 

# Resultados Regresion Logistica
summary(m3)
#
# Interpretacion:
#
# El output de la regresion logistica nos muestra distintos indicadores:
# 
# Residuals: nos muestra la distribucion de los errores.
# 
# Coefficients:
#   # Estimates
#   - nos estrega el valor log-odds promedio de nuestra variable dependiente (Intercept)
#   - nos entrega la variacion del promedio por unidad de la variable predictora.
#   # Std.Error: Error estandard de la media  
#   # z-value: Estadistico z; > |2| es significativo 
#   # Pr(>|t|): valor p; < .05 es signifitivo... ademas marca con asteriscos: *<.05 **<.01 ***<.001  
#
# Las variables utilizadas como predictoras (female, edad_alu, dur_total_carr y valor_arancel) 
# son significativas para predecir la probabilidad de deserción de los estudiantes de primer año.

modelChi = m3$null.deviance - m3$deviance
modelChi
chidf = m3$df.null - m3$df.residual
chidf
chisq.prob = 1 - pchisq(modelChi, chidf)
chisq.prob 
# Este modelo (m3) es significativamente  mejor que el modelo vacío (que contiene solo el intercepto).

## Calculo de probabilidades estimadas
mat2021_1anio$Probabilidades_estimadas = m3$fitted.values

kable(mat2021_1anio[c(1:10), c("deserta", "female", "edad_alu", "dur_total_carr","valor_arancel",
                        "Probabilidades_estimadas")], digits = 2)
#
# Ej: La probabilidad de que una mujer de 19 años, en un programa de 5 semestres de duracion 
# y con un arancel de $2.510.000 deserte es de 27%
#
## No hay Adjusted R-squared, pero podemos calcular un Pseudo R2
PseudoR2(m3)
#   - nos muestra cuanto porcentaje de la varianza presente en nuestra variable dependiente es explicada por nuestro modelo.
#

## Calculo de odds ratio
m3$coefficients
exp(m3$coefficients)
exp(confint(m3))

#
# Interpretación de odds ratio:
# Podemos decir que para un aumento de una unidad en edad_alu, esperamos ver un aumento de alrededor del 3% en 
# las probabilidades de desertar en primer año.

## Evaluación de la precisión del modelo:
# La precisión del modelo se mide como la proporción de observaciones que se han clasificado correctamente. 
# Inversamente, el error de clasificación se define como la proporción de observaciones que han sido mal clasificadas.
# Proporción de observaciones clasificadas correctamente:

probabilities = m3 %>% predict(mat2021_1anio, type = "response")
head(probabilities)
predicted = ifelse(probabilities > 0.5, 1, 0)
mean(predicted == mat2021_1anio$deserta)

# La precisión de la predicción de clasificación es de alrededor del 73%, lo cual es bueno. 
# La tasa de error de clasificación errónea es del 27%.

######################################################################################################################## 

# Visualizacion
m3. = augment(m3)

# glm
ggplot(m3., aes(edad_alu, deserta)) +
  geom_point(lwd=3,alpha=.5) +
  stat_smooth(method="glm", se=T, method.args = list(family=binomial), color="royalblue")+
  theme_bw(base_size = 22) 

ggplot(m3., aes(dur_total_carr, deserta)) +
  geom_point(lwd=3, alpha=.5) +
  stat_smooth(method="glm", se=T, method.args = list(family=binomial), color="royalblue")+
  theme_bw(base_size = 22) 

ggplot(m3., aes(valor_arancel, deserta)) +
  geom_point(lwd=3, alpha=.5) +
  stat_smooth(method="glm", se=T, method.args = list(family=binomial), color="royalblue")+
  theme_bw(base_size = 22) 

######################################################################################################################## 

# Compara Ajuste de Modelos
anova(m2, m3)
