
###############################################################################################################
######################## ##########Taller de R #2: ############################################################
############### Análisis descriptivo de datos del Servicio de Información de Educación Superior (SIES)#########
###############################################################################################################

# Instalación de Paquetes ----------------------------------------------------

install.packages("readxl")   # Paquete para importar datos en formato Excel
install.packages("janitor")  # Paquete para limpieza de datos
install.packages("ggplot2")  # Paquete para generar gráficos

# Instalación de Bibliotecas de Paquetes ----------------------------------------------------
library(readxl)
library(janitor)
library(ggplot2)

# Cambiar el directorio de trabajo -----------------------------------------------------------

getwd() # directorio de trabajo actual *get working directory*

# setwd() cambia de directorio, establece un nuevo directorio
# Hay que cambiar la dirección a aquella donde se guardaron los datos

setwd("C:/Users/lcort/Downloads")

getwd()

# Importar datos ----------------------------------------------------------

personal_2021 <- read_excel("Base-Personal-Academico-2021_SIES.xlsx")

View(personal_2021)  # Abre la base de datos en una nueva ventana


# Limpieza de datos -------------------------------------------------------

# Como se puede observar, los nombres de las variables en la base de datos están todos escritos con 
# letra mayúscula al comienzo. Se puede realizar una limpieza/transformación con el paquete janitor

personal_2021  <- clean_names(personal_2021)

View(personal_2021)  # Abre la base de datos en una nueva ventana

## ¿Qué tenemos en la base de datos? # Exploremos los datos

class(personal_2021)    # Muestra la clase de objeto es personal_2021
head(personal_2021)     # Muestra los primeros 6 casos en la base de datos
names(personal_2021)    # Lista los nombres de las variables en la base de datos
dim(personal_2021)      # Muestra el número de variables y casos
summary(personal_2021)	# Entrega estadísticos descriptivos para cada variable
str(personal_2021)      # Muestra la estructura interna de la base de datos 


# Generemos una nueva variable para distinguir entre tipos de instituciones:
### 1: Universidad Estatal CRUCH.
### 2: Universidad Privada CRUCH.
### 3: Universidad Privada.
### 4: Instituto Profesional.
### 5: Centro de Formación Técnica.
### 6: Centros de Formación Técnica Estatales.

## Primero, generamos una nueva variable (tipo_ies) que es exactamente igual a la variable codigo_institucion
personal_2021$tipo_ies <- personal_2021$codigo_institucion

## Luego, a cada valor de codigo_institucion le asignamos un valor que corresponde a su tipo de institución

### 1: Universidad Estatal CRUCH.
personal_2021$tipo_ies[personal_2021$codigo_institucion == 70] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 71] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 72] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 73] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 74] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 75] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 76] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 77] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 78] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 79] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 80] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 81] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 82] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 83] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 84] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 85] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 895] <- 1
personal_2021$tipo_ies[personal_2021$codigo_institucion == 896] <- 1

### 2: Universidad Privada CRUCH.
personal_2021$tipo_ies[personal_2021$codigo_institucion == 3] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 34] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 69] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 86] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 87] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 88] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 89] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 90] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 91] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 92] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 93] <- 2
personal_2021$tipo_ies[personal_2021$codigo_institucion == 94] <- 2

### 3: Universidad Privada.
personal_2021$tipo_ies[personal_2021$codigo_institucion == 1] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 2] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 4] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 7] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 9] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 10] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 11] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 13] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 17] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 19] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 20] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 22] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 23] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 26] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 31] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 38] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 39] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 42] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 45] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 46] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 48] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 50] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 54] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 68] <- 3
personal_2021$tipo_ies[personal_2021$codigo_institucion == 651] <- 3

### 4: Instituto Profesional.
personal_2021$tipo_ies[personal_2021$codigo_institucion == 99] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 100] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 101] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 103] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 104] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 106] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 108] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 111] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 113] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 116] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 117] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 120] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 123] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 129] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 132] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 137] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 138] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 139] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 143] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 144] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 152] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 155] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 162] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 165] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 170] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 171] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 176] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 193] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 676] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 693] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 714] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 743] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 754] <- 4
personal_2021$tipo_ies[personal_2021$codigo_institucion == 767] <- 4

### 5: Centro de Formación Técnica.
personal_2021$tipo_ies[personal_2021$codigo_institucion == 214] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 218] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 229] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 236] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 241] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 257] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 260] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 273] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 280] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 305] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 312] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 319] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 328] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 331] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 367] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 374] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 382] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 390] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 398] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 426] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 427] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 430] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 435] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 450] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 456] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 492] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 498] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 534] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 536] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 591] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 629] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 633] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 691] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 701] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 730] <- 5
personal_2021$tipo_ies[personal_2021$codigo_institucion == 258] <- 5

### 6: Centros de Formación Técnica Estatales.
personal_2021$tipo_ies[personal_2021$codigo_institucion == 902] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 906] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 908] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 909] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 910] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 911] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 912] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 913] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 914] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 915] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 916] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 917] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 918] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 919] <- 6
personal_2021$tipo_ies[personal_2021$codigo_institucion == 920] <- 6

# Visualizamos los valores generados
personal_2021$tipo_ies

# Asignamos etiquetas a los valores generados
personal_2021$tipo_ies_n <- factor(personal_2021$tipo_ies,
                    levels = c(1,2,3,4,5,6),
                    labels = c("U. ESTATAL","U. PRIVADA CRUCH","U. PRIVADA","IP","CFT PRIVADO","CFT ESTATAL"))

### Se puede seleccionar un subconjunto de los datos, así:

personal_2021_uestatal <- personal_2021[which(personal_2021$tipo_ies_n=="U. ESTATAL"),]


##############################################################################################
#                            ANÁLISIS DESCRIPTIVO:                                           #
#                                                                                            #
##############################################################################################

### 1. Análisis descriptivo univariado para variable categórica: tipo_ies

# 1.1 Tabla de frecuencia variable categórica
table(personal_2021$tipo_ies_n)

#Guardar la tabla como objeto
f <- table(personal_2021$tipo_ies_n)
f_porc <- round((prop.table(f)*100),2)
f_porc_acum <- round(cumsum(prop.table(f)*100),2)

write.csv2(f, file = "Tabla 1.csv")
write.csv2(f_porc, file= "Tabla 2.csv")
write.csv2(f_porc_acum, file= "Tabla 3.csv")

# 1.2 Visualización
## Gráfico de Barras: 
plot = as.data.frame(table(personal_2021$tipo_ies_n))
ggplot(plot, aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity")

## Gráfico de Torta:
plot = as.data.frame(table(personal_2021$tipo_ies_n))
ggplot(plot, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", color="white") +
  coord_polar("y", start=0) + 
  theme_void()


### 2. Análisis descriptivo univariado para variable continua: promedio_edad_por_institucion
# 2.1: Estadísiticos descriptivos
summary(personal_2021$promedio_edad_por_institucion)
sd(personal_2021$promedio_edad_por_institucion); var(personal_2021$promedio_edad_por_institucion)

# Guardar summary como objeto
descriptivos <- summary(personal_2021$promedio_edad_por_institucion)

#Ver nombres y valores del objeto
names(descriptivos)
as.numeric(descriptivos)

#Configurar como matriz de datos
descr_prom_edad <- as.data.frame(rbind(names(descriptivos), round(as.numeric(descriptivos),0)))
View(descr_prom_edad)

#Guardar la tabla como objeto
write.csv2(descr_prom_edad, file = "Tabla 4.csv")

# 2.2 Visualización: Histograma, densidad y boxplot: para conocer la distribución de los datos
# Histograma
ggplot(personal_2021, aes(x=promedio_edad_por_institucion))+
  geom_histogram()
# Densidad
ggplot(personal_2021, aes(x=promedio_edad_por_institucion))+
  geom_density()
# Ambos
ggplot(personal_2021, aes(x=promedio_edad_por_institucion))+
  geom_histogram(aes(y = ..density..))+
  geom_density()
# Boxplot
ggplot(personal_2021, aes(y=promedio_edad_por_institucion, x=""))+
  geom_boxplot()

### 3. Análisis descriptivo bivariado:  
### Variable continua (promedio_edad_por_institucion) vs. variable categórica (tipo_ies_n)

# 3.1 Estadísticos descriptivos por grupo de interés: tipo_ies
by(personal_2021$promedio_edad_por_institucion, personal_2021$tipo_ies_n, summary)

# 3.2 Visualización: 
### Graficamos cómo difiere el promedio y distribución de promedio de edad según tipo de institución
# Gráfico de barras (con estadísticos descriptivos)
ggplot(personal_2021, aes(x=tipo_ies_n, y=promedio_edad_por_institucion)) +
  geom_bar(stat = "summary", fun = "mean")
# Histograma
ggplot(personal_2021, aes(x = promedio_edad_por_institucion, fill = tipo_ies_n)) + 
  geom_histogram()
# Densidad
ggplot(personal_2021, aes(x=promedio_edad_por_institucion, fill = tipo_ies_n))+
  geom_density()
# Boxplot
ggplot(personal_2021, aes(y=promedio_edad_por_institucion, x="", fill = tipo_ies_n))+
  geom_boxplot()


#######################################################################################################

### EJERCICIO:

# 1. Realice un análisis descritivo univariado de la variable 
#    Porcentaje de mujeres/academicas en las instituciones de educación superior.

# 2. Realice un análisis descritivo bivariado entre las variables
#    Porcentaje de mujeres/academicas en las instituciones de educación superior y Tipo
#    de institución de educación superior,

# Para llevar a cabo este ejercicio, tenemos que crear una variable que represente el porcentaje de mujeres respecto del total de academicos
personal_2021$por_muj <- round((personal_2021$total_mujeres/personal_2021$total_general)*100)
