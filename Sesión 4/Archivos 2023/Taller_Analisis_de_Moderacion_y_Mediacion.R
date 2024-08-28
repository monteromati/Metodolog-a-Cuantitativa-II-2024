
### TALLER ANALISIS DE MODERACION Y MEDIACION EN R ###

install.packages("ggplot2")
library(ggplot2)

# Establece directorio de trabajo (working directory)

setwd("C:/Users/lcort/Downloads/")

# Abre base de datos SIMCE 2018 2do Medio Puntaje de Estudiantes 

palu <- read.table("simce2m2018_alu_privada_final-SEG.txt", # Archivo de datos TXT indicado como string o ruta completa al archivo
           header = TRUE,                                   # Si se muestra el encabezado (TRUE) o no (FALSE)
           sep = "|",                                       # Separador de las columnas del archivo
           dec = ".")                                       # Caracter utilizado para separar decimales de los números en el archivo

# Abre base de datos SIMCE 2018 2do Medio Cuestionario Estudiantes 

cest <- read.table("simce2m2018_cest_privada_final-SEG.txt", # Archivo de datos TXT indicado como string o ruta completa al archivo
                   header = TRUE,                            # Si se muestra el encabezado (TRUE) o no (FALSE)
                   sep = "|",                                # Separador de las columnas del archivo
                   dec = ".")                                # Caracter utilizado para separar decimales de los números en el archivo

# Funde ambas bases de datos en base a la variable "idalumno", presente en ambas

total <- merge(palu,cest,by="idalumno",all=TRUE)

# Abre base de datos SIMCE 2018 2do Medio Cuestionario de Padres/Apoderados 

cpad <- read.table("simce2m2018_cpad_privada_final-SEG.txt", # Archivo de datos TXT indicado como string o ruta completa al archivo
                   header = TRUE,                            # Si se muestra el encabezado (TRUE) o no (FALSE)
                   sep = "|",                                # Separador de las columnas del archivo
                   dec = ".")                                # Caracter utilizado para separar decimales de los números en el archivo

# Funde ambas bases de datos en base a la variable "idalumno", presente en ambas

total <- merge(total,cpad,by="idalumno",all=TRUE)

str(total)

##############################
### ANALISIS DE MODERACION ###
##############################

# El análisis de moderación permite comprobar la influencia de una tercera variable, Z, en la relación entre las variables X e Y. 
# La moderación comprueba cuándo o en qué condiciones se produce un efecto. 
# Los moderadores pueden reforzar, debilitar o invertir la naturaleza de una relación. 

# Pregunta de Investigación: 
# ¿Modera el género de los estudiantes la asociación entre el rendimiento en 
# matemáticas y el autoconcepto en matemáticas?

### Variable género del estudiante "gen_alu" (1=Hombre; 2=Mujer)

table(total$gen_alu)

### Genera bases de datos distintas para hombres y para mujeres
total_m <- total[total[,"gen_alu"]==2,]#base datos solo con mujeres
total_h <- total[total[,"gen_alu"]==1,]#base datos solo con hombres

### Item Autoconcepto en matemáticas
## cest_p02_09	"¿Qué tan capaz te sientes para aprender en cada una de las siguientes asignaturas? Matemática."	
# 0: Vacío
# 1: Nada capaz
# 2: Poco capaz
# 3: Bastante capaz
# 4: Muy capaz
# 99:Doble marca

table(total$cest_p02_09)

# Transforma valores perdidos en NAs
total$cest_p02_09[total$cest_p02_09 == "0"] <- NA
total$cest_p02_09[total$cest_p02_09 == "99"] <- NA

### Variable Puntaje en SIMCE Matemáticas

summary(total$ptje_mate2m_alu)

# Estadísticos básicos por grupo de interés: total$gen_alu
by(total$ptje_mate2m_alu, total$gen_alu, summary)

# Ajuste de modelos
modelo_principal <- lm(ptje_mate2m_alu ~ cest_p02_09,data=total)
modelo_mujeres <- lm(ptje_mate2m_alu ~ cest_p02_09,data=total_m)
modelo_hombres <- lm(ptje_mate2m_alu ~ cest_p02_09,data=total_h)

summary(modelo_principal)
summary(modelo_mujeres)
summary(modelo_hombres)

#prueba de Chow
#numerador
suma_R_o<-sum(modelo_principal$residuals^2)#suma residuos cuadrados
suma_R_M<-sum(modelo_mujeres$residuals^2) 
suma_R_H<-sum(modelo_hombres$residuals^2) 
k <- modelo_principal$rank  #numero de parametros
n1<- length(total_m$gen_alu)
n2<- length(total_h$gen_alu)
numerador<- (suma_R_o-(suma_R_M+suma_R_H))/k
denominador<-(suma_R_M+suma_R_H)/(n1+n2-2*k)
chow_test<- numerador/denominador# distribucion F. 
# La hipotesis nula es que los parametros de ambas regresiones son iguales. 

1-pf(chow_test,k,n1+n2-2*k) #significancia estadistica

### Método de Interaccion 
# Pimero recodificamos la variable "gen_alu"

total$mujer <- ifelse(total$gen_alu=="1", 0,ifelse(total$gen_alu=="2", 1, NA))

modelo_interaccion <- lm(ptje_mate2m_alu ~ cest_p02_09 + mujer + cest_p02_09*mujer,data=total)
summary(modelo_interaccion)

# Añade los valores predichos por el modelo a la base de datos, para poder graficarlos
total$predicted <- predict(modelo_interaccion, total)

## Grafica resultados

ggplot(total, aes(x = cest_p02_09, y = predicted, colour = factor(mujer), group = mujer)) +
            stat_smooth(method = "lm", se=FALSE)

## Interpretacion de los resultados del análisis de moderación
# Los resultados se presentan de forma similar a los resultados habituales de regresión múltiple. 
# Nuestro modelo muestra una interacción significativa entre el autoconcepto en matemáticas 
# y el género del estudiante (b = -2.98, SE = .03, p < .001). 
# La diferencia en las pendientes de hombres y mujeres muestra que 
# el género modera la relación entre el autoconcepto y el rendimiento en matemáticas, 
# siendo esta asociación más fuerte en el caso de los hombres.



#############################
### ANALISIS DE MEDIACION ###
#############################

# El análisis de mediación pone a prueba una hipotética cadena causal en la que 
# una variable X afecta a una segunda variable M y, a su vez, esa variable afecta a una tercera variable Y. 
# Los mediadores describen el cómo o el porqué de una relación (normalmente bien establecida) 
# entre otras dos variables y a veces se denominan variables intermedias, 
# ya que suelen describir el proceso a través del cual se produce un efecto. 
# A veces también se denominan efectos indirectos.

# Pregunta de Investigación: 
# ¿El ingreso familiar influye en el rendimiento en lectura de los estudiantes 
# a través de las expectativas de los padres?

### Ingreso familiar: variable cpad_p10
# "En un mes normal, ¿en cuál de los siguientes rangos se encuentra la
# suma de los ingresos de todas las personas que aportan al hogar donde vive el estudiante?
# 1: Menos de $ 100.000 
# 2: Entre $ 100.001 y $ 200.000
# 3: Entre $ 200.001 y $ 300.000 
# 4: Entre $ 300.001 y $ 400.000 
# 5: Entre $ 400.001 y $ 500.000 
# 6: Entre $ 500.001 y $ 600.000 
# 7: Entre $ 600.001 y $ 800.000 
# 8: Entre $ 800.001 y $ 1.000.000
# 9: Entre $ 1.000.001 y $ 1.200.000
# 10: Entre $ 1.200.001 y $ 1.400.000
# 11: Entre $ 1.400.001 y $ 1.600.000
# 12: Entre $ 1.600.001 y $ 1.800.000
# 13: Entre $ 1.800.001 y $ 2.000.000
# 14: Entre $ 2.000.001 y $ 2.200.000
# 15: Más de $ 2.200.000
# 0: Vacío
# 99: Doble marca

table(total$cpad_p10)

# Transforma valores perdidos en NAs
total$ingreso <- total$cpad_p10
total$ingreso[total$ingreso == "0"] <- NA
total$ingreso[total$ingreso == "99"] <- NA
table(total$ingreso)


### Expectativas de los padres: variable cpad_p13	
# "Pensando en el futuro, ¿cuál cree usted que es el nivel educacional más alto que el estudiante completará?
#	1: No creo que complete IV año de educación media
# 2: IV año de educación media técnico profesional
# 3: IV año de educación media científico humanista
# 4: Una carrera en un centro de formación técnica o instituto profesional
# 5: Una carrera en una universidad
# 6: Estudios de postgrado
# 0: Vacío
# 99: Doble marca

table(total$cpad_p13)

# Transforma valores perdidos en NAs
total$expectativas <- total$cpad_p13
total$expectativas[total$expectativas == "0"] <- NA
total$expectativas[total$expectativas == "99"] <- NA
table(total$expectativas)


# Paso 1 de 4
p1 <- lm(ptje_lect2m_alu ~ ingreso,data=total)
summary(p1)

# Paso 2 de 4
p2 <- lm(expectativas ~ ingreso,data=total)
summary(p2)

# Paso 3 de 4
p3 <- lm(ptje_lect2m_alu ~ expectativas,data=total)
summary(p3)

# Paso 4 de 4
p4 <- lm(ptje_lect2m_alu ~ ingreso + expectativas,data=total)
summary(p4)

#tamano efecto indirecto

Eff_ind <- coef(summary(p1))[2,"Estimate"]-coef(summary(p4))[2,"Estimate"] #Judd y Kenny
Eff_ind 

Eff_indi <- coef(summary(p4))[3,"Estimate"]*coef(summary(p2))[2,"Estimate"]#sobel       
Eff_indi

#efecto total
Eff_directo <- coef(summary(p4))[2,"Estimate"]
Eff_directo

Eff_total <- Eff_directo + Eff_ind
Eff_total

# Nuestro modelo de efecto total (Paso 1) muestra una relación positiva significativa entre 
# el ingreso familiar (X) y el rendimiento en lectura (Y). 

# Nuestro modelo de Paso 2 muestra que el ingreso familiar (X) también está relacionado 
# positivamente con las expectativas de los padres (M). 

# Nuestro modelo de Paso 3 muestra que las expectativas de los padres (M) predicen 
# positivamente el rendimiento en lectura (Y). 

# Nuestro modelo de Paso 4 muestra que las expectativas de los padres (M) predicen 
# positivamente el rendimiento en lectura (Y) aún cuando se controla por el ingreso familiar (X). 


# Dado que la relación entre el ingreso familiar y el rendimiento en lectura 
# disminuye pero no deja de ser significativa cuando se controla por
# las expectativas de los padres, esto sugiere que las expectativas de los padres
# media parcialmente esta relación. 