library(dplyr)
library(ggplot2)
library(ggpubr)
library(modeest)
library(BSDA)
library(readxl)
library(tidyverse)
library(RVAideMemoire)


# chi-cuadrado observaciones independientes, obs >= 5 por grupo
# Pregunta 1
# Se ha realizado un estudio acerca de la prevalencia de trastornos del lenguaje con un grupo de 9 ninas y
# 11 ninos de segundo basico. Los datos obtenidos muestran que 10 de los ninos presentan dificultades,
# mientras que solo 3 de las ninas lo hacen. Existe relación entre el sexo y la prevalencia de trastornos del
# lenguaje?


#En este ejercicio  se utiliza la prueba Fisher dado que las variables son dicotomicas
# y se quiere saber si las variables estan relacionadas

# HIPOTESIS NULA, EXISTE RELACION ENTRE EL SEXO Y LA PREVALENCIA DE TRASTORNOS DEL LENGUAJE
# HIPOTESIS ALTERNATIVA, NO EXISTE RELACION ENTRE EL SEXO Y LA PREVALENCIA DE TRASTORNOS DEL LENGUAJE, ES DECIR, INDEPENDIENTES

# Se construye la tabla de contingencia
dificultad <- c(10, 3)
no_dificultad <- c(1, 6)

# Se genera la tabla
tabla <- as.table(rbind(dificultad,no_dificultad))
dimnames(tabla) <- list(dificultad = c("Dificultad de aprendizaje", "Sin dificultad de aprendizaje"),
                        sexo = c("Niños", "Niñas"))
tabla
alfa <- 0.05
fisher.test(tabla, 1-alfa)

#P-value = 0.01664
#P-value < 0.05, es decir, se falla en rechazar la hipotesis nula, en conclusion, se puede decir que existe relacion entre el sexo 
#y la prevalencia de trastornos del lenguaje


# Pregunta 2
# Siempre tenaz en su lucha para erradicar a los vampiros de la faz de la tierra, Van Helsing desea probar
# una vacuna que, segun el, causara una grave enfermedad en estos seres una vez que beban la sangre de
# sus victimas. Para ello, ha almacenado una gran cantidad de dosis de su propia sangre, separadas en dos
# grupos: uno de ellos contiene el quimico de la vacuna, mientras el otro esta completamente limpio.
# Adicionalmente, Van Helsing cuenta con 13 vampiros cautivos, a los que alimento con sangre limpia por
# una semana. Luego de un periodo de limpieza (durante el cual los vampiros fueron alimentados con su
# dieta normal, por lo que eliminaron todo rastro de la sangre de Van Helsing), repitia el experimento con la
# sangre que contiene la vacuna. Para ambos casos, registra cuantos vampiros enfermaron, con los
# siguientes resultados:
# ? 3 vampiros no presentaron enfermedad alguna con ninguna de las dietas de Van Helsing.
# ? 2 vampiros enfermaron tras ambas dietas de Van Helsing.
# ? 2 vampiros enfermaron con la sangre limpia de Van Helsing, pero no con la sangre que contiene la
# vacuna.
# ? 6 vampiros enfermaron con la sangre que contiene la vacuna, pero no con la sangre limpia de Van
# Helsing.
# Es posible decir que la vacuna de Van Helsing esta asociada a una enfermedad en los vampiros?



# Para esta prueba se decide utilizar Mcnemar, ya que es la mas apropiada, dado que 
# se desea analizar una caracteristica la cual tiene una respuesta dicotomica, en diferentes ocasiones
# Esto se cumple dado que se desea analizar si los vampiros tras tener una diferente dieta se enfermen

#HIPOTESIS NULA, la vacuna de Van Helsing esta asociada a una enfermedad en los vampiros
#HIPOTESIS ALTERNATIVA, la vacuna de Van Helsing NO esta asociada a una enfermedad en los vampiros

vampiro <- seq(1:13)
dieta1 <- c(rep("No Enferma", 3), rep("Enferma", 2), rep("Enferma", 2), rep("No Enferma", 6))
dieta2 <- c(rep("No Enferma", 3), rep("Enferma", 2), rep("No Enferma", 2), rep("Enferma", 6))

# Se genera la tabla 
tabla <- table(dieta2, dieta1)

print(tabla)

prueba <- mcnemar.test(tabla)
prueba

# P-value = 0.2888
# P-value > 0.05
# se falla en rechazar la hipotesis nula (se acepta la nula) donde se comprueba que no hay relacion entre las dietas



# se falla en rechazar la hipotesis nula (se acepta la nula) donde se comprueba que no hay relacion entre las dietas
# jaja, la pregunta era si tenian una enfermedad

# Pregunta 3
# El 21 de marzo de 2022 se realiza un estudio acerca de la aprobacion al presidente Gabriel Boric entre los
# estudiantes de una prestigiosa universidad a fin de comparar los resultados con los obtenidos en la misma
# encuesta a nivel nacional, obteniendose los resultados que se muestran en la tabla. Refleja la opinion
# estudiantil la percepcion del pais?


# Pregunta 3

# El 21 de marzo de 2022 se realiza un estudio acerca de la aprobacion al presidente Gabriel Boric entre los
# estudiantes de una prestigiosa universidad a fin de comparar los resultados con los obtenidos en la misma
# encuesta a nivel nacional, obteniendose los resultados que se muestran en la tabla. Refleja la opinion
# estudiantil la percepcion del pais?


#En este ejercicio se decide utiliza la prueba chi-cuadrado de bondad de ajuste
# Ya que se emplea para comprobar si una muestra es representativa de la poblacion
# es decir, si una distribucion de frecuencias observadas se asemejan a una distribucion esperada

#HIPOTESIS NULA, La opinion estudiantil refleja la percepcion del pais
#HIPOTESIS ALTERNATIVA, la opinion estudiantil no refleja la percepcion del pais

estudiantes = c(208, 7, 2)
nacional = c(5046, 3421, 706)

#se crea la tabla 
tabla <- as.table(rbind(estudiantes,nacional))
dimnames(tabla) <- list(c("Estudiantes", "Nacional"),
                        c("Aprueba", "Desaprueba", "Ninguna"))
tabla
chisq.test(tabla)

#En conclusion, dado que el p-value es menor a 0.05, se rechaza la hipotesis nula, por lo que 
# la opinion estudiantil no refleja la percepcion del pais


# Pregunta 4
# La Facultad de Ingenier?a desea saber si existe diferencia significativa en el desempe?o de los estudiantes
# en asignaturas cr?ticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
# asignaturas, indica si una muestra de 50 estudiantes aprob? o reprob?. ?Qu? puede concluir la Facultad?
# Indicaci?n: obtenga la muestra a partir del archivo "EP04 Datos.csv" que se encuentra en el directorio
# compartido, usando la semilla 440. Considere un nivel de significaci?n a=0,05.


#Cargamos los datos y sacamos una muestra del 10% de los datos con la semilla indicada.
datos <- read_excel("~/ep04/EP04 Datos.xls")
size <- nrow(datos) * 0.1 # 10%
set.seed(440)
datos_filtrados <- datos[sample(nrow(datos), size), ]

#Como queremos revisar si existe una diferencia significativa entre el desempeño de los estudiantes
#con respecto a las asignaturas de primer semestre criticas, podemos realizar un test de mcnemar,
#pero ya que una de nuestras variables es categorica, procedemos con la utilizacion de Q de Cochran.

#HIPOTESIS NULA, No existe diferencia significativa entre el desempeño de los estudiantes.
#HIPOTESIS ALTERNATIVA, Existe diferencia significativa entre el desempeño de los estudiantes.

#Como nuestros datos estan escritos con R y A para reprobado y aprobado, alteramos estos datos para
#representar 0 y 1 como se requiere para este test.

datos_filtrados$Calculo[datos_filtrados$Calculo == "A"] <- 1
datos_filtrados$Calculo[datos_filtrados$Calculo == "R"] <- 0

datos_filtrados$Algebra[datos_filtrados$Algebra == "A"] <- 1
datos_filtrados$Algebra[datos_filtrados$Algebra == "R"] <- 0

datos_filtrados$Fisica[datos_filtrados$Fisica == "A"] <- 1
datos_filtrados$Fisica[datos_filtrados$Fisica == "R"] <- 0


id <- datos_filtrados$Id
calculo <- datos_filtrados$Calculo
algebra <- datos_filtrados$Algebra
fisica <- datos_filtrados$Fisica

datos <- data.frame(id, calculo, algebra, fisica)

datos <- datos %>% pivot_longer(c("calculo", "algebra", "fisica"),
                                names_to = "materia",
                                values_to = "estado")

datos[["id"]] <- factor(datos[["id"]])
datos[["materia"]] <- factor(datos[["materia"]])

cochran.qtest(estado ~ materia | id, data = datos, alpha = 0.05)

#En conclusion, como el resultado de este test nos entrega un p-value > 0.05, por lo que se acepta 
#la hipotesis nula, la cual indica que no hay diferencia significativa entre la aprobacion y reprobacion
#de las asignaturas criticas del MBI