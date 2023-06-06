library(dplyr)
library(ggplot2)
library(ggpubr)
library(modeest)
library(BSDA)
library(readxl)
library(tidyverse)
library(DescTools)
library(RVAideMemoire)

# 1. Escala de intervalos iguales para la variable dependiente
# 2. Las k muestras son obtenidas aleatorias e independientes
# 3. Se puede suponer k las poblaciones de origen siguen normalidad
# 4. Si las muestras provienen de mas de 1 población, deben tener la misma varianza (usar ezANOVA por Levenne) Homocedasticidad
# Varmax/Varmin <= 1.5 si no usamos Levenne
#Carga de datos
datos <- read.csv("~/ep05/EP05 Datos.csv", fileEncoding = "ISO-8859-1", sep=";", dec=",")

#filtrado de datos segun dificultad
dificiles <- filter(datos, dificultad=="Alta")

#filtrado por area indicada con datos anteriormente filtrados por dificultad
filterdif <- filter(dificiles, area=="Psicología" | area=="Música" | area=="Matemáticas")

#Procedemos a revisar las condiciones para utilizar ANOVA
#Como los tiempos estan en segundos, la primera condicion se cumple.
#Con el mismo resultado, por la informacion entregada en el enunciado,
#los datos de los distintos participantes son independientes y aleatorios 
#para la misma dificultad

#Demostraremos la normalidad de cada conjunto de datos a travez de shapiro

normpsi <- filter(filterdif, area=="Psicología")
shappsi <- shapiro.test(normpsi$tiempo)
shappsi

normmus <- filter(filterdif, area=="Música")
shapmus <- shapiro.test(normmus$tiempo)
shapmus

normmat <- filter(filterdif, area=="Matemáticas")
shapmat <- shapiro.test(normmat$tiempo)
shapmat

# homocedasticidad entre los distintos grupos
# Acá nos faltó probar las relaciones de Varmax y Varmin
hom1 <- var.test(x = normpsi$tiempo, y = normmat$tiempo)
hom2 <- var.test(x = normmat$tiempo, y = normmus$tiempo)
hom3 <- var.test(x = normmus$tiempo, y = normpsi$tiempo)

#Como todos los requisitos cumplen para utilizar ANOVA, procedemos a revisar con estas hipotesis

# H0 : no existen diferencias en el tiempo que tardan los usuarios en formular una consulta para un problema
#      de dificultad difícil en las áreas de psicología, música y/o matemáticas.
# Ha : existen diferencias en el tiempo que tardan los usuarios en formular una consulta para un problema
#      de dificultad difícil en las áreas de psicología, música y/o matemáticas.

#Calculo de datos de anova
anova <- aov(tiempo ~ area, data = filterdif)

#Muestra de la informacion de anova con p-value
summary(anova)
alfa = 0.025
#p-value < alfa, se falla en aceptar la hipotesis nula

#Se utiliza tukey ya que no se puede definir que un grupo es mas complejo que otro
#Se prefiere sobre Scheffe ya que es mas flexible
tukey <- TukeyHSD(anova, "area", ordered = TRUE, conf.level = 1-alfa)

tukey
#Resultados arrojan que la comparacion de los pares es muy diferente en todos los casos
