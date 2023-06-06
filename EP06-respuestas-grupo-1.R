library(tidyverse)
library(ggpubr)
library(ez)
library(nlme)
library(emmeans)

setwd("C:/Users/ignac/OneDrive/Escritorio/inferencial/ejer5")
datos_archivo <- read.csv2("EP05_Datos.csv", stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1")

"En este momento, los investigadores buscan determinar si existen diferencias en el tiempo
que tardan los usuarios en formular consultas para problemas con diferente nivel de dificultad en el área de física."

#Se filtran los datos para obtener solo datos de física y luego se filtran por dificultad

datos_fisica <- datos_archivo %>% filter(area == 'Física')

baja <- datos_fisica %>% filter(dificultad == "Baja") %>% select (tiempo)

media <- datos_fisica %>% filter(dificultad == "Media") %>% select (tiempo)

alta <- datos_fisica %>% filter(dificultad == "Alta") %>% select (tiempo)


#Se ocupa los datos auxiliares para generar los id's
#datos_aux <- data.frame(baja, media, alta)
ids = factor(rep(1:length(baja$tiempo), length.out = length(baja$tiempo)))

#Se crea el data frame y se pasa a formato largo
datos = data.frame(ids, baja, media, alta)
colnames(datos)[colnames(datos) == "tiempo"] <- "Baja"
colnames(datos)[colnames(datos) == "tiempo.1"] <- "Media"
colnames(datos)[colnames(datos) == "tiempo.2"] <- "Alta"
datos <- datos %>% pivot_longer(c("Baja", "Media", "Alta"),names_to = "dificultad" ,values_to = "tiempo")
datos[["dificultad"]] <- factor(datos[["dificultad"]])

"
Se formulan nuestras hipótesis:

H0: El tiempo medio que tardan los usuarios en formular una consulta para problemas de distintas dificultades en la
área de fisica es el mismo(baja, alta, media)

Ha: El tiempo medio para formular una consulta en el área de física es diferente para al menos una dificultad
    (baja, media, alta)

Condiciones para usar ANOVA
(1) La escala con la que se mide  la variable dependiente  tiene las propiedades de una escala de intervalos iguales
(2) Las k muestras son obtenidas de manera aleatoria  e independiente desde la población de origen
(3) Se puede suponer razonablemente que la población de origen sigue una distribución Normal
(4) Si las muestras provienen de más de una población, estas tiene una misma varianza.


Sol : 

(1) Debido a que la variable dependiente es el tiempo medio y éste se mide en una escala de intervalos iguales(segundos)
    se cumple con esta condición.

(2) Dado que los sujetos fueron elegidos y asignado a los sub-grupos tématicos entonces se cumple está condición.
    El mismo enunciado hace cumplir esta condición.

(3) Para ver la normalidad de las muestras se obtiene lo siguiente:

"
g <- ggqqplot(datos, x = "tiempo", y = "dificultad", color = "dificultad")
g <- g + facet_wrap(~ dificultad)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)
#Como se puede ver en el gráfico cada muestra de las dificultades sigue una distribución normal.
#Aunque es cierto que los datos no forman la línea recta, estos se encuentran en la región aceptada.
#Para cada dificultad ocurre lo mismo. O podiamos usar Shapiro/Q-Q


# Para la homocedasticidad es necesario realizar el test ANOVA ya que R al ocupar la función ezANOVA
# Entrega el resultado de la prueba de Mauchly, que verifica la esfericidad. Cabe recalcar que para
# todas las pruebas e hipótesis se ocupa un nivel de confianza del 95%.

anova <- ezANOVA(data = datos, dv = tiempo, within = dificultad, wid = ids, return_aov = TRUE)

print((anova$`Mauchly's Test for Sphericity`))
# Si falla, ver que pasa con GG/HF

"El resultado de la prueba de Mauchly's da un p.value de 0.934 aproximadamente, el cual es superior al
nivel de significancia de 0.05, por lo que se puede concluir con un 95% de confianza que los datos cumplen
con la condición de esfericidad"

print(anova$ANOVA)
"Veamos que sucede con nuestras hipótesis inicales ahora que ya se realizó la prueba ANOVA:
  Al verificar la respuesta se obtiene un p-valor de 5.28e-27, muy inferior al nivel de significancion definido en 0.05,
por lo que se puede concluir con un 95% de confianza que existe por lo menos una discrepancia entre los tiempos
medio para formular una consulta para los distintos niveles de dificultad del área de Física, además con esta aseveración
se decanta por rechazar la hipótesis nula y por consiguiente aceptar la hipótesis alternativa. Ahora es necesario saber entre cuáles
dificultades están presente las diferencias para que así los expertos puedan tomar las medidas necesarias."


#Para saber entre cuales dificultades se ubican las diferencias se ocupa un procedimiento Post-hoc
# Sphericity Corrections

mixto <- lme(tiempo ~ dificultad , data = datos , random = ~1 | ids)
medias <- emmeans (mixto , "dificultad")
post.hoc <- pairs (medias , adjust = "tukey")
print(post.hoc)

"AL realizar un procedimiento post-hoc, en este caso HSD de Tukey, podemos notar en cuales de estas relaciones
se presenta la diferencia, aunque no podemos saber cúanto es cuantitativamente las diferencias. Se observa que 
entre la dificultadad Alta y Media hay un p.value menor a 0.0001, muy por debajo del 0.05 que ocupamos como
nivel de significancia por lo que se extrae como conclusión que existe una diferencia entre el tiempo medio de formulación
de consulta entre estas 2 dificultades. Para el caso con las dificultades Alta y Baja ocurre exactamente lo mismo, se tiene un p.value
menor a 0.0001 indicandonos asi que también entre estas dificultades existe una diferencia entre el tiempo medio
para formular una respuesta por parte de los participantes."

g2 <- ezPlot(data = datos, dv = tiempo, wid = ids, within = dificultad, y_lab = "Tiempo promedio de formulación", x = dificultad)
print(g2)
