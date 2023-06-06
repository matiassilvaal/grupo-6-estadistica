library(dplyr)
library(BSDA)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(pwr)

n <- 100
sd <- 1
media <- 10

SE <- sd / sqrt(n)



  #Pregunta 1
#Si el ingeniero esta seguro de que el verdadero volumen medio no puede ser inferior
#a 10 litros y piensa rechazar la hipotesis nula cuando la muestra presente una media mayor 
#a 10,3 litros, �cual es la probabilidad de que cometa un error de tipo I?

#preparacion de datos para graficar
x <- seq(9.5, 10.5, 0.01)
y <- dnorm(x, 
           mean = media, 
           sd = SE)
g <- ggplot(data = data.frame(x,y), 
            aes(x))


#Graficamos la funcion de distribucion de la poblacion
g <- g + stat_function(fun = dnorm, 
                       args = list(mean = media, sd = SE), 
                       colour = "red", 
                       size = 1)

#calculamos y graficamos la distribucion del punto critico 10.3, donde se comienza a
# rechazar la hipotesis nula
galpha <- dnorm(x, 
                mean = 10.3, 
                sd = SE)

g <- g + stat_function(fun = dnorm, 
                       args = list(mean = 10.3, sd = SE), 
                       colour = "blue", 
                       size = 1)


#Coloreamos las zonas de las dos graficas que nos interesan
g <- g + geom_area(data = subset(data.frame(x,galpha), x >= 10.3),
                   mapping = aes(y = galpha), 
                   colour = "blue", 
                   fill = "blue", 
                   alpha = 0.5)
g <- g + geom_area(data = subset(data.frame(x,y), x > 10.3), 
                   mapping = aes(y = y), 
                   colour = "red", 
                   fill = "red", 
                   alpha = 0.5)

# calculamos numericamente el valor de alpha (error tipo 1) que existe con los datos,
#correspondiente al area roja que se encuentra dentro del area azul.
alpha <- pnorm(10.3 , mean = media, sd = SE, lower.tail = FALSE)
print(g)
alpha

# Entonces, el error de tipo 1 tiene una probabilidad de 0.1349898...%

  #Pregunta 2
#Si el verdadero volumen medio de los bidones fuera de 10,2 litros, ¿cual seria la 
#probabilidad de que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?

#Calculo directo del valor de beta
#se utiliza un valor de delta 0.2 obtenido del calculo de la d de Cohen.
power <- power.t.test(n = n, delta = 0.2, sig.level = alpha, power = NULL, type = "one.sample")$power
power

#entonces, el valor de beta (error tipo 2) corresponde a [1 - el poder de la estadistica]
beta <- 1-power
beta

# Preparacion de informacion para graficado
x <- seq(9.5, 10.5, 0.01)
y <- dnorm(x, 
           mean = media, 
           sd = SE)
g <- ggplot(data = data.frame(x,y), 
            aes(x))

# Graficado de la distribucion de la poblacion
g <- g + stat_function(fun = dnorm, 
                       args = list(mean = media, sd = SE), 
                       colour = "red", 
                       size = 1)

# calculo y graficado de datos sobre la funcion con media en el area de rechazo 
# de la hipotesis nula
galpha <- dnorm(x, 
                mean = 10.3, 
                sd = SE)
g <- g + stat_function(fun = dnorm, 
                       args = list(mean = 10.3, sd = SE), 
                       colour = "blue", 
                       size = 1)

#Coloreado de las areas criticas
g <- g + geom_area(data = subset(data.frame(x,galpha), x <= 10.18),
                   mapping = aes(y = galpha), 
                   colour = "blue", 
                   fill = "blue", 
                   alpha = 0.5)

g <- g + geom_area(data = subset(data.frame(x,galpha), (x <= 10.22 & x >= 10.18)),
                   mapping = aes(y = galpha),
                   colour = "green",
                   fill = "green",
                   alpha = 0.5)

g <- g + geom_area(data = subset(data.frame(x,y), x <= 10.2), 
                   mapping = aes(y = y), 
                   colour = "red", 
                   fill = "red", 
                   alpha = 0.5)
print(g)
alpha + beta
#Asi como se puede ver en el grafico de las funciones, el error tipo 2 (valor de beta) 
#corresponde al valor inferior al area azul, dejando el area de color verde como
#una zona donde no existe posibilidad de alguno de estos errores, lo que explica
#la razon de porque alpha + beta != 1

  #Pregunta 3
#Como no se conoce el verdadero volumen medio, genere un grafico del poder estadistico 
#con las condiciones anteriores, pero suponiendo que el verdadero volumen medio podria 
#variar de 10 a 10,5 litros.

# Se toma un efecto entre -0.1 y 0.1 para obtener distintos poderes
efecto <- seq(-0.1, 0.1, 0.001)

# Se calculan los niveles de significancia para cada posible media
alpha_1 <- pnorm(10, mean = media, sd = SE, lower.tail = FALSE)
alpha_2 <- pnorm(10.1, mean = media, sd = SE, lower.tail = FALSE)
alpha_3 <- pnorm(10.2, mean = media, sd = SE, lower.tail = FALSE)
alpha_4 <- pnorm(10.3, mean = media, sd = SE, lower.tail = FALSE)
alpha_5 <- pnorm(10.4, mean = media, sd = SE, lower.tail = FALSE)
alpha_6 <- pnorm(10.5 , mean = media, sd = SE, lower.tail = FALSE)

# Se calcula el poder para cada nivel de significancia entre -0.1 y 0.1
power_1 <- power.t.test(n = n, delta = efecto, sd = SE, sig.level = alpha_1, type = "one.sample", alternative = "two.sided")$power
power_2 <- power.t.test(n = n, delta = efecto, sd = SE, sig.level = alpha_2, type = "one.sample", alternative = "two.sided")$power
power_3 <- power.t.test(n = n, delta = efecto, sd = SE, sig.level = alpha_3, type = "one.sample", alternative = "two.sided")$power
power_4 <- power.t.test(n = n, delta = efecto, sd = SE, sig.level = alpha_4, type = "one.sample", alternative = "two.sided")$power
power_5 <- power.t.test(n = n, delta = efecto, sd = SE, sig.level = alpha_5, type = "one.sample", alternative = "two.sided")$power
power_6 <- power.t.test(n = n, delta = efecto, sd = SE, sig.level = alpha_6, type = "one.sample", alternative = "two.sided")$power

# Se preparan los datos para graficar
datos <- data.frame(efecto, power_1, power_2, power_3, power_4, power_5, power_6)
datos <- datos %>% pivot_longer(!"efecto", names_to = "fuente", values_to = "poder")
niveles <- c("power_1", "power_2", "power_3", "power_4", "power_5", "power_6")

etiquetas <- c("media 10", "media 10.1", "media 10.2", "media 10.3", "media 10.4", "media 10,5")

datos[["fuente"]] <- factor(datos[["fuente"]] , levels = niveles, labels = etiquetas)

# Se crea el grafico y se le da forma
g <- ggplot(datos, aes(efecto, poder, colour = factor(fuente)))
g <- g + geom_line()
g <- g + labs (colour = "")
g <- g + ylab (" Poder estadístico ")
g <- g + xlab (" Tamaño del efecto ")

g <- g + scale_color_manual(values = c("red", "blue", "green", "orange", "yellow", "pink"))
g <- g + theme_pubr()
g <- g + ggtitle (" Curvas de poder para prueba t bilateral ")
g <- g + geom_vline ( xintercept = 0 , linetype = "dashed")

# Se grafican todos los poderes estadisticos
g
  #Pregunta 4
#Considerando un volumen medio de 10 litros, ¿cuantos bidones deberian revisarse para 
#conseguir un poder estadistico de 0,75 y un nivel de significacion de 0,05?

# para esta pregunta en especifico se debe utilizar pwr.t.test, donde, especificamente, hay que
# poner n= NULL, para que de esta forma calcule "n" ya que este es el numero de bidones
# para conseguir un poder estadistico de 0,75 y un nivel de significacion de 0,05
# el delta es 0.2, donde la d de cohen la que consideramos (10.2- 10) / 1, 
# usando 10.2 como la media muestral
# 10 es el valor nulo
# 1 es la desviacion estandar
# y en este caso solo necesitamos una muestra, por esto, "one sample"

alfa <- 0.05
power2 <- 0.75
n <- pwr.t.test(n = NULL, d = 0.2, sig.level = alfa, power = power2, type = "one.sample")$n
n
# en este caso n es la cantidad de bidones, que es 175 aproximadamente

  #Pregunta 5
#¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer 
#un error de tipo I a un 1% solamente?

# esta pregunta es relacionada a la anterior, solamente cambia el nivel de significacion, 
# en esta caso el nivel de significacion es 0.01, que es el 1%

#respecto al poder  y muestra, se conservan de la pregunta anterior
n2 <- pwr.t.test(n = NULL, d = 0.2, sig.level = 0.01, power = power2, type = "one.sample")$n
n2
# en este caso n es la cantidad de bidones, que es 267 aproximadamente
