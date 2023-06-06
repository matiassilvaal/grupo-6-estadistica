library(dplyr)
library(readxl)
library(BSDA)

# Leer excel
data <- read_excel("estadistica/EP02-datos.xlsx")


  # pregunta 1

#El Comite Olimpico cree que el mejor tiempo medio de los atletas orientales antes
#de ingresar al programa de entrenamiento es inferior a 19,8 segundos. 
#¿Soportan los datos esta afirmación?

oriental <- filter(data, Raza == "Oriental")

valor_nulo <- 19.8

# Se realizara un t.test() sobre los datos previos al entrenamiento, utilizando la media
# de referencia. Se nos fue revisar normalidad (Q-Q y/o shapiro.test)
# two.sided para denotar =

test <- t.test(oriental$Previo, mu = valor_nulo, alternative = "greater")

test

# se acepta la hipotesis nula por el valor-p que nos entrega el test, por lo tanto
# la media de la muestra de atletas orientales si es menor que 19.8 s



  # pregunta 2

# ¿Sugieren los datos que la mejor marca de los 
# atletas negros se reduce en promedio mas de 1,5 segundos tras el entrenamiento?


negros <- filter(data, Raza == "Negra")
diferencia <- data.frame(negros$Previo-negros$Posterior)

# Comparacion sobre la media, entonces t test
test2 <- t.test(diferencia, mu = 1.5, alternative = "less")
test2

# se confirma la hipotesis nula por el resultado del valor-p, entonces
# la media de mejora de los tiempos si mayor a 1.5 segundos.


  #Pregunta 3

#¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales 
#por 3 segundos despues del entrenamiento?

# comparacion de dos medias no relacionadas.
test3 <- t.test(x = negros$Posterior, 
                y = oriental$Posterior, 
                paired = FALSE, 
                alternative = "less", 
                mu = 3)
test3

# se confirma la hipotesis alternativa, ya que el valor del p-value es menos que el alfa utilizado



  #Pregunta 4

#¿Sera cierto que hay mas atletas orientales que, en promedio, redujeron sus marcas 
#en 4,9 segundos que atletas blancos que lo hicieron en 3 segundos?

# Al analizar el enunciado se llega se deduce que es un problema de proporciones 

blancos <- filter(data, Raza == "Blanca")

# diferencia entre las marcas anteriores y posteriores  de la raza blanca
difb <- data.frame(blancos$Previo - blancos$Posterior)

# diferencia entre las marcas anteriores y posteriores  de la raza oriental
difo <- data.frame(oriental$Previo - oriental$Posterior)

#se filtran las distintas que tengan que cumplan el criterio
# blancos >= 3.0 
# orientales >= 4.9

difbe <- filter(difb, blancos.Previo...blancos.Posterior >= 3.0)

difoe <- filter(difo, oriental.Previo...oriental.Posterior >= 4.9)

#se calcula la cantidad de exitos dados los datos que cumplan el criterio anterior para posteriormente
#comparar las proporciones con prop.test

exitos = c(length(difoe$oriental.Previo...oriental.Posterior), length(difbe$blancos.Previo...blancos.Posterior))
n = c(27, 26)

# Prop.test -> Wilson, nos falto probar Normalidad para la proporción
test4 <- prop.test(exitos, n = n, alternative = "less")
# de esta forma se confirma que la hipotesis nula es cierta, los atletas  redujeron sus tiempos
test4
