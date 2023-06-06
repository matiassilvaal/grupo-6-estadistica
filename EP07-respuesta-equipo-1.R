library(tidyverse)
library(ggpubr)
############################################## pregunta 1 ##########################################################
setwd("C:/Users/ignac/OneDrive/Escritorio/inferencial/ejer7")  #Para probar se debe cambiar la ruta
datos_archivo <- read.csv("EP07 Datos.csv", stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1")
datos_archivo <- datos_archivo %>% filter(n.nodos >= 70)
algoritmo_A <- datos_archivo %>% select (tiempo.A)
algoritmo_B <- datos_archivo %>% select(tiempo.B)
instancia <- datos_archivo %>% select(instancia)

datos <- data.frame(instancia, algoritmo_A, algoritmo_B)

# Comprobamos si siguen una distribución normal
ggqqplot(datos$tiempo.A)
ggqqplot(datos$tiempo.B)
# No siguen una distribución normal

set.seed(73)
muestra_1 <- datos[sample(nrow(datos), 44),]
a <- muestra_1$tiempo.A[1:20]
b<- muestra_1$tiempo.B[21:44]

# Se nos dice en el enunciado que las muestras son independientes, comprobamos si las que creamos efectivamente lo son
any(a%in%b)
# Las muestras son independientes

# En base a los datos podemos comprobar que la escala es ordinal
# Tambien por enunciado sabemos que las muestras son independientes y ya comprobamos que no siguen una distribución normal
# Dado a que se pide buscar diferencias significativas decidimos usar la prueba de suma de rangos de Wilcoxon para muestras grandes
# Esto se puede hacer ya que comprobamos anteriormente las condiciones para Wilcoxon (muestras independientes, escala de medición a lo menos ordinal)


#Nivel de significancia
alfa = 0.05

# Tenemos que:
# H0: no hay diferencia significativa entre los tiempos de ejecución entre los algortimos A y B
# Ha: si hay diferencia significativa entre los tiempos de ejecución entre los algoritmos A y B

prueba <- wilcox.test(a, b, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba)
# En base a la prueba de wilcox podemos afirmar con un nivel de confianza del 95% que no hay diferencias significativas entre los tiempos de los algoritmos A y B
#Aceptando así la hipótesis nula.

######################################## pregunta 2 ##############################################

"
En este caso se desea comparar dos grupos, el algoritmo B y C, por lo que se considera t de Student ahora bien
se debe emplear un test no paramétrico, escogiendo la prueba de rangos con signo de Wilcoxon, se verifican sus condiciones:
Alternativa no paramétrica para t de Student para muestras pareadas: 
- Los pares de observaciones son independientes, lo que se cumple debido a que el resultado de una instancia
no influye al de la otra.
- Escala de medición para las observaciones intrínsicamente continua, esto se cumple ya que la escala de observación es el tiempo
que demora cada algoritmo, en milisegundos, siendo este un valor continuo.
- Escala de medición para muestras a lo menos ordinal, ya que las características establecidas para comparar los algoritmos
son jerarquizables, es decir ordinales.

H0: Los mejores tiempos para la misma instancia de los algoritmos B y C no presentan diferencias entre sus rendimientos.
Ha: Los mejores tiempos para la misma instancia de los algoritmos B y C presentan diferencia entre sus rendimientos.
#Nota: para responder esta pregunta se utiliza un nivel de confianza del 95%, es decir, alpha = 0.05
"

datos_archivo <- read.csv("EP07 Datos.csv", stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1") #Para probar se debe cambiar la ruta
datos_archivo <- datos_archivo %>% filter(n.nodos >= 70)
algoritmo_B <- datos_archivo %>% select (mejor.B)
algoritmo_C <- datos_archivo %>% select(mejor.C)
instancia <- datos_archivo %>% select(instancia)
datos_filtrado <- data.frame(instancia, algoritmo_B, algoritmo_C)

# Revision de normalidad para efectivamente poder realizar prueba parametrica
ggqqplot(datos_filtrado$mejor.B)
ggqqplot(datos_filtrado$mejor.C)
# Se pueden ver en el gráfico que las muestras no siguen una distribución normal debido a que hay datos en las 2 muestras que
# estan por fuera de la región aceptada para considerse como distribución normal.

set.seed(71)
muestreo <- datos_filtrado[sample(nrow(datos_filtrado), 24),]

test <- wilcox.test(muestreo$mejor.B, muestreo$mejor.C, alternative = "two.sided", conf.level = 0.05, paired = TRUE)
print(test)

"AL realizar el test de prueba de rangos con signos de wilcoxon se obtiene un p-value = 0.003 aproximadamente, inferior al nivel 
de significancia utilizado para esta prueba, por lo que con un 95% de confianza podemos decir que si existe una diferencia
entre el rendimiento de los mejores tiempos para la misma instancia entre el algoritmo B y C, por lo que con un 95% se falla al
aceptar la hipótesis nula."


######################################## pregunta 3 ###############################################
"
Debido a que las n muestras independientes a estudiar son mayores a 2 y de tamaño diferente se emplea
la prueba de Kruskal-Wallis en la que se deben verificar las siguientes condiciones:

- Variables independientes de a lo menos 2 niveles, en este caso se tienen 3 niveles de muestras independientes,
  correspondientes a los 3 algoritmos a analizar.
  
- Variable dependiente a lo menos ordinal, lo que se cumple debido a que los tiempos de respuesta de cada algoritmos son 
  categorarizables en tiempo siendo así ordinal.
  
- Observaciones independientes entre si, lo que se cumple ya que entre algoritmos no existe una relación.


H0: Todos los algoritmo tienen un tiempo de ejecución iguales.
Ha: Al menos uno de los algoritmos presenta un tiempo de ejecución diferente a los demás.

Nota: Para poder contrastar hipótesis se ocupara un nivel de confianza del 95%, es decir, el nivel
      de significancia es del 0.05.

"

#Lectura del archivo
datos_archivo <- read.csv("EP07 Datos.csv", stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1") #Para probar se debe cambiar la ruta

#Se filtran los datos
datos_archivo <- datos_archivo %>% filter(n.nodos >= 50)
algoritmo_A <- datos_archivo %>% select (tiempo.A)
algoritmo_B <- datos_archivo %>% select(tiempo.B)
algoritmo_C <- datos_archivo %>% select(tiempo.C)
instancia <- datos_archivo %>% select(instancia)

datos <- data.frame(instancia, algoritmo_A, algoritmo_B, algoritmo_C)

#Verificacion de normalidad para aplicar prueba no parametrica
ggqqplot(datos$tiempo.A)
ggqqplot(datos$tiempo.B)
ggqqplot(datos$tiempo.C)

#Se puede ver en cada grafico que ningua muestra sigue, ni aproximadamente una distribución normal


#Muestreo
set.seed(31)
muestreo <- datos[sample(nrow(datos), 39),]

a <- muestreo$tiempo.A[1:14]
b <- muestreo$tiempo.B[15:26]
c <- muestreo$tiempo.C[27:39]


#Dataframe para la prueba
tiempos <- c(a, b, c)

algoritmo <- c(rep("A", length(a)), rep("B", length(b)), rep("C", length(c)))

algoritmo <- factor(algoritmo)

datos_test <- data.frame(tiempos, algoritmo)

#Test
prueba <- kruskal.test(tiempos ~ algoritmo, data = datos_test)
print(prueba)

"Se puede notar que el test de Kruskall calcula un p-value = 0.034, ligeramente menor a nuestro nivel de significancia, por lo que 
se falla al aceptar la hipótesis nula, concluyendo que al menos un algoritmo presenta un tiempo de ejecución diferente a los demás
algoritmos. Ahora es posible saber entre cuales de los algoritmos se encuentra esta diferencia para eso se ocupa el procedimiento
post-hoc con el método de Holm:"


#post-hoc
post_hoc<- pairwise.wilcox.test(datos_test$tiempos, datos_test$algoritmo, p.adjust.method = "holm", paired = FALSE)
print(post_hoc)
"Se puede notar que la diferencia se encuentra principalmente sobre el algoritmo A en respecto a los otros dos algoritmos, es decir que
el algoritmo 'A' presenta un tiempo de ejecución significativamente diferente con respectos a los otros dos algoritmos."

######################################## pregunta 4 ###############################################

"
H0: Los mejores tiempos de cada algoritmo para la misma intancia tienen rendimientos similares.
HA: Al menos uno de los mejores tiempo de un algoritmo para la misma intancia tiene un rendimiento diferente a los demás.

#Nota: para responder esta pregunta se utiliza un nivel de confianza del 95%, es decir, alpha = 0.05
"
#Lectura de archivo
datos_archivo <- read.csv("EP07 Datos.csv", stringsAsFactors = TRUE, fileEncoding = "ISO-8859-1") #Para probar se debe cambiar la ruta

#Se filtran los datos
datos_archivo <- datos_archivo %>% filter(n.nodos >= 50)
mejor_b <- datos_archivo %>% select (mejor.B)
mejor_c <- datos_archivo %>% select(mejor.C)
mejor_a <- datos_archivo %>% select(mejor.A)
instancia <- datos_archivo %>% select(instancia)

datos_mejor <- data.frame(instancia, mejor_a, mejor_b, mejor_c)

normalidad_a <- ggqqplot(datos_mejor$mejor.A)
normalidad_b <- ggqqplot(datos_mejor$mejor.B)
normalidad_c <- ggqqplot(datos_mejor$mejor.C)
print(normalidad_a)
print(normalidad_b)
print(normalidad_c)

"Se puede ver que ninguna muestra sigue una distribución normal ya en los graficos se puede notar como los datos
de cada muestra no están dentro de la región aceptada para considerarse una sitribución normal. Al leer el enunciado
pensamos en realizar el test de friedman debido a que estamos comparando más de 2 medias y las muestras están correlacionadas debido a que
se nos dicen que se prueban los algoritmo con las misma instancia con iguales características. Veamos las condiciones de esta prueba:

(1): La variable independiente debe ser categórica y debe tener al menos de 3 niveles.
      Sol: Para este caso la variable independiente corresponde al tipo de algoritmo(A, B o C), entoces de por si cumple que sea categórica
      ya que representa una categoría del algortimo. Por otro lado se cumplen los 3 niveles los cuales son A, B o C.

(2): La escala de la variable dependiente debe ser, a lo menos, ordinal.
      Sol: La variable dependiente en este caso corresponde al timepo que presenta cada algoritmo, donde este tiempo
      se mide en milisegundos según el enunciado, la cual vendría siendo la escala de medición,
      entonces por definición es ordinal, debido a que se puede ordenar de menor a mayor tiempo por ejemplo.

(3): Los sujetos son una muestra son aleatoria e independiente de la población.
      Sol: El muestreo que se aplica sobre los datos originales, que posteriormente se ocupan para realizar el test, hacen cumplir
      esta condición.
"


#Muestreo independiente
set.seed(31)
muestras_4 <- datos_mejor[sample(nrow(datos_mejor), 22),]


#Dataframe para la prueba
tiempos <- c(muestras_4$mejor.A, muestras_4$mejor.B, muestras_4$mejor.C)
algoritmo <- c(rep("A", 22), rep("B", 22), rep("C", 22))
ids <- rep(1:22, 3)

datos_test<- data.frame(ids, tiempos, algoritmo)

#Se realiza el test
test <- friedman.test(tiempos ~ algoritmo| ids, data = datos_test)
print(test)

"Podemos notar que el test nos da un p-value = 0.0008 aproximadamente, muy inferior a nuestro nivel de significancia por lo
que con un 95% de confianza se rechaza la hipótesis nula y por consiguiente se acepta la alternativa, extrayendo que por lo menos
uno de los mejores tiempo de los algoritmo tiene un rendimiento diferente a los demas. Ahora sabiendo esto se puede realizar un
procedimiento post-hoc para saber entre que algoritmos se encuentra esta diferencia de rendiemientos entre sus mejores tiempos"

#Se realiza procedimiento post-hoc
posthoc<- pairwise.wilcox.test(datos_test$tiempos, datos_test$algoritmo, p.adjust.method = "holm", paired = TRUE)
print(posthoc)


"Concluyendo con los resultados de este procedimiento post-hoc con el método de Holm podemos notar 2 hechos importantes:
1. existe una diferencia entre el algoritmo A y B aunque el valor que calcula el test es realmente bajo, 0.00077 por lo que puede
no existir una diferencia significativa estadísticamente hablando.
2. Se puede notar que el algoritmo C posee una diferencia significativa con respecto a los otros 2 algoritmos"