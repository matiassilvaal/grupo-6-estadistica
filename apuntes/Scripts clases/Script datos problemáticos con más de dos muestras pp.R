
library(dplyr)
library(ggpubr)
library(tidyr)

# En el trabajo de título de una estudiante del DIINF
# se reportan tiempos de ejecución (en milisegundos) y
# la cercanía con la solución óptima (en por ciento)
# de la mejor solución encontrada con tres versiones
# de un algoritmo genético para resolver instancias
# del problema del vendedor viajero disponibles en
# repositorios públicos. Ahora debe enfrentar el
# análisis de estos datos, por que está solicitando
# ayuda de los estudiantes de Estadística Inferencial.


# Obtenemos los datos en formato ancho
dir <- "~/Downloads"
src.basename <- "Datos.csv"
src.file <- file.path(dir, src.basename)
datos <- read.csv(src.file)


cat("\n\n")
cat("Pregunta 1\n")
cat("==========\n")
cat("\n\n")
# Observando los datos, la memorista sospecha que hay
# diferencias significativas en el tiempo de ejecución
# entre las versiones A y C del algoritmo cuando las
# instancias tienen 100 o más nodos. ¿Los datos respaldan
# la intuición de la memorista?
# Para responder, filtren los datos para tener las
# instancias con 100 o más nodos y seleccionen las
# columnas de los tiempos de ejecución de las versiones
# A y C en formato ancho. Usando como semilla el valor 213,
# obtengan muestras aleatorias independientes de 20 tiempos
# registrados por la versión A y 18 tiempos registrados
# por la versión C del algoritmo. Lleven los datos a
# formato largo y utilicen la escalera de potencias de
# Tukey para analizar las muestras obtenidas.

# Primero, filtramos para quedarnos con las instancias que
# nos interesan y quitar las columnas que no necesitamos.
dw <- datos %>%
  filter(n.nodos >= 100) %>%
  select(instancia, tiempo.A, tiempo.C)

# Ahora tomamos la muestra solicitada.
# Es importante que lo hagamos con una sola llamada
# a la función sample(), para evitar que los algoritmos
# compartan alguna instancia, asegurando así muestras
# independientes.
set.seed(213)
i <- sample(1:nrow(dw), 20 + 18)
seleccion <- dw[i, ]
muestra1 <- seleccion[["tiempo.A"]][1:20]
muestra2 <- seleccion[["tiempo.C"]][21:38]

# Creamos una versión larga de los datos
dl <- data.frame(
  instancia = seleccion[["instancia"]],
  algoritmo = c(rep("A", 20), rep("C", 18)),
  tiempo = c(muestra1, muestra2)
)
dl[["instancia"]] <- factor(dl[["instancia"]])
dl[["algoritmo"]] <- factor(dl[["algoritmo"]])

# Revisemos un histograma de los datos
p1 <- gghistogram(
  dl,
  x = "tiempo",
  xlab = "algoritmo",
  color = "algoritmo", fill = "algoritmo",
  bins = 5
)
p1 <- p1 + facet_grid(~ algoritmo)
print(p1)

# Podemos ver que las muestras no parecen tomadas desde una
# distribución normal, lo que podemos confirmar con pruebas
# auxiliares de normalidad.
cat("Pruebas de normalidad\n")
cat("---------------------\n")
print(shapiro.test(muestra1))
print(shapiro.test(muestra2))

# Corresponde entonces usar una prueba no paramétrica para
# analizar estos datos. En este caso, una prueba de Wilcoxon-
# Mann-Whitney con las siguientes hipótesis:
# H0 : no hay diferencia en los tiempos de ejecución requeridos por
#      ambos algoritmos (se distribuyen de igual forma).
# HA : sí hay diferencias en los tiempos de ejecución requeridos por
#      ambos algoritmos (distribuciones distintas).

# Verifiquemos que se cumplen las condiciones para aplicar esta
# prueba no paramétrica con validez:
# 1. Las observaciones de ambas muestras son independientes.
#    De como hicimos el muestreo más arriba, podemos asegurar que las
#    muestras fueron escogidas de forma aleatoria y no comparten alguna
#    instancia.
# 2. La escala de medición empleada debe ser a lo menos ordinal.
#    Como la variable en estudio es de tiempo, que corresponde a una
#    medición física, la escala de la medición cumple con condiciones
#    más exigente que solo la ordinal, y tiene sentido hablar de
#    "más/igual/menos tiempo".

# Como se cumplen bien las condiciones, usemos el típico nivel de
# significación alfa=.05.-

# Procedamos entonces a realizar la prueba
prueba1 <- wilcox.test(muestra1, muestra2, paired = FALSE)
cat("Prueba de Wilcoxon-Mann-Whitney\n")
cat("-------------------------------\n")
print(prueba1)

# Podemos concluir, entonces, que existe fuerte evidencia
# en contra de la hipótesis nula, por lo que la rechazamos
# en favor de la alternativa. Esto es, los algorimos no
# tardan tiempos similares en resolver instancias del
# problema del vendedor viajero. Mirando los histogramas
# de los datos, podemos sugerir que el algoritmo C requiere,
# en promedio, significativamente menos tiempo de procesamiento.



cat("\n\n")
cat("Pregunta 2\n")
cat("==========\n")
cat("\n\n")

# La memorista también sospecha que, al comparar las mismas
# instancias con 70 a 85 nodos, las mejores soluciones encon-
# tradas por las versiones B y C tienen rendimientos distintos.
# ¿Estará en lo cierto?
# Para responder, filtren los datos para tener las instancias
# que tengan de 70 a 85 nodos y seleccionen las columnas con 
# el mejor rendimiento de las versiones B y C en formato ancho.
# Usando como semilla el valor 117, obtengan una muestra aleatoria
# de 24 instancias. Lleven los datos a formato largo y utilicen
# una prueba no paramétrica apropiada para analizar las
# muestras obtenidas.

# Obtenemos la muestra de datos en formato ancho.
# Como tenemos que comparar los resultados obtenidos por los
# algoritmos con *las mismas instancias*, debemos obtener
# una muestra apareada de 24 observaciones.
set.seed(117)
dw2 <- datos %>%
  filter(n.nodos >= 70 & n.nodos <= 85) %>%
  select(instancia, mejor.B, mejor.C) %>%
  sample_n(24)

# Llevamos los datos a formato largo
dl2 <- dw2 %>%
  pivot_longer(
    cols = c("mejor.B", "mejor.C"),
    names_to = "algoritmo",
    values_to = "resultado"
  )
dl2[["instancia"]] <- factor(dl2[["instancia"]])
dl2[["algoritmo"]] <- factor(dl2[["algoritmo"]])

# Revisemos los datos con un diagrama de cajas
p2 <- ggboxplot(
  dl2,
  x = "algoritmo", y = "resultado",
  xlab = "algoritmo",
  color = "algoritmo"
)
print(p2)

# Vemos que los datos para el algoritmo B presentan una leve asimetría
# y la presencia de valores atípicos. Sería prudente utilizar una prueba
# no paramétrica para el análisis, que en este caso correspondería a una
# prueba de rangos con signo de Wilcoxon, con las siguientes hipótesis:
# H0 : no hay diferencia en las mejores soluciones encontradas por las
#      versiones B y C del (se distribuyen de igual forma).
# HA : sí hay diferencias en las mejores soluciones obtenidas por ambos
#      algoritmos (distribuciones distintas).

# Verifiquemos las condiciones:
# 1. Los pares de observaciones son independientes.
#    Efectivamente, si el experimento fue realizado correc-
#    tamente por la memorista, cómo se desempeña un algorit-
#    mo no debería tener influencia en cómo rinde el segundo.
# 2. La escala de medición empleada para ambas muestras debe
#    ser a lo menos ordinal.
#    Valores porcentuales cumplen esta condición, pues podemos
#    compararlos y ordenarlos.
# 3. La escala de medición empleada para las observaciones
#    es intrínsecamente continua.
#    Al tratarse de valores porcentuales, en *teoría* estas
#    mediciones podrían tener un número indeterminado de
#    decimales, por lo que cumpliría con esta condición.

# Procedamos con la prueba no paramétrica, ya que se cumplen
# todas las condiciones.
prueba2 <- wilcox.test(
  x = dw2[["mejor.B"]],
  y = dw2[["mejor.C"]],
  paired = TRUE
)
cat("Prueba de rangos con signo de Wilcoxon\n")
cat("--------------------------------------\n")
print(prueba2)

# El resultado de la prueba indica que no hay suficiente
# evidencia (p = 0,643) para descartar la hipótesis nula
# en favor de la alternativa. Así, pareciera que el algoritmo
# B consigue resultados similares a los que se obtienen con
# el algoritmo C.



cat("\n\n")
cat("Pregunta 3\n")
cat("==========\n")
cat("\n\n")

# La memorista además cree que hay diferencias significativas en
# el tiempo de ejecución entre las versiones del algoritmo
# cuando las instancias de prueba tienen 100 o más nodos.
# ¿Los datos respaldan la intuición de la memorista?
# Para responder, filtren los datos para tener las instancias
# con 100 o más nodos y seleccionen las columnas con los
# tiempos de ejecución registrados (en formato ancho).
# Usando como semilla el valor 33, obtengan muestras
# aleatorias independientes de 12, 13 y 14 tiempos registrados
# por las versiones A, B y C, respectivamente.
# Lleven los datos a formato largo y utilicen una prueba no
# paramétrica para analizar las muestras obtenidas.

# Primero, filtramos para quedarnos con las instancias que
# nos interesan y quitar las columnas que no necesitamos.
dw3 <- datos %>%
  filter(n.nodos >= 100) %>%
  select(instancia, tiempo.A, tiempo.B, tiempo.C)

# Ahora tomamos la muestra solicitada.
nA <- 12; nB <- 13; nC <- 14
nI <- nA + nB + nC

# Es importante que lo hagamos con una sola llamada
# a la función sample(), para evitar que los algoritmos
# compartan alguna instancia, asegurando así muestras
# independientes.
set.seed(33)
i <- sample(1:nrow(dw3), nI)
seleccion <- dw3[i, ]
muestra1 <- seleccion[["tiempo.A"]][1:nA]
muestra2 <- seleccion[["tiempo.B"]][(nA+1):(nA+nB)]
muestra3 <- seleccion[["tiempo.C"]][(nA+nB+1):nI]

# Creamos una versión larga de los datos
dl3 <- data.frame(
  instancia = seleccion[["instancia"]],
  algoritmo = c(rep("A", nA), rep("B", nB), rep("C", nC)),
  tiempo = c(muestra1, muestra2, muestra3)
)
dl3[["instancia"]] <- factor(dl3[["instancia"]])
dl3[["algoritmo"]] <- factor(dl3[["algoritmo"]])

# Puesto que cada muestra contiene instancias de prueba
# distintas, la primera alternativa sería usar ANOVA de una
# vía para muestras independientes para este análisis.
# Esta prueba permitiría determinar si la memorista tiene
# o no razón en pensar que existen diferencias significativas
# en los tiempos medios de ejecución de los algoritmos.

# Verificamos las condiciones:
# Existe independencia entre las muestras, pues no hay
# elementos en común y el tiempo que tarda un algoritmo en
# alguna de las instancias escogida no debería influir en el
# tiempo que tarda otro algoritmo en otra instancia.
# También se cumple que la variable dependiente tiene una
# escala de intervalos iguales, pues es una medición física
# (tiempo).
# Veamos si se cumple con las condiciones de normalidad y
# homocedasticidad por medio de histogramas.

p3 <- gghistogram(
  dl3,
  x = "tiempo",
  xlab = "algoritmo",
  color = "algoritmo", fill = "algoritmo",
  bins = 10
)
p3 <- p3 + facet_grid(~ algoritmo)
print(p3)

# Podemos ver que las muestras no siguen un comportamiento
# aproximadamente normal, por lo que no podríamos suponer
# razonablemente que las poblaciones de donde provienen sí
# tengan dicha distribución.

# Como el problema no parece requerir un valor de las medias
# estudiadas, sería conveniente bajar las exigencias y optar
# por una prueba no paramétrica, como se nos indica en el
# enunciado. En este caso, correspondería una prueba de
# Kruskal-Wallis.

# Mirando el gráfico, no podríamos suponer tampoco que las
# formas de las distribuciones subyacentes sean iguales, por
# lo que las hipótesis no podrían referirse a medianas. Así,
# las hipótesis serían:
# H0: los tiempos promedios que tardan los algoritmos en
#     resolver las instancias de interés son similares.
# HA: al menos uno de los algoritmos exhibe tiempos promedios
#     significativamente distintos a uno de los otros dos
#     algoritmos para resolver las instancias de interés.

# Verifiquemos las condiciones:
# Ya sabemos que existe independencia ente las observaciones.
# También se verifica que la variable independiente tiene más
# de dos niveles (algoritmos A, B y C). Por último, la escala de la variable
# dependiente debe ser al menos ordinal, y sabemos que las
# mediciones físicas cumplen de sobra con tal condición.

# Aplicamos la prueba, con el nivel de significación más
# común.
alfa <- 0.05
prueba3 <- kruskal.test(tiempo ~ algoritmo, data = dl3)
cat("Prueba de Kruskal-Wallis\n")
cat("------------------------\n")
print(prueba3)

# Puesto que el valor p obtenido es mucho menor que el nivel
# de significación, se rechaza H0 en favor de HA.
# En consecuencia, podemos concluir con 95% de confianza que
# los algoritmos difieren significativamente en el tiempo
# que tardan en resolver las instancias del problema del
# vendedor viajero de interés.

# Como la prueba ómnibus de Kruskal-Wallis detecta diferencias,
# debemos hacer ahora un procedimiento post-hoc.
# Consideraremos el ajuste de Benjamini & Hochberg (1995),
# por tener mayor poder estadístico que varios otros métodos.
posthoc1 <- pairwise.wilcox.test(
  dl3[["tiempo"]],
  dl3[["algoritmo"]],
  p.adjust.method = "BH",
  paired = FALSE
)
cat("Pruebas post.hoc\n")
cat("----------------\n")
print(posthoc1)

# El procedimiento post-hoc no encuentra diferencias
# significativas entre los algoritmos A y B, pero estos dos
# algoritmos parecen tardar, en promedio, significativamente
# más que el algoritmo C en resolver las instancias del
# problema del vendedor viajero que nos interesan.



cat("\n\n")
cat("Pregunta 4\n")
cat("==========\n")
cat("\n\n")

# La memorista también instuye que, al comparar las mismas
# instancias de prueba con iguales características, las 
# mejores soluciones encontradas por las diferentes versiones
# del algoritmo tienen rendimientos distintos.
# ¿Estará en lo cierto?
# Para responder, filtren los datos para tener las instancias
# con 100 o más nodos y seleccionen las columnas con los
# mejores rendimientos registrados.
# Usando como semilla el valor 33, obtengan una muestra
# aleatoria de 26 instancias. Lleven los datos a formato
# largo y utilicen una prueba no paramétrica apropiada para
# analizar los datos obtenidos.

# Obtenemos la muestra de datos en formato ancho.
# Como tenemos que comparar los resultados obtenidos por los
# algoritmos con *las mismas instancias*, debemos obtener
# una muestra apareada de 26 observaciones.

nD <- 26
set.seed(33)
dw4 <- datos %>%
  filter(n.nodos >= 100) %>%
  select(instancia, mejor.A, mejor.B, mejor.C) %>%
  sample_n(nD)

# Llevamos los datos a formato largo
dl4 <- dw4 %>%
  pivot_longer(
    cols = c("mejor.A", "mejor.B", "mejor.C"),
    names_to = "algoritmo",
    values_to = "resultado"
  )
dl4[["instancia"]] <- factor(dl4[["instancia"]])
dl4[["algoritmo"]] <- factor(dl4[["algoritmo"]])

# Puesto que cada muestra contiene la calidad de la solución
# obtenida por cada algoritmo al resolver las mismas
# instancias de prueba, correspondería usar ANOVA de una vía
# para medidas repetidas, que permitiría determinar si la
# memorista tiene o no razón en pensar que existen diferencias
# significativas entre los diferentes algoritmos.

# Revisemos los datos con un diagrama de cajas
p4 <- ggboxplot(
  dl4,
  x = "algoritmo", y = "resultado",
  xlab = "algoritmo",
  color = "algoritmo"
)
print(p4)

# Vemos que existe algunos valores atípicos en los datos,
# además de ciertas asimetrías.
# Luego, preferimos usar una prueba no paramétrica, como nos
# instruye el enunciado. Al tratarse de medidas repetidas y
# mas de dos grupos, corresponde usar la prueba de Friedman.
# Verificamos las condiciones:
# 1. La variable independiente debe ser categórica y tener a
#    lo menos tres niveles.
#    Esto es cumplido por los datos, son tres algoritmos.
# 2. La escala de la variable dependiente debe ser a lo menos
#    ordinal.
#    Los valores porcentuales cumplen con esta condición pues
#    se pueden comparar y ordenar.
# 3. Los sujetos son una muestra aleatoria e independiente de
#    la población.
#    Por enunciado sabemos que son instancias de repositorios
#    representativas de todas (¿?) las instancias de interés
#    para ejecutar los algoritmos.

# En consecuencia, se cumplen las condiciones para aplicar la
# prueba de Friedman, con las siguientes hipótesis:
# H0: la calidad de las soluciones conseguidas por los tres
#     algoritmos se distribuyen de forma similar.
# HA: al menos uno de los algoritmos entrega soluciones
#     significativamente distinta a al menos uno de los otros
#     dos.

# Aplicamos la prueba con un nivel de significación exigente por
# la presencia de valores atípicos, alfa = ,01.-
prueba4 <- friedman.test(
  resultado ~ algoritmo | instancia,
  data = dl4
)
cat("Prueba de Friedman\n")
cat("------------------\n")
print(prueba4)

# Puesto que el valor p obtenido (p < ,001) es mucho menor
# que el nivel de significación, se rechazar la hipótesis
# nula en favor de la alternativa.
# En consecuencia, podemos concluir con 99% de confianza que
# al menos uno de los algoritmo tiene un rendimiento distinto
# a alguno de los otros o a ambos.

# Puesto que la prueba ómnibus (de Friedman) detecta
# diferencias, debemos hacer ahora un procedimiento post-hoc.
# Consideraremos el ajuste de Benjamini & Hochberg (1995),
# por tener mayor poder estadístico que varios otros métodos.
posthoc2 <- pairwise.wilcox.test(
  dl4[["resultado"]],
  dl4[["algoritmo"]],
  p.adjust.method = "BH",
  paired = TRUE
)
cat("Pruebas post.hoc\n")
cat("----------------\n")
print(posthoc2)

# El procedimiento post-hoc no encuentra diferencias
# significativas entre los algoritmos B y C, pero estos dos
# algoritmos parecen obtener, en promedio, significativamente
# peores soluciones (más lejanas a la solución óptima), que
# las que consigue el algoritmo A.

