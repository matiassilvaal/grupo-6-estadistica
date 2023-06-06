library(dplyr)
library(ggplot2)
library(ggpubr)
library(modeest)

# Leer archivo
datos <- read.csv2("~/ep01/EP01 Datos Casen 2017.csv")

hombres_ingresos <- filter(select(datos, "sexo", "ytot"), sexo == "Hombre")

grafico <- gghistogram(hombres_ingresos,
                       x = "ytot",
                       bins = 200,
                       add = "mean",
                       xlim = c(0,10000000))

info <- hombres_ingresos %>% summarise(Media = mean(ytot),
                                        Mediana = median(ytot),
                                        Varianza = var(ytot),
                                        Desviacion_Estandar = sd(ytot),
                                        Moda = mfv(ytot))

grafico
# Al graficar los datos de los ingresos de hombres de la RM, podemos ver que esta se inclina
# hacia la derecha, por lo que podemos concluir que la distribucion de los datos es 
# Asimetrica negativa

info
# En base al rango, podemos decir que la Dispersion Total es Alta.
# La desviacion estandar tambien es alta dado que se aleja bastante del promedio, 
# lo que nos da una nueva confirmacion de la dispersion que poseen los datos
# A demas, gracias a que la funcion mfv() utilizada para calcular la moda
# nos entrega un solo valor, podemos concluir que esta distribucion es Unimodal.
# Por todo lo anterior, podemos decir que el ingreso de los hombres en la RM
# es muy amplio, habiendo pocos que ganan demasiado y muchos que ganan poco.

g1 <- gghistogram ( datos ,
                    x = " Rendimiento ",
                    bins = 10 ,
                    add = " mean ",
                    xlab = " Rendimiento [ Millas /galón]",
                    ylab = " Frecuencia ",
                    color = " blue ",
                    fill = " blue ")

print ( g1 )

g <- ggboxplot ( datos [[" Potencia "]] ,
                 color = " red",
                 fill = " pink ",
                 ylab = " Potencia [hp]")

g <- g + rremove ("x. ticks ")
g <- g + rremove ("x. text ")
g <- g + rremove ("x. title ")

g2 <- ezPlot (
  data = datos ,
  dv = tiempo ,
  wid = instancia ,
  between = algoritmo ,
  y_lab = " Tiempo promedio de ejecuci ón [ms]",
  x = algoritmo
  )

g <- ggqqplot ( datos ,
                x = " tiempo ",
                y = " algoritmo ",
                color = " algoritmo ")

g <- g + facet _ wrap (~ algoritmo )
g <- g + rremove ("x. ticks ") + rremove ("x. text ")
g <- g + rremove ("y. ticks ") + rremove ("y. text ")
g <- g + rremove (" axis . title ")
print ( g )

# Crear una ú nica figura con todos los grá ficos de dispersi ón.
dispersion <- ggarrange ( gt1 , gt2 , gt3 , gt4 , gt5 , ncol = 3 , nrow = 2)
texto <- " Poblaci ón transformada por año"
titulo <- text _ grob ( texto , face = " bold ", size = 14)
dispersion <- annotate _ figure ( dispersion , top = titulo )
print ( dispersion )

