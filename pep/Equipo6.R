library(dplyr)
library(ggplot2)
library(ggpubr)
library(modeest)
library(BSDA)
library(readxl)
library(tidyverse)
library(DescTools)
library(RVAideMemoire)

datos <- read_xlsx("EI-2023-1-PE1-Datos.xlsx")

filtrado <- filter(datos, `Tipo Vehiculo` == "CAMIONETA" & Marca == "CHEVROLET")

set.seed(268334)

muestra <- filtrado[sample(nrow(filtrado), 120),]

alpha = 0.01

periodo1 <- filter(muestra, `Ano Vehiculo` <= 1999 & `Ano Vehiculo` >= 1982)
periodo2 <- filter(muestra, `Ano Vehiculo` <= 2011 & `Ano Vehiculo` >= 2000)
periodo3 <- filter(muestra, `Ano Vehiculo` <= 2023 & `Ano Vehiculo` >= 2012)

periodo1_online <- filter(periodo1, `Tipo de Pago` == "Internet")
periodo1_presencial <- filter(periodo1, `Tipo de Pago` == "Presencial")
periodo2_online <- filter(periodo2, `Tipo de Pago` == "Internet")
periodo2_presencial <- filter(periodo2, `Tipo de Pago` == "Presencial")
periodo3_online <- filter(periodo3, `Tipo de Pago` == "Internet")
periodo3_presencial <- filter(periodo3, `Tipo de Pago` == "Presencial")

online <- c(nrow(periodo1_online), nrow(periodo2_online), nrow(periodo3_online))
presencial <- c(nrow(periodo1_presencial), nrow(periodo2_presencial), nrow(periodo3_presencial))

tabla <- as.table(rbind(online, presencial))
dimnames(tabla) <- list(Pago = c("Online", "Presencial"),
                        Periodo = c("1982-1999", "2000-2011", "2012-2023"))

prueba <- chisq.test(tabla, correct = FALSE)
prueba
