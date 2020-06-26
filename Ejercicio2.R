print("Romeo Axpuac")
library(ggplot2)
#Guardamos el path del primer archivo
archivo <- "C:\\Users\\Bayyron\\Desktop\\Junio2020\\Seminario2\\Laboratorio\\Practica2\\Archivo\\cardio_train.csv"
informacion <- read.csv(archivo)
#head(informacion)
columnasReporte2 <- c("id","height")
datos <- informacion[columnasReporte2]
# dataset:
# dataset:

# basic histogram
#PESOS <- c(6.9, 6.2, 5.4, 4.7, 4.8, 6.0, 6.3, 5.0, 4.5, 6.3,
#       5.9, 6.7, 6.1, 5.0, 6.7, 5.8, 6.1, 5.7, 5.2, 4.6)
#PESOS <- datos$height
#GRAPIFCA <- hist(PESOS,labels = T,breaks = 25,freq=T, main = "PESOS",
#     xlab = 'PESOS',
#     ylab = 'FRECUENCIA') 

h= hist(datos$height, main = "HISTOGRAMA Y FRECUENCIAS DE PESOS", col="red", xlab="PESOS",labels = T)
lines(c(0,h$mids),c(0,h$counts), type = "b", pch = 20, col = "blue", lwd = 3)