print("Romeo Axpuac")
library(ggplot2)
#Guardamos el path del primer archivo
archivo <- "C:\\Users\\Bayyron\\Desktop\\Junio2020\\Seminario2\\Laboratorio\\Practica2\\Archivo\\cardio_train.csv"
informacion <- read.csv(archivo)
#head(informacion)
columnasReporte2 <- c("id","weight")
datos <- informacion[columnasReporte2]

h= hist(datos$weight, main = "HISTOGRAMA Y FRECUENCIAS DE PESOS", col="red", xlab="PESOS",labels = T)
lines(c(0,h$mids),c(0,h$counts), type = "b", pch = 20, col = "blue", lwd = 3)