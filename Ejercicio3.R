library(gridExtra)
library(grid)
print("Romeo Axpuac")
library(ggplot2)

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.1 DEL PROBLEMA 3
##*****************************************
##*****************************************


#Guardamos el path del primer archivo
archivo <- "C:\\Users\\Bayyron\\Desktop\\Junio2020\\Seminario2\\Laboratorio\\Practica2\\Archivo\\suicide.csv"
datos1 <- read.csv(archivo)
datos1 <- datos1[c("country","age","suicides_no")]
datos1 <- subset.data.frame(datos1,datos1$country=="Guatemala" &datos1$suicides_no!="NA")
datos1 <- datos1[c("age","suicides_no")]
edades <-datos1$age
edades <-replace(edades,edades=="1-4","01-04")
edades <-replace(edades,edades=="5-9","05-09")
edades <-replace(edades,edades=="Childish","01-04")
edades <-replace(edades,edades=="100 and over","99+")
valores<-datos1$suicides_no

tablafinal <- data.frame(edades,valores)
tablafinal <- aggregate(x=tablafinal$valores, by=list(edades=tablafinal$edades), FUN=sum)
tablafinal <-tablafinal[order(tablafinal$edades, decreasing = FALSE),]
tablafinal
datostabla <- tablafinal$edades
frecuencias <- tablafinal$x
frecuenciaAcumulada <- cumsum(unname(tablafinal$x))
#Frecuencia relativa
fi <- round(prop.table(frecuencias),5)
#Frecuencia relativa acumulada
Fi <- round(cumsum(prop.table(frecuencias)),5)
reportefinal <- data.frame(datostabla,frecuencias,frecuenciaAcumulada,fi,Fi)
tg <- tableGrob(reportefinal)
grid.draw(tg)


##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.2 DEL PROBLEMA 3
##*****************************************
##*****************************************

# Data
data <- data.frame(
  name = reportefinal$datostabla ,
  average = reportefinal$frecuencias,
  number = reportefinal$frecuencias
)
par(mar=c(6,4,4,4))
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) ,
                  main="Frecuencias Absolutas" )
text(my_bar, data$average+0.4 , paste(" ", data$number, sep="") ,cex=1)

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.3 DEL PROBLEMA 3
##*****************************************
##*****************************************
h= hist(reportefinal$frecuencias, main = "FRECUENCIAS", col="red", xlab="FRECUENCIAS",labels = T)
lines(c(0,h$mids),c(0,h$counts), type = "b", pch = 20, col = "blue", lwd = 3)

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.4 DEL PROBLEMA 3
##*****************************************
##*****************************************

# Data
data <- data.frame(
  name = reportefinal$datostabla ,
  average = reportefinal$frecuenciaAcumulada,
  number = reportefinal$frecuenciaAcumulada
)
# Increase bottom margin
par(mar=c(6,4,4,4))
# Basic Barplot
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  main="Frecuencias Acumuladas" )
text(my_bar, data$average+0.4 , paste("\n", data$number, sep="") ,cex=1)