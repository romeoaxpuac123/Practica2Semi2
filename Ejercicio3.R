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
informacion <- read.csv(archivo)
columnasReporte3 <- c("country","age","suicides_no")
datos1 <- informacion[columnasReporte3]
datos1 <- datos1[datos1$country =="Guatemala",]
columnasReporte3 <- c("age","suicides_no")
datos1 <- datos1[columnasReporte3]
datos1 <- datos1[datos1$suicides_no !="NA",]
datos1 <- aggregate(datos1$suicides_no, by=list(Total=datos1$age), FUN=sum)
#NOMBRE VARIABLE
Xi <- datos1$Total
#Frecuencia Absoluta
ni <- c(unname(datos1$x))
#Frecuencia Absoluta Acumulada
Ni <- cumsum(unname(datos1$x))
#Frecuencia relativa
fi <- round(prop.table(ni),5)
#Frecuencia relativa acumulada
Fi <- round(cumsum(prop.table(ni)),5)
#TablaFinal
tablafinal <- data.frame(Xi,ni,Ni,fi,Fi)
tg <- tableGrob(tablafinal)
grid.draw(tg)


##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.2 DEL PROBLEMA 3
##*****************************************
##*****************************************

# Data
data <- data.frame(
  name = tablafinal$Xi ,
  average = tablafinal$ni,
  number = tablafinal$ni
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                  main="Frecuencias Absolutas" )

# Add abline

# Add the text 
text(my_bar, data$average+0.4 , paste(" ", data$number, sep="") ,cex=1)

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.3 DEL PROBLEMA 3
##*****************************************
##*****************************************
h= hist(tablafinal$ni, main = "FRECUENCIAS", col="red", xlab="FRECUENCIAS",labels = T)
lines(c(0,h$mids),c(0,h$counts), type = "b", pch = 20, col = "blue", lwd = 3)

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.4 DEL PROBLEMA 3
##*****************************************
##*****************************************

# Data
data <- data.frame(
  name = tablafinal$Xi ,
  average = tablafinal$Fi,
  number = tablafinal$Fi
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                  main="Frecuencias Acumuladas" )

# Add abline

# Add the text 
text(my_bar, data$average+0.4 , paste(" ", data$number, sep="") ,cex=1)