#install.packages("readr") <- se importa desde la consola
#file.choose() <- sirve para obtener las rutas
print("Romeo Axpuac")
library(ggplot2)
#Guardamos el path del primer archivo
archivo <- "C:\\Users\\Bayyron\\Desktop\\Junio2020\\Seminario2\\Laboratorio\\Practica2\\Archivo\\sales.csv"

informacion <- read.csv(archivo)
#head(informacion)
columnasReporte1 <- c("Region","Country","Item.Type","Total.Profit")
datos1 <- informacion[columnasReporte1]
#datos1
datosCentroAmerica  <- datos1[datos1$Region =="Central America and the Caribbean",]
datosCentroAmerica  <- datosCentroAmerica[datosCentroAmerica$Item.Type =="Clothes",]
columnasReporte1 <- c("Country","Total.Profit")
datosCentroAmerica <- datosCentroAmerica[columnasReporte1]
datosCentroAmerica <- aggregate(datosCentroAmerica$Total.Profit, by=list(Paises=datosCentroAmerica$Country), FUN=sum)
datosCentroAmerica <- datosCentroAmerica[ datosCentroAmerica$Paises == "Guatemala" | datosCentroAmerica$Paises == "Honduras" | datosCentroAmerica$Paises == "Panama" | datosCentroAmerica$Paises == "Nicaragua" | datosCentroAmerica$Paises == "Costa Rica"| datosCentroAmerica$Paises == "El Salvador",]
datosCentroAmerica
Ejex <- datosCentroAmerica["Paises"]
Ejey <- datosCentroAmerica["x"] 
pie(datosCentroAmerica$x , labels = datosCentroAmerica$x,clockwise = TRUE, main =  "PAISES Y TOTAL DE VENTAS EN ROPAS",col = datosCentroAmerica$x)
legend("bottomleft",datosCentroAmerica$Paises,cex = 0.5, fill = datosCentroAmerica$x)

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.2 DEL PROBLEMA 1
##*****************************************
##*****************************************


informacion <- read.csv(archivo)
columnasReporte1 <- c("Sales.Channel")
datos1 <- informacion[columnasReporte1]
datos1 <- cbind(datos1,numero = c(1))
datos1 <- aggregate(datos1$numero, by=list(Sales.Channel=datos1$Sales.Channel), FUN=sum)
#head(datos1,n = 10L)
# Data
data <- data.frame(
  name = datos1$Sales.Channel,
  average = datos1$x,
  number = datos1$x
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                  main="Total Ventas Online y Offline" )

# Add abline

# Add the text 
text(my_bar, data$average+0.4 , paste(" ", data$number, sep="") ,cex=1)


##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.3 DEL PROBLEMA 1
##*****************************************
##*****************************************

informacion <- read.csv(archivo)
columnasReporte1 <- c("Order.Priority","Order.Date")
datos1 <- informacion[columnasReporte1]
datos1 <- cbind(datos1,anio= format(as.Date(datos1$Order.Date, "%m/%d/%Y"),"%Y"),numero = 1)
datos1 <- datos1[datos1$Order.Priority == 'M',]
datos1 <- datos1[c("anio","numero")]
datos1 <- aggregate(datos1$numero, by=list(Anios=datos1$anio), FUN=sum)
datos1 <- datos1[with(datos1,order(-datos1$x)),]
#datos1[1:10,] 
#datos1

# Data
data <- data.frame(
  name = datos1$Anios,
  average = datos1$x,
  number = datos1$x
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                  main="ordenes de prioridad M por año" )

# Add abline

# Add the text 
text(my_bar, data$average+0.4 , paste(" ", data$number, sep="") ,cex=1)

