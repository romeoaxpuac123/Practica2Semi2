##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.1 DEL PROBLEMA 4
##*****************************************
##*****************************************


#Guardamos el path del primer archivo
archivo <- "C:\\Users\\Bayyron\\Desktop\\Junio2020\\Seminario2\\Laboratorio\\Practica2\\Archivo\\fifa.csv"
informacion <- read.csv(archivo)
columnasReporte4 <- c("Nationality","ShotPower")
datos1 <- informacion[columnasReporte4]
datos1 <- datos1[datos1$Nationality =="Germany" & datos1$ShotPower != ""  ,]
datos1
promedio <- aggregate(datos1$ShotPower,by = list(datos1$Nationality),FUN=mean)
maximo <- aggregate(datos1$ShotPower,by = list(datos1$Nationality),FUN=max)
minimo <- aggregate(datos1$ShotPower,by = list(datos1$Nationality),FUN=min)
prom <- mean(promedio$x)
maxi <- max(maximo$x)
min <- min(minimo$x)
maxi
min
prom
x = c("Promedio","Maximo","Minimo")
y = c(prom,maxi,min)
# Data
data <- data.frame(
  name = x ,
  average = y,
  number = y
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                  main="Potencia Tiros Alemanes" )

# Add abline

# Add the text 
text(my_bar, data$average+0.4 , paste(" ", data$number, sep="") ,cex=1)


##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.2 DEL PROBLEMA 4
##*****************************************
##*****************************************
columnasReporte4 <- c("Club","Aggression")
datos1 <- informacion[columnasReporte4]
datos1 <- datos1[datos1$Club !="" & datos1$Aggression != ""  ,]
datos1 <- aggregate(datos1$Aggression,by = list(datos1$Club),FUN=mean)
maximo <- max(datos1$x)
posicionMaximo <- which.max(datos1$x)
maximoEquio <- datos1[posicionMaximo,]
maximoEquio
minimo <- min(datos1$x)
posicionMinima <- which.min(datos1$x)
MinimoEquipo <-datos1[posicionMinima,]
MinimoEquipo
x = c(maximoEquio$Group.1,MinimoEquipo$Group.1)
y = c(maximoEquio$x,MinimoEquipo$x)
# Data
data <- data.frame(
  name = x ,
  average = y,
  number = y
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=T , names.arg=data$name , 
                  las=1 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                  main="Mejor y Peor Equipo con agresividad (Promedio)." )

# Add abline

# Add the text 
text(my_bar, data$average+0.3 , paste("\n\nPROMEDIO: \n", data$number, sep="") ,cex=1)

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.3 DEL PROBLEMA 4
##*****************************************
##*****************************************
columnasReporte4 <- c("Club","Dribbling")
datos1 <- informacion[columnasReporte4]
datos1 <- datos1[datos1$Club !="" & datos1$Dribbling != ""  ,]
datos1 <- aggregate(datos1$Dribbling,by = list(datos1$Club),FUN=mean)
maximo <- max(datos1$x)
posicionMaximo <- which.max(datos1$x)
maximoEquio <- datos1[posicionMaximo,]
maximoEquio
minimo <- min(datos1$x)
posicionMinima <- which.min(datos1$x)
MinimoEquipo <-datos1[posicionMinima,]
MinimoEquipo
x = c(maximoEquio$Group.1,MinimoEquipo$Group.1)
y = c(maximoEquio$x,MinimoEquipo$x)
# Data
data <- data.frame(
  name = x ,
  average = y,
  number = y
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=T , names.arg=data$name , 
                  las=1 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  
                  main="Mejor y Peor Dribbling por Equipo (Promedio)." )

# Add abline

# Add the text 
text(my_bar, data$average+0.3 , paste("\n\nPROMEDIO: \n", data$number, sep="") ,cex=1)
