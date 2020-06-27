##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.1 DEL PROBLEMA 5
##*****************************************
##*****************************************


#Guardamos el path del primer archivo
archivo <- "C:\\Users\\Bayyron\\Desktop\\Junio2020\\Seminario2\\Laboratorio\\Practica2\\Archivo\\regresion.csv"
informacion <- read.csv(archivo)
informacion
pairs(depressionpercentage ~  stresspercentage + anxietypercentage, data=informacion, main="Matriz de dispersi�n enfermedades")

##*****************************************
##*****************************************
##VAMOS CON EL EJERCICO NO.2 DEL PROBLEMA 5
##*****************************************
##*****************************************


plot(depressionpercentage ~ stresspercentage, data = informacion, xlab = "Estres", ylab = "Depresion", main="Correlaci�n entre  Depressi�n y Estres" )
abline(a = 0, b = 1, lty = 2)
MLatin <- lm(depressionpercentage ~ stresspercentage, data = informacion)
abline(MLatin, col = "green")


plot(depressionpercentage ~ anxietypercentage, data = informacion, xlab = "Ansiedad", ylab = "Depresion", main="Correlaci�n entre  Depressi�n y Ansiedad" )
abline(a = 0, b = 1, lty = 2)
MLatin <- lm(depressionpercentage ~ anxietypercentage, data = informacion)
abline(MLatin, col = "red")

plot(stresspercentage ~ anxietypercentage, data = informacion, xlab = "Ansiedad", ylab = "Estres", main="Correlaci�n entre  Ansiedad y Estres" )
abline(a = 0, b = 1, lty = 2)
MLatin <- lm(depressionpercentage ~ anxietypercentage, data = informacion)
abline(MLatin, col = "blue")



