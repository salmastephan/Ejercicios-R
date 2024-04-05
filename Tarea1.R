# Creadora Salma Stephan 
# Llamado de bases de datos

# Llamar la base de datos desde la computadora

install.packages("readr")


titulacion <-  read_csv(file = "datos_titulacion_2_.csv")
############
#Llamar desde un repositorio (internet)

repositorio <- read_csv(file="https://raw.githubusercontent.com/ManuelLaraMVZ/titulacion_amino_acidos/main/datos_titulacion%20(2).csv")
head(repositorio) #para ver el encabezado
view(repositorio) #para ver la tabla 

########################
#grafica

install.packages("ggplot2")
library("ggplot2")
library(ggplot)

grafica2 <- ggplot(repositorio,aes(x=Volumen,y=pH))+geom_point()+labs(title="Titulación cisteina",x="Volumen ácido (uL)",y="Valor de pH")
geom_line()+labs
theme_dark()

grafica2
labs


