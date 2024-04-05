#Creador Salma Stephan

#Gráfica de dispersión

install.packages("pacman")
library(pacman)

p_load("readr","ggplot2","dplyr")

datos <- read_csv(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/Genes.csv")
head(datos)

#Extraer de genes controles (referencia)

controles <- datos %>% 
filter(Condicion=="Control")  #Doble igual significa identico
head(controles)

#Sacar los promedios

promedio_controles <- controles %>% 
summarise(Mean_C1=mean(Cx1),Mean_C2=mean(Cx2),Mean_C3=mean(Cx3),Mean_T1=mean(T1), Mean_T2=mean(T2), Mean_T3=mean(T3)) %>%   
mutate(Gen="Promedio_controles") %>%
select(7,1,2,3,4,5,6)  

promedio_controles

#Extraer los genes de la tabla "datos"

genes <- datos %>% 
filter(Condicion=="Target") %>% 
select(-2)  
genes

#ath-especie, nombre microRNA, código de la empresa

#Delta CT

DCT <- genes %>% 
mutate(DCT_C1=2^-(Cx1-promedio_controles$Mean_C1),DCT_C2=2^-(Cx2-promedio_controles$Mean_C2),DCT_C3=2^-(Cx3-promedio_controles$Mean_C3),DCT_T1=2^-(T1-promedio_controles$Mean_T1),DCT_T2=2^-(T2-promedio_controles$Mean_T2),DCT_T3=2^-(promedio_controles$Mean_T3)) %>%
select(-2,-3,-4,-5,-6,-7)
DCT

#Agrupar 2DCT

promedios_DCT <- DCT %>%
mutate(Mean_DCT_Cx=(DCT_C1+DCT_C2+DCT_C3)/3,Mean_DCT_Tx=(DCT_T1+DCT_T2+DCT_T3)/3)
promedios_DCT

#Gráfica

grafica_puntos <- ggplot(promedios_DCT,mapping = aes(x=Mean_DCT_Cx,y=Mean_DCT_Tx),colour=cut)+geom_point(size=3, color="blue")+labs(title = "Análisis de puntos", x="Condición control (2^DCT)",y="Tratamiento (2^DCT)")+geom_smooth(method = "lm",alpha=0.05,linewidth=1, span=1)+theme_minimal()
grafica_puntos


guardar <- ggsave("grafica_dispersion.jpeg", plot = grafica_puntos, width = 6, height = 4, dpi = 300)


