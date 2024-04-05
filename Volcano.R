#Creador Salma Stephan
#Gráfica volcano

library(pacman)
p_load("readr", "ggplot2", "dplyr","ggrepel","matrixTests")

datos <- read.csv(file="https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/DesnutridasvsEunutridas.csv")
head(datos)

#Obtener controles
controles <- datos %>% 
  filter(Condicion=="Control")
head(controles)

#Promedios control
promedios_control <- controles %>%  
  summarise(Mean_C1=mean(Cx1),
            Mean_C2=mean(Cx2),
            Mean_C3=mean(Cx3),
            Mean_T1=mean(T1),
            Mean_T2=mean(T2),
            Mean_T3=mean(T3)) %>% 
  mutate(Gen="promedio_controles") %>% 
  select(7,1,2,3,4,5,6)
promedios_control

#Extraer genes 
genes <- datos %>% 
  filter(Condicion=="Target") %>% 
  select(-2)
head(genes)

#∆CT (interés-referencia)
DCT <- genes %>% mutate(DCT_C1=2^-(Cx1-promedios_control$Mean_C1),
                        DCT_C2=2^-(Cx2-promedios_control$Mean_C2),
                        DCT_C3=2^-(Cx3-promedios_control$Mean_C3),
                        DCT_T1=2^-(T1-promedios_control$Mean_T1),
                        DCT_T2=2^-(T2-promedios_control$Mean_T2),
                        DCT_T3=2^-(T3-promedios_control$Mean_T3)) %>% 
  select(-2,-3,-4,-5,-6,-7)
head(DCT)

#agrupación DCTs
promedios_DCT <- DCT %>% 
  mutate(Mean_DCT_Cx=(DCT_C1+ DCT_C2+ DCT_C3)/3,
         Mean_DCT_Tx=(DCT_T1+ DCT_T2+ DCT_T3)/3)

promedios_DCT

#Definir prueba estadística

tvalue_gen <- row_t_welch(promedios_DCT [,c("DCT_C1",
                                            "DCT_C2",
                                            "DCT_C3")],
                          promedios_DCT[,c("DCT_T1",
                                           "DCT_T2",
                                           "DCT_T3")])
head(tvalue_gen)
View(tvalue_gen)

FCyPV <-promedios_DCT %>% 
  select(1,8,9) %>% 
  mutate(pvalue=tvalue_gen$pvalue,
         Fold_change=Mean_DCT_Tx/Mean_DCT_Cx) %>% 
  select(1,4,5)
FCyPV

#Valores para graficar

Logs <- FCyPV %>% 
  mutate(LPV=-log10(pvalue),
         LFC=log2(Fold_change)) %>% 
  select (1,4,5)
Logs

#grafica
Volcano1 <- ggplot(data=Logs,
                   mapping=aes(x=LFC, y=LPV))+
  geom_point(size=2,
             color="gray")+
  theme_classic()+
  labs(title = "Análisis comparativo de miRNAs", 
       caption = "Creador: Salma Stephan",
       x = expression("Log2 (2"^"-DDCT)"), 
       y = expression("-Log10(pvalue)"))
Volcano1

#límites

limite_p <- 0.05
limite_FC <- 1.5

down_regulated <- Logs %>% 
  filter(LFC < -log2(limite_FC),
         LPV> -log10(limite_p))
down_regulated

up_regulated <- Logs %>% 
  filter(LFC> log2(limite_FC),
         LPV> -log10(limite_p))
up_regulated

top_down_regulated <- down_regulated %>% 
  arrange(desc(LPV)) %>% 
  head(5)
top_down_regulated

top_up_regulated <- up_regulated %>% 
  arrange(desc(LPV)) %>% 
  head(5)
top_up_regulated


#Mejora de gráfica

Volcano2 <- Volcano1+
  geom_hline(yintercept = -log10(limite_p),
             linetype="dashed")+
  geom_vline(xintercept= c(-log2(limite_FC),log2(limite_FC)),
             linetype="dashed")
Volcano2

Volcano3 <- Volcano2+
  geom_point(data=up_regulated,
             x=up_regulated$LFC,
             y=up_regulated$LPV,
             alpha=1,
             size=3,
             color="#E64B35B2")+
  geom_point(data=down_regulated,
             x= down_regulated$LFC,
             y=down_regulated$LPV,
             alpha=1,
             size=3,
             color="#3C5488B2")
Volcano3

Volcano4 <- Volcano3+
  geom_label_repel(data = top_up_regulated,
                   aes(x = LFC, y = LPV, label = Gen),
                   box.padding =unit(0.35,"lines"), 
                   point.padding =unit(0.5,"lines"),
                   segment.color = "grey50",
                   segment.size = 0.2,
                   max.overlaps = 50,
                   nudge_x = 0.5,  
                   nudge_y = 0.5,
                   show.legend = FALSE) + 
  geom_label_repel(data = top_down_regulated,
                   aes(x = LFC, y = LPV, label = Gen),
                   box.padding =unit(0.35,"lines"), 
                   point.padding =unit(0.5,"lines"),
                   segment.color = "grey50",  
                   segment.size = 0.2, 
                   max.overlaps = 50,
                   nudge_x = -0.5,  
                   nudge_y = -0.5)
Volcano4

ggsave("Volcano.jpeg", plot=Volcano4, height=5, width=6, dpi=300)