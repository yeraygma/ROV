#Script V3 transformar el csv bruto del Envirosense a los perfiles finalizados para el informe


rm(list=ls()) #clear workspace
graphics.off() 
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)

#MANUAL !!!
#carga el archivo csv del enviros
dataenv <- read.csv("C:/Users/Administrator/Documents/R/datos_enviro_3/poligono-3a-inm-envirosense-data.csv", header = TRUE, sep = ";", dec = "," )

#crea un dataframe solo con las columnas que nos interesa
dataenv <- dataenv[,c("depthtotal","watertmp2","sg","tds","s","do","ec","ph","orp")]
#-------------------Si hay comas en el archivo csv----------------------
#sustituye comas por puntos
dataenv <- as.data.frame(apply(apply(dataenv, 2, gsub, patt= ",", replacement="."), 2, as.numeric))

#coerción de factor a numérico para dataenv
dataenv <-as.data.frame(lapply(dataenv, function(x) as.numeric(as.character(x))))
#-------------------------------------------------------

# b) perfil en que se corta unas filas intermedias de datafinal
#Selecciona unas filas intermedias
p2 <-dataenv[c(104:141),]

    
#se cambia el orden de las filas comenzando en la última y terminando en la primera
p2 <-p2[nrow(p2):1,]

#se renumeran las filas comenzando por el 1
row.names(p2) <- 1:nrow(p2)
#se seleccionan las filas correspondientes a la bajada hasta el fondo

#Quitar ciertas filas por redundancia de datos en x o outliers
p2 <- p2[-c(16),]
row.names(p2) <- 1:nrow(p2)

#---------------------------------IMPORTACIÓN de datos de datafinal en archivo csv raw_data_enviro  

datafinal<-read.csv('C:/Users/Administrator/Documents/R/rov_acui_c2/raw_data_enviro/opulent-inm-1-envirosense-data.csv', header = TRUE, sep = ";", dec = ",")

datafinal <- datafinal[,c("depthtotal","watertmp2","sg","tds","s","do","ec","ph","orp")]

datafinal$depthtotal <- -datafinal$depthtotal


#---------------------MANUAL!!!! limpiar data para perfil
p2 <- dataenv[c(116:135),]
row.names(p2) <- 1:nrow(p2)

p2 <-p2[nrow(p2):1,]
p2 <- p2[-c(11),] 

row.names(p2) <- 1:nrow(p2)









#---------------------------------PLOTS

#PLOTS alineados TODAS LAS VARIABLES añadir libreria grid

plottemp <- p2 %>%
  select(watertmp2,depthtotal) %>%
  na.omit() %>%
  ggplot() +
  geom_path(aes(x = watertmp2, y = -depthtotal), color ="red") +
  scale_x_continuous(limits = c(18,26), breaks = (18:26)) +
  ylab("profundidad (mt)") + 
  xlab("temperatura (ºC)") +
  
  theme(
    plot.title = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 8, color ="red"),
    axis.title.y = element_text(size = 8, color = "black")
  )

plots <- p2 %>%
  select(s,depthtotal) %>%
  na.omit() %>%
  ggplot() +
  geom_path(aes(x = s, y = -depthtotal), color ="black") +
  scale_x_continuous(limits = c(0,42), breaks = c(5,10,15,20,25,30,35,40,42)) +
  ylab("profundidad (mt)") + 
  xlab("salinidad (gr/L)") +
  
  theme(
    plot.title = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 8, color ="black"),
    axis.title.y = element_text(size = 8, color = "black")
  )

#------------------
plotdo <- p2 %>%
  select(do,depthtotal) %>%
  na.omit() %>%
  ggplot() +
  geom_path(aes(x = do, y = -depthtotal), color ="blue") +
  scale_x_continuous(limits = c(0,250), breaks = c(0,50,100,150,200,250)) +
  ylab("profundidad (mt)") +
  xlab("oxígeno disuelto (%)") +
  
  theme(
    plot.title = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 8, color ="blue"),
    axis.title.y = element_text(size = 8, color = "black")
  )
#-----------------

plotph <- p2 %>%
  select(ph,depthtotal) %>%
  na.omit() %>%
  ggplot() +
  geom_path(aes(x = ph, y = -depthtotal), color = "grey") +
  scale_x_continuous(limits = c(6,10), breaks = (6:10)) +
  ylab("profundidad (mt)") +
  xlab("pH") +
  
  theme(
    plot.title = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 8, color ="grey"),
    axis.title.y = element_text(size = 8, color = "black")
  )

plotorp <- p2 %>%
  select(orp,depthtotal) %>%
  na.omit() %>%
  ggplot() +
  geom_path(aes(x = orp, y = -depthtotal), color = "brown") +
  scale_x_continuous(limits = c(-100,150), breaks = c(-100,-50,0,50,100,150)) + 
  ylab("profundidad (mt)") +
  xlab("potencial REDOX (mV)") +
  
  theme(
    plot.title = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 8, color ="brown"),
    axis.title.y = element_text(size = 8, color = "black")
  )


grid.newpage()
grid.draw(rbind(ggplotGrob(plottemp), ggplotGrob(plots),ggplotGrob(plotdo), ggplotGrob(plotph),ggplotGrob(plotorp), size = "last")) +
  
  grid.text("YAIZATÚN",gp =gpar(fontsize = 12), vjust = -20)




#MANUAL!!! CREA LOS PLOTS UNA VARIABLE POR PLOT. CAMBIAR LAS VARIALES, LOS COLORES Y LOS NOMBRE DE LAS VARIABLES REPRESENTADAS 
#crea el plot
pt <- ggplot(p2, aes(orp, depthtotal)) +
  geom_path(color="red") +
  ggtitle("Perfil potencial REDOX, Acuipalma") + xlab("potencial REDOX (mV)") +
  ylab("profundidad (mt)") + 
  xlim(150,165) +
  scale_y_reverse()

pt + theme(
  plot.title = element_text(size = 12, face = "bold.italic"),
  axis.title.x = element_text(size = 10, color = "red", face = "bold"),
  axis.title.y = element_text(size = 10, color = "black")
)

#importar alldata para prueba

#pr <- read.csv('C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/allpLog26.csv', header = TRUE, sep = ";", dec = "," )
