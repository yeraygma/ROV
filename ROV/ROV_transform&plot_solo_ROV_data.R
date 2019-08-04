#Script V3 transformar el csv bruto convertido desde archivo log de Mission Planer de los datos del ROV
#objetivo extraer los datos de tiempo presión y temperatura externa y hacer una tabla para representar perfiles de la masa de agua


rm(list=ls()) #clear workspace
graphics.off() 
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)



#Establecer el tiempo inicial en base al nombre del archivo



#leer los datos en csv y crea una matriz compuesta por seleccion de las filas donde aparezca BAR2 La parte de drop= false es para archivos no distinto número de columnas
data <- read.csv("C:/Users/Administrator/Documents/R/rov_acui_c2/rov/Log_csv/log_20_2018-6-19-13-34-18.csv", header = FALSE, sep = ",", dec = "." )
data <- data[data[,"V1"] == "BAR2",, drop = FALSE]

#crea una matriz de la seleccion de las columnas 2, 3 y 5
data <- data[c("V2","V3","V5")]

#reset numeración de las filas comenzando en 1
row.names(data) <- 1:nrow(data)


#renombrar las columnas en orden
colnames(data) <- c("V1","V2","V3")

#coerción de factor a numérico
data <- lapply(data, function(x) as.numeric(as.character(x)))

#convierte los vectores resultantes en dataframe
data <- as.data.frame(sapply(data, as.numeric))

#renombra las columnas
colnames(data) <- c("t","depthtotal","watertmp1")


#MANUAL!!!
#Guarda una copia del data frame con todos los datos en csv
write.csv(data, 'C:/Users/Administrator/Documents/R/rov_acui_c2/results/alldata/alldataLog20prc2.csv', row.names=F)


#IMPORTAR data de alldataLog
#-------------

#Importar data de alldataLog
data <- read.csv('C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/allpLog18adsa2.csv', header = TRUE, sep = ",", dec = ".")

#---------
#MANUAL!!!---SELECCIONA LA INMERSIÓN 

#Identificar el timeUS de contacto con el agua del ROV. Hay que mirar en Mission planner con sensor de presión el timeUS al que corresponde e ir a data de R para identificar la fila exacta

      data <- data[c(11545:13223),]

      #reset numeración de las filas comenzando en 1
      row.names(data) <- 1:nrow(data)

      #borra la columna de tiempo
      data <- data[,2:3]

#MANUAL!!!
#Guarda una copia del data frame con todos los datos en csv
write.csv(data, 'C:/Users/Administrator/Documents/R/rov_acui_c2/results/tempprofiles/tpLog20prc2.csv', row.names=F)


#Limpia datos
  #crea un nuevo dataframe con las medias de 50 valores, es decir de cada 5 sg
  n <- 50
  datarov <-aggregate(data,list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean)[-1]

  #sustituye los N/A por 0
  datarov[is.na(datarov)] <-0

  #cambio de nombre 
  datafinal <-datarov 


# MANUAL!! LIMPIA EL PERFIL.  
  #Selecciona unas filas en las que está el perfil de subida o bajada
  p2 <-datafinal[c(126:199),]

  #Cambia el orden
   row.names(p2) <- 1:nrow(p2)
  
   #Quita filas
   p2 <-datafinal[-c(1,10:16,19:20,23:25,27:31,34:45,48:56,60:68,70:74),]

  

          
#MANUAL!!peligro !!!!
#GUARDA COPIA csv del data frame p de un perfil de subida o bajada en csv
write.csv(datafinal, 'C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/pallpar20prc2.csv', row.names=F)

#----IMPORTACIÓN ARCHIVO desde datos de datafinal en archivo csv pallpLog  y limpieza del perfil--------


datafinal <-read.csv('C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/pallpLog27cabopez.csv', header = TRUE, sep = ",", dec = ".")
p2 <- datafinal[c(494:582),]
p2 <-p2[nrow(p2):1,]
p2 <- p2[-c(27:37),] 
row.names(p2) <- 1:nrow(p2)
#------------------------------


#PLOTS alineados TODAS LAS VARIABLES añadir libreria grid

plottemp <- p2 %>%
  select(watertmp1,depthtotal) %>%
  na.omit() %>%
  ggplot() +
  geom_path(aes(x = watertmp1, y = depthtotal), color ="red") +
  scale_x_continuous(limits = c(17,23), breaks = (17:23)) +
  ylab("profundidad (mt)") + 
  xlab("temperatura (ºC)") +
  
  theme(
    plot.title = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 8, color ="red"),
    axis.title.y = element_text(size = 8, color = "black")
  )




plotdo <- p2 %>%
  select(do,depthtotal) %>%
  na.omit() %>%
  ggplot() +
  geom_path(aes(x = do, y = depthtotal), color ="blue") +
  scale_x_continuous(limits = c(4,10), breaks = (4:10)) +
  ylab("profundidad (mt)") +
  xlab("oxígeno disuelto (mg/L)") +
  
  theme(
    plot.title = element_text(size = 12, face = "bold.italic"),
    axis.title.x = element_text(size = 8, color ="blue"),
    axis.title.y = element_text(size = 8, color = "black")
  )

grid.newpage()
grid.draw(rbind(ggplotGrob(plottemp), ggplotGrob(plotdo),ggplotGrob(plotph),ggplotGrob(plotorp), size = "last")) +
  
  grid.text("PROCRÍA1",gp =gpar(fontsize = 12), vjust = -21)



  #Selecciona dos variables para representar una profundidad y la otra: temperatura, DO, pH, ORP, Salinidad, etc
  
  
  #MANUAL!!!! 1VARIABLE POR PLOT. CAMBIAR LAS VARIALES, LOS COLORES Y LOS NOMBRE DE LAS VARIABLES REPRESENTADAS 

ggplot(p2, aes(watertmp1, depthtotal)) +
    geom_path(color="red") +
  scale_x_continuous(limits = c(17,23), breaks = (17:23)) +
   ggtitle("Procría2, temperatura") +   xlab("temperatura (ºC)") +
  ylab("profundidad (mt)") +
  

theme(
  plot.title = element_text(size = 12, face = "bold.italic"),
  axis.title.x = element_text(size = 10, color = "red", face = "bold"),
  axis.title.y = element_text(size = 10, color = "black")
)

  

