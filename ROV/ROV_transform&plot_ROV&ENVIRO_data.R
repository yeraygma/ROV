#Script V3 transformar el csv bruto convertido desde archivo log de Mission Planer de los datos del ROV
#objetivo extraer los datos de tiempo presión y temperatura externa y hacer una tabla para representar perfiles de la masa de agua


rm(list=ls()) #clear workspace
graphics.off() 
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(reshape)
library(grid)



#Establecer el tiempo inicial en base al nomnbre del archivo



#leer los datos en csv y crea una matriz compuesta por seleccion de las filas donde aparezca BAR2 La parte de drop= false es para archivos no distinto número de columnas
data <- read.csv("C:/Users/Administrator/Documents/R/rov_acui_c2/rov/Log_csv/log_32_2018-7-3-15-29-14.csv", header = FALSE, sep = ",", dec = "." )
data <- data[data[,"V1"] == "BAR2",, drop = FALSE]


#crea una matriz de la seleccion de las columnas 2, 3 y 5
data <- data[c("V2","V3","V5")]

#reset numeración de las filas comenzando en 1
row.names(data) <- 1:nrow(data)

#MANUAL SI EL ARCHIVO LOG TIENE >1 INMERSIÓN!!!
#Identificar el timeUS de contacto con el agua del ROV. Hay que mirar en Mission planner con sensor de presión el timeUS al que corresponde e ir a data de R para identificar la fila exacta
#fila exactamente donde pasa de valores positivos de altura/profundidad a valores = ó negativos

dat <- data[c(2:20326),]


#reset numeración de las filas comenzando en 1
row.names(dat) <- 1:nrow(dat)


#renombrar las columnas en orden
colnames(dat) <- c("V1","V2","V3")

#coerción de factor a numérico
dat <- lapply(dat, function(x) as.numeric(as.character(x)))

#convierte los vectores resultantes en dataframe
dat <- as.data.frame(sapply(dat, as.numeric))

#renombra las columnas
colnames(dat) <- c("t","D","T")


#MANUAL!!!
#Guarda una copia del data frame con todos los datos en csv
write.csv(dat, 'C:/Users/Administrator/Documents/R/rov_acui_c2/results/alldata/alldataLog32yai3.csv', row.names=F)

#---------------------------------------------------------------------
#borra la columna de tiempo
dat <- dat[,2:3]

#MANUAL!!!
#Guarda una copia del data frame con todos los datos en csv
write.csv(dat, 'C:/Users/Administrator/Documents/R/rov_acui_c2/results/tempprofiles/tp25cx.csv', row.names=F)

#crea un nuevo dataframe con las medias de 50 valores, es decir de cada 5 sg
n <- 50
datarov <-aggregate(dat,list(rep(1:(nrow(dat)%/%n+1),each=n,len=nrow(dat))), mean)[-1]

#ENVIROSENSE
#MANUAL !!!
#carga el archivo csv del envirosense
dataenv <- read.csv("C:/Users/Administrator/Documents/R/rov_acui_c2/raw_data_enviro/yaizatun-inm3-3-julio-envirosense-data.csv", header = TRUE, sep = ";", dec = "," )

#reemplaza el separador de comas del pH por puntos y además transforma las clases de factor a numérico
dataenv <- as.data.frame(apply(apply(dataenv, 2, gsub, patt= ",", replacement="."), 2, as.numeric))

#coerción de factor a numérico para dataenv
dataenv <-as.data.frame(lapply(dataenv, function(x) as.numeric(as.character(x))))

#MANUAL!!!
#borrar las filas de antes en entrar el ROV en el agua.VER dataenv desde la primera hasta en la que la salinidad hay un -
#n=1:6
dataenv <- dataenv[-c(1:4),]

#reset numeración de las filas comenzando en 1
row.names(dataenv) <- 1:nrow(dataenv)

#renombra las columnas
colnames(datarov) <- c("depthtotal","watertmp1")


#reduce el número de filas de datarov al número de dataenv
m <- nrow(dataenv)
datarov <- datarov[1:m,]

#inserta las columnas de datarov en data env
dataenv$depthtotal <- datarov$depthtotal
dataenv$watertmp1 <- datarov$watertmp1

#sustituye los N/A por 0
dataenv[is.na(dataenv)] <-0

#crea un dataframe solo con las columnas que nos interesa
datafinal <- dataenv[,c("depthtotal","watertmp1","sg","tds","s","do","ec","ph","orp")]

#esta línea funciona exactamente igual de bien que la anterior
#dataenv2 <- subset(dataenv, select = c(depthtotal,watertmp1,sg,tds,s,do,ec,ph,orp))

#reset numeración de las filas comenzando en 1
row.names(datafinal) <- 1:nrow(datafinal)

#MANUAL !!!
#Guarda una copia del data frame datafinal en csv
write.csv(datafinal, 'C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/allpLog32yai3.csv', row.names=F)

#---------------------------------IMPORTACIÓN de datos de datafinal en archivo csv allpLog  

datafinal <-read.csv('C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/allpLog27cabopez.csv', header = TRUE, sep = ",", dec = ".")

#-------------------------------------------------MANUALLLLL!!!!! Seleccionar y limpiar las filas para los perfiles

  #Selecciona unas filas intermedias donde está el perfil de bajada o subida
p2 <-datafinal[c(124:184),]

  #Cambia el orden SI EL PERFIL ES DE SUBIDA
  #p2 <-p2[nrow(p2):1,]

  #renumerar las filas comenzando en 1
  row.names(p2) <- 1:nrow(p2)
  
  #Quitar ciertas filas por redundancia de datos en x
  p2 <- p2[-c(26:33),]  
  #renumerar las filas comenzando en 1
  row.names(p2) <- 1:nrow(p2)
  
  #Guarda una copia del data frame p de un perfil de subida o bajada en csv
  write.csv(datafinal, 'C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/pallpLog27cabopez.csv', row.names=F)

#---------------------------------IMPORTACIÓN de datos de datafinal en archivo csv pallpLog  

  datafinal <-read.csv('C:/Users/Administrator/Documents/R/rov_acui_c2/results/allparameters/allpLog25.csv', header = TRUE, sep = ",", dec = ".")
 p2 <- datafinal[c(262:301),]
 p2 <-p2[nrow(p2):1,]
 p2 <- p2[-c(31:32),] 
 row.names(p2) <- 1:nrow(p2)
#------------------------------- PLOTS
 
  
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
  
  plotph <- p2 %>%
    select(ph,depthtotal) %>%
    na.omit() %>%
    ggplot() +
    geom_path(aes(x = ph, y = depthtotal), color = "grey") +
    scale_x_continuous(limits = c(3,13), breaks = (3:13)) +
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
    geom_path(aes(x = orp, y = depthtotal), color = "brown") +
    scale_x_continuous(limits = c(-200,200), breaks = c(-200,-150,-100,-50,0,50,100,150,200)) + 
           ylab("profundidad (mt)") +
           xlab("potencial REDOX (mV)") +
           
           theme(
             plot.title = element_text(size = 12, face = "bold.italic"),
             axis.title.x = element_text(size = 8, color ="brown"),
             axis.title.y = element_text(size = 8, color = "black")
           )
  
  
  grid.newpage()
 grid.draw(rbind(ggplotGrob(plottemp), ggplotGrob(plotdo), ggplotGrob(plotph), ggplotGrob(plotorp), size = "last")) +
  
 grid.text("CABO PEZ",gp =gpar(fontsize = 12), vjust = -21)
  
  
  
  
  
  
  
  
  
  
  #crea el plot 1 varable por plot
  
 ggplot(p2, aes(watertmp1, depthtotal)) +
   geom_path() +
  ggtitle("temperatura, Polígono") + xlab("temperatura (ºC)") +
  ylab("profundidad (mt)") +
  
 
theme(
   plot.title = element_text(size = 12, face = "bold.italic"),
   axis.title.x = element_text(size = 10, color = "red", face = "bold"),
   axis.title.y = element_text(size = 10, color = "black")
 )
 
 

 
 


#PLOT varias variables en el mismo PLOT pero solo 1 escala X
p3 <- p2[c("depthtotal","watertmp1","do","ph")]
comp <- melt(p3, id.vars = "depthtotal")
ggplot(comp,aes(x =  value, y = depthtotal, color = variable)) + geom_path() 





# e) PLOTS diferentes variables en el mismo plot libreria base
  graphics.off()
 plot(p2$watertmp1,p2$depthtotal, type ="l", col = "red")
 par(new = TRUE)
 plot(p2$do,p2$depthtotal,type = "l", col = "blue")
axis(side = -4, at = pretty(range(p2$do)), col = "blue")

 
 #igual que el anterior pero con extra variable de do en color azul
 pt <- ggplot(p2, aes(watertmp1, depthtotal, colour = do)) +
   geom_path(color="red") +
   geom_point(size=1.5) +
   theme(legend.position="bottom")
 pt + labs(x = "temperatura (ºC)", y = "profundidad (mt)", title = "Perfil temperatura del agua", subtitle = "PR-EF")
 
 