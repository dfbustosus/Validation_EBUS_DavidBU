rm(list=ls())
#install.packages("argoFloats")
#library(argoFloats)
library(devtools)
#install_github("ArgoCanada/argoFloats", ref="develop")
library(argoFloats)
library(oce)
#install.packages("ncdf4")
library(ncdf4)
#install.packages("ocedata")
library(ocedata)
###### Core
# Institutos
# "AO" for Atlantic Oceanographic and Meteorological Laboratory; 
# "BO" for British Oceanographic Data Centre; 
# "CS" for Commonwealth Scientific and Industrial Research Organization; 
# "HZ" for China Second Institute of Oceanography; 
# "IF" for Ifremer, France; 
# "IN" for India National Centre for Ocean Information Services; 
# "JA" for Japan Meteorological Agency; 
# "KM" for Korea Meteorological Agency; 
# "KO" for Korea Ocean Research and Development Institute; 
# "ME" for Marine Environment Data Section;
# "NM" for National Marine Data & Information Service.
# Extraer los valores donde estan los perfiles
rm(list=ls())
# Obtener la ruta
getwd()
#rm(list=ls())
ai<-getIndex(filename = 'synthetic')
######## 1) CHLA (Clorofila) ################
# Subset by time.
from <- as.POSIXct("2011-01-01", tz="UTC")
to <- as.POSIXct("2011-12-31", tz="UTC")
subset1 <- subset(ai, time=list(from=from, to=to))
rect <- list(longitude=c(-85,-70), latitude=c(-45,-15))
subset2 <- subset(subset1, rectangle=rect)
subo<-subset(subset2,parameter='CHLA')
profiles<- getProfiles(subo)# Crear lista con perfiles 
argos1<-readProfiles(profiles) # Leer los perfiles
argos<-applyQC(argos1) # Limpiar datos con mala calidad


# Crear el vector tiempo
time_x<-c() # Al inicio vacio y se llena con el for
Lat_x<-c()
Lon_x<-c()
Cyc_x<-c()
Ins_x<-c()
Id_x<-c()
# Nuevos vectores
Sal<-c(); Sal_Adj<-c();Temp<-c(); Temp_Adj<-c();Pres<-c();Pres_Adj<-c(); CHLA<-c(); CHLA_adj<-c()
#length(argos@data[['argos']])
for (i in 1:length(argos@data[['argos']])){# Entrar a cada perfil
  y<-dim(argos@data[["argos"]][[i]]@data[["pressure"]]) # Longitud del vector presion
  y<-y[1] # Primera coordenada >>filas
  x<-argos@data[["argos"]][[i]]@data[["time"]] # Extraccion del valor tiempo
  time_1<- rep(x, y) # Repetir el valor de fecha y veces
  time_x<-append(time_x,time_1) # agregar al vector vacio
  # Latitud
  x1<-argos@data[["argos"]][[i]]@data[["latitude"]] # Extraccion del valor latitud
  Lat_1<- rep(x1, y) # Repetir el valor de fecha y veces
  Lat_x<-append(Lat_x,Lat_1) # agregar al vector vacio
  # Longitud
  x2<-argos@data[["argos"]][[i]]@data[["longitude"]] # Extraccion del valor longitud
  Lon_1<- rep(x2, y) # Repetir el valor de fecha y veces
  Lon_x<-append(Lon_x,Lon_1) # agregar al vector vacio
  # Ciclo
  x3<-argos@data[["argos"]][[i]]@metadata$cycleNumber # Extraccion del valor ciclo
  cyc_1<- rep(x3, y) # Repetir el valor de fecha y veces
  Cyc_x<-append(Cyc_x,cyc_1) # agregar al vector vacio
  # Institucion
  #x4<-argos@data[["argos"]][[i]]@metadata$institution[1] # Extraccion del valor institucion
  #ins_1<- rep(x4, y) # Repetir el valor de fecha y veces
  #Ins_x<-append(Ins_x,ins_1) # agregar al vector vacio
  # Id
  x5<-argos@data[["argos"]][[i]]@metadata$id # Extraccion del valor id
  id_1<- rep(x5, y) # Repetir el valor de fecha y veces
  Id_x<-append(Id_x,id_1) # agregar al vector vacio
  # Salinidad
  x6<-argos@data[["argos"]][[i]]@data[["salinity"]] # Extraccion de salinidad
  Sal<-append(Sal,x6) # agregar al vector vacio
  # Salinidad ajustada
  x7<-argos@data[["argos"]][[i]]@data[["salinityAdjusted"]] # Extraccion de salinidad Adj
  Sal_Adj<-append(Sal_Adj,x7) # agregar al vector vacio
  # Temperatura
  x8<-argos@data[["argos"]][[i]]@data[["temperature"]] # Extraccion de T
  Temp<-append(Temp,x8) # agregar al vector vacio
  # Temperature ajustada
  x9<-argos@data[["argos"]][[i]]@data[["temperatureAdjusted"]] # Extraccion de T adj
  Temp_Adj<-append(Temp_Adj,x9) # agregar al vector vacio
  # Presion
  x10<-argos@data[["argos"]][[i]]@data[["pressure"]] # Extraccion de presion
  Pres<-append(Pres,x10) # agregar al vector vacio
  # Presion Ajustada
  x11<-argos@data[["argos"]][[i]]@data[["pressureAdjusted"]] # Extraccion de presion
  Pres_Adj<-append(Pres_Adj,x11) # agregar al vector vacio
  # clorofila
  x12<-argos@data[["argos"]][[i]]@data[["chlorophyllA"]] # Extraccion de presion
  CHLA<-append(CHLA,x12) # agregar al vector vacio
  # clorofila ajustado
  x13<-argos@data[["argos"]][[i]]@data[["chlorophyllAAdjusted"]] # Extraccion de presion
  CHLA_adj<-append(CHLA_adj,x13) # agregar al vector vacio
}

print(length(time_x));print(length(Lat_x));print(length(Lon_x));print(length(Cyc_x));print(length(Id_x));
print(length(Sal));print(length(Sal_Adj));print(length(Temp));print(length(Temp_Adj))
print(length(Pres));print(length(Pres_Adj));print(length(CHLA));print(length(CHLA_adj))


# Crear el dataset final
df_chla<-data.frame(time_x,Lon_x, Lat_x,Pres,Pres_Adj,Temp,Temp_Adj,Sal,Sal_Adj,CHLA,CHLA_adj,Cyc_x,Id_x)
library(dplyr)
df_chla<-df_chla%>%rename(Fecha=time_x, Lon=Lon_x, Lat=Lat_x,Ciclo=Cyc_x, Id= Id_x)
# Guardar el dataset final
ruta="C:/Users/Windows/Desktop/Argo Tesis/Humboldt/BGC/"
write.csv(df_chla, file = paste(ruta,"df_chla_bgc_2010.csv",sep=''),row.names = FALSE)
