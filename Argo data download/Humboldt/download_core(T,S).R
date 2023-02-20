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

# Download (or use cached) index from one of two international servers.
index <- getIndex()

#### 0.0) 2008
# Subset by time.
from <- as.POSIXct("2019-01-01", tz="UTC")
to <- as.POSIXct("2019-12-31", tz="UTC")
subset1 <- subset(index, time=list(from=from, to=to))
# Subset by space.
rect <- list(longitude=c(-85,-70), latitude=c(-45,-15))
subset2 <- subset(subset1, rectangle=rect)
profiles<- getProfiles(subset2)# Crear lista con perfiles 
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
Sal<-c(); Sal_Adj<-c();Temp<-c(); Temp_Adj<-c();Pres<-c();Pres_Adj<-c()
#length(argos@data[['argos']])
for (i in 1:length(argos@data[['argos']])){# Entrar a cada perfil
  y<-dim(argos@data[["argos"]][[i]]@data[["pressure"]]) # Longitud del vector salinidad
  y<-y[1] # Primera coordenada >>filas
  x<-argos@data[["argos"]][[i]]@data[["time"]] # Extraccion del valor tiempo
  time_1<- rep(x, y) # Repetir el valor de fecha y veces
  time_x<-append(time_x,time_1) # agregar al vector vacio
  # Latitud
  x1<-argos@data[["argos"]][[i]]@data[["latitude"]] # Extraccion del valor tiempo
  Lat_1<- rep(x1, y) # Repetir el valor de fecha y veces
  Lat_x<-append(Lat_x,Lat_1) # agregar al vector vacio
  # Longitud
  x2<-argos@data[["argos"]][[i]]@data[["longitude"]] # Extraccion del valor tiempo
  Lon_1<- rep(x2, y) # Repetir el valor de fecha y veces
  Lon_x<-append(Lon_x,Lon_1) # agregar al vector vacio
  # Ciclo
  x3<-argos@data[["argos"]][[i]]@metadata$cycleNumber # Extraccion del valor tiempo
  cyc_1<- rep(x3, y) # Repetir el valor de fecha y veces
  Cyc_x<-append(Cyc_x,cyc_1) # agregar al vector vacio
  # Institucion
  #x4<-argos@data[["argos"]][[i]]@metadata$institution[1] # Extraccion del valor tiempo
  #ins_1<- rep(x4, y) # Repetir el valor de fecha y veces
  #Ins_x<-append(Ins_x,ins_1) # agregar al vector vacio
  # Id
  x5<-argos@data[["argos"]][[i]]@metadata$id # Extraccion del valor tiempo
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
}

print(length(time_x));print(length(Lat_x));print(length(Lon_x));print(length(Cyc_x));print(length(Id_x));
print(length(Sal));print(length(Sal_Adj));print(length(Temp));print(length(Temp_Adj))
print(length(Pres));print(length(Pres_Adj));

# Sal<-unlist(argos[['salinity']]) # poner salinidad en un vector
# Temp<-unlist(argos[['temperature']]) # poner T en un vector
# Pres<-unlist(argos[['pressure']]) # poner presion en un vector
# Sal_Adj<-unlist(argos[['salinityAdjusted']]) # poner salinidad en un vector
# Temp_Adj<-unlist(argos[['temperatureAdjusted']]) # poner T en un vector
# Pres_Adj<-unlist(argos[['pressureAdjusted']]) # poner presion en un vector

# Crear el dataset final
df<-data.frame(time_x,Lon_x, Lat_x,Pres,Pres_Adj,Temp,Temp_Adj,Sal,Sal_Adj,Cyc_x,Id_x)
num<- 993391
df<-data.frame(time_x[1:num],Lon_x[1:num], Lat_x[1:num],Pres[1:num],Pres_Adj[1:num],Temp[1:num],Temp_Adj[1:num],Sal[1:num],Sal_Adj[1:num],Cyc_x[1:num],Id_x[1:num])
library(dplyr)
library(data.table)
setnames(df,c('Fecha','Lon','Lat','Pres','Pres_Adj','Temp',
              'Temp_Adj','Sal','Sal_Adj','Ciclo','Id'))

df1<-df%>%filter(Pres_Adj< 601)

df1<-df%>%rename(Fecha=time_x, Lon=Lon_x, Lat=Lat_x,Ciclo=Cyc_x, Id= Id_x )
# Guardar el dataset final
ruta="C:/Users/Windows/Desktop/Argo Tesis/Humboldt/Core/"
write.csv(df1, file = paste(ruta,"df_T_S_2019.csv",sep=''),row.names = FALSE)

### Lectura de archivos y concatenar

# Carga de datos:
library(readr)
library(dplyr)
library(dbplyr)
ruta="C:/Users/Windows/Desktop/Argo Tesis/Humboldt/Core/"
setwd(ruta)
##################################################################################
df <- list.files(path="C:/Users/Windows/Desktop/Argo Tesis/Humboldt/Core") %>% 
  lapply(read_csv) %>% 
  bind_rows 

df1<-df %>% filter(!is.na(Sal_Adj) & !is.na(Temp_Adj) &!is.na(Lon) & !is.na(Lat) 
                   & !is.na(Pres) & !is.na(Pres_Adj) & !is.na(Ciclo) & !is.na(Id))
write.csv(df1, file = paste(ruta,"df_T_S_final.csv",sep=''),row.names = FALSE)

