#Título: Trabajo luciernagas 
#Fecha: 31 de mayo del 2026 
#Autor: Trejo Zarco Gisela 
#Conctato: giselatrejo06@gmail.com
#Notas: Estoso son solo los grpáficos que me pidio mi equipo y maquino por favor no colapses , amen 
#-----------------------------------------
#Libreria 
library(sf)
library(ggplot2)
install.packages("Makurhini")
library(Makurhini)
#Lllamar a los parches 
setwd("C:/Users/GISELA/Downloads")
parches <- st_read("Parches_paisaje.kml")
datos <- st_read("lucecitas_individuos.csv")

#Verificar que estén los pareches y la base  
#Parches 
head(parches)
names(parches)
#Base de datos 
head(datos)
names(datos)

#Ploteo base 
plot(st_geometry(parches))

# Área en hectáreas
parches$area_ha <- as.numeric(st_area(parches))/10000

#Unir los datos y los parches 
parches$Name %in% datos$Parche
  #Cambiar nombres 
  datos$Parche <- c(
    "Parche 1",
    "Parche 2",
    "Parche 3",
    "Parche 4",
    "Total"
  )
parches2 <- left_join(
  parches,
  datos,
  by = c("Name" = "Parche")
)
names(parches2)

  #Convertir la denciad en una varible  numérica 
  parches2$Densidad..ind.ha. <- as.numeric(parches2$Densidad..ind.ha.)
  class(parches2$Densidad..ind.ha.)
  parches2$Individuos.observados <-as.numeric(parches2$Individuos.observados)
  class(parches2$Individuos.observados)
  str(parches2)

#Hacer mapa de individuos con viridis
ggplot(parches2) +
  geom_sf(aes(fill = Individuos.observados)) +
  scale_fill_viridis_c() +
  theme_minimal()
#Hacer mapa de densidad 
ggplot(parches2) +
  geom_sf(aes(fill = Densidad..ind.ha.),
          color = "black") +
  scale_fill_viridis_c() +
  labs(fill = "Densidad (ind/ha)") +
  theme_minimal()