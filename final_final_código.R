#Título: Trabajo luciernagas 
#Fecha: 31 de mayo del 2026 
#Autor: Trejo Zarco Gisela 
#Conctato: giselatrejo06@gmail.com
#Notas: Estoso son solo los gráficos que me pidio mi equipo y maquina por favor no colapses  , otra vez , amen 
#-----------------------------------------
#Libreria 
library(sf)
library(ggplot2)
library(dplyr)
library(units)
#install.packages(c("remotes", "devtools", "igraph"))
#pkgbuild::has_build_tools(debug = TRUE)
#remotes::install_github(
  #"connectscape/Makurhini",
  #dependencies = TRUE
#)
library(Makurhini)
#Lllamar a los parches 
setwd("C:/Users/GISELA/Downloads")
parches <- st_read("Parches_paisaje.kml")
datos <- st_read("lucecitas_individuos.csv")
general <- st_read("lucesita_datosgenerales.csv")

#Verificar que estén los pareches y la base  
#Parches 
head(parches)
names(parches)
#Base de datos 
head(datos)
names(datos)

#Ploteo base 
plot(st_geometry(parches))

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
#Analisis de conectividad-------------------------------------------- 
#Hacer disncia métrica para Makuri 
st_crs(parches2)
#Proyectarlas en UTM 
parches2 <- st_transform(parches2, 32614)
#Dar un ID para R 
parches2$ID <- 1:nrow(parches2)
#Visulazar los parches 
plot(st_geometry(parches2))
text(st_coordinates(st_centroid(parches2)),
     labels = parches2$ID)
#Hacer la matriz de distancia 
centros <- st_centroid(parches2)
distancias <- st_distance(centros)
dist_num <- units::drop_units(distancias)
distancias
#Conectiviad binara (presnecia/ausencia)
dist_num <- as.matrix(distancias)
conexion <- ifelse(dist_num <= 500, 1, 0)
conexion
#Mapa con centroides 
centros <- st_centroid(parches2)  
#Idince de conectividad 
dist_mat <- as.matrix(
  drop_units(st_distance(centros))
)
#Crear la matriz
umbral <- 1000

A <- ifelse(dist_mat <= umbral, 1, 0)

diag(A) <- 0
#IC 
n <- nrow(A)

IIC <- sum(A) / (n * (n - 1))

IIC
#Mapa 2.0
parches2$area_ha <- as.numeric(st_area(parches2))/10000
ggplot() +
  geom_sf(data = parches2,
          aes(fill = area_ha)) +
  geom_sf(data = centros,
          size = 3) +
  scale_fill_viridis_c() +
  theme_minimal()
#Mapa con red--------------------------------------------
# Hacer las línas de conectividad
  #Sacar las coordenadas de loas centroides 
  coords <- st_coordinates(centros)
  #Hacer las  líneas para los parches 
lineas <- st_sfc(
  st_linestring(rbind(coords[1,], coords[2,])),
  st_linestring(rbind(coords[1,], coords[3,])),
  st_linestring(rbind(coords[2,], coords[3,])),
  st_linestring(rbind(coords[3,], coords[4,]))
)
  #Hacerlas un obejeto para poderlas trabajar en santa paz 
lineas_sf <- st_sf(
  ID = 1:length(lineas),
  geometry = lineas,
  crs = st_crs(parches2)
)
#Hacer el mapa
ggplot() +
  geom_sf(data = parches2,
          aes(fill = Densidad..ind.ha.),
          color = "black") +
  
  geom_sf(data = lineas_sf,
          linewidth = 1) +
  
  geom_sf(data = centros,
          aes(size = Individuos.observados)) +
  
  scale_fill_viridis_c() +
  scale_size_continuous(name = "Individuos") +
  
  theme_minimal() +
  labs(fill = "Densidad (ind/ha)")
#Parte estadistica------------------------------
# Establecer la cosas para trabajar
parches2$Name %in% general$parche
  #Cambiar nombres 
  general$parche <- c(
    "Parche 1",
    "Parche 2",
    "Parche 3",
    "Parche 4"
  )
parches3 <- left_join(
  parches2,
  general,
  by = c("Name" = "parche")
)
names(parches3)
#Verificar tipo de dato 
class(parches3$Densidad..ind.ha.)
class(parches3$cobertura_vegetal_porcentaje)
#Corrección de cobertura vegetal 
parches3$cobertura_vegetal_porcentaje <- as.numeric(
  as.character(parches3$cobertura_vegetal_porcentaje)
)
#Modelo lineal para  indv/hectarea x porcentaje de arboles 
modelo_lineal<- lm(Densidad..ind.ha.~ cobertura_vegetal_porcentaje , data = parches3)
summary(modelo_lineal)

# grafico 
library(ggplot2)

ggplot(parches3,
       aes(x = cobertura_vegetal_porcentaje,
           y = Densidad..ind.ha.)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm",
              se = TRUE) +
  labs(
    x = "Cobertura vegetal (%)",
    y = "Densidad (ind/ha)",
    title = "Relación entre cobertura vegetal y densidad"
  ) +
  theme_minimal()