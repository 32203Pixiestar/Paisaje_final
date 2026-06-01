# Titulo: Pruebas PCA 
# Autor: TrejoZarco Gisela 
#Fecha: 28 de abril de 2026 
# Nota: Queno colapace el PC-Puma , amen 
#------------------------------------------------------------------------------
#1 Cargar las librerias
library(tidyverse)
library(factoextra)
library(GGally)
library(ggcorrplot)
library(FactoMineR)

#2 Cargar base de datos 
setwd("C:/Users/GISELA/Downloads")
base <- read.csv("TENBIARE.csv")
names(base)

#3 Preparación de datos 

  #Filtrado de variables y manejo de NA
  base_modif1 <- base %>% 
  select(ENT, starts_with("PA3_"), FAC_ELE) %>% 
  na.omit() %>%
  mutate_all(as.numeric) %>% 
  group_by(ENT) %>% 
  summarise(across(everything(), mean))
  
    # Cálculo de medias ponderadas
    base_modif2 <- base %>% 
    select(ENT, starts_with("PA3_"), FAC_ELE) %>% 
    na.omit() %>%
    mutate_all(as.numeric) %>% 
    group_by(ENT) %>% 
    summarise_at(vars(starts_with("PA3_")), funs(weighted.mean(., FAC_ELE)))

    # Estandarización
    base_num <- scale(base_modif2[, 2:18])
    rownames(base_num) <- base_modif2$ENT
    base_num <- as_tibble(base_num)
    
 #4 Exploración de correlación 
    correlacion <- base_num %>% 
      cor(use = "pairwise") %>% 
      round(3)
    
    #Graficos de correlación     
      ## Gráfico con ggcorrplot
          ggcorrplot(correlacion, type = "lower", lab = TRUE, show.legend = FALSE, lab_size = 3)
      ## Gráfico con GGally
          ggpairs(base_modif2[, 2:18],
                  axisLabels = "show",
                  upper = list(continuous = wrap("cor", size = 3)),
                  lower = list(continuous = wrap("points", alpha = 0.5, size = 1)),
                  diag = list(continuous = wrap("densityDiag", size = 0.7))) +
            theme_minimal(base_size = 8) +  # Ajusta el tamaño base de texto
            theme(strip.text.x = element_text(size = 8, angle = 90),
                  strip.text.y = element_text(size = 8),
                  axis.text = element_text(size = 6))
          
  #5 Analisis de componenstes princpales
      # PCA con princomp
        pca <- princomp(base_num)
        summary(pca, loadings = TRUE)
      
      # Porcentaje de varianza explicada
        fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
        
       # Biplot
        fviz_pca_biplot(pca, repel = FALSE, col.var = "black", col.ind = "gray")
        
        # Contribuciones
        fviz_contrib(pca, choice = "var", axes = 1, top = 10)        
        fviz_contrib(pca, choice = "var", axes = 2, top = 10)        
        fviz_contrib(pca, choice = "var", axes = 3, top = 10)  
        
  # 6 PCA Ccon FactoRMine 
      pca_1 <- PCA(base_num, graph = FALSE)
      summary(pca_1)
      fviz_eig(pca_1, choice = "eigenvalue", addlabels = TRUE, ylim = c(0, 3))  
      fviz_pca_biplot(pca_1, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
      
      