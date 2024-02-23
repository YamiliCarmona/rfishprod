library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(vegan)

# Data-----------

tabla <- read_excel("data/tabla/spp_parametros_20231201.xlsx")|> 
  select(-MaxSizeTL) |>
  filter(!is.na (Linf), !is.na (LinfTL))


merged_data <- readRDS("data/tabla/fishdata_sstmean_maxsize_by_year.RDS") 
# select(-MaxSizeTL)

# merge ----------

merge_database <- merge(merged_data, tabla, by = c("Family", "Species", "A_ord", "B_pen"), all.x = TRUE)  |> 
  rename(Diet = TrophicGroup, a = "A_ord",
         b = "B_pen") |> 
  #Remove sharks rays and seahorses
  dplyr::filter(Taxa2 == "Actinopterygii" & Taxa2 != "Syngnathidae") |>
  filter(Family != "Anguillidae", Family != "Congridae", Family != "Muraenidae", Family != "Ophichthidae") |>
  #Removing small crypto
  filter(Family != "Chironemidae", Family != "Pinguipedidae", Family != "Chaenopsidae", Family != "Tripterygiidae",
         Family != "Gobiidae", Family != "Callionymidae", Family != "Blenniidae",
         Family != "Labrisomidae", Family != "Microdesmidae", Family != "Pholidichthyidae") |>
  # filtering to keep fish larger than 5cm and fish of 5cm if Maxlength <25cm
  filter (Size != 5 | (Size==5 & MaxSizeTL < 25)) |> 
  filter(!Size == 0) |>
  filter(!is.na (Linf)) |>
  filter(!is.na (SpecCode)) |> 
  filter(!is.na(sstmean))


unique(merge_database$Habitat)


ltem <- database |> 
  filter(Habitat %in% c("BLOQUES", "PARED"))

ltem |> 
  group_by(Habitat) |> 
  summarise(n = n_distinct(Reef),
            Years = n_distinct(Year))

str(ltem)


table <- ltem |> 
  select(IDReef, Reef, Year, Habitat ,Depth, Depth2, Degree, sstmean)


# PERMANOVA------------

# Selecciona las variables relevantes para el análisis
# variables <- c("Habitat", "Depth2", "Degree", "sstmean")
variables <- c("Habitat", "Depth2", "Degree")

# Crea una submatriz con las variables seleccionadas
submatriz <- ltem[, variables]

# Asegúrate de que las variables categóricas estén codificadas como factores
submatriz$Habitat <- as.factor(submatriz$Habitat)
submatriz$Depth2 <- as.factor(submatriz$Depth2)

# Convertir la columna "Depth" a tipo numérico
# submatriz$Depth <- as.numeric(submatriz$Depth)


# Verifica la estructura de la submatriz
str(submatriz)





# Convertir variables categóricas en variables dummy
dummy_variables <- model.matrix(~ Habitat + Depth2 - 1, data = submatriz)
# dummy_variables <- model.matrix(~ Habitat - 1, data = submatriz)

# Combina las variables dummy con las variables numéricas
submatriz_numeric <- cbind(dummy_variables, submatriz[, c("Degree")])
# submatriz_numeric <- cbind(dummy_variables, submatriz[,c("Degree", "Depth")])

set.seed(123) # Para reproducibilidad
sample_indices <- sample(nrow(submatriz_numeric), 1000) # Tomar una muestra de 1000 filas
sample_data <- submatriz_numeric[sample_indices, ] # Subconjunto de datos de muestra
sample_dist <- vegdist(sample_data, method = "bray") # Calcular la matriz de disimilitud para la muestra


# Realiza el análisis de dispersión multivariante
dispersion <- betadisper(sample_dist, group = submatriz$Habitat[sample_indices])

# Grafica los resultados
plot(dispersion, hull = TRUE, ellipse = TRUE)


# Realiza una prueba de permutación sobre los resultados del análisis
perm_test <- permutest(dispersion)

# Grafica los resultados con elipses
plot(dispersion, hull = FALSE, ellipse = TRUE)



# Realiza el análisis de dispersión multivariante para las variables 
dispersion_Habitat <- betadisper(sample_dist, group = submatriz$Habitat[sample_indices])
dispersion_Depth2 <- betadisper(sample_dist, group = submatriz$Depth2[sample_indices])
dispersion_Degree <- betadisper(sample_dist, group = submatriz$Degree[sample_indices])
# dispersion_sstmean <- betadisper(sample_dist, group = submatriz$sstmean[sample_indices])




# Grafica los resultados para cada variable una por una
par(mfrow=c(2, 2))  # Divide el espacio de trazado en 2 filas y 2 columnas

# Gráfica de Habitat
plot(dispersion_Habitat, pch = 19, main = "Habitat")

# Gráfica de Depth2
plot(dispersion_Depth2, pch = 19, main = "Depth2")

# Gráfica de Degree
plot(dispersion_Degree, pch = 19, main = "Degree")

# Gráfica de sstmean
# plot(dispersion_sstmean, pch = 19, main = "sstmean")

# Asegúrate de restablecer el diseño de gráficos después de terminar
par(mfrow=c(1, 1))

# Guardar figuras ------

# # Establece el dispositivo gráfico como PNG y especifica el nombre del archivo
png("figs/ltem_premanova_intento2.png", width = 8.5, height = 4.5, units = "in", res = 1000)
# 
# # Establece el diseño de gráficos para 2 filas y 2 columnas
par(mfrow=c(2, 2))
# 
# # Gráfica de Habitat
plot(dispersion_Habitat, pch = 19, main = "Habitat")
# 
# # Gráfica de Depth2
plot(dispersion_Depth2, pch = 19, main = "Depth2")
# 
# # Gráfica de Degree
plot(dispersion_Degree, pch = 19, main = "Degree")
# 
# # Gráfica de sstmean 
# plot(dispersion_sstmean, pch = 19, main = "sstmean")
# 
# # Cierra el dispositivo gráfico
dev.off()
# 
# 
# 
# # Abre un archivo PDF para guardar las gráficas
# pdf("dispersion_plots2.pdf")
# 
# # Establece el diseño de gráficos para 2 filas y 2 columnas
# par(mfrow=c(2, 2))
# 
# # Gráfica de Habitat
# plot(dispersion_Habitat, pch = 19, main = "Habitat")
# 
# # Gráfica de Depth2
# plot(dispersion_Depth, pch = 19, main = "Depth")
# 
# # Gráfica de Degree
# plot(dispersion_Degree, pch = 19, main = "Degree")
# 
# # Gráfica de sstmean (puedes descomentar si deseas incluirla)
# # plot(dispersion_sstmean, pch = 19, main = "sstmean")
# 
# # Cierra el archivo PDF
# dev.off()

# ANOSIM y PERMANOVA --------------

# Realizar ANOSIM para comparar entre las categorías de 'Habitat'
anosim_habitat <- anosim(sample_dist, submatriz$Habitat[sample_indices], permutations = 999)

# Realizar ANOSIM para comparar entre las categorías de 'Depth'
anosim_depth <- anosim(sample_dist, submatriz$Depth[sample_indices], permutations = 999)

# Realizar ANOSIM para comparar entre las categorías de 'Degree'
anosim_degree <- anosim(sample_dist, submatriz$Degree[sample_indices], permutations = 999)

# Realizar PERMANOVA con 'Habitat', 'Depth' y 'Degree' como variables explicativas
permanova_result <- adonis(sample_data ~ Habitat + Depth + Degree, data = submatriz[sample_indices,], method = "euclidean", permutations = 999)


# Mostrar los resultados
print(anosim_habitat)
print(anosim_depth)
print(anosim_degree)
print(permanova_result)

