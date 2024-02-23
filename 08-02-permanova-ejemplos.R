library(vegan)


# ejemplo 1 ----------

data(dune)
data(dune.env)

dune.dist <- vegdist(dune, method = "bray")

# default test by terms
dune.div <- adonis2(dune ~ Management * A1, data = dune.env, permutations = 999, method = "bray")

dune.div

# overall tests
adonis2(dune ~ Management * A1, data = dune.env, permutations = 999, method = "bray", by = NULL)

# Calcula la dispersión multivariante de los grupos
dispersion <- betadisper(dune.dist, group = dune.env$Management)

# Realiza una prueba de permutación sobre los resultados del análisis
perm_test <- permutest(dispersion)

# Grafica los resultados con elipses
plot(dispersion, hull = FALSE, ellipse = TRUE) ##sd ellipse


# ejemplo 2 PERMANOVA y ANOSIM------ 


# Calcular la matriz de distancia de Bray-Curtis
bray_dist <- vegdist(log(SalinasBentos + 1), method = "bray")

# Realizar ANOSIM para comparar entre las categorías de 'Time'
anosim_time <- anosim(bray_dist, Time, permutations = 999)

# Realizar ANOSIM para comparar entre las categorías de 'Zone'
anosim_zone <- anosim(bray_dist, Zone, permutations = 999)

# Realizar PERMANOVA con 'Time' y 'Zone' como variables explicativas
permanova_result <- adonis(SalinasBentos ~ Time + Zone, method = "euclidean", permutations = 999)

# Mostrar los resultados
print(anosim_time)
print(anosim_zone)
print(permanova_result)





library(vegan)
unique(ltem$Depth2)

# Realizar ANOVA ----------------
modelo_anova <- aov(sstmean ~ Habitat, data = ltem)

# Resumen del ANOVA
summary(modelo_anova)


# Test de Kruskal-Wallis----------
resultado_kruskal <- kruskal.test(Depth2 ~ Habitat, data = ltem)

# Ver los resultados
print(resultado_kruskal)






 # PCA ------------
library(stats)

# Seleccionar las variables numéricas relevantes para el PCA
datos_pca <- ltem[, c("Depth", "sstmean", "Degree")]

# Estandarizar los datos (opcional pero recomendado para PCA)
datos_pca <- scale(datos_pca)

# Realizar PCA
resultado_pca <- prcomp(datos_pca, scale = TRUE)

# Resumen del PCA
summary(resultado_pca)

# Gráfico de varianza explicada por cada componente
plot(resultado_pca, type = "l")



# Convertir la variable categórica "Habitat" en variables ficticias
encoded_habitat <- model.matrix(~ Habitat - 1, data = ltem)

# Combinar las variables numéricas relevantes y las variables ficticias
datos_pca <- cbind(datos_pca, encoded_habitat)

# Estandarizar los datos (opcional pero recomendado para PCA)
datos_pca <- scale(datos_pca)

# Realizar PCA
resultado_pca <- prcomp(datos_pca, scale = TRUE)

# Resumen del PCA
summary(resultado_pca)

# Gráfico de varianza explicada por cada componente
plot(resultado_pca, type = "l")

