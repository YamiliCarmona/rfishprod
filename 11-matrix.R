library(ggplot2)
library(dplyr)
library(gridExtra)

# DATA-------------

fishdata <- readRDS("data/tabla/fish_productivity_data.RDS")
# alldata <- readRDS("data/fishdata_prod-by-reef-allyears_1000.RDS")
alldata <- readRDS("data/fishdata_prod-by-reef-allyears-sin-H-guentherii.RDS")
matriz <- readRDS("data/tabla/reef_matrix_v1-2024-03-27.RDS") |> 
  janitor::clean_names()

str(fishdata)

fish <- fishdata |> 
  distinct(species, commercial)

reef <- fishdata |> 
  distinct(reef, protection)


data <- alldata|> 
  janitor::clean_names() 

# Añadir la información de las especies y su estado comercial a alldata
data <- merge(data, fish, by = "species", all.x = TRUE)

# Añadir la información de los arrecifes y su estado de protección a alldata
data <- merge(data, reef, by = "reef", all.x = TRUE)

# Verificar el resultado
str(data)


# quitar spp del 2014, la hoja no fue encontrada ---

# chanos <- data |>
#   filter(year == 2014, degree == "25", protection == "Not Protected", commercial == "yes")

# data <- data |> 
#   filter(!(reef == "SAN_JOSE_ANIMAS_PINACULOS" & year == 2014 & species == "Hoplopagrus guentherii"))
# Datos confimados:
# galeras <- data |> 
#   filter(reef == "MONSERRAT_GALERAS", year == 2007)

mat <- data |> 
  distinct(reef, prod, protection, year)

matr <- merge(matriz, mat, by = c("reef", "year"), all.x = TRUE) |> 
  select(-protection_status)

unique(matri$year)

# Generar la matriz de productividad y variables ambientales y humanas ------

# Matriz : Reef, Prod, sst, chla, Human Gravity, Level protection, year
matri <- matr %>%
  # filter(!reef == "MONSERRAT_GALERAS", year == 2007) |> 
  select(reef, prod, mean_sst, mean_chla, human_gravity, protection, year, social_defi, edu_lag, geometry) |> 
  filter(!is.na(mean_chla) & !is.na(prod))


# Tendencias de la productividad --------
# Sitios más productivos ---
reef_max_prod <- fishdata %>%
  group_by(reef) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(1, wt = avg_prod)

print(reef_max_prod)

# Identificación del sitio con mayor productividad y análisis de las variables asociadas
# Por ejemplo, identificar el sitio con la mayor productividad
max_prod_site <- df[df$prod == max(df$prod), ]
# Mostrar información del sitio con la mayor productividad
print(max_prod_site)

top_sites_prod <- data %>%
  group_by(reef) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(5, wt = avg_prod) %>%  # Selecciona los 5 sitios con mayor productividad
  arrange(desc(avg_prod))  # Ordena de mayor a menor productividad

print(top_sites_prod)

# Sitios menos productivos -----

bottom_sites_prod <- data %>%
  group_by(reef) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(-5, wt = avg_prod) %>%  # Selecciona los 5 sitios con menor productividad
  arrange(desc(avg_prod))  # Ordena de mayor a menor productividad

print(bottom_sites_prod)

# Especies -------
# ¿Cuál es la especie más productiva?
# y la menos? -------
spp <- fishdata |> 
  group_by(year, degree, region, island, reef, depth2, transect, species)

# Identificación de la especie más productiva -----------
species_max_prod <- fishdata %>%
  group_by(species) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(1, wt = avg_prod)

print(species_max_prod)

top_species_prod <- fishdata %>%
  group_by(species) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(5, wt = avg_prod) %>%  # Selecciona las 5 especies con mayor productividad
  arrange(desc(avg_prod))  # Ordena de mayor a menor productividad

print(top_species_prod)


species_min_prod <- fishdata %>%
  filter(!species %in% c("Chaetodipterus zonatus", "Hyporthodus acanthistius", "Mugil curema", "Sarda orientalis", "Lutjanus colorado")) |> 
  group_by(species) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(1, wt = -avg_prod)  # Aquí usamos -avg_prod para encontrar la especie con la menor productividad

print(species_min_prod)

bottom_species_prod <- fishdata %>%
  # filter(!species %in% c("Chaetodipterus zonatus", "Hyporthodus acanthistius", "Mugil curema", "Sarda orientalis", "Lutjanus colorado")) |> 
  # group_by(year, degree, reef, depth2, transect, species) %>%
  group_by(year, species) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(-5, wt = avg_prod) %>%  # Selecciona las 5 especies con menor productividad
  arrange(desc(avg_prod))  # Ordena de mayor a menor productividad

print(bottom_species_prod)


# Elegir islotes (promediar los 3), morros cp, Loreto cornado.-----------------

unique(matri$reef)

galeras <- matri |> 
  filter(reef == "MONSERRAT_GALERAS")

# CORONADO_LAJAS CORONADO_PUNTA_BLANCA  CORONADO_MONO 
# MORROS_CABO_PULMO 
# MONSERRAT_PUNTA_SURESTE, MONSERRAT_GALERAS MONSERRAT_REINITA
# CANTILES_CABO_PULMO BAJO_CABO_PULMO ISLOTE_CABO_PULMO

library(dplyr)

# Lista de sitios de interés
sitios_interes <- c("ESPIRITU_SANTO_ISLOTES_NORTE", "DANZANTE_PUNTA_NOROESTE", 
                    "CORONADO_LAJAS",# "CORONADO_PUNTA_BLANCA", "CORONADO_MONO", 
                    "MORROS_CABO_PULMO", #"MONSERRAT_PUNTA_SURESTE", 
                    "MONSERRAT_GALERAS"#, "MONSERRAT_REINITA", 
                    # "CANTILES_CABO_PULMO", "BAJO_CABO_PULMO", "ISLOTE_CABO_PULMO"
)

# Filtrar el conjunto de datos para incluir solo los sitios de interés
datos_interes <- matri[matri$reef %in% sitios_interes, ]

# Realizar un análisis exploratorio de tendencias para cada variable

# Gráfico de línea para la productividad a lo largo del tiempo para cada sitio de interés
plot_productividad <- ggplot(datos_interes, aes(x = year, y = prod, color = reef)) +
  geom_line() +
  labs(title = "Tendencia de la productividad a lo largo del tiempo",
       x = "Año",
       y = "Productividad") +
  theme_minimal()

# Gráfico de línea para la temperatura media superficial del mar (mean_sst) a lo largo del tiempo para cada sitio de interés
plot_sst <- ggplot(datos_interes, aes(x = year, y = mean_sst, color = reef)) +
  geom_line() +
  labs(title = "Tendencia de la temperatura media superficial del mar a lo largo del tiempo",
       x = "Año",
       y = "Temperatura media superficial del mar") +
  theme_minimal()

# Gráfico de línea para la clorofila a media (mean_chla) a lo largo del tiempo para cada sitio de interés
plot_chla <- ggplot(datos_interes, aes(x = year, y = mean_chla, color = reef)) +
  geom_line() +
  labs(title = "Tendencia de la clorofila a media a lo largo del tiempo",
       x = "Año",
       y = "Clorofila a media") +
  theme_minimal()


# Visualizar los gráficos
plot_productividad
plot_sst
plot_chla

# Cómo se ha portado la prod y correlar con sst  --------

# ¿Son significativas? La variables
#   ¿Qué afectan? Positiva o negativamente? ¿Aumenta o disminuye?
#   ¿Cómo se relacionan?
#   ¿Cuál es el sitio con más productividad y por qué?
#   Fase de exploración (1 semana)
# Todos los sitios, + prod, +  turnover
# ¿Cuál es el sitio que tiene mayor gravedad humana? Año del indice?

# Identificación del sitio con mayor gravedad humana ----------
site_max_gravity <- matri %>%
  filter(human_gravity == max(human_gravity))

print(site_max_gravity)
# Plantear un análisis e interpretar el resultado



ggplot(matri, aes(x = mean_sst, y = prod)) +
  geom_point() +
  labs(x = "Temperatura superficial del mar (SST)", y = "Producción") +
  ggtitle("Relación entre Producción y SST")

cor(matri$mean_sst, matri$prod)
t.test(matri$prod)
t.test(matri$mean_sst)

model <- lm(prod ~ mean_sst + mean_chla + human_gravity, data = matri)
summary(model)


max_gravity_row <- matri[which.max(matri$human_gravity), ]
max_gravity_row$reef
max_gravity_row$year


species_avg_prod <- aggregate(prod ~ species, data = fishdata, FUN = mean)
max_species <- species_avg_prod[which.max(species_avg_prod$prod), ]
max_species$species


# Carga de paquetes necesarios
library(tidyverse) # Para manipulación de datos y visualización
library(sf)        # Para trabajar con datos espaciales
library(lmtest)    # Para realizar pruebas de hipótesis en modelos de regresión lineal
library(ggplot2)   # Para graficar los resultados

# Paso 1: Análisis descriptivo
# Estadísticas descriptivas básicas
summary(matri)

# Visualización de la distribución de las variables
ggplot(matri, aes(x = prod)) + 
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de la producción") +
  theme_minimal()

ggplot(matri, aes(x = mean_sst)) + 
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = "Distribución de la temperatura superficial del mar") +
  theme_minimal()

# Paso 2: Correlación entre variables ----------
# Calcula la correlación entre prod y mean_sst
correlation <- cor(matri$prod, matri$mean_sst)
print(correlation)

# Visualiza la relación entre prod y mean_sst
ggplot(matri, aes(x = mean_sst, y = prod)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre producción y temperatura superficial del mar") +
  theme_minimal()

# Paso 3: Pruebas de significancia --------
# Prueba de correlación entre prod y mean_sst
cor_test <- cor.test(matri$prod, matri$mean_sst)
print(cor_test)

# Prueba de regresión lineal entre prod y mean_sst
model <- lm(prod ~ mean_sst, data = matri)
summary(model)

# Paso 4: Análisis de regresión ---------
# Visualización de la regresión lineal
ggplot(matri, aes(x = mean_sst, y = prod)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Regresión lineal entre producción y temperatura superficial del mar") +
  theme_minimal()



# Paso 1: División de sitios en clases de gestión y cálculo de métricas
# (Omitiré esta parte ya que no proporcionaste los datos específicos)

# Paso 2: ANOVA
# Supongamos que tienes una variable de respuesta llamada "biomasa_rel" que contiene la biomasa relativa
# y una variable llamada "clase_gestion" que contiene las clases de gestión
modelo_anova <- aov(biomass ~ protection, data = data)

# Paso 3: Comparaciones múltiples
comparaciones <- TukeyHSD(modelo_anova)

# Paso 4: Análisis de sensibilidad
# (Omitiré esta parte ya que implica ajustar umbrales y evaluar modelos)

# Paso 5: Modelado de clases de gestión
# Supongamos que tienes un dataframe llamado "datos_modelo" con las variables ambientales y socioeconómicas
library(ranger)

modelo_rf <- ranger(clase_gestion ~ ., data = datos_modelo, num.trees = 1000, mtry = 3)

# Visualización de la importancia de las variables
importance <- importance(modelo_rf)
print(importance)

# Visualización de los resultados del modelo
predicciones <- predict(modelo_rf, datos_modelo, type = "response")



plot(matri$prod ~ matri$mean_sst, xlab = "Temperatura Superficial del Mar", ylab = "Productividad", main = "Productividad vs. SST")
hist(matri$prod, xlab = "Productividad", main = "Histograma de Productividad")
library(mgcv)
modelo_gam <- gam(prod ~ mean_sst + mean_chla + human_gravity + social_defi + edu_lag, data = matri)
summary(modelo_gam)

library(rpart)
arbol <- rpart(prod ~ mean_sst + mean_chla + human_gravity + social_defi + edu_lag, data = matri)
plot(arbol)


# Calcula estadísticas descriptivas básicas
summary(matri)

# Visualiza la relación entre variables utilizando gráficos, como gráficos de dispersión o histogramas
plot(matri$variable1, matri$variable2)  # Reemplaza "variable1" y "variable2" con los nombres de las variables que deseas comparar

# Cargar las bibliotecas necesarias
library(sf)  # Para manejar datos espaciales
library(ggplot2)  # Para visualización de datos

# Cargar los datos en un objeto de data frame
df <- as.data.frame(matri)

# Resumen de los datos
summary(df)

# Análisis exploratorio
# Por ejemplo, gráfico de dispersión de Productividad vs. Temperatura media del agua
ggplot(df, aes(x = mean_sst, y = prod)) +
  geom_point() +
  labs(x = "Temperatura media del agua", y = "Productividad") +
  ggtitle("Dispersión de Productividad vs. Temperatura media del agua")

# Modelo estadístico (GAM)
# Por ejemplo, ajustar un modelo GAM para entender la relación entre la productividad y las variables ambientales
library(mgcv)  # Para ajustar modelos GAM
gam_model <- gam(prod ~ mean_sst + mean_chla + human_gravity + protection + year, data = df)
summary(gam_model)

# Análisis de redundancia
# Por ejemplo, realizar un análisis de redundancia entre la productividad y las variables ambientales y de manejo
# Suponiendo que 'protection' es una variable categórica que indica el nivel de protección del sitio
# Convertir 'protection' a factor
df$protection <- as.factor(df$protection)
# Realizar el análisis de redundancia
# (Debes instalar y cargar la biblioteca vegan para esto)
# install.packages("vegan")
library(vegan)
# Convertir 'protection' a una representación numérica
df$protection_numeric <- as.numeric(factor(df$protection, levels = unique(df$protection)))

# Ahora puedes realizar el análisis de redundancia
rd <- rda(df[, c("prod", "mean_sst", "mean_chla", "human_gravity", "protection_numeric", "year")])
summary(rd)

rd <- rda(df[, c("prod", "mean_sst", "mean_chla", "human_gravity", "protection", "year")])
summary(rd)
