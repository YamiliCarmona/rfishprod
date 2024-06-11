library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
# DATA-------------

fishdata <- readRDS("data/tabla/fish_productivity_data.RDS") |> 
  filter(!(reef == "SAN_JOSE_ANIMAS_PINACULOS" & year == 2014 & species == "Hoplopagrus guentherii"))
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
str(matri)


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

# saveRDS(matri, file = "data/matriz_prod_variables.RDS")



# Sitios más productivos ---

reef_max_prod <- data %>%
  group_by(reef) %>%
  summarize(avg_prod = mean(prod)) %>%
  top_n(1, wt = avg_prod)

print(reef_max_prod)


top_sites_prod <- fishdata %>%
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

# Tendencias de la productividad --------

# 1. Calcular la productividad promedio para cada arrecife y año
prod_trends <- data %>%
  filter(reef %in% c("DANZANTE_PUNTA_NOROESTE", "CARMEN_TINTORERA", 
                     "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_BALLENA", "CARMEN_PUNTA_LOBOS",
                     "MONSERRAT_GALERAS", "MORROS_CABO_PULMO")) |> 
  group_by(reef, year) %>%
  # group_by(year, reef, species, quantity, size) %>%
  summarize(avg_prod = mean(prod)) %>%
  ungroup()

# 2. Ordenar los arrecifes por su productividad promedio
reef_order <- prod_trends %>%
  group_by(reef) %>%
  summarize(mean_prod = mean(avg_prod)) %>%
  arrange(desc(mean_prod)) %>%
  pull(reef)

# 3. Obtener los valores máximos y mínimos de la productividad promedio para establecer los límites del eje y
y_min <- min(prod_trends$avg_prod)
y_max <- max(prod_trends$avg_prod)

# 4. Graficar las tendencias de productividad para los arrecifes más productivos
ggplot(prod_trends, aes(x = year, y = avg_prod, color = reef)) +
  geom_line() +
  scale_color_viridis_d(option = "plasma") +  # Ajustar colores según preferencia
  theme_minimal() +
  labs(x = "Año", y = "Productividad promedio", title = " ") +
  theme(legend.position = "none") +  # Suprimir leyenda si hay muchos arrecifes
  facet_wrap(~ reef, scales = "free", ncol = 2) +  # Ajustar escalas libres
  scale_x_continuous(breaks = seq(min(prod_trends$year), max(prod_trends$year), by = 8)) +
  ylim(y_min, y_max)  # Establecer límites del eje y



# ggsave("figs/tendencia_fishprod_reefs_prod.png", width = 12, height = 8, dpi=1000)



# Especies -------
# ¿Cuál es la especie más productiva?
# y la menos? -------
spp <- fishdata |> 
  group_by(year, degree, region, island, reef, depth2, transect, species)

# Identificación de la especie más productiva -----------
species_max_prod <- fishdata %>%
  # group_by(year, degree, region, island, reef, depth2, transect, species) |> 
  # summarize(sum_prod = sum(prod)) %>%
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

# 1 Caranx sexfasciatus       201.  piscivores
# 2 Lutjanus inermis          147.  macro-invertivores
# 3 Haemulon steindachneri    116. macro-invertivores
# 4 Abudefduf troschelii       83.2 planktivores
# 5 Azurina atrilobata         73.3 planktivores


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


# Filtrar las observaciones de las 5 especies más productivas
top_species_data <- fishdata %>%
  filter(species %in% top_species_prod$species)

# Calcular la productividad promedio para cada especie y año
species_prod_trends <- top_species_data %>%
  # group_by(year, degree, region, island, reef, depth2, transect, species) |>
  # summarize(sum_prod = sum(prod)) %>%
  group_by(species, year) %>%
  summarize(avg_prod = mean(prod)) %>%
  ungroup()

y_max <- max(species_prod_trends$avg_prod, na.rm = TRUE)

# Graficar las tendencias de productividad para las 5 especies más productivas a lo largo de los años
ggplot(species_prod_trends, aes(x = year, y = avg_prod, color = species)) +
  geom_line() +
  scale_color_viridis_d(option = "plasma") +  # Ajustar colores según preferencia
  theme_minimal() +
  facet_wrap(~ species, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y para cada faceta
  ylim(0, y_max) +  # Establecer el mismo límite superior en el eje Y para todos los gráficos
  labs(x = "Año", y = "Productividad", title = " ") +
  theme(legend.position = " ") 


# ggsave("figs/tendencia_fishprod_spp2.png", width = 12, height = 8, dpi=1000)


# grupos funcionales------

unique(fishdata$functional_groups)

functional_groups_prod <- fishdata %>%
  group_by(functional_groups, species) %>%
  summarize(avg_prod = mean(prod))

# 2. Identificar los grupos funcionales con la productividad promedio más alta y más baja
most_productive_functional_group <- functional_groups_prod %>%
  filter(avg_prod == max(avg_prod))

most_productive_functional_group

least_productive_functional_group <- functional_groups_prod %>%
  filter(avg_prod == min(avg_prod))

least_productive_functional_group


# Seleccionar los cinco grupos funcionales más productivos
top_functional_groups <- functional_groups_prod %>%
  arrange(desc(avg_prod)) %>%
  head(5) %>%
  pull(functional_groups)

# Filtrar los datos para incluir solo las observaciones correspondientes a los cinco grupos funcionales
top_functional_groups_data <- fishdata %>%
  filter(functional_groups %in% top_functional_groups)

# Calcular la productividad promedio para cada año y grupo funcional
yearly_prod_by_group <- top_functional_groups_data %>%
  # group_by(year, degree, region, island, reef, depth2, transect, functional_groups) |>
  # summarize(sum_prod = sum(prod)) %>%
  group_by(year, functional_groups) %>%
  summarize(avg_prod = mean(prod))

colors <- c("planktivores" = "#1f77b4",         # Azul
            "detritivores" = "#ff7f0e",         # Naranja
            "grazers" = "#2ca02c",              # Verde
            "excavator/scraper" = "#d62728",    # Rojo
            "corallivore" = "#9467bd")          # Morado


y_max <- max(yearly_prod_by_group$avg_prod, na.rm = TRUE)

# Graficar las tendencias de productividad a lo largo de los años para los cinco grupos funcionales más productivos
ggplot(yearly_prod_by_group, aes(x = year, y = avg_prod, color = functional_groups)) +
  geom_line() +
  labs(x = "Año", y = "Productividad", title = " ") +
  theme_minimal() +
  facet_wrap(~ functional_groups, scales = "free", ncol = 2) +
  ylim(0, y_max) +
  scale_color_manual(values = colors) + 
  # scale_color_viridis_d(option = "plasma") +  # Puedes ajustar los colores según tu preferencia
  theme(legend.position = " ")  # Coloca la leyenda en la parte superior

# ggsave("figs/tendencia_fishprod_functional_groups_2.png", width = 12, height = 8, dpi=1000)


# años más y menos prod -------

# Calcular la productividad promedio para cada año
yearly_prod <- data %>%
  group_by(year) %>%
  summarize(avg_prod = mean(prod))

# 4. Identificar los años con la productividad promedio más alta y más baja
most_productive_year <- yearly_prod %>%
  filter(avg_prod == max(avg_prod))

least_productive_year <- yearly_prod %>%
  filter(avg_prod == min(avg_prod))

most_productive_year
least_productive_year


# relacion con las variables-------


unique(matri$reef)
 
# galeras <- matri |> 
#   filter(reef == "MONSERRAT_GALERAS")

# CORONADO_LAJAS CORONADO_PUNTA_BLANCA  CORONADO_MONO 
# MORROS_CABO_PULMO 
# MONSERRAT_PUNTA_SURESTE, MONSERRAT_GALERAS MONSERRAT_REINITA
# CANTILES_CABO_PULMO BAJO_CABO_PULMO ISLOTE_CABO_PULMO


# Lista de sitios de interés -----

sitios_interes <- c("DANZANTE_PUNTA_NOROESTE", "CARMEN_TINTORERA", 
                    "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_BALLENA", "CARMEN_PUNTA_LOBOS",
                    "MONSERRAT_GALERAS", "MORROS_CABO_PULMO")

# c("ESPIRITU_SANTO_ISLOTES_NORTE", "DANZANTE_PUNTA_NOROESTE", 
#                     "CORONADO_LAJAS",# "CORONADO_PUNTA_BLANCA", "CORONADO_MONO", 
#                     "MORROS_CABO_PULMO", #"MONSERRAT_PUNTA_SURESTE", 
#                     "MONSERRAT_GALERAS"#, "MONSERRAT_REINITA", 
#                     # "CANTILES_CABO_PULMO", "BAJO_CABO_PULMO", "ISLOTE_CABO_PULMO"
# )


  
# Filtrar el conjunto de datos para incluir solo los sitios de interés
datos_interes <- matri[matri$reef %in% sitios_interes, ]

# Realizar un análisis exploratorio de tendencias para cada variable


# Obtener los valores máximos para establecer los límites en el eje Y
max_prod <- max(datos_interes$prod, na.rm = TRUE)
max_sst <- max(datos_interes$mean_sst, na.rm = TRUE)
max_chla <- max(datos_interes$mean_chla, na.rm = TRUE)

# Gráfico de productividad
plot_productividad <- ggplot(datos_interes, aes(x = year, y = prod, color = reef)) +
  geom_line() +
  labs(title = " ",
       x = "Año",
       y = "Productividad",
       color = " ") +  # Etiqueta de la leyenda
  ylim(0, max_prod) +  # Establecer límites en el eje Y
  facet_wrap(~ reef, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y
  theme_minimal() +
  theme(legend.position = " ")  # Mover la leyenda a la parte inferior

# Gráfico de SST
plot_sst <- ggplot(datos_interes, aes(x = year, y = mean_sst, color = reef)) +
  geom_line() +
  labs(title = "",
       x = "Año",
       y = "SST Mean",
       color = " ") +  # Etiqueta de la leyenda
  ylim(15, max_sst) +  # Establecer límites en el eje Y
  facet_wrap(~ reef, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y
  theme_minimal() +
  theme(legend.position = " ")  # Mover la leyenda a la parte inferior

# Gráfico de Clorofila
plot_chla <- ggplot(datos_interes, aes(x = year, y = mean_chla, color = reef)) +
  geom_line() +
  labs(title = " ",
       x = "Año",
       y = "Clorofila a ",
       color = " ") +  # Etiqueta de la leyenda
  ylim(0, max_chla) +  # Establecer límites en el eje Y
  facet_wrap(~ reef, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y
  theme_minimal() +
  theme(legend.position = " ")  # Mover la leyenda a la parte inferior

# Visualizar los gráficos
plot_productividad
plot_sst
plot_chla


# Gráfico de productividad y SST ------------
plot_prod_sst <- ggplot(matrix, aes(x = year)) +
  geom_point(aes(y = prod, color = "Productividad"), size = 1) +
  geom_point(aes(y = mean_sst, color = "SST Mean"), size = 1) +
  # facet_wrap(~ reef, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y
  ylim(0, max_y) +
  labs(title = " ",
       x = "Año",
       y = " ",
       color = " ") +  # Etiqueta de la leyenda
  scale_color_manual(values = c("Productividad" = "blue", "SST Mean" = "red")) +
  theme_minimal() +
  theme(legend.position = " ")  # Mover la leyenda a la parte inferior

plot_prod_sst

# Gráfico de productividad y Clorofila--------
plot_prod_chla <- matrix |> 
  group_by(year, reef, mean_chla) |> 
  summarize(avg_prod = mean(prod)) |> 
  ggplot(aes(x = year)) +
  geom_point(aes(y = prod, color = "Productividad"), size = 1) +
  geom_point(aes(y = mean_chla, color = "Clorofila a"), size = 1) +
  # facet_wrap(~ reef, scales = "free_y", ncol = 2) +  # Escala libre en el eje Y
  ylim(0, 6) +
  labs(title = " ",
       x = "Año",
       y = " ",
       color = " ") +  # Etiqueta de la leyenda
  scale_color_manual(values = c("Productividad" = "blue", "Clorofila a" = "green")) +
  theme_minimal() +
  theme(legend.position = " ")  # Mover la leyenda a la parte inferior

plot_prod_chla


# Gráfica de cambio de temperatura a lo largo de los años

matrix$year <- factor(matrix$year)


sst<- matrix |> 
  group_by(year) |> 
  summarize(mean_sst = mean(mean_sst)) |>
  ggplot(aes(x = year, y = mean_sst, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(
    title = "",
    x = "Year",
    y = "sstmean"
  ) +
  theme_minimal()


chla <- matrix |> 
  group_by(year) |> 
  summarize(mean_chla = mean(mean_chla)) |>
  ggplot(aes(x = year, y = mean_chla, group = 1)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  labs(
    title = "",
    x = "Year",
    y = "mean_chla"
  ) +
  theme_minimal()



hg <- matrix |> 
  group_by(year) |> 
  summarize(human_gravity = mean(human_gravity)) |>
  ggplot(aes(x = year, y = human_gravity, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "",
    x = "Year",
    y = "human gravity"
  ) +
  theme_minimal()



pd <- matrix %>% 
  group_by(year) %>% 
  summarize(prod = mean(prod)) %>% 
  ggplot(aes(x = year, y = prod, group = 1)) +  # Agrupar todos los puntos en una única línea
  geom_point(color = "brown") +
  geom_line(color = "brown") +
  labs(
    title = "",
    x = "Year",
    y = "Productivity (g d^−1 ha^−1)"
  ) +
  theme_minimal()


sst
chla
hg
pd


library(patchwork)



# Combinar los gráficos en una sola figura
combined_plot <- sst + chla + hg + pd + 
  plot_layout(ncol = 1) + 
  plot_annotation(title = " ",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

# Mostrar el gráfico combinado
print(combined_plot)

# Guardar el gráfico combinado en un archivo
ggsave("figs/combined_var.png", combined_plot, width = 12, height = 16, dpi = 300)




mpa <- matrix %>%
  # group_by(year, degree, region, island, reef, depth2, transect, productivity) %>%
  # summarise(product = sum(productivity)) %>%
  group_by(year, protection) %>%
  summarise(prod = mean(prod))

mpa$year <- factor(mpa$year)

ggplot(mpa, aes(x = year, y = prod, fill = year)) +
  # geom_boxplot() +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  facet_wrap(~ protection, scales = "free", ncol = 1)  +
  theme(legend.position = "none")+
  ylim(0, max(mpa$prod, na.rm = TRUE)) 

# ggsave("figs/prod_protection_mpa.png", width = 12, height = 8, dpi=1000)

matrix <- matri %>%
  filter(!is.na(prod) & !is.na(mean_sst) & !is.na(mean_chla) & !is.na(human_gravity) & !is.na(protection))

# Gráfico de dispersión para productividad vs SST
plot_prod_sst <- ggplot(matrix, aes(x = mean_sst, y = prod, color = reef)) +
  geom_point() +
  labs(title = " ",
       x = "SST",
       y = "Productividad",
       color = " ") +
  theme_minimal() +
  theme(legend.position = " ")


plot_prod_sst

# Gráfico de dispersión para productividad vs Clorofila
plot_prod_chla <- ggplot(matrix, aes(x = mean_chla, y = prod, color = reef)) +
  geom_point() +
  labs(title = " ",
       x = "Clorofila a",
       y = "Productividad",
       color = " ") +
  theme_minimal() +
  theme(legend.position = " ")

# Gráfico de dispersión para productividad vs Human Gravity
plot_prod_human_gravity <- ggplot(datos_interes, aes(x = human_gravity, y = prod, color = reef)) +
  geom_point() +
  labs(title = " ",
       x = "Human Gravity",
       y = "Productividad",
       color = " ") +
  theme_minimal() +
  theme(legend.position = " ")

# Gráfico de dispersión para productividad vs Protection
plot_prod_protection <- ggplot(matrix, aes(x = protection, y = prod, color = reef)) +
  geom_point() +
  labs(title = " ",
       x = "Protección",
       y = "Productividad",
       color = " ") +
  theme_minimal() +
  theme(legend.position = " ")


plot_prod_protection



ggplot(data, aes(x = productivity, y = prod)) +
  # geom_boxplot() +
  geom_point( ) +
  # geom_line(color = "red", group = 1) +
  labs(title = " ", x = "turnover", fill = "Year") +
  ylab("Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  theme(legend.position = "none")  



# Cómo se ha portado la prod y correlar con sst  --------


# Identificación del sitio con mayor gravedad humana ----------
site_max_gravity <- matri %>%
  filter(human_gravity == max(human_gravity))

print(site_max_gravity)

max_gravity_row <- matri[which.max(matri$human_gravity), ]
max_gravity_row$reef
max_gravity_row$year



ggplot(matri, aes(x = mean_sst, y = prod)) +
  geom_point() +
  labs(x = "SST", y = "Producción") +
  ggtitle(" ")

cor(matri$mean_sst, matri$prod)
t.test(matri$prod)
t.test(matri$mean_sst)

model <- lm(prod ~ mean_sst + mean_chla + human_gravity, data = matri)
summary(model)




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
  labs(title = " ") +
  theme_minimal()

ggplot(matri, aes(x = mean_sst)) + 
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  labs(title = " ") +
  theme_minimal()

# Paso 2: Correlación entre variables ----------
# Calcula la correlación entre prod y mean_sst
correlation <- cor(matri$prod, matri$mean_sst)
print(correlation)

# Visualiza la relación entre prod y mean_sst
ggplot(matri, aes(x = mean_sst, y = prod)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = " ") +
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
