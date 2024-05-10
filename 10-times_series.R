library(ggplot2)
library(dplyr)
library(gridExtra)

# DATA-------------

fishdata <- readRDS("data/tabla/fish_productivity_data.RDS")
alldata <- readRDS("data/fishdata_prod-by-reef-allyears_1000.RDS")
# data2010 <- readRDS("data/fishdata_product-by-reef-2010.RDS")
# data2023 <- readRDS("data/fishdata_product-by-reef-2023.RDS")

# fdt <- readRDS("data/fish_data_prod-by-spp.RDS")
# fpd <- readRDS("data/fish_datagr_prod-by-species-allyears.RDS") #sin turnover

str(fishdata)

fish <- fishdata |> 
  distinct(species, commercial)

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


# quitar los chanos o spp del 2014 ---

# chanos <- data |>
#   filter(year == 2014, degree == "25", protection == "Not Protected", commercial == "yes")

data <- data |> 
  filter(!(reef == "SAN_JOSE_ANIMAS_PINACULOS" & year == 2014 & species == "Hoplopagrus guentherii"))


# Series de tiempo
# tendencia de la biomasa de peces en distintas latitudes

# Agarrar la biomasa de fabio y ver que tal la productividad
# 
# data <- fishdata |> 
#   filter(!is.na(commercial)) |> 
#   group_by(year, degree, region, protection, reef, habitat, depth2, transect, functional_groups, commercial)%>%
#   summarise(biom = sum(biom), prod = sum(prod), productivity = sum(productivity))


# # de sur a norte
# data$degree <- factor(data$degree, levels = c(23, 24, 25, 26))

#  de norte a sur --------
data$degree <- factor(data$degree, levels = c(26, 25, 24, 23))
data$year <- factor(data$year)

# biomass--------

biom <- data %>%
  mutate(biomass = (quantity * a * (size^b)) / (area * 100)) %>%
  group_by(year, degree, region, island, reef, depth2, transect) %>%
  summarise(biomass = sum(biomass)) %>%
  group_by(year) %>%
  summarise(biomass = mean(biomass))



# Graficar la tendencia de la biomasa promedio por año
ggplot(biom, aes(x = as.factor(year), y = biomass)) +
  geom_point(color = "blue", fill = "lightblue") +  # Boxplot para la distribución de la biomasa
  geom_line(color = "red", group = 1) +  # Línea de tendencia
  labs(title = " ",
       x = "Año",
       y = "Biomasa promedio (ton/ha)") +
  theme_classic()

# ggsave("figs/biomass_favoretto.png", width = 12, height = 8, dpi=1000)

#   tendencias-----------
# biomasa
# prod ------
# turnover
# promedio x año

# Ajustar los ejes------

p0 <- ggplot(biom, aes(x = as.factor(year), y = biomass)) +
  geom_point(color = "blue", fill = "lightblue") +  # Boxplot para la distribución de la biomasa
  geom_line(color = "red", group = 1) +  # Línea de tendencia
  labs(title = " ",
       x = "Año",
       y = "Biomasa promedio (ton/ha)") +
  theme_classic()


p1 <- ggplot(data, aes(x = year, y = biom, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = "Biomass (kg ha^−1)") +
  theme_classic() +
  theme(legend.position = "none")

p2 <- ggplot(data, aes(x = year, y = prod, fill = year)) +
  geom_point() +
  labs(title = " ", x = " ", fill = "Year") +
  ylab("Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  theme(legend.position = "none")  


p3 <- ggplot(data, aes(x = year, y = productivity, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = "Year", y = "Turnover (P/B × 100 % per day)") +
  theme_classic() +
  theme(legend.position = "none")


# Determina el rango máximo de biomasa en todos los datos
max_biomass <- max(max(biom$biomass), max(data$prod), max(data$productivity))

# Gráfico p0
p0 <- ggplot(biom, aes(x = as.factor(year), y = biomass)) +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ",
       x = "Año",
       y = "Biomasa promedio (ton/ha)") +
  theme_classic() +
  ylim(0, max_biomass)  # Establecer el mismo rango para el eje y


# Gráfico p2
p2 <- ggplot(data, aes(x = year, y = prod, fill = year)) +
  geom_boxplot() +  # Agregado
  labs(title = " ", x = " ", fill = "Year") +
  ylab("Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0, max_biomass)  # Establecer el mismo rango para el eje y

# Gráfico p3
p3 <- ggplot(data, aes(x = year, y = productivity, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = "Year", y = "Turnover (P/B × 100 % per day)") +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0, max_biomass)  # Establecer el mismo rango para el eje y

# Mostrar los gráficos
p0
p2
p3

grid.arrange(p0, p2,  p3,  ncol = 1)

# ggsave("figs/tendencias_anuales_prod_turn.png", arrangeGrob(p2, p3, ncol = 1), width = 12, height = 8, dpi=1000)

# ggsave("figs/tendencias_anuales_biom_prod_turn_3.png", arrangeGrob(p0, p2, p3, ncol = 1), width = 10, height = 15, units = "in")




# degree ----------
deg <- data %>%
  mutate(biomass = (quantity * a * (size^b)) / (area * 100)) %>%
  group_by(year, degree, region, island, reef, depth2, transect) %>%
  summarise(biomass = sum(biomass)) %>%
  group_by(year, degree) %>%
  summarise(biomass = mean(biomass))

# Ajustar los ejes y del primer gráfico
p_deg <- ggplot(deg, aes(x = as.factor(year), y = biomass)) +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ",
       x = "Año",
       y = "Biomasa promedio (ton/ha)") +
  theme_classic() +
  facet_wrap(~ degree, scales = "free", ncol = 1) +
  ylim(0, max(deg$biomass, na.rm = TRUE))  # Establecer el mismo rango para el eje y en todos los gráficos

# ggsave("figs/biomass_favoretto_degree.png", width = 12, height = 8, dpi=1000)

de <- data %>%
  # group_by(year, degree, region, island, reef, depth2, transect, productivity) %>%
  # summarise(product = sum(productivity)) %>%
  group_by(year, degree) %>%
  summarise(prod = mean(prod), productivity = mean(productivity))

# Ajustar los ejes y del segundo gráfico
p_de <- ggplot(de, aes(x = year, y = prod, fill = year)) +
  # geom_boxplot() +
  geom_point() +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = " ", fill = "Year") +
  ylab("Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~ degree, scales = "free", ncol = 1) +
  ylim(0, max(de$prod, na.rm = TRUE))  # Establecer el mismo rango para el eje y en todos los gráficos

# ggsave("figs/prod_anual_degree.png", width = 12, height = 8, dpi=1000)

# Ajustar los ejes y del tercer gráfico
p_data <- ggplot(de, aes(x = year, y = productivity, color = as.factor(year))) +
  geom_point() +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = "Year", y = "Turnover (P/B × 100 % per day)", color = "Year") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(~ degree, scales = "free", ncol = 1) +
  ylim(0, max(de$productivity, na.rm = TRUE))    # Establecer el mismo rango para el eje y en todos los gráficos

# Mostrar los gráficos
print(p_deg)
print(p_de)
print(p_data)


# ggsave("figs/turnover_anual_degree.png", width = 12, height = 8, dpi=1000)

# MPAS Open Areas -------
mpas <- data %>%
  mutate(biomass = (quantity * a * (size^b)) / (area * 100)) %>%
  group_by(year, degree, region, island, protection, reef, depth2, transect) %>%
  summarise(biomass = sum(biomass)) %>%
  group_by(year, protection) %>%
  summarise(biomass = mean(biomass))

# Ajustar los ejes y del primer gráfico
p_mpas <- ggplot(mpas, aes(x = as.factor(year), y = biomass)) +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ",
       x = "Año",
       y = "Biomasa promedio (ton/ha)") +
  theme_classic() +
  facet_wrap(~ protection, scales = "free", ncol = 1) +
  ylim(0, max(mpas$biomass, na.rm = TRUE)) 

# ggsave("figs/biomass_favoretto_protection.png", width = 12, height = 8, dpi=1000)

mpa <- data %>%
  # group_by(year, degree, region, island, reef, depth2, transect, productivity) %>%
  # summarise(product = sum(productivity)) %>%
  group_by(year, protection) %>%
  summarise(prod = mean(prod), productivity = mean(productivity))

ggplot(mpa, aes(x = year, y = prod, fill = year)) +
  # geom_boxplot() +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  facet_wrap(~ protection, scales = "free", ncol = 1)  +
  theme(legend.position = "none")+
  ylim(0, max(mpa$prod, na.rm = TRUE)) 

# ggsave("figs/prod_protection.png", width = 12, height = 8, dpi=1000)

ggplot(mpa, aes(x = year, y = productivity, fill = year)) +
  # geom_boxplot() +
  geom_point(color = "blue", fill = "lightblue") +
  geom_line(color = "red", group = 1) +
  labs(title = " ", x = "Year", y = "Turnover (P/B × 100 % per day)") +
  theme_classic() +
  facet_wrap(~ protection, scales = "free", ncol = 1)  +
  theme(legend.position = "none")+
  ylim(0, max(mpa$productivity, na.rm = TRUE)) 

# ggsave("figs/turnover_protection.png", width = 12, height = 8, dpi=1000)



# spp comerciales -----------
# Pesca entre especies comerciales y no comerciales

data <- data |> 
  filter(!is.na(commercial)) 

ggplot(data, aes(x = year, y = prod, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  facet_wrap(~ commercial, scales = "free", ncol = 1)  +
  theme(legend.position = "none")

data <- fishdata |> 
  filter(!is.na(commercial)) |> 
  group_by(year, degree, region, protection, reef, habitat, depth2, transect, functional_groups, commercial)%>%
  summarise(biomass = sum(biom), product = sum(prod), turnover = sum(productivity))

data <- fishdata |>   
  mutate(w = a * (size^b),
         biom = w * quantity,
         prod = ifelse(mortality == T, (somatic_growth * quantity), 0)) %>%
  group_by(year, degree, region, protection, reef, habitat, depth2, transect, functional_groups, commercial, family, species) %>%
  summarise(biom = sum(biom) / sum(area), # (kg ha^−1)
            prod = sum(prod) / sum(area), # (g d^−1 ha^−1)
            productivity = (prod / biom) * 100,
            .groups = "drop") %>%
  group_by(year, commercial, .drop = TRUE) %>%
  summarise(biom = mean(biom), # Promedio de Biom
            prod = mean(prod), # Promedio de Prod
            productivity = mean(productivity),
            .groups = "drop") %>%
  ungroup()

data$year <- factor(data$year)



spc <- fishdata %>%
  filter(!is.na(commercial)) |>
  mutate(biomass = (quantity * a * (size^b)) / (area * 100)) %>%
  group_by(year, degree, region, island, protection, reef, depth2, transect, commercial) %>%
  summarise(biomass = sum(biomass)) %>%
  group_by(year, commercial) %>%
  summarise(biomass = mean(biomass))

ggplot(spc, aes(x = as.factor(year), y = biomass)) +
  geom_point(color = "blue", fill = "lightblue") +  
  geom_line(color = "red", group = 1) +  
  labs(title = " ",
       x = "Año",
       y = "Biomasa promedio (ton/ha)") +
  theme_classic()+
  facet_wrap(~ commercial, scales = "free", ncol = 1)  

# ggsave("figs/fishdata_biomass_favoretto_spp_comerciales.png", width = 12, height = 8, dpi=1000)



ggplot(data, aes(x = year, y = prod, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  facet_wrap(~ commercial, scales = "free", ncol = 1)  +
  theme(legend.position = "none")

# ggsave("figs/fishdata_prod_spp_comerciales.png", width = 12, height = 8, dpi=1000)

ggplot(data, aes(x = year, y = turnover, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = "Turnover (P/B × 100 % per day)") +
  theme_classic() +
  facet_wrap(~ commercial, scales = "free", ncol = 1)  +
  theme(legend.position = "none")

# ggsave("figs/fishdata_turnover_spp_comerciales.png", width = 12, height = 8, dpi=1000)


# grupos funcionales ---------


gf <- fishdata %>%
  filter(!is.na(commercial)) |>
  mutate(biomass = (quantity * a * (size^b)) / (area * 100)) %>%
  group_by(year, degree, region, island, protection, reef, depth2, transect, functional_groups) %>%
  summarise(biomass = sum(biomass)) %>%
  group_by(year, functional_groups) %>%
  summarise(biomass = mean(biomass))

ggplot(gf, aes(x = as.factor(year), y = biomass)) +
  geom_point(color = "blue", fill = "lightblue") +  
  geom_line(color = "red", group = 1) +  
  labs(title = " ",
       x = "Año",
       y = "Biomasa promedio (ton/ha)") +
  theme_classic()+
  facet_wrap(~ functional_groups, scales = "free", ncol = 2)  

# ggsave("figs/fishdata_biomass_favoretto_spp_functional_groups.png", width = 12, height = 8, dpi=1000)


data <- fishdata |>   
  mutate(w = a * (size^b),
         biom = w * quantity,
         prod = ifelse(mortality == T, (somatic_growth * quantity), 0)) %>%
  group_by(year, degree, region, protection, reef, habitat, depth2, transect, functional_groups, commercial, family, species) %>%
  summarise(biom = sum(biom) / sum(area), # (kg ha^−1)
            prod = sum(prod) / sum(area), # (g d^−1 ha^−1)
            productivity = (prod / biom) * 100,
            .groups = "drop") %>%
  group_by(year, functional_groups, .drop = TRUE) %>%
  summarise(biom = mean(biom), # Promedio de Biom
            prod = mean(prod), # Promedio de Prod
            productivity = mean(productivity),
            .groups = "drop") %>%
  ungroup()

data$year <- factor(data$year)

ggplot(data, aes(x = year, y = prod, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_classic() +
  facet_wrap(~ functional_groups, scales = "free", ncol = 2)  +
  theme(legend.position = "none")

# ggsave("figs/fishdata_prod_spp_functional_groups.png", width = 12, height = 8, dpi=1000)

ggplot(data, aes(x = year, y = turnover, fill = year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = "Turnover suma (P/B × 100 % per day)") +
  theme_classic() +
  facet_wrap(~ functional_groups, scales = "free", ncol = 2)  +
  theme(legend.position = "none")

# ggsave("figs/fishdata_turnover_spp_functional_groups.png", width = 12, height = 8, dpi=1000)







# -series de tiempo- biomasa

# Agrupar datos por año y latitud, y calcular la biomasa promedio
biomasa_promedio <- data %>%
  group_by(year, degree) %>%
  summarise(biomasa_promedio = mean(biom, na.rm = TRUE))

# Graficar las tendencias de la biomasa en diferentes latitudes
ggplot(biomasa_promedio, aes(x = year, y = biomasa_promedio, group = degree, color = degree)) +
  geom_line() +
  labs(title = "Tendencias de biomasa de peces en diferentes latitudes",
       x = "Año",
       y = "Biomasa promedio") +
  theme_minimal()


# Cargar los datos
data <- data
# Gráfico de dispersión de la biomasa vs. año
plot(data$year, data$biomass, xlab = "Año", ylab = "Biomasa", main = "Tendencia temporal de biomasa")
# Ajustar un modelo de regresión lineal
lm_model <- lm(biomass ~ year, data = data)
summary(lm_model)
# Interpretar coeficientes del modelo
coef(lm_model)


# Crear la figura
ggplot(data = fishdata, aes(x = year, y = biomass)) +
  # Agregar la línea de la tendencia de la biomasa de peces
  geom_line(color = "orange") +
  # Etiquetas de los ejes y título
  labs(x = "Año", y = "Biomasa de peces", title = "Tendencia de biomasa de peces") +
  # Tema
  theme_minimal() +
  theme(legend.position = "none")
