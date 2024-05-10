library(ggplot2)
library(dplyr)
library(gridExtra)

# data ---------

alldata <- readRDS("data/fishdata_prod-by-reef-allyears_1000.RDS")
data2010 <- readRDS("data/fishdata_product-by-reef-2010.RDS")
data2023 <- readRDS("data/fishdata_product-by-reef-2023.RDS")


# Graficar---------
ggplot(alldata, aes(x = Habitat, y = Year, fill = Depth2)) +
  geom_boxplot() +
  facet_wrap(~Degree) +
  labs(title = "Boxplot de Distance por Habitat separado por Latitude y Depth2",
       x = "Habitat",
       y = "Distance")


# Crear boxplots
ltem <- alldata |> 
  filter(Habitat %in% c("BLOQUES", "PARED"))

# 
# Biom = sum(Biom)/250,# (kg ha^−1) # porqué 500?
# Prod = sum(Prod)/250,#g d^−1 ha^−1
# #Individual Biomass
# IndBiom = (W/Area),
# Productivity = (Prod/Biom)*100) %>%
# (Biom = mean(Biom),
#        Prod = mean(Prod),
#        Productivity = mean(Productivity))  
# 
#     log10ProdB = Productivity,# % per day
#     log10Biom = log10(Biom+1), #(g m -2)
#     log10Prod = log10(Prod+1), # (g m-2 d-1)




# Filtrar y organizar los datos
filtered_data <- ltem %>%
  filter(Year==2010) |> 
  filter(Depth2 %in% c("Shallow", "Deep"), Degree %in% c(23, 24, 25, 26))

# Crear el gráfico
ggplot(filtered_data, aes(x = Habitat, y = Prod, fill = Degree)) +
  geom_boxplot() +
  facet_grid(Depth2 ~ Degree, scales = "free", space = "free") +
  labs(title = " ", x = "Habitat", y = "Productivity") +
  theme_minimal()



# ggsave("figs/boxplots_habitat-depth2-degree_Prod.png", width = 8.5, height = 4.5, dpi=1000)





s23 <- ltem %>%
  filter(Depth2 == "Shallow", Degree == 23) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = as.factor(Habitat))) +
  geom_boxplot() +
  labs(title = "Shallow, Degree 23", x = NULL, y = " ") +
  theme(legend.position = "none") +  # Elimina la leyenda de colores
  guides(fill = FALSE) +  # Elimina cualquier guía de color restante
  theme_minimal()


s24 <- ltem %>%
  filter(Depth2 == "Shallow", Degree == 24) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = as.factor(Habitat))) +
  geom_boxplot() +
  facet_wrap(~ Depth2) +
  labs(title = "24", x = NULL, y = NULL) +
  theme(legend.position = "none") +  
  guides(fill = FALSE) +
  theme_minimal()

s25 <- ltem %>%
  filter(Depth2 == "Shallow", Degree == 25) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = as.factor(Habitat))) +
  geom_boxplot() +
  labs(title = "25", x = NULL, y = "Biomass Turnover (P/B × 100 % per day)") +
  theme(legend.position = "none") +  
  guides(fill = FALSE) +
  theme_minimal()

s26 <- ltem %>%
  filter(Depth2 == "Shallow", Degree == 26) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = as.factor(Habitat))) +
  geom_boxplot() +
  labs(title = "26", x = "Habitat", y = " ") +
  theme(legend.position = "none") +  
  guides(fill = FALSE)+ 
  theme_minimal()

d23 <- ltem %>%
  filter(Depth2 == "Deep", Degree == 23) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = as.factor(Habitat))) +
  geom_boxplot() +
  labs(title = "Deep, Degree 23", x = NULL) +
  theme(legend.position = "none") +  
  guides(fill = FALSE) +
  theme_minimal()

d24 <- ltem %>%
  filter(Depth2 == "Deep", Degree == 24) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = Habitat)) +
  geom_boxplot() +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(legend.position = "none") +  
  guides(fill = FALSE) +
  theme_minimal()

d25 <- ltem %>%
  filter(Depth2 == "Deep", Degree == 25) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = Habitat)) +
  geom_boxplot() +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(legend.position = "none") +  
  guides(fill = FALSE) +
  theme_minimal()

d26 <- ltem %>%
  filter(Depth2 == "Deep", Degree == 26) %>%
  ggplot(aes(x = Habitat, y = Productivity, fill = Habitat)) +
  geom_boxplot() +
  labs(title = NULL, x = "Habitat", y = NULL) +
  guides(fill = FALSE) +
  theme_minimal()


grid.arrange(s23, d23, s24, d24, s25, d25, s26, d26, ncol = 2)
