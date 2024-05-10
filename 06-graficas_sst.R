library(ggplot2)
library(tidyr)

# Biomass & productivity per year 
# sumar la biomasa total
# sumar la productividad a las biomasas
# asociar la prod a la temperatura
# Correlacionar la biomasa total


datayear <- readRDS("data/datafishes-year.RDS") 
  # filter(Year == 2021) |> 
  # distinct(Reef, Species, Quantity)
data2 <- readRDS("data/datafishes2.RDS")

byreef <- readRDS("data/anegada-sst-yearly.RDS")

todo <- readRDS("data/fish-data-sst-avg.RDS")

str(byreef)

# Sumar la biomasa total y la productividad por año
sitio <- byreef |>
  # distinct(Reef, sstmean)
  # filter(Reef == "ANEGADA") |>
  group_by(Year, Reef, Transect, Depth, sstmean) %>%
  summarize(TotalBiom = sum(Biom),
            TotalProd = sum(Prod), #(g d^−1 ha^−1)
            IndBiomT = sum(IndBiom))


mean_sitios <- sitio |>
  group_by(Year) |>
  mutate(
    meanBiom = mean(TotalBiom),
    meanProd = mean(TotalProd),
    meanIndBiom = mean(IndBiomT)
  )

# Gráfica de biomasa
ggplot(byreef, aes(x = Year, y = log10Biom)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = " ",
    x = "Year",
    y = ""
  ) +
  theme_minimal()



# Gráfica de cambio de temperatura a lo largo de los años
ggplot(byreef, aes(x = Year, y = sstmean)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Temperatura",
    x = "Year",
    y = "Temperatura (sstmean)"
  ) +
  theme_minimal()





# Combinar las columnas relacionadas en una sola columna
combined_data <- gather(mean_sitios, key = "variable", value = "value", meanProd, sstmean)

# Gráfica combinada
ggplot(combined_data, aes(x = Year, y = value, color = variable, shape = variable)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Relación entre Biomasa y Temperatura",
    x = "Year",
    y = "Valor",
    color = "Variable",
    shape = "Variable"
  ) +
  theme_minimal()


# Gráfica de biomasa
biomasa_plot <- ggplot(todo, aes(x = Year, y = log10Biom)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "ANEGADA",
    x = "Year",
    y = "log10Biom (g m -2)"
  ) +
  theme_minimal()

# Gráfica de cambio de temperatura a lo largo de los años
temperatura_plot <- ggplot(todo, aes(x = Year, y = sstmean)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(
    title = "",
    x = "Year",
    y = "sstmean"
  ) +
  theme_minimal()

# Combinar las dos gráficas en una grilla
grid_plot <- gridExtra::grid.arrange(biomasa_plot, temperatura_plot, ncol = 1)

# Mostrar la grilla combinada
print(grid_plot)



library(ggplot2)

# Gráfico de biomasa con boxplot
biomasa_boxplot <- ggplot(todo, aes(x = as.factor(Year), y = log10Biom)) +
  geom_boxplot(fill = "blue", color = "blue", alpha = 0.7) +
  labs(
    title = "ANEGADA - Variabilidad de Biomasa",
    x = "Year",
    y = "log10Biom (g m -2)"
  ) +
  theme_minimal()

# Gráfico de cambio de temperatura a lo largo de los años
temperatura_plot <- ggplot(todo, aes(x = Year, y = sstmean)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(
    title = "",
    x = "Year",
    y = "sstmean"
  ) +
  theme_minimal()

# Combinar los gráficos en una grilla
grid_plot <- gridExtra::grid.arrange(biomasa_boxplot, temperatura_plot, ncol = 1)

# Mostrar la grilla combinada
print(grid_plot)

library(ggplot2)


  
ggplot(data_prod_brut, aes(x = sstmean, y = log10Biom)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Agrega la línea de regresión
  labs(x = "sstmean", y = "log10Biom") +
  theme_minimal()


# correlaciones------------
correlation <- cor(data_prod_brut$log10Biom, data_prod_brut$Year)

ggplot(data_prod_brut, aes(x = Year, y = log10Biom)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    x = "Year",
    y = "log10Biom",
    title = paste("Correlación:", round(correlation, 2))
  ) +
  theme_minimal()







 # fabio ----------
correlation <- cor(data_prod_brut$Prod , data_prod_brut$Year)

ggplot(data_prod_brut, aes(x = Year, y = Prod )) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    x = "Year",
    y = "Prod",
    title = paste("Correlación:", round(correlation, 2))
  ) +
  theme_minimal()

str(data_prod_brut)

lagged <- data_prod_brut |> 
  mutate(TotalProd = Biom + Prod) |> 
  group_by(Year, Reef) |> 
  summarise(Biom = mean(Biom), 
            Prod = mean(Prod*365), 
            TotalProd = mean(TotalProd)) |> 
  arrange(Year, Reef) |> 
  group_by(Reef) |>
  mutate(lagged_TP = lag(TotalProd, n = 1)) |> 
  filter(!is.na(lagged_TP))


  ggplot(lagged, aes(x = Biom, y = lagged_TP)) +
  geom_point()+
  geom_smooth(method = "glm")


correlation <- cor(lagged$Biom, lagged$lagged_TP)

ggplot(lagged) +
  geom_point(aes(x = Biom, y = Prod), col = "red")
  # geom_point(aes(x = Year, y = lagged_TP), col = "blue")
  # geom_smooth(method = "glm")






# Imprimir el valor de la correlación
print(paste("Correlación entre log10Biom y sstmean:", correlation))

cor_matrix <- cor(todo[, c("log10Biom", "sstmean", "log10Prod", "log10ProdB" )])  # Agrega más variables si es necesario
print(cor_matrix)


# Calcula la correlación
correlation <- cor(byreef$Biom, dplyr::lag(byreef$Biom))

# Imprime la correlación
print(paste("Correlación entre Biomasa de un año y Biomasa del siguiente año en", byreef, ":", correlation))

# Crea un gráfico de dispersión con línea de regresión (opcional)
ggplot(byreef, aes(x = log10Biom, y = dplyr::lag(log10Biom))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste("Correlación en", byreef),
       x = "Biomasa del año actual",
       y = "Biomasa del año siguiente") +
  theme_minimal()




# Cargar las bibliotecas necesarias
library(ggpubr)

# Seleccionar las variables de interés para el análisis de correlación
variables_interes <- data_prod_brut %>% 
  select(Biomass, sstmean, sst_sd, sst_range, sst_max, sst_min)

# Calcular la matriz de correlación
correlaciones <- cor.test(variables_interes, method = "pearson")

# Imprimir los resultados
print(correlaciones)

# Visualizar las correlaciones con ggscatter
scatter_plot <- ggscatter(data_prod_brut, x = "Biomass", y = "sstmean", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Biomasa", ylab = "Temperatura media del mar",
                          font.x = 10, font.y = 10)
scatter_plot

