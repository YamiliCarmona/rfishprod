
# Biomass & productivity per year 
# sumar la biomasa total
# sumar la productividad a las biomasas
# asociar la prod a la temperatura
# Correlacionar la biomasa total


datayear <- readRDS("data/datafishes-year.RDS") 
  # filter(Year == 2021) |> 
  # distinct(Reef, Species, Quantity)
data2 <- readRDS("data/datafishes2.RDS")

str(datayear)

# Sumar la biomasa total y la productividad por año
sitio <- datayear |>
  # distinct(Reef, sstmean)
  filter(Reef == "ANEGADA") |>
  group_by(Year, Reef) %>%
  summarize(TotalBiom = sum(Biom),
            TotalProd = sum(Prod),
            IndBiomT = sum(IndBiom))



# Gráfica de biomasa
ggplot(sitio, aes(x = Year, y = TotalBiom)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = " ",
    x = "Year",
    y = ""
  ) +
  theme_minimal()


# Calcular el promedio de temperatura por año y por sitio
# sstyear <- sst_resumen %>%
# group_by(Year, Reef) 

# Gráfica de cambio de temperatura a lo largo de los años
ggplot(lobera_cp, aes(x = Year, y = sstmean)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Temperatura",
    x = "Year",
    y = "Temperatura (sstmean)"
  ) +
  theme_minimal()