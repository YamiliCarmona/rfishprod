# Moddelar un arrecife en el tiempo, como ha estado la produtividad en el tiempo, promedios anuales.


sitios <- readRDS("data/tabla/coordinates_of_rocky_reefs_consistently_monitored.RDS")

print(sitios, n = 43)


# Temperatura promedio por años por arrecife----------------
sst_points_resumen <- readRDS("sst_points_resumen.rds")

sst_points_resumen

# data fish ------------
fish <- readRDS("data/tabla/ltem_historic_updated_2024-01-15.RDS") |> 
  filter(Label == "PEC") |>
  filter(!Region%in% c ("Revillagigedo", "Islas Marias", "Ixtapa", "Huatulco", "Bahia Banderas")) |> 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen= as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size=as.numeric(Size),
    Area= as.numeric(Area),
    Month= as.numeric(Month),
    Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |> 
  # Biomass = (A_ord * (Size^B_pen) * Quantity) / (1000 * Area),
  # Biomass = ((A_ord* (Size^B_pen))/1000) * Quantity) |>
  mutate(
    Biomass=as.numeric(Biomass),
    TrophicGroup = factor(TrophicGroup, 
                          levels = c("Piscivoro", 
                                     "Carnivoro", 
                                     "Herbivoro", 
                                     "Zooplanctivoro")), 
    Region = factor(Region),
    TrophicLevelF = cut(as.numeric(TrophicLevel), 
                        breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                        labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                        right = FALSE))


# Select reefs -------

sitios <- sitios |> 
  mutate(Reef = recode(Reef,
                       "BALLENA" = "ESPIRITU_SANTO_BALLENA",
                       "ISLOTES_ESTE" ="ESPIRITU_SANTO_ISLOTES_ESTE",
                       "ISLOTES_NORTE" = "ESPIRITU_SANTO_ISLOTES_NORTE",
                       "PAILEBOTE" ="ESPIRITU_SANTO_PAILEBOTE",
                       "PARTIDA_NORESTE" = "ESPIRITU_SANTO_PARTIDA_NORESTE",
                       "PUNTA_LOBOS" = "ESPIRITU_SANTO_PUNTA_LOBOS"))

reef_select <- fish %>%
  filter(Reef %in% sitios$Reef)

reef <- reef_select |> 
  distinct(Reef)

unique(sitios$Reef)



# Tamaños maximos observados cada año ------------

TallaMax <- reef_select |> 
  # filter(!Year == 2023) |> 
  filter(!is.na(Size)) |> 
  group_by(Year, Reef, Species) |> 
  arrange(Size) |> 
  mutate(MaxSizeTL = max(Size)) |>
  ungroup()

MaxSize <- TallaMax |> 
   distinct(Year, Reef, Species, MaxSizeTL)


colnames(sst_points_resumen) <- c("Reef", "Longitude", "Latitude", "Year", "values")

sst_points_resumen$Year <- as.numeric(substring(sst_points_resumen$Year, 2))
str(TallaMax)


# Fusionar los datos basándote en las columnas "Reef" y "Year"
merged_data <- merge(TallaMax, sst_points_resumen[c("Reef", "Year", "values")], by = c("Reef", "Year"), all.x = TRUE)

merged_data <- merged_data %>%
  rename(sstmean = values)

print(merged_data)

