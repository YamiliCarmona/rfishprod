# Moddelar un arrecife en el tiempo, como ha estado la produtividad en el tiempo, promedios anuales.


sitios <- readRDS("data/tabla/coordinates_of_rocky_reefs_consistently_monitored.RDS")

print(sitios, n = 43)


# Temperatura promedio por años por arrecife----------------
# sst_points_resumen <- readRDS("sst_points_resumen.rds")
# 
sst_data <- readRDS("data/tabla/sst_for_reefs.RDS")

# data fish ------------
fish <- readRDS("data/tabla/ltem_historic_updated_2024-01-23.RDS") |> 
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

reefs <- reef_select |> 
  distinct(Year, IDReef, Reef, Longitude, Latitude)

unique(reefs$Reef)


# Tamaños maximos observados cada año ------------

TallaMaxsp <- reef_select |> 
  # filter(!Year == 2023) |> 
  filter(!is.na(Size)) |> 
  group_by(Year, Species) |> 
  arrange(Size) |> 
  mutate(MaxSizeTL = max(Size)) |>
  ungroup()

MaxSizesp <- TallaMax |> 
   distinct(Year, Reef, Species, MaxSizeTL)


# colnames(sst_mean) <- c("Reef", "Longitude", "Latitude", "Year", "values")

# sst_mean$Year <- as.numeric(substring(sst_mean$Year, 2))

sst_mean <- sst_data %>%
  rename(Year = year, sstmean= sst_avg)

str(merged_data)

# Fusionar los datos basándote en las columnas "Reef" y "Year"
newsites <- left_join(reefs, sst_mean, by = c("Reef", "Year"))

merged_data <- merge(TallaMaxsp, newsites, by = c("Region","Reef", "Year", "IDReef", "Longitude", "Latitude"), all.x = TRUE)

print(merged_data)

arrecifes <- merged_data |> 
  distinct(Region, Reef, Habitat)
  
tablita <- merge_database |> 
  distinct(Year, IDReef, Reef, Longitude, Latitude, Species, MaxSizeTL, sstmean)


sj <- merged_data |> 
  filter(Year == 2014, Reef == "SAN_JOSE_ANIMAS_PINACULOS")
  select(-MaxSizeTL) 
  
  
merged_data <- merged_data |> 
  mutate(Habitat = ifelse(Reef == "ISLOTE_CABO_PULMO" & Year == 2021 & Depth == 5 & Habitat == "HABITAT", "PARED", Habitat))

saveRDS(merged_data, "data/tabla/fishdata_sstmean_maxsize_by_year.RDS")
