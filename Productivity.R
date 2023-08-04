# load 

library(dplyr)
library(tidyverse)
library(ramify)
library(ggplot2)

# load data -----------------

fish_ltem <- readRDS("data/LTEM_historic_updated_27122022.RDS") |> 
  filter(Region == "La Paz", Reef=="ESPIRITU_SANTO_ISLOTES_ESTE", Label == "PEC") |>  
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen= as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size=as.numeric(Size),
    Area= as.numeric(Area),
    Month= as.numeric(Month),
    Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |>  
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

# fish_ltem <- fish_ltem |> 
#   mutate(
#     SpecCode = case_when(
#       Species == "Acanthurus xanthopterus" ~ 1261,
#       Species == "Anisotremus interruptus" ~ 8253,
#       Species == "Balistes polylepis" ~ 4276,
#       Species == "Caranx caballus" ~ 1900,
#       Species == "Caranx sexfasciatus" ~ 1917,
#       Species == "Cephalopholis panamensis" ~ 8761,
#       Species == "Decapterus macarellus" ~ 993,
#       Species == "Epinephelus labriformis" ~ 8196,
#       Species == "Haemulon sexfasciatum" ~ 13718,
#       Species == "Holacanthus passer" ~ 8294,
#       Species == "Lutjanus argentiventris" ~ 1408,
#       Species == "Mycteroperca rosacea" ~ 9398,
#       Species == "Scarus ghobban" ~ 5548,
#       Species == "Scarus rubroviolaceus" ~ 5555,
#       Species == "Stegastes flavilatus" ~ 12512,
#       Species == "Stegastes rectifraenum" ~ 12516
#     ),
#     Diet = case_when(
#       Species == "Acanthurus xanthopterus" ~ "HerDet",
#       Species == "Anisotremus interruptus" ~ "InvMob",
#       Species == "Balistes polylepis" ~ "InvMob",
#       Species == "Caranx caballus" ~ "FisCep",
#       Species == "Caranx sexfasciatus" ~ "InvMob",
#       Species == "Cephalopholis panamensis" ~ "FisCep",
#       Species == "Decapterus macarellus" ~ "Plktiv",
#       Species == "Epinephelus labriformis" ~ "FisCep",
#       Species == "Haemulon sexfasciatum" ~ "BtPlDw",
#       Species == "Holacanthus passer" ~ "InvSes",
#       Species == "Lutjanus argentiventris" ~ "InvMob",
#       Species == "Mycteroperca rosacea" ~ "InvMob",
#       Species == "Scarus ghobban" ~ "HerDet",
#       Species == "Scarus rubroviolaceus" ~ "HerDet",
#       Species == "Stegastes flavilatus" ~ "Omnivr",
#       Species == "Stegastes rectifraenum" ~ "Omnivr"
#     ),
#     Position = case_when(
#       Species == "Acanthurus xanthopterus" ~ "BtPlDw",
#       Species == "Anisotremus interruptus" ~ "BtPlDw",
#       Species == "Balistes polylepis" ~ "BtPlAs",
#       Species == "Caranx caballus" ~ "PelgAs",
#       Species == "Caranx sexfasciatus" ~ "PelgAs",
#       Species == "Cephalopholis panamensis" ~ "BnthDw",
#       Species == "Decapterus macarellus" ~ "PelgAs",
#       Species == "Epinephelus labriformis" ~ "BnthDw",
#       Species == "Haemulon sexfasciatum" ~ "BtPlDw",
#       Species == "Holacanthus passer" ~ "BnthDw",
#       Species == "Lutjanus argentiventris" ~ "BtPlDw",
#       Species == "Mycteroperca rosacea" ~ "BnthDw",
#       Species == "Scarus ghobban" ~ "BtPlDw",
#       Species == "Scarus rubroviolaceus" ~ "BtPlDw",
#       Species == "Stegastes flavilatus" ~ "BnthDw",
#       Species == "Stegastes rectifraenum" ~ "BnthDw"
#     )
#   )

# SpecCode, Diet, Position, Method
fish_ltem <- merge(fish_ltem, db[, c('Species', 'SpecCode', 'Diet', 'Position', 'Method')],
                   by = 'Species', all.x = TRUE, suffixes = c("", ".db"))

# Si hay columnas duplicadas después de la combinación, eliminar las duplicadas de .db
duplicated_columns <- names(fish_ltem)[duplicated(names(fish_ltem))]
fish_ltem <- fish_ltem[, !grepl("\\.db$", names(fish_ltem)) | !names(fish_ltem) %in% duplicated_columns]

# Si hay columnas duplicadas después de la combinación, renombrar las columnas en .db
for (col in setdiff(names(fish_ltem), names(db))) {
  if (grepl("\\.db$", col)) {
    new_colname <- gsub("\\.db$", "", col)
    names(fish_ltem)[names(fish_ltem) == col] <- new_colname
  }
}

# Tallas maximas
fish_ltem <- fish_ltem |> 
  group_by(Species) |> 
  mutate(MaxSizeTL = max(Size)) |> 
  ungroup()
# 
# Lmeas <- fish_ltem$Size # Vector de tallas iniciales
# a <- fish_ltem$A_ord
# b <- fish_ltem$B_pen
t <- 365

# sstmean 
fish_ltem <- fish_ltem |> 
  filter(!is.na(SpecCode)) |> 
  mutate(sstmean = 25.5) |> 
  select(Family, Species, SpecCode, Size, MaxSizeTL, Diet, Position, A_ord, B_pen, Longitude, Latitude, sstmean)

# Calcular la masa corporal individual -----------

fish_ltem <- fish_ltem |> 
  # rename(Lmeas = Size, a =  A_ord , b = B_pen ) |> 
  mutate(Mti = A_ord * (Size ^ B_pen))
  # mutate(Mti = a * (Lmeas ^ b))

# Biomasa total del conjunto de peces (Biomasa en pie):

biomasa_total <- fish_ltem |> 
  summarise(B_t = sum(Mti))

# Predicts standardised growth parameter Kmax for reef fishes ----------------
# predKmax(traits, dataset, fmod, params = NULL, niter, nrounds = 150, verbose = 0, print_every = 1000, return = c('pred', 'relimp', 'models'), lowq = 0.25, uppq = 0.75) 

Kmaxpred <- predKmax(traits = fish_ltem, fmod = ~ MaxSizeTL + sstmean + Diet + Position, niter = 1000)

# Acceder al elemento 'pred' de la lista Kmax
Kmax_pred <- Kmaxpred$pred

# Kmax_pred <- Kmax_pred |> 
#   select(Family, Species, SpecCode, Size, MaxSizeTL, Diet, Position, A_ord, B_pen, Longitude, Latitude, sstmean, Kmax)

# Usar la función predM con el método 'Pauly' para obtener las estimaciones de t0 (a0)

a0_estimates <- predM(Lmeas = fish_ltem$Size, Lmax = fish_ltem$MaxSizeTL, Kmax = Kmax_pred$Kmax, temp = fish_ltem$sstmean, method = 'Pauly')

# Applying_growth 
# Applies VBGF to fish length data ---------------------
# applyVBGF (Lmeas, t = 1, Lmax, Kmax, L0, t0, t0lowbound = -0.5,  silent = T)

resultado_longitud <- applyVBGF(Lmeas = fish_ltem$Size, t = t, Lmax = fish_ltem$MaxSizeTL, Kmax = Kmax_pred$Kmax, t0 = a0_estimates)

Lgr <- resultado_longitud # Vector de tallas calculadas con VBGF
ctrgr(Lmeas = fish_ltem$Size, Lgr, silent = FALSE) # Verificar si el crecimiento es válido

# Expected somatic growth in weight----------------
# somaGain (a, b, Lmeas, t = 1, Lmax, Kmax, t0, t0lowbound = -0.5,  silent = T)

resultado_pesos <- somaGain(a = fish_ltem$A_ord, b = fish_ltem$B_pen, Lmeas = fish_ltem$Size, t = t, Lmax = fish_ltem$MaxSizeTL, Kmax = Kmax_pred$Kmax, t0 = a0_estimates)

#   Predicts natural mortality rates Z/M for reef fishes -----------
# predM (Lmeas, t = 1, Lmax, Kmax, temp, Lr, p = 0.5, exp = -0.91, method = c('Lorenzen'))

predicciones_mortalidad <- predM(Lmeas = fish_ltem$Size, t = t, Lmax = fish_ltem$MaxSizeTL, Kmax = fish_ltem$MaxSizeTL, temp = fish_ltem$sstmean, Lr, p = 0.5, exp = -0.91, method = 'Lorenzen')

M <- predicciones_mortalidad 

# Applying_mortality
# Expected per capita loss due to natural mortality ---------------
# somaLoss (M, Wei, Lmeas, a, b, t = 1)

perdida_per_capita <- somaLoss(M = M, Lmeas = fish_ltem$Size, a = fish_ltem$A_ord, b = fish_ltem$B_pen, t = t)

# Applies stochastic natural mortality -----------
# applyMstoch(M, t = 1) 

stochastic <- applyMstoch(M, t = t)
# applyMstoch(M, t) # Generar muestras aleatorias de sobrevivencia


# Productividad = Crecimiento Somático Total - Pérdidas debido a la Mortalidad ------

# Calcular el Crecimiento Somático Total (suma de ganancias de peso)
crecimiento_somatico_total <- sum(resultado_pesos)

# Calcular las Pérdidas debido a la Mortalidad (suma de pérdidas de peso por mortalidad)
perdidas_mortalidad <- sum(perdida_per_capita)

# Calcular la Productividad
productividad <- crecimiento_somatico_total - perdidas_mortalidad


print(productividad)




# Crear un dataframe con las tallas observadas y calculadas
data_plot <- data.frame(
  Lmeas = fish_ltem$Size,
  Lgr = Lgr
)


# Graficar las tallas observadas y calculadas
ggplot(data_plot, aes(x = Lmeas, y = Lgr)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Talla Observada (Lmeas)", y = "Talla Calculada (Lgr)") +
  theme_minimal()


# Crear un dataframe con los tamaños máximos y la temperatura del agua
data_maxsize_temp <- data.frame(
  TamanoMaximo = fish_ltem$MaxSizeTL,
  TemperaturaAgua = fish_ltem$sstmean
)

# Gráfica de dispersión del tamaño máximo vs. temperatura del agua
ggplot(data_maxsize_temp, aes(x = TemperaturaAgua, y = TamanoMaximo)) +
  geom_point(color = "purple") +
  labs(x = "Temperatura del Agua", y = "Tamaño Máximo") +
  theme_minimal()

