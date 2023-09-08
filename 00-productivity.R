library(devtools)
devtools::install_github("renatoamorais/rfishprod")
library(rfishprod)
library(dplyr)
library(tidyverse)
library(ramify)
library(ggplot2)
library(openxlsx)
library(patchwork)


# load data -----------------

tabla <- read.xlsx("data/spp_parametros_07092023.xlsx") |>
  filter(!is.na (Linf)) |>
  mutate(sstmean = 27) |> 
  mutate(LinfTL = ifelse(is.na(LinfTL), Linf, LinfTL))



fish <- readRDS("data/LTEM_historic_updated_27122022.RDS") |>
  # fish <- readRDS("data/LTEM_historic_updated_16082023.RDS") |>  
  mutate(Family = ifelse(Genus == "Rypticus", "Grammistidae", Family)) |> 
  filter(Label == "PEC") |> 
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
                        right = FALSE)) |> 
  mutate(Species = recode(Species, "Rypticus courtenayi   " = "Rypticus courtenayi",
                          "Carangoides orthogrammus" = "Ferdauia orthogrammus",
                          "Epinephelus acanthistius" = "Hyporthodus acanthistius",
                          "Zapterix exasperata" = "Zapteryx exasperata",
                          "Anisotremus caesius " = "Anisotremus caesius",
                          "Chromis atrilobata" ="Azurina atrilobata",
                          "Cirrhithus rivulatus" = "Cirrhitus rivulatus",
                          "Dasyatis dipterura" = "Hypanus dipterurus",
                          "Dasyatis longus" ="Hypanus longus",
                          "Hermosilla azurea" = "Kyphosus azureus",
                          "Kyphosus analogus" = "Kyphosus vaigiensis",
                          "Ostracion meleagris meleagris" ="Ostracion meleagris",
                          "Sectator ocyurus" = "Kyphosus ocyurus",
                          "Sargocentron suborbitalis" = "Sargocentron suborbitale",
                          "Rhinobatos productus" = "Pseudobatos productus"))


# Calcular Kmax

tabla <- tabla |> 
  mutate(Kmax = K * Linf / LinfTL) 


merged_data <- merge(fish, tabla, by = c("Family", "Species", "A_ord", "B_pen"), all.x = TRUE) |>  
  rename(Diet = TrophicGroup) |> 
  filter(!Size == 0) |> 
  filter(!is.na (Linf)) |> 
  filter(!is.na (Size)) |> 
  filter(!is.na (SpecCode)) |> 
  filter(!is.na (MaxSizeTL)) 



ltem_db <- merged_data |> 
  filter(Year == 2022, Region == "Loreto") |> 
  distinct(Family, Species, SpecCode, MaxSizeTL, A_ord, B_pen, Diet, 
           LinfTL, K, O, Kmax, Longitude, Latitude, sstmean, Method) 

ltem_repdata <- merged_data |> 
  filter(Year == 2022, Region == "Loreto") 

t <- 1


# Calcular la masa corporal individual -----------

biomasa <- ltem_repdata |> 
  mutate(Biomass = ((A_ord* (Size^B_pen))/1000) * Quantity) 


# Biomasa total del conjunto de peces (Standing biomass):

bt_ltem <- ltem_repdata |> 
  summarise(Bt = sum(Biomass)) 


# Predicts standardised growth parameter Kmax for reef fishes ----------------
# predKmax(traits, dataset, fmod, params = NULL, niter, nrounds = 150, verbose = 0, print_every = 1000, return = c('pred', 'relimp', 'models'), lowq = 0.25, uppq = 0.75) 

Kmaxpred_ltem <- predKmax(traits = ltem_repdata, dataset = ltem_db, fmod = ~ sstmean + MaxSizeTL + Diet + Method, niter = 1000)


# Acceder al elemento 'pred' de la lista Kmax
Kmax_pred_ltem <- Kmaxpred_ltem$pred 



# Applying_growth 
# Applies VBGF to fish length data ---------------------
# applyVBGF (Lmeas, t = 1, Lmax, Kmax, L0, t0, t0lowbound = -0.5,  silent = T)

longitud_ltem <-applyVBGF(Lmeas = ltem_repdata$Size, t = t, Lmax = ltem_repdata$MaxSizeTL, Kmax = Kmax_pred_ltem$Kmax)


Lgr <- longitud_ltem # Vector de tallas calculadas con VBGF
ctrgr(Lmeas = ltem_repdata$Size, Lgr, silent = FALSE) # Verificar si el crecimiento es válido

# Expected somatic growth in weight----------------
# somaGain (a, b, Lmeas, t = 1, Lmax, Kmax, t0, t0lowbound = -0.5,  silent = T)

pesos_ltem <- somaGain(a = ltem_repdata$A_ord, b = ltem_repdata$B_pen, Lmeas = ltem_repdata$Size, t = t, Lmax = ltem_repdata$MaxSizeTL, Kmax = Kmax_pred_ltem$Kmax)

#   Predicts natural mortality rates Z/M for reef fishes -----------
# predM (Lmeas, t = 1, Lmax, Kmax, temp, Lr, p = 0.5, exp = -0.91, method = c('Lorenzen'))

pred_mortalidad <- predM(Lmeas = ltem_repdata$Size, t = t, Lmax = ltem_repdata$MaxSizeTL, Kmax = Kmax_pred_ltem$Kmax, temp = ltem_repdata$sstmean, Lr, p = 0.5, exp = -0.91, method = 'Lorenzen')

M <- pred_mortalidad

# Applying_mortality
# Expected per capita loss due to natural mortality ---------------
# somaLoss (M, Wei, Lmeas, a, b, t = 1)

perdida_per_capita <- somaLoss(M = M, Lmeas = ltem_repdata$Size, a = ltem_repdata$A_ord, b = ltem_repdata$B_pen, t = t)

# Applies stochastic natural mortality -----------
# applyMstoch(M, t = 1) 

stochastic <- applyMstoch(M, t = t)
# applyMstoch(M, t) # Generar muestras aleatorias de sobrevivencia


# Productividad = Biomasa + Crecimiento Somático Total - Pérdidas debido a la Mortalidad ------

# Calcular el Crecimiento Somático Total (suma de ganancias de peso)
crecimiento_somatico <- sum(pesos_ltem)

# Calcular las Pérdidas debido a la Mortalidad (suma de pérdidas de peso por mortalidad)
perdidas_mortalidad <- sum(perdida_per_capita)


# Productividad -------
productividad_ltem <- (bt_ltem + crecimiento_somatico - perdidas_mortalidad)


print(productividad_ltem)


ltem_repdata$Kmax_pred <- Kmax_pred_ltem$Kmax
ltem_repdata$length <- longitud_ltem
ltem_repdata$weight <- pesos_ltem
ltem_repdata$M <- M
ltem_repdata$Perdida <- perdida_per_capita
ltem_repdata$Stochastic <- stochastic

pro_ltem <- ltem_repdata |> 
  mutate(Productivity = Biomass + weight - Perdida)

# ruta_archivo <- "data/fish_parameters_loreto_2022.xlsx"

# Exportar el dataframe a un archivo Excel
# write.xlsx(pro_ltem, file = ruta_archivo)


# Graficas ------------------


# Calcular las sumas de biomasa y productividad por sitio
sum_by_site <- pro_ltem |> 
  # filter(Genus%in%c("Mycteroperca","Scarus" )) |> 
  group_by(Year, Region, Reef, Transect) |> 
  mutate(total_biomass = sum(Biomass),
         total_productivity = sum(Productivity))


ggplot(data = sum_by_site, aes(x = total_biomass, y = total_productivity, color = Region)) +
  geom_point(size = 1, shape = 18) +
  geom_smooth(method="lm", se=F) +
  labs(title = "",
       x = "Biomass (kg/ha)", y = "Productivity (g ha^-1 day^-1)") +
  theme_classic()



# Biomass--------------


p1 <- ggplot(data = sum_by_site, aes(x = reorder(as.numeric(factor(Reef)), -total_biomass), y = total_biomass, color = Reef)) +
  # geom_jitter()+
  stat_summary(fun.data = "mean_se", geom = "point", size = 3, shape = 18, position = position_jitterdodge(jitter.width = 0.2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar") +
  labs(title = " ",
       x = "Reef", y = "Biomass (Kg/ha)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))



# Productividad --------

p2 <- ggplot(data = sum_by_site, aes(x =reorder(as.numeric(factor(Reef)), -total_biomass), y = total_productivity, color = Reef)) +
  stat_summary(fun.data = "mean_se", geom = "point", size = 3, shape = 18, position = position_jitterdodge(jitter.width = 0.2)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar") +
  labs(title = " ",
       x = "Reef", y = "Productivity (g ha^-1 day^-1)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))



p1 +p2 + plot_layout(guides = "collect")


# ggsave("figs/ltem_prod_98-99.png", width = 8.5, height = 4.5, dpi=1000)


