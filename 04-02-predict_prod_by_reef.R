library(readxl)
library(devtools)
library(rfishprod)
library(dplyr)
library(tidyverse)
library(ramify)
library(ggplot2)
library(openxlsx)
library(patchwork)
library(harrypotter)
library(ggpubr)
library(ggrepel)


tabla <- read_excel("data/tabla/spp_parametros_20231201.xlsx")|> 
  # select(-MaxSizeTL) |>
  filter(!is.na (Linf), !is.na (LinfTL))

t <- 1

merged_data <- readRDS("data/tabla/fishdata_sstmean_maxsize_by_year.RDS") |> 
  select(-MaxSizeTL)

# merge ----------

merge_database <- merge(merged_data, tabla, by = c("Family", "Species", "A_ord", "B_pen"), all.x = TRUE)  |> 
  rename(Diet = TrophicGroup, a = "A_ord",
         b = "B_pen") |> 
  #Remove sharks rays and seahorses
  dplyr::filter(Taxa2 == "Actinopterygii" & Taxa2 != "Syngnathidae") |>
  filter(Family != "Anguillidae", Family != "Congridae", Family != "Muraenidae", Family != "Ophichthidae") |>
  #Removing small crypto
  filter(Family != "Chironemidae", Family != "Pinguipedidae", Family != "Chaenopsidae", Family != "Tripterygiidae",
         Family != "Gobiidae", Family != "Callionymidae", Family != "Blenniidae",
         Family != "Labrisomidae", Family != "Microdesmidae", Family != "Pholidichthyidae") |>
  # filtering to keep fish larger than 5cm and fish of 5cm if Maxlength <25cm
  filter (Size != 5 | (Size==5 & MaxSizeTL < 25)) |> 
  filter(!Size == 0) |>
  filter(!is.na (Linf)) |>
  filter(!is.na (SpecCode)) |> 
  filter(!is.na(sstmean))

str(merge_database)

# Formula from Morais and Bellwood (2018) #
# fmod <- formula(~ sstmean + MaxSizeTL)
# fmod <- formula(~ sstmean + MaxSizeTL + Diet)
fmod <- formula(~ sstmean + MaxSizeTL + Diet + Method)


ltem_db <- merge_database |> 
  distinct(Family, Species, SpecCode, MaxSizeTL, a, b, Diet, 
           LinfTL, K, Kmax, O, Longitude, Latitude, sstmean, Method) 

ltem_repdata <- merge_database |> 
  select(-Kmax)


# Predecir Kmax. Si  no me deja correr la función sin la columna Kmax, que datos debo poner? se calcula aparte?
# usé los datos de K
datagr <- rfishprod::predKmax(ltem_repdata,
                              dataset = ltem_db, 
                              fmod = fmod,
                              niter = 1000,
                              return = 'pred')

datagr <- datagr$pred

# Predicting M/Z: the instantaneous mortality rate (Recommendation: see help file for) #
datagr$Md <- with (datagr, rfishprod::predM(Lmeas = Size,
                                            Lmax = MaxSizeTL,
                                            Kmax = Kmax,
                                            t = 1,
                                            method = 'Gislason'))

# Positioning your fish in their growth trajectory #
# aka. what's the size they're supposed to have on the next day? #
datagr$Size_nextday= with(datagr, rfishprod::applyVBGF (Lmeas = Size,
                                                        Lmax = MaxSizeTL,
                                                        Kmax = Kmax,
                                                        t = 1))

# Estimating gross somatic growth (g) #
datagr$somatic_growth = with(datagr, rfishprod::somaGain (a = a,
                                                          b = b,
                                                          Lmeas = Size,
                                                          Lmax = MaxSizeTL,
                                                          Kmax = Kmax,
                                                          t = 1))

range(datagr$somatic_growth)

# Applying stochastic mortality #
datagr$mortality = rfishprod::applyMstoch(datagr$Md,t=1)


#Alternatively, estimating per capita mass loss due to mortality #
datagr$soma_loss = with(datagr, rfishprod::somaLoss (M = Md,
                                                     Lmeas = Size,
                                                     a = a,
                                                     b = b,
                                                     t = 1))


datagr_prod = datagr %>%
  #Calculating biomass turnover-----------------
mutate(W = a*(Size^b),
       Biom = (W*Quantity),
       Prod = ifelse(mortality == T, (somatic_growth * Quantity),0))


#' #' Calculating productivity------------------
#' 
# data_with_prod =  datagr |>
#   #Calculating production -----------------
#   #Weight of fish at time of census
#   mutate(W = a*Size^b,
#          #Age of fish at time of census
#          t = (1/Kmax)*log((MaxSizeTL)/((1-(Size/MaxSizeTL))*MaxSizeTL)),
#          #Projected size one year later
#          Ltx = MaxSizeTL * (1-exp(-Kmax*(t+365))),
#          #Projected weight one year later
#          Wtx = a*Ltx**b,
#          #Production
#          Wgain = Wtx - W,
#          #Biomass------
#          # Biomass = (a * (Size^b) * Quantity) / (1000 * Area),
#          Biom = (W*Quantity)/Area,
#          #Individual Biomass
#          IndBiom = (W/Area),
#          #Production-----------
#          # Prod = (Wgain * Biom)/Area,
#          Prod = (Wgain * Quantity)/Area,
#          #Individual production
#          IndProd = (Wgain/Area)) %>%
# 
#   filter(!is.na(Prod))

# At the scale of the community (transect)---------
data_prod_brut = datagr_prod %>%
  #Sum for each transect
  group_by(Year,Reef,Depth2,Transect) %>%
  mutate(
    Biom = sum(Biom)/Area,# (kg ha^−1) # porqué 500?
    Prod = sum(Prod)/Area,#g d^−1 ha^−1
    Productivity = (Prod/Biom)*100) %>%
  ungroup() |> 
#Mean for each site
group_by(Year, Reef) %>%
mutate(Biom = mean(Biom),
       Prod = mean(Prod),
       Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  #joinin with transect data
  # dplyr::select(IDReef, Reef, Biom, Prod, Productivity) %>%
  # distinct(IDReef, .keep_all = T) %>%
  # left_join(transect_info, by ="IDReef") %>%
  #Transforming data
  mutate(
    log10ProdB = Productivity,# % per day
    log10Biom = log10(Biom+1), #(g m -2)
    log10Prod = log10(Prod+1), # (g m-2 d-1)
    Latitude = as.numeric(as.character(Latitude)),
    Longitude = as.numeric(as.character(Longitude)))
# dplyr::select(Year, Degree, Region, IDReef, Reef, Habitat, Transect, Depth, Depth2, sstmean, Biom, Prod, Productivity, log10ProdB, log10Biom, log10Prod) %>%
# distinct(Year, Degree, Region, IDReef, Reef, Habitat, Transect, Depth, Depth2, sstmean, Biom, Prod, Productivity, log10ProdB, log10Biom, log10Prod, .keep_all = T)

# dplyr::rename(site_code = IDReef) |>
# dplyr::select(IDReef, Reef, Biom, Prod, Productivity, Transect, Latitude, Longitude, Depth, Region, log10ProdB, log10Biom, log10Prod) %>%
# distinct(Reef,Depth,Transect, .keep_all = T)
str(data_prod_brut)

# saveRDS(data_prod_brut, "data/fishdata_prod-by-reef-allyears_1000.RDS")



print(paste("Minimum biomass:",min(data_prod_brut$Biom)))
print(paste("Maximum biomass:", max(data_prod_brut$Biom)))
print(paste("Minimum production:",min(data_prod_brut$Prod)))
print(paste("Maximum production:", max(data_prod_brut$Prod)))
print(paste("Minimum turnover:",min(data_prod_brut$Productivity)))
print(paste("Maximum turnover:", max(data_prod_brut$Productivity)))

# data_management---------------
biom75 = quantile(data_prod_brut$log10Biom,0.95)
biom25 = quantile(data_prod_brut$log10Biom,0.25)
prod75 = quantile(data_prod_brut$log10ProdB,0.75)
prod25 = quantile(data_prod_brut$log10ProdB,0.25)
max_biom = max(data_prod_brut$log10Biom)


#Diving data into 3 classes for each biomass/productivity relationship
management = data_prod_brut %>% 
  
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                        ifelse(log10ProdB > prod75,"partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
  mutate(Class = as.factor(Class))

range(management$Productivity)
range(management$Biom)
range(management$Prod)


unique(management$Region)
# plot by class--------------
#eje y = Biomass turnover, P/B × 100 (% per day)
# eje x <- log[standing biomass (g m–2)]
# filll = Class (Low biomass/turnover High turnover High biomass Mid-range)
# Class=c("deadzone"="Low biomass/turnover","partial"="High turnover","pristine"="High biomass", "transition" = "transition")


unique_points <- management[!duplicated(management$Reef), ]

ggplot(management, aes(x = log10Biom, y = log10ProdB, fill = Class)) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("deadzone" = "#FF9900", "partial" = "#3366CC", "pristine" = "#33CC33", "transition" = "#9900CC"),
                    labels = c("deadzone" = "Low biomass/turnover", "partial" = "High turnover", "pristine" = "High biomass", "transition" = "Transition")) +
  labs(x = "log(standing biomass (g m^-2))", y = "Biomass Turnover (P/B × 100 % per day)") +
  theme_minimal() #+
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 1.5, max.overlaps = 300)


# ggsave("figs/ltem_fish_prod_2010.png", width = 8.5, height = 4.5, dpi=1000)
