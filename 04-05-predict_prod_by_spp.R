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
  mutate(Lmax=MaxSizeTL) |> 
  filter(!is.na (Linf), !is.na (LinfTL))

tabla$Kmax <- tabla$K


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
spp <- merge_database |> 
  distinct(Species)

# Formula from Morais and Bellwood (2018) #
# fmod <- formula(~ sstmean + MaxSizeTL)
# fmod <- formula(~ sstmean + MaxSizeTL + Diet)
fmod <- formula(~ sstmean + MaxSizeTL + Diet + Method)
# fmod <- formula (~ sstmean + MaxSizeTL + Diet) #Nutrient_prod

ltem_db <- merge_database |> 
  distinct(Family, Species, SpecCode, MaxSizeTL, Diet, a, b, 
           LinfTL, K, Kmax, O, Longitude, Latitude, sstmean, Method) 

ltem_repdata <- merge_database |> 
  select(-Kmax)


# Predicting Kmax, the standardised VBGF parameter (Recommendation: use 100s to 1000s iterations) #
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

# calc_prod_rfishprod---------
datagr_prod = datagr %>%
  #Calculating biomass turnover-----------------
mutate(W = a*(Size^b),
       Biom = (W*Quantity),
       Prod = ifelse(mortality == T, (somatic_growth * Quantity),0))


# saveRDS(datagr_prod, "data/fish_datagr_prod-by-species-allyears.RDS")

datagr <- readRDS("data/fish_datagr_prod-by-species-allyears.RDS")

# datagr_prod <- datagr %>%
#   mutate(W = a * (Size^b),
#          Biom = W * Quantity,
#          Prod = ifelse(mortality == T, (somatic_growth * Quantity), 0)) %>%
#   group_by(Year, Protection_level, Region, Reef, Degree, Habitat, Depth2, Transect, Diet, Family, Species) %>%
#   summarise(Biom = sum(Biom) / sum(Area), #  (kg ha^−1)
#             Prod = sum(Prod) / sum(Area), # (g d^−1 ha^−1)
#             Productivity = (Prod / Biom) * 100) %>%
#   summarise(Biom = mean(Biom), # (kg ha^−1)
#             Prod = mean(Prod), # (g d^−1 ha^−1)
#             Productivity = mean(Productivity)) %>% # (% por día)
#   ungroup() 


# saveRDS(datagr_prod, "data/fish_data_prod-by-spp.RDS")

datagr <- readRDS("data/fish_datagr_prod-by-species-allyears.RDS")

datagr_prod <- datagr %>%
  mutate(W = a * (Size^b),
         Biom = W * Quantity,
         Prod = ifelse(mortality == T, (somatic_growth * Quantity), 0)) %>%
  group_by(Year, Region, Reef, Degree, Habitat, Depth2, Transect, Family, Species) %>%
  summarise(Biom = sum(Biom) / sum(Area), # (kg ha^−1)
            Prod = sum(Prod) / sum(Area), # (g d^−1 ha^−1)
            Productivity = (Prod / Biom) * 100,
            .groups = "drop") %>%
  group_by(Year, Region, Reef, Degree, Habitat, Depth2, Transect, Family, Species, .drop = TRUE) %>%
  summarise(Biom = mean(Biom), # Promedio de Biom
            Prod = mean(Prod), # Promedio de Prod
            Productivity = mean(Productivity),
            .groups = "drop") %>%
  ungroup() 

str(datagr_prod)


fish <- fishdata |> 
  distinct(species, commercial, trophic_level_f, trophic_level, functional_groups, diet, max_size_tl)

reef <- fishdata |> 
  distinct(reef, protection, latitude, longitude, id_reef, island)



#' #' #' Calculating productivity------------------
data_with_prod2 =  datagr |>
#' 
#Calculating production -----------------
#Weight of fish at time of census
mutate(W = a*Size^b,
       #Age of fish at time of census
       t = (1/Kmax)*log((MaxSizeTL)/((1-(Size/MaxSizeTL))*MaxSizeTL)),
       #Projected size one year later
       Ltx = MaxSizeTL * (1-exp(-Kmax*(t+365))),
       #Projected weight one year later-----------
       Wtx = a*Ltx**b,
       #Production
       Wgain = Wtx - W,
       #Biomass------
       # Biomass = (a * (Size^b) * Quantity) / (1000 * Area),
       Biom = (W*Quantity)/Area,
       #Individual Biomass
       IndBiom = (W/Area),
       #Production-----------
       # Prod = (Wgain * Biom)/Area,
       Prod = (Wgain * Quantity)/Area,
       #Individual production
       IndProd = (Wgain/Area)) %>%

  filter(!is.na(Prod))
#' 
#' 
#'  # At the scale of the community (transect)---------
#' data_prod_brut = datagr %>%
#'   #Sum for each transect
#'   group_by(Year, Island, Reef, Degree, Habitat, Depth2, Transect, Diet, Species) %>%
#'   mutate(
#'     Biom = sum(Biom)/Area,# (kg ha^−1) # porqué 500?
#'     Prod = sum(Prod)/Area,#g d^−1 ha^−1
#'     Productivity = (Prod/Biom)*100) |> 
#'   ungroup()
#' #Mean for each site
#' group_by(Year, Reef, Species) %>%
#' mutate(Biom = mean(Biom),
#'        Prod = mean(Prod),
#'        Productivity = mean(Productivity)) %>% 
#'   ungroup() %>%
#'   #Transforming data
#'   mutate(
#'     log10ProdB = Productivity,# % per day
#'     log10Biom = log10(Biom+1), #(g m -2)
#'     log10Prod = log10(Prod+1), # (g m-2 d-1)
#'     Latitude = as.numeric(as.character(Latitude)),
#'     Longitude = as.numeric(as.character(Longitude)))
#' 
#' str(data_prod_brut)



# saveRDS(data_prod_brut, "data/fishdata_productivity-by-reef-allyears.RDS")